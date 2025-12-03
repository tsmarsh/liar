## Summary

Build proper REPL infrastructure with incremental JIT compilation and nREPL server for IDE integration.

**Depends on:** jdbem (codegen emits lIR AST)

## Goals

1. **Incremental JIT** - Definitions compile once, persist in memory, expressions reference them
2. **nREPL server** - Standard protocol for IDE integration (Calva, CIDER, Emacs, etc.)
3. **Context preservation** - Session state accumulates across evaluations
4. **Fast feedback** - No recompiling everything on each input

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     IDE / Editor                         │
│            (VS Code + Calva, Emacs + CIDER)             │
└─────────────────────┬───────────────────────────────────┘
                      │ TCP (bencode)
┌─────────────────────▼───────────────────────────────────┐
│                   nREPL Server                           │
│  - Session management                                    │
│  - Message routing                                       │
│  - Bencode encode/decode                                 │
└─────────────────────┬───────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────┐
│                    Session                               │
│  - Namespace/context                                     │
│  - Accumulated definitions                               │
│  - Symbol table                                          │
└─────────────────────┬───────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────┐
│               Incremental Evaluator                      │
│  - liar compiler                                         │
│  - OrcJIT engine (persistent)                           │
│  - Add definitions without recompiling                   │
└─────────────────────┬───────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────┐
│                    OrcJIT                                │
│  - JITDylib per session                                 │
│  - Symbol resolution across modules                      │
│  - Lazy compilation                                      │
└─────────────────────────────────────────────────────────┘
```

## Component 1: OrcJIT Wrapper

LLVM's OrcJIT (On-Request Compilation JIT) supports incremental compilation. Inkwell has basic support, but we may need lower-level LLVM bindings.

**lir-codegen/src/orc.rs:**
```rust
//! OrcJIT wrapper for incremental compilation
//!
//! Unlike the simple JIT which consumes a module, OrcJIT allows
//! adding new modules over time while preserving symbol resolution.

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::orc::{JITDylib, LLJIT, LLJITBuilder};
use std::collections::HashMap;

pub struct IncrementalJit {
    context: Context,
    jit: LLJIT,
    /// Main dylib where all symbols live
    main_dylib: JITDylib,
    /// Track defined symbols for lookup
    symbols: HashMap<String, SymbolInfo>,
}

pub struct SymbolInfo {
    pub name: String,
    pub kind: SymbolKind,
    pub address: Option<u64>,
}

pub enum SymbolKind {
    Function { params: Vec<Type>, ret: Type },
    Global { ty: Type },
}

impl IncrementalJit {
    pub fn new() -> Result<Self, String> {
        let context = Context::create();
        let jit = LLJITBuilder::new()
            .build()
            .map_err(|e| format!("Failed to create LLJIT: {}", e))?;
        
        let main_dylib = jit.get_main_jit_dylib();
        
        Ok(Self {
            context,
            jit,
            main_dylib,
            symbols: HashMap::new(),
        })
    }
    
    /// Add a compiled module to the JIT
    /// New symbols become available for future compilations
    pub fn add_module(&mut self, module: Module) -> Result<(), String> {
        // Add module to main dylib
        self.jit
            .add_module(&self.main_dylib, module)
            .map_err(|e| format!("Failed to add module: {}", e))?;
        
        // Track new symbols
        // (extract from module before adding)
        
        Ok(())
    }
    
    /// Look up a symbol's address
    pub fn lookup(&self, name: &str) -> Result<u64, String> {
        self.jit
            .lookup(name)
            .map_err(|e| format!("Symbol not found: {}", e))
            .map(|addr| addr as u64)
    }
    
    /// Compile and add a function definition
    pub fn define_function(&mut self, func: &lir_core::ast::FunctionDef) -> Result<(), String> {
        // Create a new module for this function
        let module = self.context.create_module(&func.name);
        
        // Compile function into module
        let codegen = CodeGen::new(&self.context, &module);
        codegen.compile_function(func)?;
        
        // Add to JIT
        self.add_module(module)?;
        
        // Track symbol
        self.symbols.insert(func.name.clone(), SymbolInfo {
            name: func.name.clone(),
            kind: SymbolKind::Function {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                ret: func.return_type.clone(),
            },
            address: None, // Lazy - resolved on first call
        });
        
        Ok(())
    }
    
    /// Evaluate an expression by wrapping in a thunk
    pub fn eval_expr(&mut self, expr: &lir_core::ast::Expr) -> Result<Value, String> {
        // Create unique thunk name
        let thunk_name = format!("__eval_{}", self.eval_counter);
        self.eval_counter += 1;
        
        // Wrap expression in a no-arg function
        let thunk = lir_core::ast::FunctionDef {
            name: thunk_name.clone(),
            params: vec![],
            return_type: infer_type(expr),
            body: expr.clone(),
        };
        
        // Compile and add
        self.define_function(&thunk)?;
        
        // Look up and call
        let addr = self.lookup(&thunk_name)?;
        self.call_thunk(addr, &thunk.return_type)
    }
    
    /// Call a no-arg function at address, interpret result as given type
    fn call_thunk(&self, addr: u64, ret_type: &Type) -> Result<Value, String> {
        // Transmute address to function pointer and call
        // This is unsafe but that's JIT life
        unsafe {
            match ret_type {
                Type::I64 => {
                    let f: extern "C" fn() -> i64 = std::mem::transmute(addr);
                    Ok(Value::I64(f()))
                }
                Type::Double => {
                    let f: extern "C" fn() -> f64 = std::mem::transmute(addr);
                    Ok(Value::Double(f()))
                }
                Type::I1 => {
                    let f: extern "C" fn() -> bool = std::mem::transmute(addr);
                    Ok(Value::I1(f()))
                }
                // ... other types
                _ => Err(format!("Unsupported return type: {:?}", ret_type)),
            }
        }
    }
}
```

## Component 2: Session Context

Each REPL/nREPL session has isolated state:

**liar-repl/src/session.rs:**
```rust
//! Session management for REPL
//!
//! A session accumulates definitions and maintains JIT state.

use lir_codegen::orc::IncrementalJit;
use std::collections::HashMap;

pub struct Session {
    id: String,
    jit: IncrementalJit,
    /// Source of all definitions (for re-compilation if needed)
    definitions_source: String,
    /// Parsed definitions for lookup/completion
    definitions: HashMap<String, Definition>,
    /// Current namespace (for future namespacing support)
    namespace: String,
}

pub struct Definition {
    pub name: String,
    pub kind: DefKind,
    pub source: String,
    pub doc: Option<String>,
}

pub enum DefKind {
    Function { params: Vec<Param>, ret_type: Option<String> },
    Constant { value_type: Option<String> },
    Struct { fields: Vec<Field> },
    Protocol { methods: Vec<MethodSig> },
}

impl Session {
    pub fn new(id: String) -> Result<Self, String> {
        Ok(Self {
            id,
            jit: IncrementalJit::new()?,
            definitions_source: String::new(),
            definitions: HashMap::new(),
            namespace: "user".to_string(),
        })
    }
    
    /// Evaluate input, returning result or error
    pub fn eval(&mut self, input: &str) -> EvalResult {
        let input = input.trim();
        
        // Parse to determine what kind of input
        match self.classify_input(input) {
            InputKind::Definition(def) => self.add_definition(def),
            InputKind::Expression(expr) => self.eval_expression(expr),
            InputKind::Command(cmd) => self.handle_command(cmd),
        }
    }
    
    fn add_definition(&mut self, source: &str) -> EvalResult {
        // Parse liar
        let ast = liar::parse(source)?;
        
        // Type check with existing context
        liar::check_with_context(&ast, &self.definitions)?;
        
        // Generate lIR AST
        let lir = liar::codegen(&ast)?;
        
        // Add to JIT
        for func in &lir.functions {
            self.jit.define_function(func)?;
        }
        
        // Track definition
        self.definitions_source.push_str(source);
        self.definitions_source.push('\n');
        // ... extract and store Definition ...
        
        EvalResult::Defined(extract_name(source))
    }
    
    fn eval_expression(&mut self, source: &str) -> EvalResult {
        // Parse as expression
        let expr = liar::parse_expr(source)?;
        
        // Type check with context
        let expr_type = liar::infer_expr_with_context(&expr, &self.definitions)?;
        
        // Generate lIR expression
        let lir_expr = liar::codegen_expr(&expr)?;
        
        // Evaluate
        let value = self.jit.eval_expr(&lir_expr)?;
        
        EvalResult::Value(format_value(&value, &expr_type))
    }
    
    /// Get completions for a prefix
    pub fn completions(&self, prefix: &str) -> Vec<Completion> {
        self.definitions
            .keys()
            .filter(|name| name.starts_with(prefix))
            .map(|name| Completion {
                candidate: name.clone(),
                kind: self.definitions[name].kind.to_string(),
            })
            .collect()
    }
    
    /// Look up documentation for a symbol
    pub fn lookup(&self, symbol: &str) -> Option<&Definition> {
        self.definitions.get(symbol)
    }
}

pub enum EvalResult {
    Value(String),
    Defined(String),
    Error(String),
    Interrupted,
}
```

## Component 3: nREPL Server

**liar-nrepl/Cargo.toml:**
```toml
[package]
name = "liar-nrepl"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "liar-nrepl"
path = "src/main.rs"

[dependencies]
liar = { path = "../liar" }
liar-repl = { path = "../liar-repl" }
lir-core = { path = "../lir-core" }
lir-codegen = { path = "../lir-codegen" }
bencode = "0.3"
tokio = { version = "1", features = ["full"] }
uuid = { version = "1", features = ["v4"] }
```

**liar-nrepl/src/main.rs:**
```rust
//! nREPL server for liar
//!
//! Implements the nREPL protocol for IDE integration.
//! https://nrepl.org/nrepl/building_servers.html

use bencode::{Bencode, Decoder, Encoder};
use liar_repl::Session;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use uuid::Uuid;

type Sessions = Arc<Mutex<HashMap<String, Session>>>;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let port = std::env::var("NREPL_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(7888);
    
    let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).await?;
    println!("nREPL server started on port {}", port);
    
    // Write .nrepl-port file for IDE discovery
    std::fs::write(".nrepl-port", port.to_string())?;
    
    let sessions: Sessions = Arc::new(Mutex::new(HashMap::new()));
    
    loop {
        let (socket, _) = listener.accept().await?;
        let sessions = Arc::clone(&sessions);
        tokio::spawn(async move {
            if let Err(e) = handle_connection(socket, sessions).await {
                eprintln!("Connection error: {}", e);
            }
        });
    }
}

async fn handle_connection(
    mut socket: TcpStream,
    sessions: Sessions,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = vec![0u8; 65536];
    
    loop {
        let n = socket.read(&mut buf).await?;
        if n == 0 {
            break;
        }
        
        // Decode bencode message
        let msg = Decoder::decode(&buf[..n])?;
        let request = parse_request(&msg)?;
        
        // Handle request
        let responses = handle_request(&request, &sessions)?;
        
        // Send responses
        for response in responses {
            let encoded = Encoder::encode(&response)?;
            socket.write_all(&encoded).await?;
        }
    }
    
    Ok(())
}

struct Request {
    id: String,
    op: String,
    session: Option<String>,
    code: Option<String>,
    // ... other fields
}

fn handle_request(req: &Request, sessions: &Sessions) -> Result<Vec<Bencode>, String> {
    match req.op.as_str() {
        "clone" => handle_clone(req, sessions),
        "close" => handle_close(req, sessions),
        "eval" => handle_eval(req, sessions),
        "load-file" => handle_load_file(req, sessions),
        "completions" => handle_completions(req, sessions),
        "lookup" => handle_lookup(req, sessions),
        "interrupt" => handle_interrupt(req, sessions),
        "describe" => handle_describe(req),
        _ => Ok(vec![error_response(&req.id, "unknown op")]),
    }
}

fn handle_clone(req: &Request, sessions: &Sessions) -> Result<Vec<Bencode>, String> {
    let session_id = Uuid::new_v4().to_string();
    let session = Session::new(session_id.clone())?;
    
    sessions.lock().unwrap().insert(session_id.clone(), session);
    
    Ok(vec![response(&req.id, vec![
        ("new-session", Bencode::String(session_id)),
        ("status", Bencode::List(vec![Bencode::String("done".into())])),
    ])])
}

fn handle_eval(req: &Request, sessions: &Sessions) -> Result<Vec<Bencode>, String> {
    let session_id = req.session.as_ref().ok_or("no session")?;
    let code = req.code.as_ref().ok_or("no code")?;
    
    let mut sessions = sessions.lock().unwrap();
    let session = sessions.get_mut(session_id).ok_or("session not found")?;
    
    let result = session.eval(code);
    
    match result {
        EvalResult::Value(val) => Ok(vec![
            response(&req.id, vec![
                ("value", Bencode::String(val)),
            ]),
            response(&req.id, vec![
                ("status", Bencode::List(vec![Bencode::String("done".into())])),
            ]),
        ]),
        EvalResult::Defined(name) => Ok(vec![
            response(&req.id, vec![
                ("value", Bencode::String(format!("#'{}", name))),
            ]),
            response(&req.id, vec![
                ("status", Bencode::List(vec![Bencode::String("done".into())])),
            ]),
        ]),
        EvalResult::Error(err) => Ok(vec![
            response(&req.id, vec![
                ("err", Bencode::String(err)),
            ]),
            response(&req.id, vec![
                ("status", Bencode::List(vec![
                    Bencode::String("done".into()),
                    Bencode::String("error".into()),
                ])),
            ]),
        ]),
        EvalResult::Interrupted => Ok(vec![
            response(&req.id, vec![
                ("status", Bencode::List(vec![
                    Bencode::String("done".into()),
                    Bencode::String("interrupted".into()),
                ])),
            ]),
        ]),
    }
}

fn handle_completions(req: &Request, sessions: &Sessions) -> Result<Vec<Bencode>, String> {
    let session_id = req.session.as_ref().ok_or("no session")?;
    let prefix = req.prefix.as_ref().unwrap_or(&String::new());
    
    let sessions = sessions.lock().unwrap();
    let session = sessions.get(session_id).ok_or("session not found")?;
    
    let completions: Vec<Bencode> = session
        .completions(prefix)
        .into_iter()
        .map(|c| Bencode::Dict(vec![
            ("candidate".into(), Bencode::String(c.candidate)),
            ("type".into(), Bencode::String(c.kind)),
        ].into_iter().collect()))
        .collect();
    
    Ok(vec![response(&req.id, vec![
        ("completions", Bencode::List(completions)),
        ("status", Bencode::List(vec![Bencode::String("done".into())])),
    ])])
}

fn handle_describe(req: &Request) -> Result<Vec<Bencode>, String> {
    // Describe server capabilities
    Ok(vec![response(&req.id, vec![
        ("ops", Bencode::Dict(vec![
            ("clone".into(), Bencode::Dict(Default::default())),
            ("close".into(), Bencode::Dict(Default::default())),
            ("eval".into(), Bencode::Dict(Default::default())),
            ("load-file".into(), Bencode::Dict(Default::default())),
            ("completions".into(), Bencode::Dict(Default::default())),
            ("lookup".into(), Bencode::Dict(Default::default())),
            ("interrupt".into(), Bencode::Dict(Default::default())),
            ("describe".into(), Bencode::Dict(Default::default())),
        ].into_iter().collect())),
        ("versions", Bencode::Dict(vec![
            ("liar".into(), Bencode::String(env!("CARGO_PKG_VERSION").into())),
            ("nrepl".into(), Bencode::String("0.1.0".into())),
        ].into_iter().collect())),
        ("status", Bencode::List(vec![Bencode::String("done".into())])),
    ])])
}

fn response(id: &str, fields: Vec<(&str, Bencode)>) -> Bencode {
    let mut dict: HashMap<String, Bencode> = fields
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect();
    dict.insert("id".to_string(), Bencode::String(id.to_string()));
    Bencode::Dict(dict)
}
```

## Component 4: Updated REPL (using Session)

**liar-repl/src/main.rs:**
```rust
//! liar Interactive REPL
//!
//! Uses incremental JIT for fast evaluation.

use liar_repl::Session;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

const BANNER: &str = r#"
liar REPL - A Lisp that doesn't lie about memory
Type expressions to evaluate, :help for commands, :quit to exit
"#;

fn main() {
    println!("{}", BANNER);

    let mut rl = match DefaultEditor::new() {
        Ok(editor) => editor,
        Err(e) => {
            eprintln!("Failed to initialize editor: {}", e);
            std::process::exit(1);
        }
    };

    let mut session = match Session::new("repl".to_string()) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to initialize session: {}", e);
            std::process::exit(1);
        }
    };

    // Load stdlib
    let stdlib = include_str!("../../lib/stdlib.liar");
    match session.eval(stdlib) {
        EvalResult::Error(e) => eprintln!("Warning: failed to load stdlib: {}", e),
        _ => {}
    }

    loop {
        let readline = rl.readline("liar> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                
                // Handle commands
                if line.starts_with(':') {
                    match line {
                        ":help" | ":h" => print_help(),
                        ":quit" | ":q" => break,
                        _ if line.starts_with(":load ") => {
                            let path = line.strip_prefix(":load ").unwrap().trim();
                            match std::fs::read_to_string(path) {
                                Ok(contents) => {
                                    match session.eval(&contents) {
                                        EvalResult::Value(v) => println!("{}", v),
                                        EvalResult::Defined(n) => println!("defined: {}", n),
                                        EvalResult::Error(e) => eprintln!("{}", e),
                                        _ => {}
                                    }
                                }
                                Err(e) => eprintln!("Error: {}", e),
                            }
                        }
                        _ => println!("Unknown command. Type :help for help."),
                    }
                    continue;
                }
                
                // Evaluate
                match session.eval(line) {
                    EvalResult::Value(v) => println!("{}", v),
                    EvalResult::Defined(n) => println!("defined: {}", n),
                    EvalResult::Error(e) => eprintln!("{}", e),
                    EvalResult::Interrupted => println!("Interrupted"),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
}
```

## Directory Structure

```
liar-main/
├── lib/
│   └── stdlib.liar
├── lir-codegen/
│   └── src/
│       ├── lib.rs
│       ├── codegen.rs
│       ├── jit.rs
│       └── orc.rs           # NEW: OrcJIT wrapper
├── liar-repl/
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs           # Exports Session
│       ├── main.rs          # CLI REPL
│       └── session.rs       # Session management
├── liar-nrepl/
│   ├── Cargo.toml
│   └── src/
│       └── main.rs          # nREPL server
└── liar-cert/
    ├── features/
    └── tests/
        └── cert.rs          # Can use Session directly
```

## Workspace Cargo.toml

```toml
[workspace]
members = [
    # ... existing ...
    "liar-repl",
    "liar-nrepl",
    "liar-cert",
]
```

## Implementation Order

1. **OrcJIT wrapper** (lir-codegen/src/orc.rs)
   - Get basic incremental compilation working
   - Test: define function, call it, define another, call both

2. **Session** (liar-repl/src/session.rs)
   - Wire up to OrcJIT
   - Handle definitions vs expressions
   - Test: REPL workflow

3. **CLI REPL** (liar-repl/src/main.rs)
   - Use Session
   - Commands, history, stdlib loading
   - Test: interactive use

4. **nREPL server** (liar-nrepl/)
   - Bencode protocol
   - Session management
   - eval, completions, lookup
   - Test: connect from Calva/CIDER

5. **Cucumber integration** (liar-cert/)
   - Use Session for evaluation
   - Feature files for stdlib

## Key Technical Challenges

### 1. OrcJIT in Inkwell

Inkwell's OrcJIT support may be limited. Alternatives:
- Use `llvm-sys` directly for OrcJIT
- Use the simple JIT with module recreation (slower but works)
- Write a thin OrcJIT wrapper in C, call via FFI

### 2. Expression Parsing

liar parser currently expects top-level definitions. Need:
```rust
// liar/src/lib.rs
pub fn parse_expr(source: &str) -> Result<Expr, ParseError>
```

Modify parser to have an "expression mode" entry point.

### 3. Type Inference with Context

When evaluating `(square 5)`, need to know `square` exists and its type:
```rust
// liar/src/lib.rs
pub fn infer_expr_with_context(
    expr: &Expr,
    context: &HashMap<String, Definition>,
) -> Result<Type, TypeError>
```

### 4. Symbol Resolution Across Modules

When compiling `(square 5)` after `square` was JIT'd separately, LLVM needs to resolve the symbol. OrcJIT handles this via JITDylib symbol tables.

## Acceptance Criteria

- [ ] `IncrementalJit::define_function` adds function to JIT
- [ ] `IncrementalJit::eval_expr` wraps and calls expression
- [ ] Symbols resolve across separately-compiled functions
- [ ] `Session::eval` handles both definitions and expressions
- [ ] CLI REPL uses incremental JIT
- [ ] nREPL server starts and accepts connections
- [ ] nREPL `eval` op works
- [ ] nREPL `completions` op works
- [ ] IDE (Calva or CIDER) can connect and evaluate
- [ ] `.nrepl-port` file written on startup

## Notes for Agent

- OrcJIT is the hard part. Start there. Get a minimal test working before building Session.
- If OrcJIT doesn't work in Inkwell, fallback to recompiling (the "simple" approach) but encapsulate it behind the same Session API so we can swap later.
- The nREPL protocol is well-documented: https://nrepl.org/nrepl/building_servers.html
- Bencode is simple: strings, ints, lists, dicts. Use an existing crate.
- Test nREPL with `nc localhost 7888` and manually sending bencode, or use existing nREPL clients.

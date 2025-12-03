//! nREPL server for liar
//!
//! Implements the nREPL protocol for IDE integration.
//! See: https://nrepl.org/nrepl/building_servers.html

use inkwell::context::Context;
use liar_repl::session::{format_value, Session};
use liar_repl::EvalResult;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use uuid::Uuid;

/// nREPL message
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
struct Message {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    op: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    session: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    code: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    value: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    status: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    err: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    out: Option<String>,
    #[serde(rename = "new-session", skip_serializing_if = "Option::is_none")]
    new_session: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    ops: Option<HashMap<String, HashMap<String, String>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    versions: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    completions: Option<Vec<CompletionItem>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    prefix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    symbol: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    info: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CompletionItem {
    candidate: String,
    #[serde(rename = "type")]
    kind: String,
}

struct NreplServer {
    port: u16,
}

impl NreplServer {
    fn new(port: u16) -> Self {
        Self { port }
    }

    fn run(&self) {
        let listener = TcpListener::bind(format!("127.0.0.1:{}", self.port)).unwrap();
        println!("nREPL server started on port {}", self.port);

        // Write .nrepl-port file for IDE discovery
        if let Err(e) = std::fs::write(".nrepl-port", self.port.to_string()) {
            eprintln!("Warning: failed to write .nrepl-port: {}", e);
        }

        // Sessions are stored per-connection for now
        // In the future, sessions could be shared across connections
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    std::thread::spawn(move || {
                        if let Err(e) = handle_connection(stream) {
                            eprintln!("Connection error: {}", e);
                        }
                    });
                }
                Err(e) => eprintln!("Connection failed: {}", e),
            }
        }
    }
}

fn handle_connection(mut stream: TcpStream) -> Result<(), String> {
    // Create LLVM context and sessions map for this connection
    let context = Context::create();
    let mut sessions: HashMap<String, SessionWrapper> = HashMap::new();

    let mut buffer = vec![0u8; 65536];

    loop {
        // Read bencode message
        let n = stream
            .read(&mut buffer)
            .map_err(|e| format!("read error: {}", e))?;
        if n == 0 {
            break;
        }

        // Decode message
        let msg: Message = match serde_bencode::from_bytes(&buffer[..n]) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("Failed to decode message: {}", e);
                continue;
            }
        };

        // Handle the message
        let responses = handle_message(&msg, &context, &mut sessions);

        // Send responses
        for response in responses {
            let encoded =
                serde_bencode::to_bytes(&response).map_err(|e| format!("encode error: {}", e))?;
            stream
                .write_all(&encoded)
                .map_err(|e| format!("write error: {}", e))?;
        }
    }

    // Clean up .nrepl-port file if we're the last connection
    // (simplified - in production would track connections)

    Ok(())
}

/// Wrapper to hold Session with 'static lifetime constraint
struct SessionWrapper<'ctx> {
    session: Session<'ctx>,
}

fn handle_message<'ctx>(
    msg: &Message,
    context: &'ctx Context,
    sessions: &mut HashMap<String, SessionWrapper<'ctx>>,
) -> Vec<Message> {
    let id = msg.id.clone().unwrap_or_default();
    let op = msg.op.clone().unwrap_or_default();

    match op.as_str() {
        "clone" => handle_clone(&id, context, sessions),
        "close" => handle_close(&id, msg, sessions),
        "eval" => handle_eval(&id, msg, sessions),
        "completions" => handle_completions(&id, msg, sessions),
        "lookup" => handle_lookup(&id, msg, sessions),
        "describe" => handle_describe(&id),
        _ => vec![error_response(&id, &format!("unknown op: {}", op))],
    }
}

fn handle_clone<'ctx>(
    id: &str,
    context: &'ctx Context,
    sessions: &mut HashMap<String, SessionWrapper<'ctx>>,
) -> Vec<Message> {
    let session_id = Uuid::new_v4().to_string();
    match Session::new(context, session_id.clone()) {
        Ok(session) => {
            sessions.insert(session_id.clone(), SessionWrapper { session });
            vec![Message {
                id: Some(id.to_string()),
                new_session: Some(session_id),
                status: Some(vec!["done".to_string()]),
                ..Default::default()
            }]
        }
        Err(e) => vec![error_response(id, &e)],
    }
}

fn handle_close(
    id: &str,
    msg: &Message,
    sessions: &mut HashMap<String, SessionWrapper>,
) -> Vec<Message> {
    if let Some(ref session_id) = msg.session {
        sessions.remove(session_id);
    }
    vec![Message {
        id: Some(id.to_string()),
        status: Some(vec!["done".to_string()]),
        ..Default::default()
    }]
}

fn handle_eval(
    id: &str,
    msg: &Message,
    sessions: &mut HashMap<String, SessionWrapper>,
) -> Vec<Message> {
    let session_id = match &msg.session {
        Some(s) => s,
        None => return vec![error_response(id, "no session")],
    };

    let code = match &msg.code {
        Some(c) => c,
        None => return vec![error_response(id, "no code")],
    };

    let session_wrapper = match sessions.get_mut(session_id) {
        Some(s) => s,
        None => return vec![error_response(id, "session not found")],
    };

    let result = session_wrapper.session.eval(code);

    match result {
        EvalResult::Value(val) => vec![
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                value: Some(format_value(&val)),
                ..Default::default()
            },
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                status: Some(vec!["done".to_string()]),
                ..Default::default()
            },
        ],
        EvalResult::Defined(name) => vec![
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                value: Some(format!("#'{}", name)),
                ..Default::default()
            },
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                status: Some(vec!["done".to_string()]),
                ..Default::default()
            },
        ],
        EvalResult::Error(err) => vec![
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                err: Some(err),
                ..Default::default()
            },
            Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                status: Some(vec!["done".to_string(), "error".to_string()]),
                ..Default::default()
            },
        ],
    }
}

fn handle_completions(
    id: &str,
    msg: &Message,
    sessions: &mut HashMap<String, SessionWrapper>,
) -> Vec<Message> {
    let session_id = match &msg.session {
        Some(s) => s,
        None => return vec![error_response(id, "no session")],
    };

    let prefix = msg.prefix.clone().unwrap_or_default();

    let session_wrapper = match sessions.get(session_id) {
        Some(s) => s,
        None => return vec![error_response(id, "session not found")],
    };

    let completions: Vec<CompletionItem> = session_wrapper
        .session
        .completions(&prefix)
        .into_iter()
        .map(|name| CompletionItem {
            candidate: name,
            kind: "function".to_string(),
        })
        .collect();

    vec![Message {
        id: Some(id.to_string()),
        session: Some(session_id.clone()),
        completions: Some(completions),
        status: Some(vec!["done".to_string()]),
        ..Default::default()
    }]
}

fn handle_lookup(
    id: &str,
    msg: &Message,
    sessions: &mut HashMap<String, SessionWrapper>,
) -> Vec<Message> {
    let session_id = match &msg.session {
        Some(s) => s,
        None => return vec![error_response(id, "no session")],
    };

    let symbol = match &msg.symbol {
        Some(s) => s,
        None => return vec![error_response(id, "no symbol")],
    };

    let session_wrapper = match sessions.get(session_id) {
        Some(s) => s,
        None => return vec![error_response(id, "session not found")],
    };

    match session_wrapper.session.lookup(symbol) {
        Some(def) => {
            let mut info = HashMap::new();
            info.insert("name".to_string(), def.name.clone());
            info.insert("kind".to_string(), format!("{:?}", def.kind));
            vec![Message {
                id: Some(id.to_string()),
                session: Some(session_id.clone()),
                info: Some(info),
                status: Some(vec!["done".to_string()]),
                ..Default::default()
            }]
        }
        None => vec![Message {
            id: Some(id.to_string()),
            session: Some(session_id.clone()),
            status: Some(vec!["done".to_string(), "no-info".to_string()]),
            ..Default::default()
        }],
    }
}

fn handle_describe(id: &str) -> Vec<Message> {
    let mut ops = HashMap::new();
    for op in &[
        "clone",
        "close",
        "eval",
        "completions",
        "lookup",
        "describe",
    ] {
        ops.insert(op.to_string(), HashMap::new());
    }

    let mut versions = HashMap::new();
    versions.insert("liar".to_string(), env!("CARGO_PKG_VERSION").to_string());
    versions.insert("nrepl".to_string(), "0.1.0".to_string());

    vec![Message {
        id: Some(id.to_string()),
        ops: Some(ops),
        versions: Some(versions),
        status: Some(vec!["done".to_string()]),
        ..Default::default()
    }]
}

fn error_response(id: &str, msg: &str) -> Message {
    Message {
        id: Some(id.to_string()),
        err: Some(msg.to_string()),
        status: Some(vec!["done".to_string(), "error".to_string()]),
        ..Default::default()
    }
}

fn main() {
    let port = std::env::var("NREPL_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(7888);

    let server = NreplServer::new(port);
    server.run();
}

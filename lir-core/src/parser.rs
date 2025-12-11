//! Parser for lIR S-expressions

use crate::ast::*;
use crate::error::ParseError;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

/// Result of parsing: either an expression, function definition, extern declaration, global, or struct
#[derive(Debug, Clone, PartialEq)]
pub enum ParseResult {
    Expr(Expr),
    Function(FunctionDef),
    ExternDecl(ExternDecl),
    Global(GlobalDef),
    Struct(StructDef),
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_expr()?;
        // Ensure we consumed all input
        if self.lexer.peek()?.is_some() {
            return Err(ParseError::UnexpectedToken("trailing input".to_string()));
        }
        Ok(expr)
    }

    /// Parse a top-level item: either an expression or a function definition
    /// Ensures all input is consumed (for single-item parsing)
    pub fn parse_item(&mut self) -> Result<ParseResult, ParseError> {
        let result = self.parse_one_item()?;
        // Ensure we consumed all input
        if self.lexer.peek()?.is_some() {
            return Err(ParseError::UnexpectedToken("trailing input".to_string()));
        }
        Ok(result)
    }

    /// Parse multiple top-level items (functions, declarations, etc.)
    pub fn parse_items(&mut self) -> Result<Vec<ParseResult>, ParseError> {
        let mut items = Vec::new();
        while self.lexer.peek()?.is_some() {
            items.push(self.parse_one_item()?);
        }
        Ok(items)
    }

    /// Parse a single top-level item without checking for trailing input
    fn parse_one_item(&mut self) -> Result<ParseResult, ParseError> {
        // Peek to see if this is a function definition or declaration
        self.expect(Token::LParen)?;

        let token = self.lexer.peek()?.ok_or(ParseError::UnexpectedEof)?.clone();

        if let Token::Ident(ref name) = token {
            if name == "define" {
                self.lexer.next_token_peeked()?; // consume "define"
                let func = self.parse_function_def()?;
                self.expect(Token::RParen)?;
                return Ok(ParseResult::Function(func));
            } else if name == "declare" {
                self.lexer.next_token_peeked()?; // consume "declare"
                let decl = self.parse_extern_decl()?;
                self.expect(Token::RParen)?;
                return Ok(ParseResult::ExternDecl(decl));
            } else if name == "global" {
                self.lexer.next_token_peeked()?; // consume "global"
                let global = self.parse_global_def()?;
                self.expect(Token::RParen)?;
                return Ok(ParseResult::Global(global));
            } else if name == "defstruct" {
                self.lexer.next_token_peeked()?; // consume "defstruct"
                let struct_def = self.parse_struct_def()?;
                self.expect(Token::RParen)?;
                return Ok(ParseResult::Struct(struct_def));
            }
        }

        // Not a function - parse as expression
        // We already consumed LParen, so parse the form directly
        // Check if this is a vector literal: (<N x type> values...)
        if let Some(Token::LAngle) = self.lexer.peek()? {
            let expr = self.parse_vector_literal()?;
            self.expect(Token::RParen)?;
            return Ok(ParseResult::Expr(expr));
        }

        let token = self
            .lexer
            .next_token_peeked()?
            .ok_or(ParseError::UnexpectedEof)?;

        let expr = match token {
            Token::Ident(ref name) => self.parse_form(name)?,
            _ => {
                return Err(ParseError::Expected {
                    expected: "identifier".to_string(),
                    found: format!("{}", token),
                })
            }
        };

        self.expect(Token::RParen)?;

        Ok(ParseResult::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek()? {
            Some(Token::LParen) => self.parse_sexpr(),
            Some(Token::LAngle) => self.parse_vector_literal(),
            Some(Token::LBrace) => self.parse_struct_literal(),
            Some(Token::Ident(ref s)) if s.starts_with('%') => {
                // Local variable reference
                let name = s.clone();
                self.lexer.next_token_peeked()?; // consume the token
                Ok(Expr::LocalRef(name[1..].to_string())) // strip the %
            }
            Some(Token::Ident(ref s)) if s.starts_with('@') => {
                // Global/function reference
                let name = s.clone();
                self.lexer.next_token_peeked()?; // consume the token
                Ok(Expr::GlobalRef(name[1..].to_string())) // strip the @
            }
            Some(Token::Ident(ref s)) => {
                // Could be a bare parameter name (without %)
                let name = s.clone();
                self.lexer.next_token_peeked()?;
                Ok(Expr::LocalRef(name))
            }
            _ => Err(ParseError::Expected {
                expected: "expression".to_string(),
                found: format!("{:?}", self.lexer.peek()?),
            }),
        }
    }

    fn parse_sexpr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::LParen)?;

        // Check if this is a vector literal: (<N x type> values...)
        if let Some(Token::LAngle) = self.lexer.peek()? {
            let expr = self.parse_vector_literal()?;
            // parse_vector_literal doesn't expect RParen, we handle it here
            self.expect(Token::RParen)?;
            return Ok(expr);
        }

        let token = self
            .lexer
            .next_token_peeked()?
            .ok_or(ParseError::UnexpectedEof)?;

        let expr = match token {
            Token::Ident(ref name) => self.parse_form(name)?,
            _ => {
                return Err(ParseError::Expected {
                    expected: "identifier".to_string(),
                    found: format!("{}", token),
                })
            }
        };

        self.expect(Token::RParen)?;
        Ok(expr)
    }

    fn parse_form(&mut self, name: &str) -> Result<Expr, ParseError> {
        match name {
            // Type literals
            "i1" | "i8" | "i16" | "i32" | "i64" => self.parse_int_literal(name),
            "float" | "double" => self.parse_float_literal(name),
            "ptr" => self.parse_ptr_literal(),

            // String literal
            "string" => self.parse_string_literal(),

            // Integer arithmetic
            "add" => self.parse_binop(|l, r| Expr::Add(Box::new(l), Box::new(r))),
            "sub" => self.parse_binop(|l, r| Expr::Sub(Box::new(l), Box::new(r))),
            "mul" => self.parse_binop(|l, r| Expr::Mul(Box::new(l), Box::new(r))),
            "sdiv" => self.parse_binop(|l, r| Expr::SDiv(Box::new(l), Box::new(r))),
            "udiv" => self.parse_binop(|l, r| Expr::UDiv(Box::new(l), Box::new(r))),
            "srem" => self.parse_binop(|l, r| Expr::SRem(Box::new(l), Box::new(r))),
            "urem" => self.parse_binop(|l, r| Expr::URem(Box::new(l), Box::new(r))),

            // Float arithmetic
            "fadd" => self.parse_binop(|l, r| Expr::FAdd(Box::new(l), Box::new(r))),
            "fsub" => self.parse_binop(|l, r| Expr::FSub(Box::new(l), Box::new(r))),
            "fmul" => self.parse_binop(|l, r| Expr::FMul(Box::new(l), Box::new(r))),
            "fdiv" => self.parse_binop(|l, r| Expr::FDiv(Box::new(l), Box::new(r))),
            "frem" => self.parse_binop(|l, r| Expr::FRem(Box::new(l), Box::new(r))),

            // Bitwise
            "and" => self.parse_binop(|l, r| Expr::And(Box::new(l), Box::new(r))),
            "or" => self.parse_binop(|l, r| Expr::Or(Box::new(l), Box::new(r))),
            "xor" => self.parse_binop(|l, r| Expr::Xor(Box::new(l), Box::new(r))),
            "shl" => self.parse_binop(|l, r| Expr::Shl(Box::new(l), Box::new(r))),
            "lshr" => self.parse_binop(|l, r| Expr::LShr(Box::new(l), Box::new(r))),
            "ashr" => self.parse_binop(|l, r| Expr::AShr(Box::new(l), Box::new(r))),
            "ctpop" => {
                let val = self.parse_expr()?;
                Ok(Expr::Ctpop(Box::new(val)))
            }

            // Comparisons
            "icmp" => self.parse_icmp(),
            "fcmp" => self.parse_fcmp(),

            // Conversions
            "trunc" => self.parse_conversion(|ty, v| Expr::Trunc {
                ty,
                value: Box::new(v),
            }),
            "zext" => self.parse_conversion(|ty, v| Expr::ZExt {
                ty,
                value: Box::new(v),
            }),
            "sext" => self.parse_conversion(|ty, v| Expr::SExt {
                ty,
                value: Box::new(v),
            }),
            "fptrunc" => self.parse_conversion(|ty, v| Expr::FPTrunc {
                ty,
                value: Box::new(v),
            }),
            "fpext" => self.parse_conversion(|ty, v| Expr::FPExt {
                ty,
                value: Box::new(v),
            }),
            "fptoui" => self.parse_conversion(|ty, v| Expr::FPToUI {
                ty,
                value: Box::new(v),
            }),
            "fptosi" => self.parse_conversion(|ty, v| Expr::FPToSI {
                ty,
                value: Box::new(v),
            }),
            "uitofp" => self.parse_conversion(|ty, v| Expr::UIToFP {
                ty,
                value: Box::new(v),
            }),
            "sitofp" => self.parse_conversion(|ty, v| Expr::SIToFP {
                ty,
                value: Box::new(v),
            }),

            // Select
            "select" => self.parse_select(),

            // Vector operations
            "extractelement" => self.parse_extractelement(),
            "insertelement" => self.parse_insertelement(),
            "shufflevector" => self.parse_shufflevector(),

            // Aggregate operations
            "extractvalue" => self.parse_extractvalue(),
            "insertvalue" => self.parse_insertvalue(),

            // Return instruction
            "ret" => self.parse_ret(),

            // Memory operations
            "alloca" => self.parse_alloca(),
            "load" => self.parse_load(),
            "store" => self.parse_store(),
            "getelementptr" => self.parse_getelementptr(false),
            "inbounds" => {
                // This is getelementptr inbounds - expect 'getelementptr' keyword conceptually
                // but the 'inbounds' keyword triggers the variant
                // Actually, we should parse: (getelementptr inbounds type ptr indices...)
                // So if we see just "inbounds", that's an error. We handle it in parse_getelementptr
                Err(ParseError::UnknownOperation(
                    "inbounds without getelementptr".to_string(),
                ))
            }

            // Control flow
            "br" => self.parse_br(),
            "phi" => self.parse_phi(),

            // Function call
            "call" => self.parse_call(),
            "tailcall" => self.parse_tailcall(),
            "indirect-call" => self.parse_indirect_call(),

            // Array operations
            "array-alloc" => self.parse_array_alloc(),
            "array-get" => self.parse_array_get(),
            "array-set" => self.parse_array_set(),
            "array-len" => self.parse_array_len(),
            "array-ptr" => self.parse_array_ptr(),

            // Ownership operations
            "alloc" => self.parse_alloc_own(),
            "borrow" => self.parse_borrow(),
            "drop" => self.parse_drop(),
            "move" => self.parse_move(),

            // Reference counting operations
            "rc-alloc" => self.parse_rc_alloc(),
            "rc-clone" => self.parse_rc_clone(),
            "rc-drop" => self.parse_rc_drop(),
            "rc-count" => self.parse_rc_count(),
            "rc-ptr" => self.parse_rc_ptr(),

            // Memory deallocation
            "free" => self.parse_free(),

            // Atomic memory operations
            "atomic-load" => self.parse_atomic_load(),
            "atomic-store" => self.parse_atomic_store(),
            "atomicrmw" => self.parse_atomicrmw(),
            "cmpxchg" => self.parse_cmpxchg(),
            "fence" => self.parse_fence(),

            // Let bindings
            "let" => self.parse_let(),

            _ => Err(ParseError::UnknownOperation(name.to_string())),
        }
    }

    /// Parse a return instruction: (ret) or (ret value)
    fn parse_ret(&mut self) -> Result<Expr, ParseError> {
        // Check if there's a value or just void return
        match self.lexer.peek()? {
            Some(Token::RParen) => {
                // void return - don't consume the RParen, caller handles it
                Ok(Expr::Ret(None))
            }
            _ => {
                // return with value
                let value = self.parse_expr()?;
                Ok(Expr::Ret(Some(Box::new(value))))
            }
        }
    }

    /// Parse alloca: (alloca type) or (alloca type count)
    fn parse_alloca(&mut self) -> Result<Expr, ParseError> {
        // Parse the type to allocate (supports ptr and scalars)
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.param_type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Check for optional count
        let count = match self.lexer.peek()? {
            Some(Token::RParen) => None,
            _ => Some(Box::new(self.parse_expr()?)),
        };

        Ok(Expr::Alloca { ty, count })
    }

    /// Parse load: (load type ptr)
    fn parse_load(&mut self) -> Result<Expr, ParseError> {
        // Parse the type to load (supports ptr, scalars, and anonymous structs)
        let ty = match self.lexer.peek()?.cloned() {
            Some(Token::LBrace) => {
                self.lexer.next_token_peeked()?; // consume {
                self.parse_anon_struct_param_type()?
            }
            Some(Token::Ident(ref s)) => {
                let ty = self.param_type_from_name(s)?;
                self.lexer.next_token_peeked()?; // consume type name
                ty
            }
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse the pointer
        let ptr = self.parse_expr()?;

        Ok(Expr::Load {
            ty,
            ptr: Box::new(ptr),
        })
    }

    /// Parse store: (store value ptr)
    fn parse_store(&mut self) -> Result<Expr, ParseError> {
        // Parse the value to store
        let value = self.parse_expr()?;

        // Parse the pointer
        let ptr = self.parse_expr()?;

        Ok(Expr::Store {
            value: Box::new(value),
            ptr: Box::new(ptr),
        })
    }

    /// Parse getelementptr: (getelementptr [inbounds] type ptr indices...)
    fn parse_getelementptr(&mut self, _: bool) -> Result<Expr, ParseError> {
        // Check for 'inbounds' keyword
        let inbounds = match self.lexer.peek()? {
            Some(Token::Ident(s)) if s == "inbounds" => {
                self.lexer.next_token_peeked()?; // consume 'inbounds'
                true
            }
            _ => false,
        };

        // Parse the element type - could be scalar, ptr, or %struct.name
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s.starts_with("%struct.") => {
                GepType::Struct(s[8..].to_string()) // strip %struct. prefix
            }
            Some(Token::Ident(ref s)) if s == "ptr" => GepType::Ptr,
            Some(Token::Ident(ref s)) => GepType::Scalar(self.type_from_name(s)?),
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse the base pointer
        let ptr = self.parse_expr()?;

        // Parse indices until we hit RParen
        let mut indices = Vec::new();
        while !matches!(self.lexer.peek()?, Some(Token::RParen)) {
            indices.push(self.parse_expr()?);
        }

        if indices.is_empty() {
            return Err(ParseError::Expected {
                expected: "at least one index".to_string(),
                found: ")".to_string(),
            });
        }

        Ok(Expr::GetElementPtr {
            ty,
            ptr: Box::new(ptr),
            indices,
            inbounds,
        })
    }

    /// Parse struct literal: { expr expr ... }
    fn parse_struct_literal(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();

        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RBrace {
                break;
            }
            fields.push(self.parse_expr()?);
        }
        self.expect(Token::RBrace)?;

        Ok(Expr::StructLit(fields))
    }

    /// Parse extractvalue: (extractvalue aggregate index...)
    fn parse_extractvalue(&mut self) -> Result<Expr, ParseError> {
        let aggregate = self.parse_expr()?;

        // Parse indices (integers)
        let mut indices = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            match self.lexer.next_token_peeked()? {
                Some(Token::Integer(n)) => {
                    indices.push(n as u32);
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "index".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        if indices.is_empty() {
            return Err(ParseError::Expected {
                expected: "at least one index".to_string(),
                found: ")".to_string(),
            });
        }

        Ok(Expr::ExtractValue {
            aggregate: Box::new(aggregate),
            indices,
        })
    }

    /// Parse insertvalue: (insertvalue aggregate value index...)
    fn parse_insertvalue(&mut self) -> Result<Expr, ParseError> {
        let aggregate = self.parse_expr()?;
        let value = self.parse_expr()?;

        // Parse indices (integers)
        let mut indices = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            match self.lexer.next_token_peeked()? {
                Some(Token::Integer(n)) => {
                    indices.push(n as u32);
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "index".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        if indices.is_empty() {
            return Err(ParseError::Expected {
                expected: "at least one index".to_string(),
                found: ")".to_string(),
            });
        }

        Ok(Expr::InsertValue {
            aggregate: Box::new(aggregate),
            value: Box::new(value),
            indices,
        })
    }

    /// Parse branch: (br label) or (br cond true-label false-label)
    fn parse_br(&mut self) -> Result<Expr, ParseError> {
        // First token could be a label (unconditional) or an expression (conditional)
        // We need to peek ahead to determine which

        // Try to parse the first thing as an expression
        let first = self.parse_expr()?;

        // Check if there are more tokens (conditional) or not (unconditional with expr as label ref)
        match self.lexer.peek()? {
            Some(Token::RParen) => {
                // Unconditional branch - first was a label (but parsed as LocalRef)
                // Actually, it should just be an identifier, let's check
                if let Expr::LocalRef(label) = first {
                    Ok(Expr::Br(BranchTarget::Unconditional(label)))
                } else {
                    Err(ParseError::Expected {
                        expected: "label".to_string(),
                        found: format!("{:?}", first),
                    })
                }
            }
            _ => {
                // Conditional branch - first is condition, then two labels
                let true_label = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(s)) => s,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "true label".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                let false_label = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(s)) => s,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "false label".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                Ok(Expr::Br(BranchTarget::Conditional {
                    cond: Box::new(first),
                    true_label,
                    false_label,
                }))
            }
        }
    }

    /// Parse phi: (phi type (label1 val1) (label2 val2) ...)
    fn parse_phi(&mut self) -> Result<Expr, ParseError> {
        // Parse the type
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse incoming values: (label value) ...
        let mut incoming = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            self.expect(Token::LParen)?;

            // Parse label
            let label = match self.lexer.next_token_peeked()? {
                Some(Token::Ident(s)) => s,
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "block label".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            };

            // Parse value
            let value = self.parse_expr()?;
            self.expect(Token::RParen)?;

            incoming.push((label, Box::new(value)));
        }

        Ok(Expr::Phi { ty, incoming })
    }

    /// Parse call: (call @function-name args...)
    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        // Parse function name (must start with @)
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) if s.starts_with('@') => s[1..].to_string(),
            Some(Token::Ident(s)) => {
                return Err(ParseError::Expected {
                    expected: "function name starting with @".to_string(),
                    found: s,
                })
            }
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "function name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse arguments
        let mut args = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            args.push(self.parse_expr()?);
        }

        Ok(Expr::Call { name, args })
    }

    /// Parse tailcall: (tailcall @function-name args...)
    fn parse_tailcall(&mut self) -> Result<Expr, ParseError> {
        // Parse function name (must start with @)
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) if s.starts_with('@') => s[1..].to_string(),
            Some(Token::Ident(s)) => {
                return Err(ParseError::Expected {
                    expected: "function name starting with @".to_string(),
                    found: s,
                })
            }
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "function name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse arguments
        let mut args = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            args.push(self.parse_expr()?);
        }

        Ok(Expr::TailCall { name, args })
    }

    /// Parse indirect-call: (indirect-call fn_ptr ret_ty args...)
    /// Example: (indirect-call fn_ptr_expr i64 arg1 arg2)
    fn parse_indirect_call(&mut self) -> Result<Expr, ParseError> {
        // Parse function pointer expression
        let fn_ptr = Box::new(self.parse_expr()?);

        // Parse return type
        let ret_ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => self.param_type_from_name(&s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "return type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse arguments
        let mut args = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            args.push(self.parse_expr()?);
        }

        Ok(Expr::IndirectCall {
            fn_ptr,
            ret_ty,
            args,
        })
    }

    /// Parse array-alloc: (array-alloc type size)
    /// Example: (array-alloc i64 10)
    fn parse_array_alloc(&mut self) -> Result<Expr, ParseError> {
        // Parse the element type (must be a scalar type)
        let elem_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "element type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse the size (must be a constant integer)
        let size = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n as u32,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "array size (integer)".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok(Expr::ArrayAlloc { elem_type, size })
    }

    /// Parse array-get: (array-get type size arr idx)
    /// Example: (array-get i64 10 arr (i64 5))
    fn parse_array_get(&mut self) -> Result<Expr, ParseError> {
        // Parse element type
        let elem_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "element type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse array size
        let size = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n as u32,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "array size".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse array expression
        let array = self.parse_expr()?;

        // Parse index expression
        let index = self.parse_expr()?;

        Ok(Expr::ArrayGet {
            elem_type,
            size,
            array: Box::new(array),
            index: Box::new(index),
        })
    }

    /// Parse array-set: (array-set type size arr idx val)
    /// Example: (array-set i64 10 arr (i64 5) (i64 42))
    fn parse_array_set(&mut self) -> Result<Expr, ParseError> {
        // Parse element type
        let elem_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "element type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse array size
        let size = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n as u32,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "array size".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse array expression
        let array = self.parse_expr()?;

        // Parse index expression
        let index = self.parse_expr()?;

        // Parse value expression
        let value = self.parse_expr()?;

        Ok(Expr::ArraySet {
            elem_type,
            size,
            array: Box::new(array),
            index: Box::new(index),
            value: Box::new(value),
        })
    }

    /// Parse array-len: (array-len size)
    /// Returns the compile-time constant size
    fn parse_array_len(&mut self) -> Result<Expr, ParseError> {
        // Parse array size
        let size = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n as u32,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "array size".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok(Expr::ArrayLen { size })
    }

    /// Parse array-ptr: (array-ptr array)
    fn parse_array_ptr(&mut self) -> Result<Expr, ParseError> {
        // Parse the array expression
        let array = self.parse_expr()?;

        Ok(Expr::ArrayPtr {
            array: Box::new(array),
        })
    }

    /// Parse alloc: (alloc own T)
    fn parse_alloc_own(&mut self) -> Result<Expr, ParseError> {
        // Expect "own" keyword
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "own" => {}
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "own".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        }

        // Parse element type
        let elem_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "element type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok(Expr::AllocOwn { elem_type })
    }

    /// Parse borrow: (borrow ref x) or (borrow refmut x)
    fn parse_borrow(&mut self) -> Result<Expr, ParseError> {
        // Expect "ref" or "refmut" keyword
        let is_mut = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "ref" => false,
            Some(Token::Ident(ref s)) if s == "refmut" => true,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "ref or refmut".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse value expression
        let value = self.parse_expr()?;

        if is_mut {
            Ok(Expr::BorrowRefMut {
                value: Box::new(value),
            })
        } else {
            Ok(Expr::BorrowRef {
                value: Box::new(value),
            })
        }
    }

    /// Parse drop: (drop x)
    fn parse_drop(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::Drop {
            value: Box::new(value),
        })
    }

    /// Parse move: (move x)
    fn parse_move(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::Move {
            value: Box::new(value),
        })
    }

    /// Parse rc-alloc: (rc-alloc T)
    fn parse_rc_alloc(&mut self) -> Result<Expr, ParseError> {
        // Parse element type
        let elem_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "element type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok(Expr::RcAlloc { elem_type })
    }

    /// Parse rc-clone: (rc-clone x)
    fn parse_rc_clone(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::RcClone {
            value: Box::new(value),
        })
    }

    /// Parse rc-drop: (rc-drop x)
    fn parse_rc_drop(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::RcDrop {
            value: Box::new(value),
        })
    }

    /// Parse rc-count: (rc-count x)
    fn parse_rc_count(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::RcCount {
            value: Box::new(value),
        })
    }

    /// Parse rc-ptr: (rc-ptr x)
    fn parse_rc_ptr(&mut self) -> Result<Expr, ParseError> {
        let value = self.parse_expr()?;
        Ok(Expr::RcPtr {
            value: Box::new(value),
        })
    }

    /// Parse free: (free ptr)
    fn parse_free(&mut self) -> Result<Expr, ParseError> {
        let ptr = self.parse_expr()?;
        Ok(Expr::Free { ptr: Box::new(ptr) })
    }

    /// Parse memory ordering for atomic operations
    fn parse_ordering(&mut self) -> Result<MemoryOrdering, ParseError> {
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => match s.as_str() {
                "monotonic" => Ok(MemoryOrdering::Monotonic),
                "acquire" => Ok(MemoryOrdering::Acquire),
                "release" => Ok(MemoryOrdering::Release),
                "acq_rel" => Ok(MemoryOrdering::AcqRel),
                "seq_cst" => Ok(MemoryOrdering::SeqCst),
                _ => Err(ParseError::UnknownOperation(format!(
                    "invalid memory ordering: {}",
                    s
                ))),
            },
            Some(tok) => Err(ParseError::Expected {
                expected: "memory ordering".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Parse atomic-load: (atomic-load ordering type ptr)
    fn parse_atomic_load(&mut self) -> Result<Expr, ParseError> {
        let ordering = self.parse_ordering()?;

        // Parse the type to load
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse the pointer
        let ptr = self.parse_expr()?;

        Ok(Expr::AtomicLoad {
            ordering,
            ty,
            ptr: Box::new(ptr),
        })
    }

    /// Parse atomic-store: (atomic-store ordering value ptr)
    fn parse_atomic_store(&mut self) -> Result<Expr, ParseError> {
        let ordering = self.parse_ordering()?;

        // Parse the value to store
        let value = self.parse_expr()?;

        // Parse the pointer
        let ptr = self.parse_expr()?;

        Ok(Expr::AtomicStore {
            ordering,
            value: Box::new(value),
            ptr: Box::new(ptr),
        })
    }

    /// Parse atomicrmw: (atomicrmw op ordering ptr value)
    fn parse_atomicrmw(&mut self) -> Result<Expr, ParseError> {
        // Parse the operation
        let op = self.parse_atomicrmw_op()?;

        // Parse the ordering
        let ordering = self.parse_ordering()?;

        // Parse the pointer
        let ptr = self.parse_expr()?;

        // Parse the value
        let value = self.parse_expr()?;

        Ok(Expr::AtomicRMW {
            op,
            ordering,
            ptr: Box::new(ptr),
            value: Box::new(value),
        })
    }

    /// Parse atomic read-modify-write operation
    fn parse_atomicrmw_op(&mut self) -> Result<AtomicRMWOp, ParseError> {
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => match s.as_str() {
                "xchg" => Ok(AtomicRMWOp::Xchg),
                "add" => Ok(AtomicRMWOp::Add),
                "sub" => Ok(AtomicRMWOp::Sub),
                "and" => Ok(AtomicRMWOp::And),
                "or" => Ok(AtomicRMWOp::Or),
                "xor" => Ok(AtomicRMWOp::Xor),
                "min" => Ok(AtomicRMWOp::Min),
                "max" => Ok(AtomicRMWOp::Max),
                "umin" => Ok(AtomicRMWOp::UMin),
                "umax" => Ok(AtomicRMWOp::UMax),
                _ => Err(ParseError::UnknownOperation(format!(
                    "invalid atomicrmw operation: {}",
                    s
                ))),
            },
            Some(tok) => Err(ParseError::Expected {
                expected: "atomicrmw operation".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    /// Parse cmpxchg: (cmpxchg ordering ptr expected new)
    /// Returns { old_value, success_flag } struct
    fn parse_cmpxchg(&mut self) -> Result<Expr, ParseError> {
        // Parse the ordering
        let ordering = self.parse_ordering()?;

        // Parse the pointer
        let ptr = self.parse_expr()?;

        // Parse the expected value
        let expected = self.parse_expr()?;

        // Parse the new value
        let new_value = self.parse_expr()?;

        Ok(Expr::CmpXchg {
            ordering,
            ptr: Box::new(ptr),
            expected: Box::new(expected),
            new_value: Box::new(new_value),
        })
    }

    /// Parse fence: (fence ordering)
    fn parse_fence(&mut self) -> Result<Expr, ParseError> {
        // Parse the ordering
        let ordering = self.parse_ordering()?;
        Ok(Expr::Fence { ordering })
    }

    /// Parse let: (let ((name1 expr1) (name2 expr2) ...) body...)
    fn parse_let(&mut self) -> Result<Expr, ParseError> {
        // Expect opening paren for bindings list
        self.expect(Token::LParen)?;

        let mut bindings = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            self.expect(Token::LParen)?;

            // Parse binding name
            let name = match self.lexer.next_token_peeked()? {
                Some(Token::Ident(s)) => s,
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "binding name".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            };

            // Parse binding value
            let value = self.parse_expr()?;
            self.expect(Token::RParen)?;

            bindings.push((name, Box::new(value)));
        }
        self.expect(Token::RParen)?; // close bindings list

        // Parse body expressions until outer RParen
        let mut body = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            body.push(self.parse_expr()?);
        }

        if body.is_empty() {
            return Err(ParseError::Expected {
                expected: "let body expression".to_string(),
                found: ")".to_string(),
            });
        }

        Ok(Expr::Let { bindings, body })
    }

    /// Parse function definition body: (name return-type) (params...) body...
    fn parse_function_def(&mut self) -> Result<FunctionDef, ParseError> {
        // Parse (name return-type)
        self.expect(Token::LParen)?;
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "function name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        let return_type = match self.lexer.peek()? {
            Some(Token::Ident(ref s)) => {
                let s = s.clone(); // Clone to avoid borrow conflict
                let ret_ty = self.return_type_from_name(&s)?;
                self.lexer.next_token_peeked()?; // consume the type name
                ret_ty
            }
            Some(Token::LBrace) => {
                // Anonymous struct return type: { type, type, ... }
                self.parse_anon_struct_return_type()?
            }
            Some(tok) => {
                let found = format!("{}", tok);
                return Err(ParseError::Expected {
                    expected: "return type".to_string(),
                    found,
                });
            }
            None => return Err(ParseError::UnexpectedEof),
        };
        self.expect(Token::RParen)?;

        // Parse parameters: ((type name) ...)
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            // Parse (type name) or (own/ref/refmut type name)
            self.expect(Token::LParen)?;
            let (param_type, param_name) = self.parse_param()?;
            self.expect(Token::RParen)?;
            params.push(Param {
                ty: param_type,
                name: param_name,
            });
        }
        self.expect(Token::RParen)?;

        // Parse blocks
        let mut blocks = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            blocks.push(self.parse_block()?);
        }

        Ok(FunctionDef {
            name,
            return_type,
            params,
            blocks,
        })
    }

    /// Parse extern declaration: name return-type (param-types...)
    /// (declare name return-type (param-types...))
    /// (declare name return-type (param-types... ...))  ; varargs
    fn parse_extern_decl(&mut self) -> Result<ExternDecl, ParseError> {
        // Parse name
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "function name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse return type (allow void and ptr)
        let return_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "ptr" => ReturnType::Ptr,
            Some(Token::Ident(ref s)) => ReturnType::Scalar(self.type_from_name(s)?),
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "return type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse parameter types: (type type ... or type type ...)
        self.expect(Token::LParen)?;
        let mut param_types = Vec::new();
        let mut varargs = false;

        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            match self.lexer.next_token_peeked()? {
                Some(Token::Ident(ref s)) if s == "..." => {
                    varargs = true;
                    // ... must be last
                    if self.lexer.peek()? != Some(&Token::RParen) {
                        return Err(ParseError::UnexpectedToken(
                            "... must be last in parameter list".to_string(),
                        ));
                    }
                }
                Some(Token::Ident(ref s)) if s == "ptr" => {
                    param_types.push(ParamType::Ptr);
                }
                Some(Token::Ident(ref s)) => {
                    let ty = self.type_from_name(s)?;
                    param_types.push(ParamType::Scalar(ty));
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "parameter type".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        self.expect(Token::RParen)?;

        Ok(ExternDecl {
            name,
            return_type,
            param_types,
            varargs,
        })
    }

    /// Parse struct definition: name (field-types...)
    /// (defstruct point (double double))
    /// (defstruct person (ptr i32 double))
    fn parse_struct_def(&mut self) -> Result<StructDef, ParseError> {
        // Parse name
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "struct name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse field types: (type type ...)
        self.expect(Token::LParen)?;
        let mut fields = Vec::new();

        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            match self.lexer.peek()?.cloned() {
                Some(Token::LBrace) => {
                    // Anonymous struct field type: { ptr, ptr }
                    self.lexer.next_token_peeked()?; // consume {
                    let anon_struct = self.parse_anon_struct_param_type()?;
                    fields.push(anon_struct);
                }
                Some(Token::Ident(ref s)) if s == "ptr" => {
                    self.lexer.next_token_peeked()?; // consume ptr
                    fields.push(ParamType::Ptr);
                }
                Some(Token::Ident(ref s)) => {
                    let ty = self.type_from_name(s)?;
                    self.lexer.next_token_peeked()?; // consume type name
                    fields.push(ParamType::Scalar(ty));
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "field type".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }
        self.expect(Token::RParen)?;

        Ok(StructDef { name, fields })
    }

    /// Parse global definition: name type initializer [:constant]
    /// (global counter i32 (i32 0))
    /// (global pi double (double 3.14159) :constant)
    fn parse_global_def(&mut self) -> Result<GlobalDef, ParseError> {
        // Parse name
        let name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "global name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse type (scalar or ptr)
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "ptr" => ParamType::Ptr,
            Some(Token::Ident(ref s)) => ParamType::Scalar(self.type_from_name(s)?),
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse initializer expression
        let initializer = self.parse_expr()?;

        // Check for optional :constant flag
        let is_constant = if let Some(Token::Ident(ref s)) = self.lexer.peek()? {
            if s == ":constant" {
                self.lexer.next_token_peeked()?;
                true
            } else {
                false
            }
        } else {
            false
        };

        Ok(GlobalDef {
            name,
            ty,
            initializer,
            is_constant,
        })
    }

    /// Parse a basic block: (block label instructions...)
    fn parse_block(&mut self) -> Result<BasicBlock, ParseError> {
        self.expect(Token::LParen)?;

        // Expect 'block' keyword
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "block" => {}
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "block".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        }

        // Parse label
        let label = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "block label".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Parse instructions
        let mut instructions = Vec::new();
        while let Some(tok) = self.lexer.peek()? {
            if *tok == Token::RParen {
                break;
            }
            instructions.push(self.parse_expr()?);
        }
        self.expect(Token::RParen)?;

        Ok(BasicBlock {
            label,
            instructions,
        })
    }

    fn parse_int_literal(&mut self, type_name: &str) -> Result<Expr, ParseError> {
        let ty = self.type_from_name(type_name)?;
        let value = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "integer".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };
        Ok(Expr::IntLit { ty, value })
    }

    fn parse_float_literal(&mut self, type_name: &str) -> Result<Expr, ParseError> {
        let ty = self.type_from_name(type_name)?;
        let value = match self.lexer.next_token_peeked()? {
            Some(Token::Float(n)) => FloatValue::Number(n),
            Some(Token::Integer(n)) => FloatValue::Number(n as f64),
            Some(Token::Inf) => FloatValue::Inf,
            Some(Token::NegInf) => FloatValue::NegInf,
            Some(Token::Nan) => FloatValue::Nan,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "float".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };
        Ok(Expr::FloatLit { ty, value })
    }

    fn parse_ptr_literal(&mut self) -> Result<Expr, ParseError> {
        // Expect 'null' keyword for null pointer
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) if s == "null" => Ok(Expr::NullPtr),
            Some(tok) => Err(ParseError::Expected {
                expected: "null".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_string_literal(&mut self) -> Result<Expr, ParseError> {
        // Expect a string token
        match self.lexer.next_token_peeked()? {
            Some(Token::StringLit(s)) => Ok(Expr::StringLit(s)),
            Some(tok) => Err(ParseError::Expected {
                expected: "string literal".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_vector_literal(&mut self) -> Result<Expr, ParseError> {
        let ty = self.parse_vector_type()?;
        let mut elements = Vec::with_capacity(ty.count as usize);

        for _ in 0..ty.count {
            // Each element is either a bare number or a sub-expression
            match self.lexer.peek()? {
                Some(Token::Integer(_))
                | Some(Token::Float(_))
                | Some(Token::Inf)
                | Some(Token::NegInf)
                | Some(Token::Nan) => {
                    let value = self.parse_scalar_value(&ty.element)?;
                    elements.push(value);
                }
                Some(Token::LParen) => {
                    elements.push(self.parse_expr()?);
                }
                _ => {
                    return Err(ParseError::Expected {
                        expected: "vector element".to_string(),
                        found: format!("{:?}", self.lexer.peek()?),
                    })
                }
            }
        }

        // Don't expect RParen here - the caller handles it
        Ok(Expr::VectorLit { ty, elements })
    }

    fn parse_scalar_value(&mut self, ty: &ScalarType) -> Result<Expr, ParseError> {
        if ty.is_integer() {
            match self.lexer.next_token_peeked()? {
                Some(Token::Integer(n)) => Ok(Expr::IntLit {
                    ty: ty.clone(),
                    value: n,
                }),
                Some(tok) => Err(ParseError::Expected {
                    expected: "integer".to_string(),
                    found: format!("{}", tok),
                }),
                None => Err(ParseError::UnexpectedEof),
            }
        } else {
            let value = match self.lexer.next_token_peeked()? {
                Some(Token::Float(n)) => FloatValue::Number(n),
                Some(Token::Integer(n)) => FloatValue::Number(n as f64),
                Some(Token::Inf) => FloatValue::Inf,
                Some(Token::NegInf) => FloatValue::NegInf,
                Some(Token::Nan) => FloatValue::Nan,
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "float".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            };
            Ok(Expr::FloatLit {
                ty: ty.clone(),
                value,
            })
        }
    }

    fn parse_vector_type(&mut self) -> Result<VectorType, ParseError> {
        self.expect(Token::LAngle)?;
        let count = match self.lexer.next_token_peeked()? {
            Some(Token::Integer(n)) => n as u32,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "vector count".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Expect 'x'
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) if s == "x" => {}
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "x".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        }

        // Parse element type
        let element = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        self.expect(Token::RAngle)?;

        Ok(VectorType { count, element })
    }

    fn parse_binop<F>(&mut self, f: F) -> Result<Expr, ParseError>
    where
        F: FnOnce(Expr, Expr) -> Expr,
    {
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        Ok(f(lhs, rhs))
    }

    fn parse_icmp(&mut self) -> Result<Expr, ParseError> {
        let pred = self.parse_icmp_pred()?;
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        Ok(Expr::ICmp {
            pred,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn parse_fcmp(&mut self) -> Result<Expr, ParseError> {
        let pred = self.parse_fcmp_pred()?;
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        Ok(Expr::FCmp {
            pred,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    fn parse_icmp_pred(&mut self) -> Result<ICmpPred, ParseError> {
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => match s.as_str() {
                "eq" => Ok(ICmpPred::Eq),
                "ne" => Ok(ICmpPred::Ne),
                "slt" => Ok(ICmpPred::Slt),
                "sle" => Ok(ICmpPred::Sle),
                "sgt" => Ok(ICmpPred::Sgt),
                "sge" => Ok(ICmpPred::Sge),
                "ult" => Ok(ICmpPred::Ult),
                "ule" => Ok(ICmpPred::Ule),
                "ugt" => Ok(ICmpPred::Ugt),
                "uge" => Ok(ICmpPred::Uge),
                _ => Err(ParseError::UnknownPredicate(s)),
            },
            Some(tok) => Err(ParseError::Expected {
                expected: "icmp predicate".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_fcmp_pred(&mut self) -> Result<FCmpPred, ParseError> {
        match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => match s.as_str() {
                "oeq" => Ok(FCmpPred::Oeq),
                "one" => Ok(FCmpPred::One),
                "olt" => Ok(FCmpPred::Olt),
                "ole" => Ok(FCmpPred::Ole),
                "ogt" => Ok(FCmpPred::Ogt),
                "oge" => Ok(FCmpPred::Oge),
                "ord" => Ok(FCmpPred::Ord),
                "ueq" => Ok(FCmpPred::Ueq),
                "une" => Ok(FCmpPred::Une),
                "ult" => Ok(FCmpPred::Ult),
                "ule" => Ok(FCmpPred::Ule),
                "ugt" => Ok(FCmpPred::Ugt),
                "uge" => Ok(FCmpPred::Uge),
                "uno" => Ok(FCmpPred::Uno),
                _ => Err(ParseError::UnknownPredicate(s)),
            },
            Some(tok) => Err(ParseError::Expected {
                expected: "fcmp predicate".to_string(),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn parse_conversion<F>(&mut self, f: F) -> Result<Expr, ParseError>
    where
        F: FnOnce(ScalarType, Expr) -> Expr,
    {
        let ty = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };
        let value = self.parse_expr()?;
        Ok(f(ty, value))
    }

    fn parse_select(&mut self) -> Result<Expr, ParseError> {
        let cond = self.parse_expr()?;
        let true_val = self.parse_expr()?;
        let false_val = self.parse_expr()?;
        Ok(Expr::Select {
            cond: Box::new(cond),
            true_val: Box::new(true_val),
            false_val: Box::new(false_val),
        })
    }

    fn parse_extractelement(&mut self) -> Result<Expr, ParseError> {
        let vec = self.parse_expr()?;
        let idx = self.parse_expr()?;
        Ok(Expr::ExtractElement {
            vec: Box::new(vec),
            idx: Box::new(idx),
        })
    }

    fn parse_insertelement(&mut self) -> Result<Expr, ParseError> {
        let vec = self.parse_expr()?;
        let val = self.parse_expr()?;
        let idx = self.parse_expr()?;
        Ok(Expr::InsertElement {
            vec: Box::new(vec),
            val: Box::new(val),
            idx: Box::new(idx),
        })
    }

    fn parse_shufflevector(&mut self) -> Result<Expr, ParseError> {
        let vec1 = self.parse_expr()?;
        let vec2 = self.parse_expr()?;
        let mask = self.parse_expr()?;
        Ok(Expr::ShuffleVector {
            vec1: Box::new(vec1),
            vec2: Box::new(vec2),
            mask: Box::new(mask),
        })
    }

    fn type_from_name(&self, name: &str) -> Result<ScalarType, ParseError> {
        match name {
            "i1" => Ok(ScalarType::I1),
            "i8" => Ok(ScalarType::I8),
            "i16" => Ok(ScalarType::I16),
            "i32" => Ok(ScalarType::I32),
            "i64" => Ok(ScalarType::I64),
            "float" => Ok(ScalarType::Float),
            "double" => Ok(ScalarType::Double),
            "void" => Ok(ScalarType::Void),
            _ => Err(ParseError::UnknownType(name.to_string())),
        }
    }

    fn param_type_from_name(&self, name: &str) -> Result<ParamType, ParseError> {
        match name {
            "ptr" => Ok(ParamType::Ptr),
            "i1" => Ok(ParamType::Scalar(ScalarType::I1)),
            "i8" => Ok(ParamType::Scalar(ScalarType::I8)),
            "i16" => Ok(ParamType::Scalar(ScalarType::I16)),
            "i32" => Ok(ParamType::Scalar(ScalarType::I32)),
            "i64" => Ok(ParamType::Scalar(ScalarType::I64)),
            "float" => Ok(ParamType::Scalar(ScalarType::Float)),
            "double" => Ok(ParamType::Scalar(ScalarType::Double)),
            "void" => Ok(ParamType::Scalar(ScalarType::Void)),
            _ => Err(ParseError::UnknownType(name.to_string())),
        }
    }

    /// Parse a function parameter: (type name), (own/ref/refmut type name), or ({ type, type } name)
    fn parse_param(&mut self) -> Result<(ParamType, String), ParseError> {
        // Check for anonymous struct type: { type, type, ... }
        let peeked = self.lexer.peek()?.cloned();
        if matches!(peeked, Some(Token::LBrace)) {
            self.lexer.next_token_peeked()?; // consume '{'
            let anon_type = self.parse_anon_struct_param_type()?;
            // Parse parameter name
            let param_name = match self.lexer.next_token_peeked()? {
                Some(Token::Ident(s)) => s,
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "parameter name".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            };
            return Ok((anon_type, param_name));
        }

        let first = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "type or ownership modifier".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        // Check for ownership modifiers
        let param_type = match first.as_str() {
            "own" => {
                // Parse: own type name
                let inner_type = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(ref s)) => self.type_from_name(s)?,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "type".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                ParamType::Own(Box::new(inner_type))
            }
            "ref" => {
                // Parse: ref type name
                let inner_type = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(ref s)) => self.type_from_name(s)?,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "type".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                ParamType::Ref(Box::new(inner_type))
            }
            "refmut" => {
                // Parse: refmut type name
                let inner_type = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(ref s)) => self.type_from_name(s)?,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "type".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                ParamType::RefMut(Box::new(inner_type))
            }
            "rc" => {
                // Parse: rc type name
                let inner_type = match self.lexer.next_token_peeked()? {
                    Some(Token::Ident(ref s)) => self.type_from_name(s)?,
                    Some(tok) => {
                        return Err(ParseError::Expected {
                            expected: "type".to_string(),
                            found: format!("{}", tok),
                        })
                    }
                    None => return Err(ParseError::UnexpectedEof),
                };
                ParamType::Rc(Box::new(inner_type))
            }
            _ => {
                // Regular type
                self.param_type_from_name(&first)?
            }
        };

        // Parse parameter name
        let param_name = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(s)) => s,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "parameter name".to_string(),
                    found: format!("{}", tok),
                })
            }
            None => return Err(ParseError::UnexpectedEof),
        };

        Ok((param_type, param_name))
    }

    fn return_type_from_name(&self, name: &str) -> Result<ReturnType, ParseError> {
        match name {
            "ptr" => Ok(ReturnType::Ptr),
            "i1" => Ok(ReturnType::Scalar(ScalarType::I1)),
            "i8" => Ok(ReturnType::Scalar(ScalarType::I8)),
            "i16" => Ok(ReturnType::Scalar(ScalarType::I16)),
            "i32" => Ok(ReturnType::Scalar(ScalarType::I32)),
            "i64" => Ok(ReturnType::Scalar(ScalarType::I64)),
            "float" => Ok(ReturnType::Scalar(ScalarType::Float)),
            "double" => Ok(ReturnType::Scalar(ScalarType::Double)),
            "void" => Ok(ReturnType::Scalar(ScalarType::Void)),
            _ => Err(ParseError::UnknownType(name.to_string())),
        }
    }

    /// Parse anonymous struct return type: { type, type, ... }
    /// Parse an anonymous struct parameter type: { type, type, ... }
    /// Assumes the opening '{' has already been consumed.
    fn parse_anon_struct_param_type(&mut self) -> Result<ParamType, ParseError> {
        let mut fields = Vec::new();

        loop {
            // Peek to check for closing brace
            let token = self.lexer.peek()?.cloned();
            match token {
                Some(Token::RBrace) => {
                    self.lexer.next_token_peeked()?; // consume }
                    break;
                }
                Some(Token::Ident(ref s)) => {
                    let ty = self.param_type_from_name(s)?;
                    self.lexer.next_token_peeked()?; // consume the type name
                    fields.push(ty);

                    // Check for comma or closing brace
                    match self.lexer.peek()? {
                        Some(Token::Comma) => {
                            self.lexer.next_token_peeked()?; // consume comma
                        }
                        Some(Token::RBrace) => {
                            // Will be consumed in next iteration
                        }
                        Some(tok) => {
                            let found = format!("{}", tok);
                            return Err(ParseError::Expected {
                                expected: "',' or '}'".to_string(),
                                found,
                            });
                        }
                        None => return Err(ParseError::UnexpectedEof),
                    }
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "type name or '}'".to_string(),
                        found: format!("{}", tok),
                    });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        Ok(ParamType::AnonStruct(fields))
    }

    fn parse_anon_struct_return_type(&mut self) -> Result<ReturnType, ParseError> {
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();

        loop {
            // Peek to check for closing brace
            let token = self.lexer.peek()?.cloned();
            match token {
                Some(Token::RBrace) => {
                    self.lexer.next_token_peeked()?; // consume }
                    break;
                }
                Some(Token::Ident(ref s)) => {
                    let ty = self.param_type_from_name(s)?;
                    self.lexer.next_token_peeked()?; // consume the type name
                    fields.push(ty);

                    // Check for comma or closing brace
                    match self.lexer.peek()? {
                        Some(Token::Comma) => {
                            self.lexer.next_token_peeked()?; // consume comma
                        }
                        Some(Token::RBrace) => {
                            // Will be consumed in next iteration
                        }
                        Some(tok) => {
                            let found = format!("{}", tok);
                            return Err(ParseError::Expected {
                                expected: "',' or '}'".to_string(),
                                found,
                            });
                        }
                        None => return Err(ParseError::UnexpectedEof),
                    }
                }
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "type name or '}'".to_string(),
                        found: format!("{}", tok),
                    });
                }
                None => return Err(ParseError::UnexpectedEof),
            }
        }

        Ok(ReturnType::AnonStruct(fields))
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        match self.lexer.next_token_peeked()? {
            Some(tok) if tok == expected => Ok(()),
            Some(tok) => Err(ParseError::Expected {
                expected: format!("{}", expected),
                found: format!("{}", tok),
            }),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int_literal() {
        let expr = Parser::new("(i32 42)").parse().unwrap();
        assert!(matches!(
            expr,
            Expr::IntLit {
                ty: ScalarType::I32,
                value: 42
            }
        ));
    }

    #[test]
    fn test_parse_float_literal() {
        let expr = Parser::new("(double 3.14)").parse().unwrap();
        assert!(matches!(
            expr,
            Expr::FloatLit {
                ty: ScalarType::Double,
                value: FloatValue::Number(_)
            }
        ));
    }

    #[test]
    fn test_parse_add() {
        let expr = Parser::new("(add (i32 5) (i32 3))").parse().unwrap();
        assert!(matches!(expr, Expr::Add(_, _)));
    }

    #[test]
    fn test_parse_icmp() {
        let expr = Parser::new("(icmp eq (i32 5) (i32 5))").parse().unwrap();
        assert!(matches!(
            expr,
            Expr::ICmp {
                pred: ICmpPred::Eq,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_select() {
        let expr = Parser::new("(select (i1 1) (i32 10) (i32 20))")
            .parse()
            .unwrap();
        assert!(matches!(expr, Expr::Select { .. }));
    }

    #[test]
    fn test_parse_extern_decl() {
        let result = Parser::new("(declare malloc ptr (i64))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::ExternDecl(decl) => {
                assert_eq!(decl.name, "malloc");
                assert_eq!(decl.return_type, ReturnType::Ptr);
                assert_eq!(decl.param_types.len(), 1);
                assert!(!decl.varargs);
            }
            _ => panic!("expected ExternDecl"),
        }
    }

    #[test]
    fn test_parse_extern_decl_varargs() {
        let result = Parser::new("(declare printf i32 (ptr ...))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::ExternDecl(decl) => {
                assert_eq!(decl.name, "printf");
                assert_eq!(decl.return_type, ReturnType::Scalar(ScalarType::I32));
                assert_eq!(decl.param_types.len(), 1);
                assert!(decl.varargs);
            }
            _ => panic!("expected ExternDecl"),
        }
    }

    #[test]
    fn test_parse_extern_decl_void() {
        let result = Parser::new("(declare exit void (i32))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::ExternDecl(decl) => {
                assert_eq!(decl.name, "exit");
                assert_eq!(decl.return_type, ReturnType::Scalar(ScalarType::Void));
                assert_eq!(decl.param_types.len(), 1);
            }
            _ => panic!("expected ExternDecl"),
        }
    }

    #[test]
    fn test_parse_global_int() {
        let result = Parser::new("(global counter i32 (i32 0))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Global(global) => {
                assert_eq!(global.name, "counter");
                assert_eq!(global.ty, ParamType::Scalar(ScalarType::I32));
                assert!(!global.is_constant);
            }
            _ => panic!("expected Global"),
        }
    }

    #[test]
    fn test_parse_global_constant() {
        let result = Parser::new("(global pi double (double 3.14159) :constant)")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Global(global) => {
                assert_eq!(global.name, "pi");
                assert_eq!(global.ty, ParamType::Scalar(ScalarType::Double));
                assert!(global.is_constant);
            }
            _ => panic!("expected Global"),
        }
    }

    #[test]
    fn test_parse_global_ptr() {
        let result = Parser::new("(global buffer ptr null)")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Global(global) => {
                assert_eq!(global.name, "buffer");
                assert_eq!(global.ty, ParamType::Ptr);
                assert!(!global.is_constant);
            }
            _ => panic!("expected Global"),
        }
    }

    #[test]
    fn test_parse_string_literal() {
        let result = Parser::new("(string \"hello world\")").parse().unwrap();
        assert_eq!(result, Expr::StringLit("hello world".to_string()));
    }

    #[test]
    fn test_parse_string_with_escapes() {
        let result = Parser::new("(string \"hello\\nworld\")").parse().unwrap();
        assert_eq!(result, Expr::StringLit("hello\nworld".to_string()));
    }

    #[test]
    fn test_parse_getelementptr() {
        let result = Parser::new("(getelementptr i8 (ptr null) (i64 0))")
            .parse()
            .unwrap();
        match result {
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                assert_eq!(ty, GepType::Scalar(ScalarType::I8));
                assert!(matches!(*ptr, Expr::NullPtr));
                assert_eq!(indices.len(), 1);
                assert!(!inbounds);
            }
            _ => panic!("expected GetElementPtr"),
        }
    }

    #[test]
    fn test_parse_getelementptr_inbounds() {
        let result = Parser::new("(getelementptr inbounds i32 (ptr null) (i64 5) (i32 0))")
            .parse()
            .unwrap();
        match result {
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                assert_eq!(ty, GepType::Scalar(ScalarType::I32));
                assert!(matches!(*ptr, Expr::NullPtr));
                assert_eq!(indices.len(), 2);
                assert!(inbounds);
            }
            _ => panic!("expected GetElementPtr"),
        }
    }

    #[test]
    fn test_parse_getelementptr_struct_type() {
        let result = Parser::new("(getelementptr %struct.point (ptr null) (i32 0) (i32 1))")
            .parse()
            .unwrap();
        match result {
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                assert_eq!(ty, GepType::Struct("point".to_string()));
                assert!(matches!(*ptr, Expr::NullPtr));
                assert_eq!(indices.len(), 2);
                assert!(!inbounds);
            }
            _ => panic!("expected GetElementPtr"),
        }
    }

    #[test]
    fn test_parse_getelementptr_struct_type_inbounds() {
        let result =
            Parser::new("(getelementptr inbounds %struct.adder_env (ptr null) (i32 0) (i32 0))")
                .parse()
                .unwrap();
        match result {
            Expr::GetElementPtr {
                ty,
                ptr,
                indices,
                inbounds,
            } => {
                assert_eq!(ty, GepType::Struct("adder_env".to_string()));
                assert!(matches!(*ptr, Expr::NullPtr));
                assert_eq!(indices.len(), 2);
                assert!(inbounds);
            }
            _ => panic!("expected GetElementPtr"),
        }
    }

    #[test]
    fn test_parse_struct_def() {
        let result = Parser::new("(defstruct point (double double))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Struct(def) => {
                assert_eq!(def.name, "point");
                assert_eq!(def.fields.len(), 2);
                assert_eq!(def.fields[0], ParamType::Scalar(ScalarType::Double));
                assert_eq!(def.fields[1], ParamType::Scalar(ScalarType::Double));
            }
            _ => panic!("expected Struct"),
        }
    }

    #[test]
    fn test_parse_struct_def_mixed_types() {
        let result = Parser::new("(defstruct person (ptr i32 double))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Struct(def) => {
                assert_eq!(def.name, "person");
                assert_eq!(def.fields.len(), 3);
                assert_eq!(def.fields[0], ParamType::Ptr);
                assert_eq!(def.fields[1], ParamType::Scalar(ScalarType::I32));
                assert_eq!(def.fields[2], ParamType::Scalar(ScalarType::Double));
            }
            _ => panic!("expected Struct"),
        }
    }

    // =========================================================================
    // Higher-Level Pattern Tests
    // =========================================================================
    // These tests demonstrate that lIR can express patterns used by higher-level
    // languages (closures, protocols, etc.) without any language-specific support.
    // lIR is purely generic primitives - the patterns emerge from composition.

    #[test]
    fn test_closure_pattern_env_struct() {
        // Environment struct for captured variables - pure lIR
        let result = Parser::new("(defstruct __env_0 (i64 i64))")
            .parse_item()
            .unwrap();
        match result {
            ParseResult::Struct(def) => {
                assert_eq!(def.name, "__env_0");
                assert_eq!(def.fields.len(), 2);
            }
            _ => panic!("expected struct"),
        }
    }

    #[test]
    fn test_closure_pattern_struct_literal() {
        // Closure struct literal: { fn_ptr, env_ptr }
        let expr = Parser::new("{ @__lambda_0 env_ptr }").parse().unwrap();
        match expr {
            Expr::StructLit(fields) => {
                assert_eq!(fields.len(), 2);
                assert!(matches!(fields[0], Expr::GlobalRef(_)));
                assert!(matches!(fields[1], Expr::LocalRef(_)));
            }
            _ => panic!("expected struct literal"),
        }
    }

    #[test]
    fn test_closure_pattern_extract_fn_ptr() {
        // Extract function pointer from closure struct
        let expr = Parser::new("(extractvalue closure 0)").parse().unwrap();
        assert!(matches!(expr, Expr::ExtractValue { indices, .. } if indices == vec![0]));
    }

    #[test]
    fn test_closure_pattern_indirect_call() {
        // Indirect call through function pointer
        let expr = Parser::new("(indirect-call fn_ptr i64 env_ptr x)")
            .parse()
            .unwrap();
        match expr {
            Expr::IndirectCall { args, .. } => {
                assert_eq!(args.len(), 2);
            }
            _ => panic!("expected indirect call"),
        }
    }

    #[test]
    fn test_atom_pattern_atomic_ops() {
        // Atomic operations used to implement atom semantics
        let load = Parser::new("(atomic-load seq_cst i64 ptr)")
            .parse()
            .unwrap();
        assert!(matches!(load, Expr::AtomicLoad { .. }));

        let store = Parser::new("(atomic-store seq_cst (i64 42) ptr)")
            .parse()
            .unwrap();
        assert!(matches!(store, Expr::AtomicStore { .. }));
    }

    #[test]
    fn test_protocol_pattern_direct_dispatch() {
        // Protocol dispatch compiles to direct function call
        // (greet person) → (call @__Greet_Person__greet person)
        let expr = Parser::new("(call @__Greet_Person__greet person)")
            .parse()
            .unwrap();
        match expr {
            Expr::Call { name, args } => {
                assert_eq!(name, "__Greet_Person__greet");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected call"),
        }
    }
}

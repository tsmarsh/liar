//! Parser for lIR S-expressions

use crate::ast::*;
use crate::error::ParseError;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

/// Result of parsing: either an expression, function definition, extern declaration, or global
#[derive(Debug, Clone, PartialEq)]
pub enum ParseResult {
    Expr(Expr),
    Function(FunctionDef),
    ExternDecl(ExternDecl),
    Global(GlobalDef),
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
    pub fn parse_item(&mut self) -> Result<ParseResult, ParseError> {
        // Peek to see if this is a function definition or declaration
        self.expect(Token::LParen)?;

        let token = self.lexer.peek()?.ok_or(ParseError::UnexpectedEof)?.clone();

        if let Token::Ident(ref name) = token {
            if name == "define" {
                self.lexer.next_token_peeked()?; // consume "define"
                let func = self.parse_function_def()?;
                self.expect(Token::RParen)?;
                // Ensure we consumed all input
                if self.lexer.peek()?.is_some() {
                    return Err(ParseError::UnexpectedToken("trailing input".to_string()));
                }
                return Ok(ParseResult::Function(func));
            } else if name == "declare" {
                self.lexer.next_token_peeked()?; // consume "declare"
                let decl = self.parse_extern_decl()?;
                self.expect(Token::RParen)?;
                // Ensure we consumed all input
                if self.lexer.peek()?.is_some() {
                    return Err(ParseError::UnexpectedToken("trailing input".to_string()));
                }
                return Ok(ParseResult::ExternDecl(decl));
            } else if name == "global" {
                self.lexer.next_token_peeked()?; // consume "global"
                let global = self.parse_global_def()?;
                self.expect(Token::RParen)?;
                // Ensure we consumed all input
                if self.lexer.peek()?.is_some() {
                    return Err(ParseError::UnexpectedToken("trailing input".to_string()));
                }
                return Ok(ParseResult::Global(global));
            }
        }

        // Not a function - parse as expression
        // We already consumed LParen, so parse the form directly
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

        // Ensure we consumed all input
        if self.lexer.peek()?.is_some() {
            return Err(ParseError::UnexpectedToken("trailing input".to_string()));
        }

        Ok(ParseResult::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek()? {
            Some(Token::LParen) => self.parse_sexpr(),
            Some(Token::LAngle) => self.parse_vector_literal(),
            Some(Token::Ident(ref s)) if s.starts_with('%') => {
                // Local variable reference
                let name = s.clone();
                self.lexer.next_token_peeked()?; // consume the token
                Ok(Expr::LocalRef(name[1..].to_string())) // strip the %
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

            // Return instruction
            "ret" => self.parse_ret(),

            // Memory operations
            "alloca" => self.parse_alloca(),
            "load" => self.parse_load(),
            "store" => self.parse_store(),

            // Control flow
            "br" => self.parse_br(),
            "phi" => self.parse_phi(),

            // Function call
            "call" => self.parse_call(),

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
        // Parse the type to allocate
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

        // Check for optional count
        let count = match self.lexer.peek()? {
            Some(Token::RParen) => None,
            _ => Some(Box::new(self.parse_expr()?)),
        };

        Ok(Expr::Alloca { ty, count })
    }

    /// Parse load: (load type ptr)
    fn parse_load(&mut self) -> Result<Expr, ParseError> {
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

        let return_type = match self.lexer.next_token_peeked()? {
            Some(Token::Ident(ref s)) => self.type_from_name(s)?,
            Some(tok) => {
                return Err(ParseError::Expected {
                    expected: "return type".to_string(),
                    found: format!("{}", tok),
                })
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
            // Parse (type name)
            self.expect(Token::LParen)?;
            let param_type = match self.lexer.next_token_peeked()? {
                Some(Token::Ident(ref s)) => self.type_from_name(s)?,
                Some(tok) => {
                    return Err(ParseError::Expected {
                        expected: "parameter type".to_string(),
                        found: format!("{}", tok),
                    })
                }
                None => return Err(ParseError::UnexpectedEof),
            };
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
}

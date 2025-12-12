//! Parser - S-expression parsing to AST

use crate::ast::*;
use crate::error::{CompileError, Result};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::span::{Span, Spanned};

/// Parser state
pub struct Parser<'a> {
    tokens: Vec<Token>,
    pos: usize,
    #[allow(dead_code)]
    source: &'a str,
    eof_token: Token,
    /// Counter for generating unique destructuring temp variable names
    destr_counter: u64,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Result<Self> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize()?;
        Ok(Self {
            tokens,
            pos: 0,
            source,
            eof_token: Token::new(TokenKind::Eof, Span::default()),
            destr_counter: 0,
        })
    }

    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program> {
        let mut items = Vec::new();

        while !self.is_eof() {
            items.push(self.parse_item()?);
        }

        Ok(Program { items })
    }

    /// Parse a single top-level item
    fn parse_item(&mut self) -> Result<Spanned<Item>> {
        let start = self.current_span();
        self.expect(TokenKind::LParen)?;

        let item = match &self.peek().kind {
            TokenKind::Defun => {
                self.advance();
                Item::Defun(self.parse_defun()?)
            }
            TokenKind::Def => {
                self.advance();
                Item::Def(self.parse_def()?)
            }
            TokenKind::Defstruct => {
                self.advance();
                Item::Defstruct(self.parse_defstruct()?)
            }
            TokenKind::Defprotocol => {
                self.advance();
                Item::Defprotocol(self.parse_defprotocol()?)
            }
            TokenKind::ExtendProtocol => {
                self.advance();
                Item::ExtendProtocol(self.parse_extend_protocol()?)
            }
            TokenKind::Defmacro => {
                self.advance();
                Item::Defmacro(self.parse_defmacro()?)
            }
            TokenKind::Extern => {
                self.advance();
                Item::Extern(self.parse_extern()?)
            }
            _ => return Err(CompileError::parse(
                self.current_span(),
                "expected top-level form: defun, def, defstruct, defprotocol, extend-protocol, defmacro, or extern",
            )),
        };

        self.expect(TokenKind::RParen)?;
        let span = start.merge(self.prev_span());

        Ok(Spanned::new(item, span))
    }

    /// Parse function definition
    fn parse_defun(&mut self) -> Result<Defun> {
        let name = self.parse_symbol()?;
        let params = self.parse_params()?;

        // Optional return type annotation
        let return_type = if self.check(TokenKind::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_expr()?;

        Ok(Defun {
            name,
            params,
            return_type,
            body,
        })
    }

    /// Parse parameter list
    fn parse_params(&mut self) -> Result<Vec<Param>> {
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();

        while !self.check(TokenKind::RParen) {
            params.push(self.parse_param()?);
        }

        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    /// Parse a single parameter
    fn parse_param(&mut self) -> Result<Param> {
        // Check for & (mutable reference parameter)
        let mutable = if self.check(TokenKind::Amp) {
            self.advance();
            true
        } else {
            false
        };

        let name = self.parse_symbol()?;

        // Optional type annotation: name: Type
        let ty = if self.check(TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(Param { name, ty, mutable })
    }

    /// Parse def (constant definition)
    fn parse_def(&mut self) -> Result<Def> {
        let name = self.parse_symbol()?;
        let value = self.parse_expr()?;
        Ok(Def { name, value })
    }

    /// Parse defstruct
    fn parse_defstruct(&mut self) -> Result<Defstruct> {
        let name = self.parse_symbol()?;
        self.expect(TokenKind::LParen)?;

        let mut fields = Vec::new();
        while !self.check(TokenKind::RParen) {
            let field_name = self.parse_symbol()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;
            fields.push(StructField {
                name: field_name,
                ty,
            });
        }

        self.expect(TokenKind::RParen)?;
        Ok(Defstruct { name, fields })
    }

    /// Parse protocol definition: (defprotocol Name "docstring"? (method [self args...] "doc"?)...)
    fn parse_defprotocol(&mut self) -> Result<Defprotocol> {
        let name = self.parse_symbol()?;

        // Optional docstring
        let doc = if let TokenKind::String(s) = &self.peek().kind {
            let s = s.clone();
            self.advance();
            Some(s)
        } else {
            None
        };

        // Parse method signatures
        let mut methods = Vec::new();
        while !self.check(TokenKind::RParen) {
            self.expect(TokenKind::LParen)?;
            let method_name = self.parse_symbol()?;

            // Parse parameter list [self arg1 arg2]
            self.expect(TokenKind::LBracket)?;
            let mut params = Vec::new();
            while !self.check(TokenKind::RBracket) {
                params.push(self.parse_symbol()?);
            }
            self.expect(TokenKind::RBracket)?;

            // Optional method docstring
            let method_doc = if let TokenKind::String(s) = &self.peek().kind {
                let s = s.clone();
                self.advance();
                Some(s)
            } else {
                None
            };

            self.expect(TokenKind::RParen)?;
            methods.push(ProtocolMethod {
                name: method_name,
                params,
                doc: method_doc,
            });
        }

        Ok(Defprotocol { name, doc, methods })
    }

    /// Parse macro definition: (defmacro name (params...) body)
    fn parse_defmacro(&mut self) -> Result<Defmacro> {
        let name = self.parse_symbol()?;

        // Parse simple parameter list (just names, no types for macros)
        self.expect(TokenKind::LParen)?;
        let mut params = Vec::new();
        while !self.check(TokenKind::RParen) {
            params.push(self.parse_symbol()?);
        }
        self.expect(TokenKind::RParen)?;

        let body = self.parse_expr()?;

        Ok(Defmacro { name, params, body })
    }

    /// Parse protocol extension: (extend-protocol ProtocolName TypeName (method [self args...] body)...)
    fn parse_extend_protocol(&mut self) -> Result<ExtendProtocol> {
        let protocol = self.parse_symbol()?;
        let type_name = self.parse_symbol()?;

        // Parse method implementations
        let mut implementations = Vec::new();
        while !self.check(TokenKind::RParen) {
            self.expect(TokenKind::LParen)?;
            let method_name = self.parse_symbol()?;

            // Parse parameter list [self arg1 arg2]
            self.expect(TokenKind::LBracket)?;
            let mut params = Vec::new();
            while !self.check(TokenKind::RBracket) {
                params.push(self.parse_symbol()?);
            }
            self.expect(TokenKind::RBracket)?;

            // Parse body
            let body = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;

            implementations.push(MethodImpl {
                name: method_name,
                params,
                body,
            });
        }

        Ok(ExtendProtocol {
            protocol,
            type_name,
            implementations,
        })
    }

    /// Parse extern declaration: (extern name ret-type (param-types...))
    /// Optionally with varargs: (extern name ret-type (param-types... ...))
    fn parse_extern(&mut self) -> Result<Extern> {
        let name = self.parse_symbol()?;
        let return_type = self.parse_type()?;

        // Parse parameter types list
        self.expect(TokenKind::LParen)?;
        let mut param_types = Vec::new();
        let mut varargs = false;

        while !self.check(TokenKind::RParen) {
            // Check for varargs marker "..."
            if self.check(TokenKind::Ellipsis) {
                self.advance();
                varargs = true;
                break;
            }
            param_types.push(self.parse_type()?);
        }
        self.expect(TokenKind::RParen)?;

        Ok(Extern {
            name,
            return_type,
            param_types,
            varargs,
        })
    }

    /// Parse a type annotation
    fn parse_type(&mut self) -> Result<Spanned<Type>> {
        let start = self.current_span();

        let ty = if self.check(TokenKind::Amp) {
            self.advance();
            let inner = self.parse_type()?;
            Type::Ref(Box::new(inner.node))
        } else if self.check(TokenKind::LParen) {
            self.advance();

            if self.check(TokenKind::Arrow) {
                // Function type: (-> (A B) C)
                self.advance();
                self.expect(TokenKind::LParen)?;
                let mut params = Vec::new();
                while !self.check(TokenKind::RParen) {
                    params.push(self.parse_type()?.node);
                }
                self.expect(TokenKind::RParen)?;
                let ret = self.parse_type()?;
                self.expect(TokenKind::RParen)?;
                Type::Fn(params, Box::new(ret.node))
            } else if self.check(TokenKind::RParen) {
                // Unit type: ()
                self.advance();
                Type::Unit
            } else {
                // Tuple type: (A B C)
                let mut types = Vec::new();
                while !self.check(TokenKind::RParen) {
                    types.push(self.parse_type()?.node);
                }
                self.expect(TokenKind::RParen)?;
                Type::Tuple(types)
            }
        } else {
            let name = self.parse_symbol()?;
            Type::Named(name.node)
        };

        let span = start.merge(self.prev_span());
        Ok(Spanned::new(ty, span))
    }

    /// Parse an expression
    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>> {
        let start = self.current_span();

        let expr = match &self.peek().kind {
            TokenKind::Int(n) => {
                let n = *n;
                self.advance();
                Expr::Int(n)
            }
            TokenKind::Float(f) => {
                let f = *f;
                self.advance();
                Expr::Float(f)
            }
            TokenKind::String(s) => {
                let s = s.clone();
                self.advance();
                Expr::String(s)
            }
            TokenKind::True => {
                self.advance();
                Expr::Bool(true)
            }
            TokenKind::False => {
                self.advance();
                Expr::Bool(false)
            }
            TokenKind::Nil => {
                self.advance();
                Expr::Nil
            }
            TokenKind::Quote => {
                self.advance();
                let sym = self.parse_symbol()?;
                Expr::Quote(sym.node)
            }
            TokenKind::At => {
                self.advance();
                let expr = self.parse_expr()?;
                Expr::AtomDeref(Box::new(expr))
            }
            TokenKind::Backtick => {
                self.advance();
                let expr = self.parse_expr()?;
                Expr::Quasiquote(Box::new(expr))
            }
            TokenKind::Comma => {
                self.advance();
                let expr = self.parse_expr()?;
                Expr::Unquote(Box::new(expr))
            }
            TokenKind::CommaAt => {
                self.advance();
                let expr = self.parse_expr()?;
                Expr::UnquoteSplicing(Box::new(expr))
            }
            TokenKind::Keyword(s) => {
                let s = s.clone();
                self.advance();
                Expr::Keyword(s)
            }
            TokenKind::LBracket => {
                self.advance();
                let elements = self.parse_vector_elements()?;
                self.expect(TokenKind::RBracket)?;
                Expr::Vector(elements)
            }
            TokenKind::LBrace => {
                self.advance();
                let pairs = self.parse_map_pairs()?;
                self.expect(TokenKind::RBrace)?;
                Expr::Map(pairs)
            }
            // Conventional mutable collections
            TokenKind::LAngleBracket => {
                self.advance();
                let elements = self.parse_conv_vector_elements()?;
                self.expect(TokenKind::RBracketAngle)?;
                Expr::ConvVector(elements)
            }
            TokenKind::LAngleBrace => {
                self.advance();
                let pairs = self.parse_conv_map_pairs()?;
                self.expect(TokenKind::RBraceAngle)?;
                Expr::ConvMap(pairs)
            }
            // SIMD vectors
            TokenKind::DoubleLAngle => {
                self.advance();
                let elements = self.parse_simd_elements()?;
                self.expect(TokenKind::DoubleRAngle)?;
                Expr::SimdVector(elements)
            }
            // Byte arrays: #[1 2 3]
            TokenKind::HashLBracket => {
                self.advance();
                let bytes = self.parse_byte_array_elements()?;
                self.expect(TokenKind::RBracket)?;
                Expr::ByteArray(bytes)
            }
            // Regex literals: #r"pattern"flags
            TokenKind::Regex(pattern, flags) => {
                let pattern = pattern.clone();
                let flags = flags.clone();
                self.advance();
                Expr::Regex { pattern, flags }
            }
            TokenKind::Symbol(s) => {
                let s = s.clone();
                self.advance();
                Expr::Var(s)
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_compound_expr()?;
                self.expect(TokenKind::RParen)?;
                expr
            }
            _ => {
                return Err(CompileError::parse(
                    self.current_span(),
                    format!("unexpected token: {:?}", self.peek().kind),
                ))
            }
        };

        let span = start.merge(self.prev_span());
        Ok(Spanned::new(expr, span))
    }

    /// Parse a compound expression (inside parens)
    fn parse_compound_expr(&mut self) -> Result<Expr> {
        match &self.peek().kind {
            TokenKind::Let => {
                self.advance();
                self.parse_let(false)
            }
            TokenKind::Plet => {
                self.advance();
                self.parse_let(true)
            }
            TokenKind::Fn => {
                self.advance();
                self.parse_lambda()
            }
            TokenKind::If => {
                self.advance();
                self.parse_if()
            }
            TokenKind::Do => {
                self.advance();
                self.parse_do()
            }
            TokenKind::Set => {
                self.advance();
                self.parse_set()
            }
            TokenKind::Ref => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Expr::Ref(Box::new(expr)))
            }
            TokenKind::RefMut => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Expr::RefMut(Box::new(expr)))
            }
            TokenKind::Deref => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Expr::Deref(Box::new(expr)))
            }
            TokenKind::Unsafe => {
                self.advance();
                let body = self.parse_expr()?;
                Ok(Expr::Unsafe(Box::new(body)))
            }
            TokenKind::Dot => {
                self.advance();
                self.parse_field_access()
            }
            // Atom operations
            TokenKind::Atom => {
                self.advance();
                let value = self.parse_expr()?;
                Ok(Expr::Atom(Box::new(value)))
            }
            TokenKind::Swap => {
                self.advance();
                let atom = self.parse_expr()?;
                let func = self.parse_expr()?;
                Ok(Expr::Swap(Box::new(atom), Box::new(func)))
            }
            TokenKind::Reset => {
                self.advance();
                let atom = self.parse_expr()?;
                let value = self.parse_expr()?;
                Ok(Expr::Reset(Box::new(atom), Box::new(value)))
            }
            TokenKind::CompareAndSet => {
                self.advance();
                let atom = self.parse_expr()?;
                let old = self.parse_expr()?;
                let new = self.parse_expr()?;
                Ok(Expr::CompareAndSet {
                    atom: Box::new(atom),
                    old: Box::new(old),
                    new: Box::new(new),
                })
            }
            // Async/await
            TokenKind::Async => {
                self.advance();
                let body = self.parse_expr()?;
                Ok(Expr::Async(Box::new(body)))
            }
            TokenKind::Await => {
                self.advance();
                let future = self.parse_expr()?;
                Ok(Expr::Await(Box::new(future)))
            }
            // STM operations
            TokenKind::Dosync => {
                self.advance();
                let mut exprs = Vec::new();
                while !self.check(TokenKind::RParen) {
                    exprs.push(self.parse_expr()?);
                }
                Ok(Expr::Dosync(exprs))
            }
            TokenKind::RefSet => {
                self.advance();
                let ref_expr = self.parse_expr()?;
                let value = self.parse_expr()?;
                Ok(Expr::RefSetStm(Box::new(ref_expr), Box::new(value)))
            }
            TokenKind::Alter => {
                self.advance();
                let ref_expr = self.parse_expr()?;
                let fn_expr = self.parse_expr()?;
                let mut args = Vec::new();
                while !self.check(TokenKind::RParen) {
                    args.push(self.parse_expr()?);
                }
                Ok(Expr::Alter {
                    ref_expr: Box::new(ref_expr),
                    fn_expr: Box::new(fn_expr),
                    args,
                })
            }
            TokenKind::Commute => {
                self.advance();
                let ref_expr = self.parse_expr()?;
                let fn_expr = self.parse_expr()?;
                let mut args = Vec::new();
                while !self.check(TokenKind::RParen) {
                    args.push(self.parse_expr()?);
                }
                Ok(Expr::Commute {
                    ref_expr: Box::new(ref_expr),
                    fn_expr: Box::new(fn_expr),
                    args,
                })
            }
            // Iterator operations
            TokenKind::Iter => {
                self.advance();
                let coll = self.parse_expr()?;
                Ok(Expr::Iter(Box::new(coll)))
            }
            TokenKind::Collect => {
                self.advance();
                let iter = self.parse_expr()?;
                Ok(Expr::Collect(Box::new(iter)))
            }
            // Overflow handling (ADR-017)
            TokenKind::Boxed => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Expr::Boxed(Box::new(expr)))
            }
            TokenKind::Wrapping => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Expr::Wrapping(Box::new(expr)))
            }
            // Macro support
            TokenKind::Gensym => {
                self.advance();
                // Optional prefix string
                let prefix = if let TokenKind::String(s) = &self.peek().kind {
                    let s = s.clone();
                    self.advance();
                    Some(s)
                } else {
                    None
                };
                Ok(Expr::Gensym(prefix))
            }
            _ => self.parse_call(),
        }
    }

    /// Parse let binding
    fn parse_let(&mut self, parallel: bool) -> Result<Expr> {
        self.expect(TokenKind::LParen)?;
        let mut bindings = Vec::new();

        while !self.check(TokenKind::RParen) {
            self.expect(TokenKind::LParen)?;

            // Check for destructuring pattern: ((StructName field1 field2 ...) value)
            if self.check(TokenKind::LParen) {
                let destr_bindings = self.parse_destructuring_binding()?;
                bindings.extend(destr_bindings);
            } else {
                // Regular binding: (name value) or (name: type value)
                let name = self.parse_symbol()?;

                let ty = if self.check(TokenKind::Colon) {
                    self.advance();
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = self.parse_expr()?;
                self.expect(TokenKind::RParen)?;

                bindings.push(LetBinding { name, ty, value });
            }
        }

        self.expect(TokenKind::RParen)?;
        let body = self.parse_expr()?;

        if parallel {
            Ok(Expr::Plet(bindings, Box::new(body)))
        } else {
            Ok(Expr::Let(bindings, Box::new(body)))
        }
    }

    /// Parse a destructuring binding pattern and desugar it
    /// Input: ((StructName field1 field2 ...) value)
    /// Output: bindings for temp var and field accesses
    fn parse_destructuring_binding(&mut self) -> Result<Vec<LetBinding>> {
        let span = self.current_span();

        // Parse the pattern: (StructName field1 field2 ...)
        self.expect(TokenKind::LParen)?;

        // First symbol is the struct name (not used in desugaring, just for documentation)
        let _struct_name = self.parse_symbol()?;

        // Remaining symbols are field names
        let mut field_names = Vec::new();
        while !self.check(TokenKind::RParen) {
            field_names.push(self.parse_symbol()?);
        }
        self.expect(TokenKind::RParen)?;

        // Parse the value expression
        let value = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;

        // Generate temp variable name
        let temp_name = format!("__destr_{}", self.destr_counter);
        self.destr_counter += 1;

        let mut result = Vec::new();

        // First binding: temp = value
        result.push(LetBinding {
            name: Spanned::new(temp_name.clone(), span),
            ty: None,
            value,
        });

        // Field bindings: fieldN = (. temp fieldN)
        for field in field_names {
            let field_access = Expr::Field(
                Box::new(Spanned::new(Expr::Var(temp_name.clone()), span)),
                field.clone(),
            );
            result.push(LetBinding {
                name: field,
                ty: None,
                value: Spanned::new(field_access, span),
            });
        }

        Ok(result)
    }

    /// Parse lambda
    fn parse_lambda(&mut self) -> Result<Expr> {
        let params = self.parse_params()?;
        let body = self.parse_expr()?;
        Ok(Expr::Lambda(params, Box::new(body)))
    }

    /// Parse if expression
    fn parse_if(&mut self) -> Result<Expr> {
        let cond = self.parse_expr()?;
        let then = self.parse_expr()?;
        let else_ = self.parse_expr()?;
        Ok(Expr::If(Box::new(cond), Box::new(then), Box::new(else_)))
    }

    /// Parse do block
    fn parse_do(&mut self) -> Result<Expr> {
        let mut exprs = Vec::new();
        while !self.check(TokenKind::RParen) {
            exprs.push(self.parse_expr()?);
        }
        Ok(Expr::Do(exprs))
    }

    /// Parse set! (mutation)
    fn parse_set(&mut self) -> Result<Expr> {
        let name = self.parse_symbol()?;
        let value = self.parse_expr()?;
        Ok(Expr::Set(name, Box::new(value)))
    }

    /// Parse field access
    fn parse_field_access(&mut self) -> Result<Expr> {
        let expr = self.parse_expr()?;
        let field = self.parse_symbol()?;
        Ok(Expr::Field(Box::new(expr), field))
    }

    /// Parse function call
    fn parse_call(&mut self) -> Result<Expr> {
        let func = self.parse_expr()?;
        let mut args = Vec::new();

        while !self.check(TokenKind::RParen) {
            args.push(self.parse_expr()?);
        }

        Ok(Expr::Call(Box::new(func), args))
    }

    /// Parse a symbol
    fn parse_symbol(&mut self) -> Result<Spanned<String>> {
        let span = self.current_span();
        match &self.peek().kind {
            TokenKind::Symbol(s) => {
                let s = s.clone();
                self.advance();
                Ok(Spanned::new(s, span))
            }
            _ => Err(CompileError::parse(span, "expected symbol")),
        }
    }

    /// Parse vector elements: 1 2 3
    fn parse_vector_elements(&mut self) -> Result<Vec<Spanned<Expr>>> {
        let mut elements = Vec::new();
        while !self.check(TokenKind::RBracket) {
            elements.push(self.parse_expr()?);
        }
        Ok(elements)
    }

    /// Parse map pairs: :a 1 :b 2
    fn parse_map_pairs(&mut self) -> Result<Vec<(Spanned<Expr>, Spanned<Expr>)>> {
        let mut pairs = Vec::new();
        while !self.check(TokenKind::RBrace) {
            let key = self.parse_expr()?;
            let value = self.parse_expr()?;
            pairs.push((key, value));
        }
        Ok(pairs)
    }

    /// Parse conventional vector elements: 1 2 3 (terminated by ]>)
    fn parse_conv_vector_elements(&mut self) -> Result<Vec<Spanned<Expr>>> {
        let mut elements = Vec::new();
        while !self.check(TokenKind::RBracketAngle) {
            elements.push(self.parse_expr()?);
        }
        Ok(elements)
    }

    /// Parse conventional map pairs: :a 1 :b 2 (terminated by }>)
    fn parse_conv_map_pairs(&mut self) -> Result<Vec<(Spanned<Expr>, Spanned<Expr>)>> {
        let mut pairs = Vec::new();
        while !self.check(TokenKind::RBraceAngle) {
            let key = self.parse_expr()?;
            let value = self.parse_expr()?;
            pairs.push((key, value));
        }
        Ok(pairs)
    }

    /// Parse SIMD vector elements: 1 2 3 4 (terminated by >>)
    fn parse_simd_elements(&mut self) -> Result<Vec<Spanned<Expr>>> {
        let mut elements = Vec::new();
        while !self.check(TokenKind::DoubleRAngle) {
            elements.push(self.parse_expr()?);
        }
        Ok(elements)
    }

    /// Parse byte array elements: 1 2 3 or 0x61 0x62 (terminated by ])
    fn parse_byte_array_elements(&mut self) -> Result<Vec<u8>> {
        let mut bytes = Vec::new();
        while !self.check(TokenKind::RBracket) {
            let span = self.current_span();
            match &self.peek().kind {
                TokenKind::Int(n) => {
                    let n = *n;
                    self.advance();
                    if !(0..=255).contains(&n) {
                        return Err(CompileError::parse(
                            span,
                            format!("byte value {} out of range (0-255)", n),
                        ));
                    }
                    bytes.push(n as u8);
                }
                _ => {
                    return Err(CompileError::parse(
                        span,
                        "expected integer (0-255) in byte array",
                    ))
                }
            }
        }
        Ok(bytes)
    }

    // Helper methods

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&self.eof_token)
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.pos];
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }

    fn check(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(&kind)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<()> {
        if self.check(kind.clone()) {
            self.advance();
            Ok(())
        } else {
            Err(CompileError::parse(
                self.current_span(),
                format!("expected {:?}, found {:?}", kind, self.peek().kind),
            ))
        }
    }

    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn current_span(&self) -> Span {
        self.peek().span
    }

    fn prev_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_expr() {
        let mut parser = Parser::new("(+ 1 2)").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Call(func, args) => {
                assert!(matches!(func.node, Expr::Var(s) if s == "+"));
                assert_eq!(args.len(), 2);
            }
            _ => panic!("expected call"),
        }
    }

    #[test]
    fn test_parse_atom() {
        let mut parser = Parser::new("(atom 0)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Atom(_)));
    }

    #[test]
    fn test_parse_atom_deref() {
        let mut parser = Parser::new("@counter").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::AtomDeref(inner) => {
                assert!(matches!(inner.node, Expr::Var(s) if s == "counter"));
            }
            _ => panic!("expected atom deref"),
        }
    }

    #[test]
    fn test_parse_reset() {
        let mut parser = Parser::new("(reset! counter 10)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Reset(_, _)));
    }

    #[test]
    fn test_parse_swap() {
        let mut parser = Parser::new("(swap! counter inc)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Swap(_, _)));
    }

    #[test]
    fn test_parse_compare_and_set() {
        let mut parser = Parser::new("(compare-and-set! counter 0 1)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::CompareAndSet { .. }));
    }

    #[test]
    fn test_parse_vector() {
        let mut parser = Parser::new("[1 2 3]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Vector(elements) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("expected vector"),
        }
    }

    #[test]
    fn test_parse_empty_vector() {
        let mut parser = Parser::new("[]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Vector(elements) => {
                assert!(elements.is_empty());
            }
            _ => panic!("expected empty vector"),
        }
    }

    #[test]
    fn test_parse_map() {
        let mut parser = Parser::new("{:a 1 :b 2}").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Map(pairs) => {
                assert_eq!(pairs.len(), 2);
            }
            _ => panic!("expected map"),
        }
    }

    #[test]
    fn test_parse_keyword() {
        let mut parser = Parser::new(":foo").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Keyword(name) => {
                assert_eq!(name, "foo");
            }
            _ => panic!("expected keyword"),
        }
    }

    #[test]
    fn test_parse_nested_collections() {
        let mut parser = Parser::new("[1 [2 3] {:a 4}]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Vector(elements) => {
                assert_eq!(elements.len(), 3);
                assert!(matches!(elements[1].node, Expr::Vector(_)));
                assert!(matches!(elements[2].node, Expr::Map(_)));
            }
            _ => panic!("expected nested vector"),
        }
    }

    #[test]
    fn test_parse_async() {
        let mut parser = Parser::new("(async (fetch url))").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Async(_)));
    }

    #[test]
    fn test_parse_await() {
        let mut parser = Parser::new("(await future)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Await(_)));
    }

    #[test]
    fn test_parse_conv_vector() {
        let mut parser = Parser::new("<[1 2 3]>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ConvVector(elements) => {
                assert_eq!(elements.len(), 3);
            }
            _ => panic!("expected conv vector"),
        }
    }

    #[test]
    fn test_parse_empty_conv_vector() {
        let mut parser = Parser::new("<[]>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ConvVector(elements) => {
                assert!(elements.is_empty());
            }
            _ => panic!("expected empty conv vector"),
        }
    }

    #[test]
    fn test_parse_conv_map() {
        let mut parser = Parser::new("<{:a 1 :b 2}>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ConvMap(pairs) => {
                assert_eq!(pairs.len(), 2);
            }
            _ => panic!("expected conv map"),
        }
    }

    #[test]
    fn test_parse_nested_conv_collections() {
        let mut parser = Parser::new("<[1 <[2 3]> <{:a 4}>]>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ConvVector(elements) => {
                assert_eq!(elements.len(), 3);
                assert!(matches!(elements[1].node, Expr::ConvVector(_)));
                assert!(matches!(elements[2].node, Expr::ConvMap(_)));
            }
            _ => panic!("expected nested conv vector"),
        }
    }

    #[test]
    fn test_parse_dosync() {
        let mut parser = Parser::new("(dosync (alter a + 1))").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Dosync(_)));
    }

    #[test]
    fn test_parse_ref_set() {
        let mut parser = Parser::new("(ref-set a 10)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::RefSetStm(_, _)));
    }

    #[test]
    fn test_parse_alter() {
        let mut parser = Parser::new("(alter account - amount)").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                assert!(matches!(ref_expr.node, Expr::Var(ref s) if s == "account"));
                assert!(matches!(fn_expr.node, Expr::Var(ref s) if s == "-"));
                assert_eq!(args.len(), 1);
            }
            _ => panic!("expected alter"),
        }
    }

    #[test]
    fn test_parse_commute() {
        let mut parser = Parser::new("(commute counter + 1)").unwrap();
        let expr = parser.parse_expr().unwrap();
        assert!(matches!(expr.node, Expr::Commute { .. }));
    }

    #[test]
    fn test_parse_simd_vector() {
        let mut parser = Parser::new("<<1 2 3 4>>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::SimdVector(elements) => {
                assert_eq!(elements.len(), 4);
            }
            _ => panic!("expected simd vector"),
        }
    }

    #[test]
    fn test_parse_empty_simd_vector() {
        let mut parser = Parser::new("<<>>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::SimdVector(elements) => {
                assert!(elements.is_empty());
            }
            _ => panic!("expected empty simd vector"),
        }
    }

    #[test]
    fn test_parse_simd_float_vector() {
        let mut parser = Parser::new("<<1.0 2.0 3.0 4.0>>").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::SimdVector(elements) => {
                assert_eq!(elements.len(), 4);
                assert!(matches!(elements[0].node, Expr::Float(_)));
            }
            _ => panic!("expected simd vector"),
        }
    }

    #[test]
    fn test_parse_defprotocol() {
        let source = r#"
            (defprotocol Seq
              "Sequence protocol"
              (first [self] "Returns the first element")
              (rest [self] "Returns remaining elements"))
        "#;
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            Item::Defprotocol(p) => {
                assert_eq!(p.name.node, "Seq");
                assert_eq!(p.doc, Some("Sequence protocol".to_string()));
                assert_eq!(p.methods.len(), 2);
                assert_eq!(p.methods[0].name.node, "first");
                assert_eq!(p.methods[0].params.len(), 1);
                assert_eq!(p.methods[0].params[0].node, "self");
            }
            _ => panic!("expected defprotocol"),
        }
    }

    #[test]
    fn test_parse_defprotocol_no_doc() {
        let source = r#"
            (defprotocol Counted
              (count [self]))
        "#;
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            Item::Defprotocol(p) => {
                assert_eq!(p.name.node, "Counted");
                assert!(p.doc.is_none());
                assert_eq!(p.methods.len(), 1);
            }
            _ => panic!("expected defprotocol"),
        }
    }

    #[test]
    fn test_parse_extend_protocol() {
        let source = r#"
            (extend-protocol Seq PersistentVector
              (first [self] (nth self 0))
              (rest [self] (subvec self 1)))
        "#;
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            Item::ExtendProtocol(e) => {
                assert_eq!(e.protocol.node, "Seq");
                assert_eq!(e.type_name.node, "PersistentVector");
                assert_eq!(e.implementations.len(), 2);
                assert_eq!(e.implementations[0].name.node, "first");
                assert_eq!(e.implementations[0].params.len(), 1);
            }
            _ => panic!("expected extend-protocol"),
        }
    }

    #[test]
    fn test_parse_iter() {
        let mut parser = Parser::new("(iter [1 2 3])").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Iter(coll) => {
                assert!(matches!(coll.node, Expr::Vector(_)));
            }
            _ => panic!("expected iter"),
        }
    }

    #[test]
    fn test_parse_collect() {
        let mut parser = Parser::new("(collect it)").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Collect(iter) => {
                assert!(matches!(iter.node, Expr::Var(ref s) if s == "it"));
            }
            _ => panic!("expected collect"),
        }
    }

    #[test]
    fn test_parse_byte_array() {
        let mut parser = Parser::new("#[1 2 3]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ByteArray(bytes) => {
                assert_eq!(bytes, vec![1, 2, 3]);
            }
            _ => panic!("expected byte array"),
        }
    }

    #[test]
    fn test_parse_empty_byte_array() {
        let mut parser = Parser::new("#[]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ByteArray(bytes) => {
                assert!(bytes.is_empty());
            }
            _ => panic!("expected empty byte array"),
        }
    }

    #[test]
    fn test_parse_byte_array_hex() {
        let mut parser = Parser::new("#[0 127 255]").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::ByteArray(bytes) => {
                assert_eq!(bytes, vec![0, 127, 255]);
            }
            _ => panic!("expected byte array"),
        }
    }

    #[test]
    fn test_parse_regex() {
        let mut parser = Parser::new(r#"#r"hello""#).unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Regex { pattern, flags } => {
                assert_eq!(pattern, "hello");
                assert_eq!(flags, "");
            }
            _ => panic!("expected regex"),
        }
    }

    #[test]
    fn test_parse_regex_with_flags() {
        let mut parser = Parser::new(r#"#r"pattern"im"#).unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Regex { pattern, flags } => {
                assert_eq!(pattern, "pattern");
                assert_eq!(flags, "im");
            }
            _ => panic!("expected regex"),
        }
    }

    #[test]
    fn test_parse_regex_with_escapes() {
        let mut parser = Parser::new(r#"#r"\d+""#).unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Regex { pattern, flags } => {
                assert_eq!(pattern, r"\d+");
                assert_eq!(flags, "");
            }
            _ => panic!("expected regex"),
        }
    }

    #[test]
    fn test_parse_boxed() {
        let mut parser = Parser::new("(boxed (* x y))").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Boxed(inner) => {
                assert!(matches!(inner.node, Expr::Call(_, _)));
            }
            _ => panic!("expected boxed"),
        }
    }

    #[test]
    fn test_parse_wrapping() {
        let mut parser = Parser::new("(wrapping (+ a b))").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Wrapping(inner) => {
                assert!(matches!(inner.node, Expr::Call(_, _)));
            }
            _ => panic!("expected wrapping"),
        }
    }

    #[test]
    fn test_parse_nested_overflow() {
        let mut parser = Parser::new("(boxed (wrapping (* x y)))").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Boxed(inner) => {
                assert!(matches!(inner.node, Expr::Wrapping(_)));
            }
            _ => panic!("expected boxed"),
        }
    }

    #[test]
    fn test_parse_extern() {
        let mut parser = Parser::new("(extern malloc ptr (i64))").unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            crate::ast::Item::Extern(ext) => {
                assert_eq!(ext.name.node, "malloc");
                assert_eq!(
                    ext.return_type.node,
                    crate::ast::Type::Named("ptr".to_string())
                );
                assert_eq!(ext.param_types.len(), 1);
                assert_eq!(
                    ext.param_types[0].node,
                    crate::ast::Type::Named("i64".to_string())
                );
                assert!(!ext.varargs);
            }
            _ => panic!("expected extern"),
        }
    }

    #[test]
    fn test_parse_extern_varargs() {
        let mut parser = Parser::new("(extern printf i32 (ptr ...))").unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            crate::ast::Item::Extern(ext) => {
                assert_eq!(ext.name.node, "printf");
                assert_eq!(
                    ext.return_type.node,
                    crate::ast::Type::Named("i32".to_string())
                );
                assert_eq!(ext.param_types.len(), 1);
                assert!(ext.varargs);
            }
            _ => panic!("expected extern"),
        }
    }

    #[test]
    fn test_parse_extern_void() {
        let mut parser = Parser::new("(extern free void (ptr))").unwrap();
        let program = parser.parse_program().unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0].node {
            crate::ast::Item::Extern(ext) => {
                assert_eq!(ext.name.node, "free");
                assert_eq!(
                    ext.return_type.node,
                    crate::ast::Type::Named("void".to_string())
                );
                assert_eq!(ext.param_types.len(), 1);
                assert!(!ext.varargs);
            }
            _ => panic!("expected extern"),
        }
    }

    #[test]
    fn test_parse_destructuring_let() {
        let mut parser = Parser::new("(let (((Point x y) p)) (+ x y))").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Let(bindings, _body) => {
                // Destructuring expands to: __destr_0 = p, x = (. __destr_0 x), y = (. __destr_0 y)
                assert_eq!(bindings.len(), 3);
                assert!(bindings[0].name.node.starts_with("__destr_"));
                assert_eq!(bindings[1].name.node, "x");
                assert_eq!(bindings[2].name.node, "y");
                // Check that x and y are field accesses
                assert!(matches!(bindings[1].value.node, Expr::Field(..)));
                assert!(matches!(bindings[2].value.node, Expr::Field(..)));
            }
            _ => panic!("expected let"),
        }
    }

    #[test]
    fn test_parse_mixed_destructuring_let() {
        let mut parser = Parser::new("(let ((a 1) ((Point x y) p) (b 2)) body)").unwrap();
        let expr = parser.parse_expr().unwrap();
        match expr.node {
            Expr::Let(bindings, _body) => {
                // a = 1, __destr_0 = p, x = field, y = field, b = 2
                assert_eq!(bindings.len(), 5);
                assert_eq!(bindings[0].name.node, "a");
                assert!(bindings[1].name.node.starts_with("__destr_"));
                assert_eq!(bindings[2].name.node, "x");
                assert_eq!(bindings[3].name.node, "y");
                assert_eq!(bindings[4].name.node, "b");
            }
            _ => panic!("expected let"),
        }
    }
}

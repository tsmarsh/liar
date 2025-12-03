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
            _ => {
                return Err(CompileError::parse(
                    self.current_span(),
                    "expected top-level form: defun, def, or defstruct",
                ))
            }
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
            TokenKind::Match => {
                self.advance();
                self.parse_match()
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
            _ => self.parse_call(),
        }
    }

    /// Parse let binding
    fn parse_let(&mut self, parallel: bool) -> Result<Expr> {
        self.expect(TokenKind::LParen)?;
        let mut bindings = Vec::new();

        while !self.check(TokenKind::RParen) {
            self.expect(TokenKind::LParen)?;
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

        self.expect(TokenKind::RParen)?;
        let body = self.parse_expr()?;

        if parallel {
            Ok(Expr::Plet(bindings, Box::new(body)))
        } else {
            Ok(Expr::Let(bindings, Box::new(body)))
        }
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

    /// Parse match expression
    fn parse_match(&mut self) -> Result<Expr> {
        let scrutinee = self.parse_expr()?;
        let mut arms = Vec::new();

        while !self.check(TokenKind::RParen) {
            self.expect(TokenKind::LParen)?;
            let pattern = self.parse_pattern()?;
            let body = self.parse_expr()?;
            self.expect(TokenKind::RParen)?;
            arms.push(MatchArm { pattern, body });
        }

        Ok(Expr::Match(Box::new(scrutinee), arms))
    }

    /// Parse a pattern
    fn parse_pattern(&mut self) -> Result<Spanned<Pattern>> {
        let start = self.current_span();

        let pattern = match &self.peek().kind {
            TokenKind::Symbol(s) if s == "_" => {
                self.advance();
                Pattern::Wildcard
            }
            TokenKind::Symbol(s) => {
                let s = s.clone();
                self.advance();
                Pattern::Var(s)
            }
            TokenKind::Int(n) => {
                let n = *n;
                self.advance();
                Pattern::Literal(Literal::Int(n))
            }
            TokenKind::True => {
                self.advance();
                Pattern::Literal(Literal::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Pattern::Literal(Literal::Bool(false))
            }
            TokenKind::Nil => {
                self.advance();
                Pattern::Literal(Literal::Nil)
            }
            _ => return Err(CompileError::parse(self.current_span(), "expected pattern")),
        };

        let span = start.merge(self.prev_span());
        Ok(Spanned::new(pattern, span))
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
}

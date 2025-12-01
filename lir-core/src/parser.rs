//! Parser for lIR S-expressions

use crate::ast::*;
use crate::error::ParseError;
use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
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

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek()? {
            Some(Token::LParen) => self.parse_sexpr(),
            Some(Token::LAngle) => self.parse_vector_literal(),
            _ => Err(ParseError::Expected {
                expected: "expression".to_string(),
                found: format!("{:?}", self.lexer.peek()?),
            }),
        }
    }

    fn parse_sexpr(&mut self) -> Result<Expr, ParseError> {
        self.expect(Token::LParen)?;

        let token = self.lexer.next()?.ok_or(ParseError::UnexpectedEof)?;

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

            _ => Err(ParseError::UnknownOperation(name.to_string())),
        }
    }

    fn parse_int_literal(&mut self, type_name: &str) -> Result<Expr, ParseError> {
        let ty = self.type_from_name(type_name)?;
        let value = match self.lexer.next()? {
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
        let value = match self.lexer.next()? {
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

    fn parse_vector_literal(&mut self) -> Result<Expr, ParseError> {
        let ty = self.parse_vector_type()?;
        let mut elements = Vec::with_capacity(ty.count as usize);

        for _ in 0..ty.count {
            // Each element is either a bare number or a sub-expression
            match self.lexer.peek()? {
                Some(Token::Integer(_)) | Some(Token::Float(_)) | Some(Token::Inf)
                | Some(Token::NegInf) | Some(Token::Nan) => {
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

        self.expect(Token::RParen)?;
        Ok(Expr::VectorLit { ty, elements })
    }

    fn parse_scalar_value(&mut self, ty: &ScalarType) -> Result<Expr, ParseError> {
        if ty.is_integer() {
            match self.lexer.next()? {
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
            let value = match self.lexer.next()? {
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
        let count = match self.lexer.next()? {
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
        match self.lexer.next()? {
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
        let element = match self.lexer.next()? {
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
        match self.lexer.next()? {
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
        match self.lexer.next()? {
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
        let ty = match self.lexer.next()? {
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
            _ => Err(ParseError::UnknownType(name.to_string())),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        match self.lexer.next()? {
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
        assert!(matches!(expr, Expr::IntLit { ty: ScalarType::I32, value: 42 }));
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
}

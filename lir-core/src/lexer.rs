//! Lexer for lIR S-expressions

use crate::error::ParseError;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LAngle,
    RAngle,
    Integer(i128),
    Float(f64),
    Ident(String),
    Inf,
    NegInf,
    Nan,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LAngle => write!(f, "<"),
            Token::RAngle => write!(f, ">"),
            Token::Integer(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Inf => write!(f, "inf"),
            Token::NegInf => write!(f, "-inf"),
            Token::Nan => write!(f, "nan"),
        }
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    peeked: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Result<Option<&Token>, ParseError> {
        if self.peeked.is_none() {
            self.peeked = self.next_token()?;
        }
        Ok(self.peeked.as_ref())
    }

    pub fn next_token_peeked(&mut self) -> Result<Option<Token>, ParseError> {
        if self.peeked.is_some() {
            Ok(self.peeked.take())
        } else {
            self.next_token()
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else if c == ';' {
                // Skip comment to end of line
                while let Some(&c) = self.chars.peek() {
                    self.chars.next();
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Result<Option<Token>, ParseError> {
        self.skip_whitespace();

        let c = match self.chars.peek() {
            Some(&c) => c,
            None => return Ok(None),
        };

        match c {
            '(' => {
                self.chars.next();
                Ok(Some(Token::LParen))
            }
            ')' => {
                self.chars.next();
                Ok(Some(Token::RParen))
            }
            '<' => {
                self.chars.next();
                Ok(Some(Token::LAngle))
            }
            '>' => {
                self.chars.next();
                Ok(Some(Token::RAngle))
            }
            '-' => {
                self.chars.next();
                // Check for -inf or negative number
                self.skip_whitespace();
                if let Some(&next) = self.chars.peek() {
                    if next == 'i' {
                        // Could be -inf
                        let ident = self.read_ident();
                        if ident == "inf" {
                            return Ok(Some(Token::NegInf));
                        }
                        // Otherwise it's an error or some other ident
                        Err(ParseError::UnexpectedToken(format!("-{}", ident)))?
                    } else if next.is_ascii_digit() || next == '.' {
                        let num = self.read_number()?;
                        match num {
                            Token::Integer(n) => Ok(Some(Token::Integer(-n))),
                            Token::Float(n) => Ok(Some(Token::Float(-n))),
                            _ => unreachable!(),
                        }
                    } else {
                        Err(ParseError::UnexpectedToken("-".to_string()))
                    }
                } else {
                    Err(ParseError::UnexpectedToken("-".to_string()))
                }
            }
            '0'..='9' => self.read_number().map(Some),
            'a'..='z' | 'A'..='Z' | '_' => {
                let ident = self.read_ident();
                match ident.as_str() {
                    "inf" => Ok(Some(Token::Inf)),
                    "nan" => Ok(Some(Token::Nan)),
                    _ => Ok(Some(Token::Ident(ident))),
                }
            }
            _ => {
                self.chars.next();
                Err(ParseError::UnexpectedToken(c.to_string()))
            }
        }
    }

    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        while let Some(&c) = self.chars.peek() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        ident
    }

    fn read_number(&mut self) -> Result<Token, ParseError> {
        let mut num_str = String::new();
        let mut is_float = false;
        let mut is_binary = false;
        let mut has_exponent = false;

        // Check for 0b prefix (binary)
        if let Some(&'0') = self.chars.peek() {
            num_str.push('0');
            self.chars.next();
            if let Some(&'b') = self.chars.peek() {
                is_binary = true;
                self.chars.next();
                num_str.clear(); // Don't include 0b in parsed string
            }
        }

        if is_binary {
            // Read binary digits
            while let Some(&c) = self.chars.peek() {
                if c == '0' || c == '1' {
                    num_str.push(c);
                    self.chars.next();
                } else {
                    break;
                }
            }
            let value = i128::from_str_radix(&num_str, 2)
                .map_err(|_| ParseError::InvalidNumber(format!("0b{}", num_str)))?;
            return Ok(Token::Integer(value));
        }

        // Read integer/float
        while let Some(&c) = self.chars.peek() {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.chars.next();
            } else if c == '.' && !is_float && !has_exponent {
                is_float = true;
                num_str.push(c);
                self.chars.next();
            } else if (c == 'e' || c == 'E') && !has_exponent {
                is_float = true;
                has_exponent = true;
                num_str.push(c);
                self.chars.next();
                // Handle optional sign after exponent
                if let Some(&sign) = self.chars.peek() {
                    if sign == '+' || sign == '-' {
                        num_str.push(sign);
                        self.chars.next();
                    }
                }
            } else {
                break;
            }
        }

        if is_float {
            let value: f64 = num_str
                .parse()
                .map_err(|_| ParseError::InvalidNumber(num_str.clone()))?;
            Ok(Token::Float(value))
        } else {
            let value: i128 = num_str
                .parse()
                .map_err(|_| ParseError::InvalidNumber(num_str.clone()))?;
            Ok(Token::Integer(value))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lex = Lexer::new("(add (i32 42) (i32 -5))");
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::LParen));
        assert_eq!(
            lex.next_token_peeked().unwrap(),
            Some(Token::Ident("add".to_string()))
        );
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::LParen));
        assert_eq!(
            lex.next_token_peeked().unwrap(),
            Some(Token::Ident("i32".to_string()))
        );
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Integer(42)));
    }

    #[test]
    fn test_float_tokens() {
        let mut lex = Lexer::new("3.25 1.0e10 -2.5e-3");
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Float(3.25)));
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Float(1.0e10)));
        assert_eq!(
            lex.next_token_peeked().unwrap(),
            Some(Token::Float(-2.5e-3))
        );
    }

    #[test]
    fn test_special_floats() {
        let mut lex = Lexer::new("inf -inf nan");
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Inf));
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::NegInf));
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Nan));
    }

    #[test]
    fn test_binary_literal() {
        let mut lex = Lexer::new("0b1100");
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Integer(12)));
    }

    #[test]
    fn test_vector_angle_brackets() {
        let mut lex = Lexer::new("<4 x i32>");
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::LAngle));
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::Integer(4)));
        assert_eq!(
            lex.next_token_peeked().unwrap(),
            Some(Token::Ident("x".to_string()))
        );
        assert_eq!(
            lex.next_token_peeked().unwrap(),
            Some(Token::Ident("i32".to_string()))
        );
        assert_eq!(lex.next_token_peeked().unwrap(), Some(Token::RAngle));
    }
}

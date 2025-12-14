//! # LDN - Liar Data Notation
//!
//! A data serialization format using s-expression syntax, similar to Clojure's EDN.
//!
//! ## Example
//!
//! ```rust
//! use ldn::Value;
//!
//! let source = r#"
//! {:project {:name "my-app" :version "0.1.0"}
//!  :deps {foo {:path "../foo"}}}
//! "#;
//!
//! let value = ldn::parse(source).unwrap();
//!
//! // Access nested values
//! let name = value.get_in(&["project", "name"]);
//! assert_eq!(name, Some(&Value::String("my-app".to_string())));
//! ```
//!
//! ## Data Types
//!
//! LDN supports the following data types:
//!
//! | Type | Syntax | Example |
//! |------|--------|---------|
//! | nil | `nil` | `nil` |
//! | bool | `true`, `false` | `true` |
//! | int | digits | `42`, `-17` |
//! | float | digits with `.` or `e` | `3.14`, `1e10` |
//! | string | `"..."` | `"hello"` |
//! | keyword | `:name` | `:foo`, `:my-key` |
//! | symbol | bare name | `foo`, `foo.bar` |
//! | vector | `[...]` | `[1 2 3]` |
//! | map | `{...}` | `{:a 1 :b 2}` |
//! | set | `#{...}` | `#{1 2 3}` |

mod error;
mod lexer;
mod parser;
mod value;

pub use error::{Error, LexError, ParseError, Result};
pub use value::Value;

/// Parse LDN source into a Value
///
/// # Example
///
/// ```rust
/// let value = ldn::parse("{:name \"test\"}").unwrap();
/// assert!(value.is_map());
/// ```
pub fn parse(source: &str) -> Result<Value> {
    let mut lex = lexer::Lexer::new(source);
    let tokens = lex.tokenize()?;
    let mut parser = parser::Parser::new(tokens);
    parser.parse().map_err(Error::Parse)
}

/// Parse LDN from a file
///
/// # Example
///
/// ```rust,no_run
/// let value = ldn::parse_file(std::path::Path::new("project.edn")).unwrap();
/// ```
pub fn parse_file(path: &std::path::Path) -> Result<Value> {
    let source = std::fs::read_to_string(path)?;
    parse(&source)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        assert_eq!(parse("nil").unwrap(), Value::Nil);
        assert_eq!(parse("true").unwrap(), Value::Bool(true));
        assert_eq!(parse("42").unwrap(), Value::Int(42));
        assert_eq!(parse("3.5").unwrap(), Value::Float(3.5));
        assert_eq!(
            parse("\"hello\"").unwrap(),
            Value::String("hello".to_string())
        );
        assert_eq!(parse(":foo").unwrap(), Value::Keyword("foo".to_string()));
        assert_eq!(parse("bar").unwrap(), Value::Symbol("bar".to_string()));
    }

    #[test]
    fn test_parse_collections() {
        assert_eq!(
            parse("[1 2 3]").unwrap(),
            Value::Vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );

        let map = parse("{:a 1}").unwrap();
        assert_eq!(map.get_keyword("a"), Some(&Value::Int(1)));

        assert_eq!(
            parse("#{1 2}").unwrap(),
            Value::Set(vec![Value::Int(1), Value::Int(2)])
        );
    }

    #[test]
    fn test_parse_project_edn() {
        let source = r#"
{:project {:name "my-app"
           :version "0.1.0"
           :entry "src/main.liar"}
 :paths ["src" "lib"]
 :deps {liar.collections {:path "../liar-collections"}
        http.client {:git "https://github.com/user/http-client"
                     :sha "abc123def456"}}}
"#;
        let value = parse(source).unwrap();

        // Check project info
        assert_eq!(
            value.get_in(&["project", "name"]),
            Some(&Value::String("my-app".to_string()))
        );
        assert_eq!(
            value.get_in(&["project", "version"]),
            Some(&Value::String("0.1.0".to_string()))
        );

        // Check paths
        let paths = value.get_keyword("paths").unwrap();
        assert!(paths.is_vector());
        assert_eq!(paths.as_vector().unwrap().len(), 2);

        // Check deps
        let deps = value.get_keyword("deps").unwrap();
        assert!(deps.is_map());

        // Check a specific dep
        let http_client = deps.get_symbol("http.client").unwrap();
        assert_eq!(
            http_client.get_keyword("git"),
            Some(&Value::String(
                "https://github.com/user/http-client".to_string()
            ))
        );
    }

    #[test]
    fn test_comments() {
        let source = r#"
; This is a comment
{:a 1  ; inline comment
 :b 2}
"#;
        let value = parse(source).unwrap();
        assert_eq!(value.get_keyword("a"), Some(&Value::Int(1)));
        assert_eq!(value.get_keyword("b"), Some(&Value::Int(2)));
    }

    #[test]
    fn test_commas_optional() {
        // Commas are whitespace in LDN
        let source = "{:a 1, :b 2, :c 3}";
        let value = parse(source).unwrap();
        assert_eq!(value.get_keyword("a"), Some(&Value::Int(1)));
        assert_eq!(value.get_keyword("b"), Some(&Value::Int(2)));
        assert_eq!(value.get_keyword("c"), Some(&Value::Int(3)));
    }

    #[test]
    fn test_nested_structures() {
        let source = "{:vec [1 {:nested true} 3] :set #{:a :b}}";
        let value = parse(source).unwrap();

        let vec = value.get_keyword("vec").unwrap();
        assert!(vec.is_vector());

        let set = value.get_keyword("set").unwrap();
        assert!(set.is_set());
    }

    #[test]
    fn test_error_handling() {
        // Unterminated string
        assert!(parse("\"hello").is_err());

        // Unterminated vector
        assert!(parse("[1 2").is_err());

        // Odd map elements
        assert!(parse("{:a}").is_err());
    }
}

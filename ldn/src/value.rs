//! LDN Value types
//!
//! The core data types for Liar Data Notation.

/// LDN value - all possible data types
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Nil/null value
    Nil,
    /// Boolean
    Bool(bool),
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit floating point
    Float(f64),
    /// UTF-8 string
    String(String),
    /// Keyword (e.g., :foo)
    Keyword(String),
    /// Symbol (e.g., foo, foo.bar)
    Symbol(String),
    /// Vector [1 2 3]
    Vector(Vec<Value>),
    /// Map {:a 1 :b 2}
    Map(Vec<(Value, Value)>),
    /// Set #{1 2 3}
    Set(Vec<Value>),
}

impl Value {
    // Type checking methods

    /// Returns true if this is a nil value
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Returns true if this is a boolean value
    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    /// Returns true if this is an integer value
    pub fn is_int(&self) -> bool {
        matches!(self, Value::Int(_))
    }

    /// Returns true if this is a float value
    pub fn is_float(&self) -> bool {
        matches!(self, Value::Float(_))
    }

    /// Returns true if this is a string value
    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    /// Returns true if this is a keyword value
    pub fn is_keyword(&self) -> bool {
        matches!(self, Value::Keyword(_))
    }

    /// Returns true if this is a symbol value
    pub fn is_symbol(&self) -> bool {
        matches!(self, Value::Symbol(_))
    }

    /// Returns true if this is a vector value
    pub fn is_vector(&self) -> bool {
        matches!(self, Value::Vector(_))
    }

    /// Returns true if this is a map value
    pub fn is_map(&self) -> bool {
        matches!(self, Value::Map(_))
    }

    /// Returns true if this is a set value
    pub fn is_set(&self) -> bool {
        matches!(self, Value::Set(_))
    }

    // Accessor methods

    /// Returns the boolean value if this is a Bool, None otherwise
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns the integer value if this is an Int, None otherwise
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    /// Returns the float value if this is a Float, None otherwise
    pub fn as_float(&self) -> Option<f64> {
        match self {
            Value::Float(f) => Some(*f),
            _ => None,
        }
    }

    /// Returns the string value if this is a String, None otherwise
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the keyword name if this is a Keyword, None otherwise
    pub fn as_keyword(&self) -> Option<&str> {
        match self {
            Value::Keyword(k) => Some(k),
            _ => None,
        }
    }

    /// Returns the symbol name if this is a Symbol, None otherwise
    pub fn as_symbol(&self) -> Option<&str> {
        match self {
            Value::Symbol(s) => Some(s),
            _ => None,
        }
    }

    /// Returns the vector elements if this is a Vector, None otherwise
    pub fn as_vector(&self) -> Option<&[Value]> {
        match self {
            Value::Vector(v) => Some(v),
            _ => None,
        }
    }

    /// Returns the map pairs if this is a Map, None otherwise
    pub fn as_map(&self) -> Option<&[(Value, Value)]> {
        match self {
            Value::Map(m) => Some(m),
            _ => None,
        }
    }

    /// Returns the set elements if this is a Set, None otherwise
    pub fn as_set(&self) -> Option<&[Value]> {
        match self {
            Value::Set(s) => Some(s),
            _ => None,
        }
    }

    // Map access methods

    /// Get a value from a map by key
    pub fn get(&self, key: &Value) -> Option<&Value> {
        match self {
            Value::Map(pairs) => pairs.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    /// Get a value from a map by keyword name
    ///
    /// This is a convenience method for the common case of keyword-keyed maps.
    /// ```
    /// use ldn::Value;
    /// let map = Value::Map(vec![
    ///     (Value::Keyword("name".to_string()), Value::String("foo".to_string())),
    /// ]);
    /// assert_eq!(map.get_keyword("name"), Some(&Value::String("foo".to_string())));
    /// ```
    pub fn get_keyword(&self, key: &str) -> Option<&Value> {
        match self {
            Value::Map(pairs) => pairs
                .iter()
                .find(|(k, _)| matches!(k, Value::Keyword(s) if s == key))
                .map(|(_, v)| v),
            _ => None,
        }
    }

    /// Get a nested value by following a path of keyword keys
    ///
    /// ```
    /// use ldn::Value;
    /// // Equivalent to: {:project {:name "foo"}}
    /// let v = Value::Map(vec![
    ///     (Value::Keyword("project".to_string()),
    ///      Value::Map(vec![
    ///          (Value::Keyword("name".to_string()), Value::String("foo".to_string())),
    ///      ])),
    /// ]);
    /// assert_eq!(
    ///     v.get_in(&["project", "name"]),
    ///     Some(&Value::String("foo".to_string()))
    /// );
    /// ```
    pub fn get_in(&self, keys: &[&str]) -> Option<&Value> {
        let mut current = self;
        for key in keys {
            current = current.get_keyword(key)?;
        }
        Some(current)
    }

    /// Get a value from a map by symbol name
    pub fn get_symbol(&self, key: &str) -> Option<&Value> {
        match self {
            Value::Map(pairs) => pairs
                .iter()
                .find(|(k, _)| matches!(k, Value::Symbol(s) if s == key))
                .map(|(_, v)| v),
            _ => None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s.escape_default()),
            Value::Keyword(k) => write!(f, ":{}", k),
            Value::Symbol(s) => write!(f, "{}", s),
            Value::Vector(v) => {
                write!(f, "[")?;
                for (i, elem) in v.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "{{")?;
                for (i, (k, v)) in m.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{} {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Set(s) => {
                write!(f, "#{{")?;
                for (i, elem) in s.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_nil() {
        assert!(Value::Nil.is_nil());
        assert!(!Value::Int(42).is_nil());
    }

    #[test]
    fn test_as_int() {
        assert_eq!(Value::Int(42).as_int(), Some(42));
        assert_eq!(Value::String("foo".to_string()).as_int(), None);
    }

    #[test]
    fn test_get_keyword() {
        let map = Value::Map(vec![
            (
                Value::Keyword("name".to_string()),
                Value::String("test".to_string()),
            ),
            (Value::Keyword("version".to_string()), Value::Int(1)),
        ]);

        assert_eq!(
            map.get_keyword("name"),
            Some(&Value::String("test".to_string()))
        );
        assert_eq!(map.get_keyword("version"), Some(&Value::Int(1)));
        assert_eq!(map.get_keyword("missing"), None);
    }

    #[test]
    fn test_get_in() {
        let nested = Value::Map(vec![(
            Value::Keyword("project".to_string()),
            Value::Map(vec![(
                Value::Keyword("name".to_string()),
                Value::String("my-app".to_string()),
            )]),
        )]);

        assert_eq!(
            nested.get_in(&["project", "name"]),
            Some(&Value::String("my-app".to_string()))
        );
        assert_eq!(nested.get_in(&["project", "missing"]), None);
        assert_eq!(nested.get_in(&["missing"]), None);
    }

    #[test]
    fn test_display() {
        assert_eq!(Value::Nil.to_string(), "nil");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::Keyword("foo".to_string()).to_string(), ":foo");
        assert_eq!(
            Value::Vector(vec![Value::Int(1), Value::Int(2)]).to_string(),
            "[1 2]"
        );
    }
}

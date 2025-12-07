//! Value type for JIT evaluation results
//!
//! Represents typed results from lIR expression evaluation.

/// Typed result from JIT evaluation
#[derive(Debug, Clone)]
pub enum Value {
    I1(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Float(f32),
    Double(f64),
    Ptr(u64), // Pointer as raw address
    // Vector types - stored as Vec for flexibility
    VecI1(Vec<bool>),
    VecI8(Vec<i8>),
    VecI16(Vec<i16>),
    VecI32(Vec<i32>),
    VecI64(Vec<i64>),
    VecFloat(Vec<f32>),
    VecDouble(Vec<f64>),
    // Struct type - stored as Vec of boxed Values
    Struct(Vec<Value>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::I1(a), Value::I1(b)) => a == b,
            (Value::I8(a), Value::I8(b)) => a == b,
            (Value::I16(a), Value::I16(b)) => a == b,
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b || (a.is_nan() && b.is_nan()),
            (Value::Double(a), Value::Double(b)) => a == b || (a.is_nan() && b.is_nan()),
            (Value::Ptr(a), Value::Ptr(b)) => a == b,
            (Value::VecI1(a), Value::VecI1(b)) => a == b,
            (Value::VecI8(a), Value::VecI8(b)) => a == b,
            (Value::VecI16(a), Value::VecI16(b)) => a == b,
            (Value::VecI32(a), Value::VecI32(b)) => a == b,
            (Value::VecI64(a), Value::VecI64(b)) => a == b,
            (Value::VecFloat(a), Value::VecFloat(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| x == y || (x.is_nan() && y.is_nan()))
            }
            (Value::VecDouble(a), Value::VecDouble(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b.iter())
                        .all(|(x, y)| x == y || (x.is_nan() && y.is_nan()))
            }
            (Value::Struct(a), Value::Struct(b)) => a == b,
            _ => false,
        }
    }
}

/// Helper to format float with decimal point
fn format_float(v: f32) -> String {
    if v.is_nan() {
        "nan".to_string()
    } else if v.is_infinite() {
        if v > 0.0 {
            "inf".to_string()
        } else {
            "-inf".to_string()
        }
    } else {
        let s = format!("{}", v);
        if s.contains('.') || s.contains('e') {
            s
        } else {
            format!("{}.0", s)
        }
    }
}

/// Helper to format double with decimal point
fn format_double(v: f64) -> String {
    if v.is_nan() {
        "nan".to_string()
    } else if v.is_infinite() {
        if v > 0.0 {
            "inf".to_string()
        } else {
            "-inf".to_string()
        }
    } else {
        let s = format!("{}", v);
        if s.contains('.') || s.contains('e') {
            s
        } else {
            format!("{}.0", s)
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::I1(v) => write!(f, "(i1 {})", if *v { 1 } else { 0 }),
            Value::I8(v) => write!(f, "(i8 {})", v),
            Value::I16(v) => write!(f, "(i16 {})", v),
            Value::I32(v) => write!(f, "(i32 {})", v),
            Value::I64(v) => write!(f, "(i64 {})", v),
            Value::Float(v) => write!(f, "(float {})", format_float(*v)),
            Value::Double(v) => write!(f, "(double {})", format_double(*v)),
            Value::Ptr(v) => {
                if *v == 0 {
                    write!(f, "(ptr null)")
                } else {
                    write!(f, "(ptr 0x{:x})", v)
                }
            }
            // Vector display: (<N x type> v1 v2 v3 ...)
            Value::VecI1(vals) => {
                write!(f, "(<{} x i1>", vals.len())?;
                for v in vals {
                    write!(f, " {}", if *v { 1 } else { 0 })?;
                }
                write!(f, ")")
            }
            Value::VecI8(vals) => {
                write!(f, "(<{} x i8>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI16(vals) => {
                write!(f, "(<{} x i16>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI32(vals) => {
                write!(f, "(<{} x i32>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecI64(vals) => {
                write!(f, "(<{} x i64>", vals.len())?;
                for v in vals {
                    write!(f, " {}", v)?;
                }
                write!(f, ")")
            }
            Value::VecFloat(vals) => {
                write!(f, "(<{} x float>", vals.len())?;
                for v in vals {
                    write!(f, " {}", format_float(*v))?;
                }
                write!(f, ")")
            }
            Value::VecDouble(vals) => {
                write!(f, "(<{} x double>", vals.len())?;
                for v in vals {
                    write!(f, " {}", format_double(*v))?;
                }
                write!(f, ")")
            }
            // Struct display: { val1 val2 ... }
            Value::Struct(fields) => {
                write!(f, "{{")?;
                for (i, val) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, " {}", val)?;
                }
                write!(f, " }}")
            }
        }
    }
}

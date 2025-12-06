//! Codegen tests

use super::*;
use crate::parser::Parser;

fn compile(source: &str) -> String {
    let mut parser = Parser::new(source).unwrap();
    let program = parser.parse_program().unwrap();
    generate_string(&program).unwrap()
}

#[test]
fn test_generate_simple() {
    let lir = compile("(defun add (a b) (+ a b))");
    assert!(lir.contains("define"));
    assert!(lir.contains("add"));
}

#[test]
fn test_comparison_ops() {
    let lir = compile("(defun cmp (a b) (< a b))");
    assert!(lir.contains("icmp slt"));

    let lir = compile("(defun cmp (a b) (<= a b))");
    assert!(lir.contains("icmp sle"));

    let lir = compile("(defun cmp (a b) (!= a b))");
    assert!(lir.contains("icmp ne"));
}

#[test]
fn test_let_binding() {
    let lir = compile("(defun foo () (let ((x 1) (y 2)) (+ x y)))");
    assert!(lir.contains("let"));
    assert!(lir.contains("(i64 1)"));
    assert!(lir.contains("(i64 2)"));
}

#[test]
fn test_if_expr() {
    let lir = compile("(defun max (a b) (if (> a b) a b))");
    assert!(lir.contains("select"));
    assert!(lir.contains("icmp sgt"));
}

#[test]
fn test_do_block() {
    let lir = compile("(defun foo () (do 1 2 3))");
    assert!(lir.contains("(i64 3)"));
}

#[test]
fn test_function_call() {
    let lir = compile(
        r#"
        (defun double (x) (* x 2))
        (defun quad (x) (double (double x)))
        "#,
    );
    assert!(lir.contains("call @double"));
}

#[test]
fn test_borrow_ref() {
    let lir = compile("(defun get-ref (x) (ref x))");
    assert!(lir.contains("borrow ref"));
}

#[test]
fn test_borrow_refmut() {
    let lir = compile("(defun get-mut (x) (ref-mut x))");
    assert!(lir.contains("borrow refmut"));
}

#[test]
fn test_ownership_ops() {
    let lir = compile("(defun test-drop (x) (drop x))");
    assert!(lir.contains("(drop x)"));

    let lir = compile("(defun test-move (x) (move x))");
    assert!(lir.contains("(move x)"));
}

#[test]
fn test_rc_new() {
    let lir = compile("(defun make-rc () (rc-new 42))");
    assert!(lir.contains("rc-alloc"));
    assert!(lir.contains("rc-ptr"));
}

#[test]
fn test_share() {
    let lir = compile("(defun make-shared () (share 42))");
    assert!(lir.contains("rc-alloc"));
    assert!(lir.contains("rc-ptr"));
}

#[test]
fn test_clone() {
    let lir = compile("(defun clone-it (x) (clone x))");
    assert!(lir.contains("rc-clone"));
}

#[test]
fn test_atom_create() {
    reset_var_counter();
    let lir = compile("(defun make-counter () (atom 0))");
    assert!(lir.contains("rc-alloc"));
    assert!(lir.contains("atomic-store"));
}

#[test]
fn test_atom_deref() {
    let lir = compile("(defun read-atom (a) @a)");
    assert!(lir.contains("atomic-load"));
    assert!(lir.contains("seq_cst"));
}

#[test]
fn test_atom_reset() {
    let lir = compile("(defun set-atom (a v) (reset! a v))");
    assert!(lir.contains("atomic-store"));
}

#[test]
fn test_atom_swap() {
    reset_var_counter();
    let lir = compile("(defun inc-atom (a) (swap! a inc))");
    assert!(lir.contains("atomic-load"));
    assert!(lir.contains("call @inc"));
    assert!(lir.contains("atomic-store"));
}

#[test]
fn test_atom_compare_and_set() {
    let lir = compile("(defun cas-atom (a old new) (compare-and-set! a old new))");
    assert!(lir.contains("cmpxchg"));
    assert!(lir.contains("extractvalue"));
}

#[test]
fn test_vector_literal() {
    let lir = compile("(defun make-vec () [1 2 3])");
    assert!(lir.contains("(i64 1)"));
    assert!(lir.contains("(i64 2)"));
    assert!(lir.contains("(i64 3)"));
}

#[test]
fn test_map_literal() {
    let lir = compile("(defun make-map () {:a 1 :b 2})");
    assert!(lir.contains(":a"));
    assert!(lir.contains("(i64 1)"));
}

#[test]
fn test_keyword_literal() {
    let lir = compile("(defun get-key () :foo)");
    assert!(lir.contains(":foo"));
}

#[test]
fn test_async() {
    let lir = compile("(defun fetch-data () (async (compute)))");
    // Async is currently a stub - just check it compiles
    assert!(lir.contains("define"));
}

#[test]
fn test_await() {
    let lir = compile("(defun wait-data (f) (await f))");
    assert!(lir.contains("define"));
}

#[test]
fn test_conv_vector_literal() {
    let lir = compile("(defun make-conv-vec () <[1 2 3]>)");
    assert!(lir.contains("(i64 1)"));
    assert!(lir.contains("(i64 2)"));
    assert!(lir.contains("(i64 3)"));
}

#[test]
fn test_conv_map_literal() {
    let lir = compile("(defun make-conv-map () <{:a 1 :b 2}>)");
    assert!(lir.contains(":a"));
    assert!(lir.contains("(i64 1)"));
}

#[test]
fn test_dosync() {
    let lir = compile("(defun transfer () (dosync (alter a - 50)))");
    assert!(lir.contains("define"));
}

#[test]
fn test_ref_set() {
    let lir = compile("(defun set-val (r) (ref-set r 10))");
    assert!(lir.contains("store"));
}

#[test]
fn test_alter() {
    let lir = compile("(defun update (r) (alter r + 1))");
    // Alter should generate a load, call, and store
    assert!(lir.contains("define"));
}

#[test]
fn test_commute() {
    let lir = compile("(defun inc (r) (commute r + 1))");
    assert!(lir.contains("define"));
}

#[test]
fn test_simd_vector_int() {
    let lir = compile("(defun make-vec () <<1 2 3 4>>)");
    assert!(lir.contains("vector"));
    assert!(lir.contains("(i64 1)"));
    assert!(lir.contains("(i64 4)"));
}

#[test]
fn test_simd_vector_float() {
    let lir = compile("(defun make-vec () <<1.0 2.0 3.0 4.0>>)");
    assert!(lir.contains("vector"));
    assert!(lir.contains("double"));
}

#[test]
fn test_defprotocol() {
    // Protocols are currently skipped in output
    let lir = compile(
        r#"
        (defprotocol Seq
          (first [self])
          (rest [self]))
        "#,
    );
    // Should compile without error
    assert!(lir.is_empty() || !lir.contains("define"));
}

#[test]
fn test_extend_protocol() {
    // Protocol extensions are currently skipped
    let lir = compile(
        r#"
        (defprotocol Counted
          (count [self]))
        (extend-protocol Counted PersistentVector
          (count [self] (len self)))
        "#,
    );
    assert!(lir.is_empty() || !lir.contains("extend"));
}

#[test]
fn test_iter() {
    let lir = compile("(defun make-iter (v) (iter v))");
    assert!(lir.contains("define"));
}

#[test]
fn test_collect() {
    let lir = compile("(defun materialize (it) (collect it))");
    assert!(lir.contains("define"));
}

#[test]
fn test_iter_pipeline() {
    let lir = compile("(defun squares () (collect (iter [1 2 3])))");
    assert!(lir.contains("define"));
}

#[test]
fn test_byte_array() {
    let lir = compile("(defun get-bytes () #[1 2 3])");
    assert!(lir.contains("(i8 1)"));
    assert!(lir.contains("(i8 2)"));
    assert!(lir.contains("(i8 3)"));
}

#[test]
fn test_byte_array_empty() {
    let lir = compile("(defun get-empty () #[])");
    assert!(lir.contains("define"));
}

#[test]
fn test_regex() {
    let lir = compile(r#"(defun get-pattern () #r"hello")"#);
    assert!(lir.contains("regex:hello"));
}

#[test]
fn test_regex_with_flags() {
    let lir = compile(r#"(defun get-pattern () #r"pattern"im)"#);
    assert!(lir.contains("regex:pattern:im"));
}

#[test]
fn test_boxed_arithmetic() {
    let lir = compile("(defun big-mult (x y) (boxed (* x y)))");
    assert!(lir.contains("mul"));
}

#[test]
fn test_wrapping_arithmetic() {
    let lir = compile("(defun wrap-add (a b) (wrapping (+ a b)))");
    assert!(lir.contains("add"));
}

#[test]
fn test_extern() {
    let lir = compile("(extern malloc ptr (i64))");
    assert!(lir.contains("declare"));
    assert!(lir.contains("malloc"));
    assert!(lir.contains("ptr"));
    assert!(lir.contains("i64"));
}

#[test]
fn test_extern_varargs() {
    let lir = compile("(extern printf i32 (ptr ...))");
    assert!(lir.contains("declare"));
    assert!(lir.contains("printf"));
    assert!(lir.contains("i32"));
    assert!(lir.contains("..."));
}

#[test]
fn test_extern_void() {
    let lir = compile("(extern free void (ptr))");
    assert!(lir.contains("declare"));
    assert!(lir.contains("free"));
    assert!(lir.contains("void"));
}

#[test]
fn test_lambda_pure() {
    // Pure lambda (no captures) - use full pipeline
    let lir = crate::compile("(defun mk-adder () (fn (x y) (+ x y)))").unwrap();
    assert!(lir.contains("define"));
    // Should generate a lambda function
    assert!(lir.contains("__lambda_"));
    // Should reference the lambda with @
    assert!(lir.contains("@__lambda_"));
}

#[test]
fn test_lambda_with_capture() {
    // Lambda with capture - use full pipeline
    let lir = crate::compile("(defun constantly (v) (fn () v))").unwrap();
    assert!(lir.contains("define"));
    // Should generate a lambda function
    assert!(lir.contains("__lambda_"));
    // Should generate env struct
    assert!(lir.contains("defstruct"));
    assert!(lir.contains("_env"));
}

#[test]
fn test_bit_and() {
    let lir = compile("(defun test (a b) (bit-and a b))");
    assert!(lir.contains("(and"));
}

#[test]
fn test_bit_or() {
    let lir = compile("(defun test (a b) (bit-or a b))");
    assert!(lir.contains("(or"));
}

#[test]
fn test_bit_xor() {
    let lir = compile("(defun test (a b) (bit-xor a b))");
    assert!(lir.contains("(xor"));
}

#[test]
fn test_bit_not() {
    let lir = compile("(defun test (a) (bit-not a))");
    // bit-not is XOR with -1
    assert!(lir.contains("(xor"));
    assert!(lir.contains("(i64 -1)"));
}

#[test]
fn test_bit_shift_left() {
    let lir = compile("(defun test (a b) (bit-shift-left a b))");
    assert!(lir.contains("(shl"));
}

#[test]
fn test_bit_shift_right() {
    let lir = compile("(defun test (a b) (bit-shift-right a b))");
    assert!(lir.contains("(lshr"));
}

#[test]
fn test_arithmetic_shift_right() {
    let lir = compile("(defun test (a b) (arithmetic-shift-right a b))");
    assert!(lir.contains("(ashr"));
}

#[test]
fn test_popcount() {
    let lir = compile("(defun test (a) (popcount a))");
    assert!(lir.contains("(ctpop"));
}

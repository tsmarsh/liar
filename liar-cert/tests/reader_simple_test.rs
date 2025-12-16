//! Simple reader test - incremental build-up

use liar_cert::{compile_and_call, format_value};

#[test]
fn test_basic_lexer() {
    let source = r#"
(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64))

(defun make-lexer (src len: i64) -> ptr
  (share (Lexer src 0 len)))

(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (. lex lex-len))
      -1
      (zext i64 (load-byte (ptr+ (. lex lex-src) (lexer-pos lex))))))

(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 65))
        (s2 (store-byte (ptr+ buf 1) 66))
        (s3 (store-byte (ptr+ buf 2) 67))
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((input (make-input))
        (lex (make-lexer input 3)))
    (peek-char lex)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "65");
}

#[test]
fn test_boxed_int() {
    let source = r#"
(defstruct BoxedInt (boxed-int-val: i64))
(defun box-int (v) -> ptr (share (BoxedInt v)))
(defun unbox-int (b: ptr) -> i64 (. b boxed-int-val))

(defun test () -> i64
  (let ((b (box-int 42)))
    (unbox-int b)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "42");
}

#[test]
fn test_pcons() {
    let source = r#"
(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr pcons-tl: ptr) -> ptr (share (PCons h pcons-tl)))
(defun pcons-head (c: ptr) -> ptr (. c pcons-hd))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))

(defstruct BoxedInt (boxed-int-val: i64))
(defun box-int (v) -> ptr (share (BoxedInt v)))
(defun unbox-int (b: ptr) -> i64 (. b boxed-int-val))

(defun test () -> i64
  (let ((pair (pcons (box-int 10) (box-int 20)))
        (first (pcons-head pair))
        (second (pcons-tail pair)))
    (+ (unbox-int first) (unbox-int second))))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "30");
}

#[test]
fn test_read_digits() {
    let source = r#"
(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64))
(defun make-lexer (src len: i64) -> ptr (share (Lexer src 0 len)))
(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))
(defun lexer-src (lex: ptr) -> ptr (. lex lex-src))
(defun lexer-len (lex: ptr) -> i64 (. lex lex-len))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (lexer-len lex))
      -1
      (zext i64 (load-byte (ptr+ (lexer-src lex) (lexer-pos lex))))))

(defun advance-char (lex: ptr) -> ptr
  (share (Lexer (lexer-src lex) (+ (lexer-pos lex) 1) (lexer-len lex))))

(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr pcons-tl: ptr) -> ptr (share (PCons h pcons-tl)))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))

(defstruct BoxedInt (boxed-int-val: i64))
(defun box-int (v) -> ptr (share (BoxedInt v)))
(defun unbox-int (b: ptr) -> i64 (. b boxed-int-val))

(defun digit? (c: i64)
  (if (>= c 48) (if (<= c 57) true false) false))

(defun read-digits (lex: ptr acc: i64) -> ptr
  (let ((c (peek-char lex)))
    (if (digit? c)
        (read-digits (advance-char lex) (+ (* acc 10) (- c 48)))
        (pcons lex (box-int acc)))))

;; Input: "123"
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 49))
        (s2 (store-byte (ptr+ buf 1) 50))
        (s3 (store-byte (ptr+ buf 2) 51))
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((input (make-input))
        (lex (make-lexer input 3))
        (result (read-digits lex 0))
        (val (pcons-tail result)))
    (unbox-int val)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "123");
}

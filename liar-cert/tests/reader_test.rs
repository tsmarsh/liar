//! Reader tests
//!
//! Tests for the liar S-expression reader implemented in liar.

use liar_cert::{compile_and_call, format_value};

const READER_SOURCE: &str = r#"
;; ============================================================================
;; value.liar
;; ============================================================================

(defstruct ICons (icons-hd: i64 icons-tl: ptr))
(defun icons (h icons-tl: ptr) -> ptr (share (ICons h icons-tl)))
(defun icons-head (c: ptr) -> i64 (. c icons-hd))
(defun icons-tail (c: ptr) -> ptr (. c icons-tl))
(defun icons? (x: ptr) (instance? x ICons))

(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr pcons-tl: ptr) -> ptr (share (PCons h pcons-tl)))
(defun pcons-head (c: ptr) -> ptr (. c pcons-hd))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))
(defun pcons? (x: ptr) (instance? x PCons))

(defstruct SCons (scons-hd: ptr scons-tl: ptr scons-line: i64 scons-col: i64))
(defun scons (h: ptr scons-tl: ptr) -> ptr (share (SCons h scons-tl 0 0)))
(defun scons-loc (h: ptr scons-tl: ptr line col) -> ptr (share (SCons h scons-tl line col)))
(defun scons-head (c: ptr) -> ptr (. c scons-hd))
(defun scons-tail (c: ptr) -> ptr (. c scons-tl))
(defun scons-line (c: ptr) -> i64 (. c scons-line))
(defun scons-col (c: ptr) -> i64 (. c scons-col))
(defun scons? (x: ptr) (instance? x SCons))

(defstruct BoxedInt (boxed-int-val: i64))
(defun box-int (v) -> ptr (share (BoxedInt v)))
(defun unbox-int (b: ptr) -> i64 (. b boxed-int-val))
(defun boxed-int? (x: ptr) (instance? x BoxedInt))

(defstruct LiarString (liar-str-data: ptr liar-str-len: i64))
(defun liar-string (liar-str-data: ptr liar-str-len) -> ptr (share (LiarString liar-str-data liar-str-len)))
(defun liar-string? (x: ptr) (instance? x LiarString))
(defun liar-string-data (s: ptr) -> ptr (. s liar-str-data))
(defun liar-string-len (s: ptr) -> i64 (. s liar-str-len))

;; ============================================================================
;; symbols.liar
;; ============================================================================

(defstruct Symbol (sym-id: i64 sym-name: ptr))
(defun symbol-id (s: ptr) -> i64 (. s sym-id))
(defun symbol-name (s: ptr) -> ptr (. s sym-name))
(defun symbol? (x: ptr) (instance? x Symbol))

(defstruct SymbolTable (sym-tab-data: ptr sym-tab-cap: i64 sym-tab-count: i64))
(defun make-symbol-table (cap) -> ptr
  (let ((data (heap-array-ptr (+ cap 1))))
    (share (SymbolTable data cap 0))))
(defun sym-table-data (t: ptr) -> ptr (. t sym-tab-data))
(defun sym-table-cap (t: ptr) -> i64 (. t sym-tab-cap))
(defun sym-table-count (t: ptr) -> i64 (. t sym-tab-count))

(defun streq-loop (a: ptr b: ptr idx)
  (let ((ca (load-byte (ptr+ a idx)))
        (cb (load-byte (ptr+ b idx))))
    (if (= ca cb)
        (if (= ca 0)
            true
            (streq-loop a b (+ idx 1)))
        false)))

(defun streq (a: ptr b: ptr)
  (streq-loop a b 0))

(defun find-symbol-loop (data: ptr name count idx) -> i64
  (if (>= idx count)
      -1
      (let ((stored-name (aget-ptr data idx)))
        (if (streq stored-name name)
            idx
            (find-symbol-loop data name count (+ idx 1))))))

(defun find-symbol (table: ptr name) -> i64
  (let ((data (sym-table-data table))
        (count (sym-table-count table)))
    (find-symbol-loop data name count 0)))

(defun add-symbol (table: ptr name) -> i64
  (let ((data (sym-table-data table))
        (count (sym-table-count table))
        (s (aset-ptr data count name)))
    count))

(defun intern (table: ptr name) -> ptr
  (let ((existing (find-symbol table name)))
    (if (>= existing 0)
        (share (Symbol existing name))
        (let ((new-id (add-symbol table name)))
          (share (Symbol new-id name))))))

;; ============================================================================
;; reader.liar (simplified)
;; ============================================================================

(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64 lex-line: i64 lex-col: i64))

(defun make-lexer (src len: i64) -> ptr
  (share (Lexer src 0 len 1 1)))

(defun lexer-src (lex: ptr) -> ptr (. lex lex-src))
(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))
(defun lexer-len (lex: ptr) -> i64 (. lex lex-len))
(defun lexer-line (lex: ptr) -> i64 (. lex lex-line))
(defun lexer-col (lex: ptr) -> i64 (. lex lex-col))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (lexer-len lex))
      -1
      (zext i64 (load-byte (ptr+ (lexer-src lex) (lexer-pos lex))))))

(defun peek-char-at (lex: ptr offset: i64) -> i64
  (let ((pos (+ (lexer-pos lex) offset)))
    (if (>= pos (lexer-len lex))
        -1
        (zext i64 (load-byte (ptr+ (lexer-src lex) pos))))))

(defun advance-char (lex: ptr) -> ptr
  (let ((c (peek-char lex))
        (pos (+ (lexer-pos lex) 1)))
    (if (= c 10)
        (share (Lexer (lexer-src lex) pos (lexer-len lex) (+ (lexer-line lex) 1) 1))
        (share (Lexer (lexer-src lex) pos (lexer-len lex) (lexer-line lex) (+ (lexer-col lex) 1))))))

(defun advance-n (lex: ptr n: i64) -> ptr
  (if (<= n 0)
      lex
      (advance-n (advance-char lex) (- n 1))))

(defun eof? (c: i64) (= c -1))
(defun whitespace? (c: i64)
  (if (= c 32) true (if (= c 10) true (if (= c 9) true (if (= c 13) true false)))))
(defun digit? (c: i64)
  (if (>= c 48) (if (<= c 57) true false) false))
(defun alpha? (c: i64)
  (if (>= c 97)
      (if (<= c 122) true (if (>= c 65) (if (<= c 90) true false) false))
      (if (>= c 65) (if (<= c 90) true false) false)))
(defun symbol-start? (c: i64)
  (if (alpha? c) true
      (if (= c 95) true (if (= c 43) true (if (= c 45) true (if (= c 42) true
      (if (= c 47) true (if (= c 60) true (if (= c 62) true (if (= c 61) true
      (if (= c 63) true (if (= c 33) true (if (= c 38) true false)))))))))))))
(defun symbol-char? (c: i64)
  (if (symbol-start? c) true (if (digit? c) true (if (= c 46) true false))))

(defun skip-to-eol (lex: ptr) -> ptr
  (let ((c (peek-char lex)))
    (if (eof? c) lex
        (if (= c 10) (advance-char lex)
            (skip-to-eol (advance-char lex))))))

(defun skip-ws (lex: ptr) -> ptr
  (let ((c (peek-char lex)))
    (if (eof? c) lex
        (if (whitespace? c) (skip-ws (advance-char lex))
            (if (= c 59)
                (skip-ws (skip-to-eol lex))
                lex)))))

(defun read-digits (lex: ptr acc: i64) -> ptr
  (let ((c (peek-char lex)))
    (if (digit? c)
        (read-digits (advance-char lex) (+ (* acc 10) (- c 48)))
        (pcons lex (box-int acc)))))

(defun read-number (lex: ptr) -> ptr
  (let ((c (peek-char lex)))
    (if (= c 45)
        (let ((result (read-digits (advance-char lex) 0)))
          (pcons (pcons-head result)
                 (box-int (- 0 (unbox-int (pcons-tail result))))))
        (read-digits lex 0))))

(defun count-symbol-len (lex: ptr len: i64) -> i64
  (let ((c (peek-char-at lex len)))
    (if (symbol-char? c)
        (count-symbol-len lex (+ len 1))
        len)))

(defun copy-bytes-loop (src: ptr dest: ptr n: i64 idx: i64) -> i64
  (if (>= idx n)
      n
      (let ((b (load-byte (ptr+ src idx)))
            (s (store-byte (ptr+ dest idx) b)))
        (copy-bytes-loop src dest n (+ idx 1)))))

(defun copy-bytes (src: ptr dest: ptr n: i64) -> i64
  (copy-bytes-loop src dest n 0))

(defun read-symbol (lex: ptr table: ptr) -> ptr
  (let ((start-pos (lexer-pos lex))
        (len (count-symbol-len lex 0))
        (buf (heap-array (+ len 1)))
        (c (copy-bytes (ptr+ (lexer-src lex) start-pos) buf len))
        (s (store-byte (ptr+ buf len) 0))
        (new-lex (advance-n lex len))
        (sym (intern table buf)))
    (pcons new-lex sym)))

(defun read-list-elements (lex: ptr table: ptr) -> ptr
  (let ((lex2 (skip-ws lex))
        (c (peek-char lex2))
        (line (lexer-line lex2))
        (col (lexer-col lex2)))
    (if (= c 41)
        (pcons (advance-char lex2) nil)
        (if (eof? c)
            (pcons lex2 nil)
            (let ((elem-result (read-value lex2 table))
                  (elem-lex (pcons-head elem-result))
                  (elem-val (pcons-tail elem-result))
                  (rest-result (read-list-elements elem-lex table))
                  (rest-lex (pcons-head rest-result))
                  (rest-list (pcons-tail rest-result)))
              (pcons rest-lex (scons-loc elem-val rest-list line col)))))))

(defun read-list (lex: ptr table: ptr) -> ptr
  (let ((start-lex (advance-char lex)))
    (read-list-elements start-lex table)))

(defun read-value (lex: ptr table: ptr) -> ptr
  (let ((lex2 (skip-ws lex))
        (c (peek-char lex2)))
    (if (eof? c)
        (pcons lex2 nil)
        (if (= c 40)
            (read-list lex2 table)
            (if (digit? c)
                (read-number lex2)
                (if (= c 45)
                    (if (digit? (peek-char-at lex2 1))
                        (read-number lex2)
                        (read-symbol lex2 table))
                    (if (symbol-start? c)
                        (read-symbol lex2 table)
                        (pcons (advance-char lex2) nil))))))))

(defun read-all-loop (lex: ptr table: ptr acc: ptr) -> ptr
  (let ((lex2 (skip-ws lex))
        (c (peek-char lex2)))
    (if (eof? c)
        acc
        (let ((result (read-value lex2 table))
              (new-lex (pcons-head result))
              (val (pcons-tail result)))
          (if (nil? val)
              acc
              (read-all-loop new-lex table (scons val acc)))))))

(defun reverse-list-loop (lst: ptr acc: ptr) -> ptr
  (if (nil? lst)
      acc
      (reverse-list-loop (scons-tail lst)
                         (scons (scons-head lst) acc))))

(defun reverse-list (lst: ptr) -> ptr
  (reverse-list-loop lst nil))

(defun read-all (src: ptr len: i64 table: ptr) -> ptr
  (let ((lex (make-lexer src len))
        (exprs (read-all-loop lex table nil)))
    (reverse-list exprs)))

;; ============================================================================
;; Test helpers
;; ============================================================================

;; Count elements in an SCons list
(defun count-list (lst: ptr) -> i64
  (if (nil? lst)
      0
      (+ 1 (count-list (scons-tail lst)))))
"#;

#[test]
fn test_lexer_peek_char() {
    let source = format!(
        r#"{}
;; Test: create lexer and peek first char
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 65))  ;; 'A'
        (s2 (store-byte (ptr+ buf 1) 66))  ;; 'B'
        (s3 (store-byte (ptr+ buf 2) 67))  ;; 'C'
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((input (make-input))
        (lex (make-lexer input 3)))
    (peek-char lex)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "65"); // 'A'
}

#[test]
fn test_lexer_advance_char() {
    let source = format!(
        r#"{}
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 65))
        (s2 (store-byte (ptr+ buf 1) 66))
        (s3 (store-byte (ptr+ buf 2) 67))
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((input (make-input))
        (lex (make-lexer input 3))
        (lex2 (advance-char lex)))
    (peek-char lex2)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "66"); // 'B'
}

#[test]
fn test_read_single_number() {
    let source = format!(
        r#"{}
;; Input: "123"
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 49))  ;; '1'
        (s2 (store-byte (ptr+ buf 1) 50))  ;; '2'
        (s3 (store-byte (ptr+ buf 2) 51))  ;; '3'
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 3) table))
        (val (pcons-tail result)))
    (unbox-int val)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "123");
}

#[test]
fn test_read_negative_number() {
    let source = format!(
        r#"{}
;; Input: "-42"
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 45))  ;; '-'
        (s2 (store-byte (ptr+ buf 1) 52))  ;; '4'
        (s3 (store-byte (ptr+ buf 2) 50))  ;; '2'
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 3) table))
        (val (pcons-tail result)))
    (unbox-int val)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "-42");
}

#[test]
fn test_read_symbol() {
    let source = format!(
        r#"{}
;; Input: "foo"
(defun make-input () -> ptr
  (let ((buf (heap-array 4))
        (s1 (store-byte buf 102))  ;; 'f'
        (s2 (store-byte (ptr+ buf 1) 111))  ;; 'o'
        (s3 (store-byte (ptr+ buf 2) 111))  ;; 'o'
        (s4 (store-byte (ptr+ buf 3) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 3) table))
        (sym (pcons-tail result)))
    (symbol-id sym)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    // First symbol should have id 0
    assert_eq!(format_value(&result), "0");
}

#[test]
fn test_read_empty_list() {
    let source = format!(
        r#"{}
;; Input: "()"
(defun make-input () -> ptr
  (let ((buf (heap-array 3))
        (s1 (store-byte buf 40))  ;; '('
        (s2 (store-byte (ptr+ buf 1) 41))  ;; ')'
        (s3 (store-byte (ptr+ buf 2) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 2) table))
        (val (pcons-tail result)))
    (if (nil? val) 1 0)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "1"); // nil = empty list
}

#[test]
fn test_read_simple_list() {
    let source = format!(
        r#"{}
;; Input: "(1 2 3)"
(defun make-input () -> ptr
  (let ((buf (heap-array 8))
        (s0 (store-byte buf 40))  ;; '('
        (s1 (store-byte (ptr+ buf 1) 49))  ;; '1'
        (s2 (store-byte (ptr+ buf 2) 32))  ;; ' '
        (s3 (store-byte (ptr+ buf 3) 50))  ;; '2'
        (s4 (store-byte (ptr+ buf 4) 32))  ;; ' '
        (s5 (store-byte (ptr+ buf 5) 51))  ;; '3'
        (s6 (store-byte (ptr+ buf 6) 41))  ;; ')'
        (s7 (store-byte (ptr+ buf 7) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 7) table))
        (lst (pcons-tail result)))
    (count-list lst)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "3"); // list has 3 elements
}

#[test]
fn test_read_list_first_element() {
    let source = format!(
        r#"{}
;; Input: "(42 99)"
(defun make-input () -> ptr
  (let ((buf (heap-array 8))
        (s0 (store-byte buf 40))  ;; '('
        (s1 (store-byte (ptr+ buf 1) 52))  ;; '4'
        (s2 (store-byte (ptr+ buf 2) 50))  ;; '2'
        (s3 (store-byte (ptr+ buf 3) 32))  ;; ' '
        (s4 (store-byte (ptr+ buf 4) 57))  ;; '9'
        (s5 (store-byte (ptr+ buf 5) 57))  ;; '9'
        (s6 (store-byte (ptr+ buf 6) 41))  ;; ')'
        (s7 (store-byte (ptr+ buf 7) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 7) table))
        (lst (pcons-tail result))
        (first (scons-head lst)))
    (unbox-int first)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "42");
}

#[test]
fn test_read_all_expressions() {
    let source = format!(
        r#"{}
;; Input: "1 2 3" (three separate numbers)
(defun make-input () -> ptr
  (let ((buf (heap-array 6))
        (s0 (store-byte buf 49))  ;; '1'
        (s1 (store-byte (ptr+ buf 1) 32))  ;; ' '
        (s2 (store-byte (ptr+ buf 2) 50))  ;; '2'
        (s3 (store-byte (ptr+ buf 3) 32))  ;; ' '
        (s4 (store-byte (ptr+ buf 4) 51))  ;; '3'
        (s5 (store-byte (ptr+ buf 5) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (exprs (read-all input 5 table)))
    (count-list exprs)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "3"); // 3 expressions
}

#[test]
fn test_skip_comment() {
    let source = format!(
        r#"{}
;; Input: "; comment\n42"
(defun make-input () -> ptr
  (let ((buf (heap-array 13))
        (s0 (store-byte buf 59))  ;; ';'
        (s1 (store-byte (ptr+ buf 1) 32))  ;; ' '
        (s2 (store-byte (ptr+ buf 2) 99))  ;; 'c'
        (s3 (store-byte (ptr+ buf 3) 111))  ;; 'o'
        (s4 (store-byte (ptr+ buf 4) 109))  ;; 'm'
        (s5 (store-byte (ptr+ buf 5) 109))  ;; 'm'
        (s6 (store-byte (ptr+ buf 6) 101))  ;; 'e'
        (s7 (store-byte (ptr+ buf 7) 110))  ;; 'n'
        (s8 (store-byte (ptr+ buf 8) 116))  ;; 't'
        (s9 (store-byte (ptr+ buf 9) 10))  ;; '\n'
        (sa (store-byte (ptr+ buf 10) 52))  ;; '4'
        (sb (store-byte (ptr+ buf 11) 50))  ;; '2'
        (sc (store-byte (ptr+ buf 12) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-value (make-lexer input 12) table))
        (val (pcons-tail result)))
    (unbox-int val)))
"#,
        READER_SOURCE
    );

    let result = compile_and_call(&source, "test").unwrap();
    assert_eq!(format_value(&result), "42");
}

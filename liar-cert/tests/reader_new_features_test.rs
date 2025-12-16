//! Tests for new reader features: quasiquote, unquote
//!
//! These tests verify the newly added reader functionality using simplified
//! inline reader code that follows the patterns from reader_test.rs

use liar_cert::{compile_and_call, format_value};

// =============================================================================
// Quasiquote Tests
// =============================================================================

#[test]
fn test_read_quasiquote() {
    // Input: "`x" should produce a 2-element list (quasiquote x)
    let source = r#"
(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64 lex-line: i64 lex-col: i64))
(defun make-lexer (src len: i64) -> ptr (share (Lexer src 0 len 1 1)))
(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))
(defun lexer-src (lex: ptr) -> ptr (. lex lex-src))
(defun lexer-len (lex: ptr) -> i64 (. lex lex-len))
(defun lexer-line (lex: ptr) -> i64 (. lex lex-line))
(defun lexer-col (lex: ptr) -> i64 (. lex lex-col))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (lexer-len lex))
      -1
      (zext i64 (load-byte (ptr+ (lexer-src lex) (lexer-pos lex))))))

(defun advance-char (lex: ptr) -> ptr
  (let ((pos (+ (lexer-pos lex) 1)))
    (share (Lexer (lexer-src lex) pos (lexer-len lex) (lexer-line lex) (+ (lexer-col lex) 1)))))

(defun advance-n (lex: ptr n: i64) -> ptr
  (if (<= n 0) lex (advance-n (advance-char lex) (- n 1))))

(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr t: ptr) -> ptr (share (PCons h t)))
(defun pcons-head (c: ptr) -> ptr (. c pcons-hd))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))

(defstruct SCons (scons-hd: ptr scons-tl: ptr scons-line: i64 scons-col: i64))
(defun scons (h: ptr t: ptr) -> ptr (share (SCons h t 0 0)))
(defun scons-loc (h: ptr t: ptr line col) -> ptr (share (SCons h t line col)))
(defun scons-head (c: ptr) -> ptr (. c scons-hd))
(defun scons-tail (c: ptr) -> ptr (. c scons-tl))

(defstruct Symbol (sym-id: i64 sym-name: ptr))
(defun symbol-id (s: ptr) -> i64 (. s sym-id))
(defun symbol-name (s: ptr) -> ptr (. s sym-name))

(defstruct SymbolTable (st-data: ptr st-cap: i64 st-count: i64))
(defun make-symbol-table (cap) -> ptr
  (share (SymbolTable (heap-array-ptr (+ cap 1)) cap 0)))

(defun streq-loop (a: ptr b: ptr idx)
  (let ((ca (load-byte (ptr+ a idx)))
        (cb (load-byte (ptr+ b idx))))
    (if (= ca cb)
        (if (= ca 0) true (streq-loop a b (+ idx 1)))
        false)))
(defun streq (a: ptr b: ptr) (streq-loop a b 0))

(defun find-symbol-loop (data: ptr name count idx) -> i64
  (if (>= idx count) -1
      (let ((stored (aget-ptr data idx)))
        (if (streq stored name) idx
            (find-symbol-loop data name count (+ idx 1))))))
(defun find-symbol (table: ptr name) -> i64
  (find-symbol-loop (. table st-data) name (. table st-count) 0))
(defun add-symbol (table: ptr name) -> i64
  (let ((data (. table st-data))
        (count (. table st-count))
        (s (aset-ptr data count name)))
    count))
(defun intern (table: ptr name) -> ptr
  (let ((existing (find-symbol table name)))
    (if (>= existing 0)
        (share (Symbol existing name))
        (share (Symbol (add-symbol table name) name)))))

(defun alpha? (c: i64)
  (if (>= c 97) (if (<= c 122) true false) false))
(defun symbol-char? (c: i64) (alpha? c))

(defun count-symbol-len (lex: ptr len: i64) -> i64
  (let ((c (zext i64 (load-byte (ptr+ (lexer-src lex) (+ (lexer-pos lex) len))))))
    (if (symbol-char? c) (count-symbol-len lex (+ len 1)) len)))

(defun copy-bytes-loop (src: ptr dest: ptr n: i64 idx: i64) -> i64
  (if (>= idx n) n
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

;; Create "quasiquote" string
(defun make-quasiquote-str () -> ptr
  (let ((buf (heap-array 11))
        (s0 (store-byte buf 113))
        (s1 (store-byte (ptr+ buf 1) 117))
        (s2 (store-byte (ptr+ buf 2) 97))
        (s3 (store-byte (ptr+ buf 3) 115))
        (s4 (store-byte (ptr+ buf 4) 105))
        (s5 (store-byte (ptr+ buf 5) 113))
        (s6 (store-byte (ptr+ buf 6) 117))
        (s7 (store-byte (ptr+ buf 7) 111))
        (s8 (store-byte (ptr+ buf 8) 116))
        (s9 (store-byte (ptr+ buf 9) 101))
        (sa (store-byte (ptr+ buf 10) 0)))
    buf))

(defun read-quasiquote (lex: ptr table: ptr) -> ptr
  (let ((start-lex (advance-char lex))
        (line (lexer-line lex))
        (col (lexer-col lex))
        (qq-str (make-quasiquote-str))
        (qq-sym (intern table qq-str))
        (val-result (read-symbol start-lex table))
        (val-lex (pcons-head val-result))
        (val (pcons-tail val-result)))
    (pcons val-lex (scons-loc qq-sym (scons-loc val nil line col) line col))))

;; Count elements in SCons list
(defun count-list (lst: ptr) -> i64
  (if (nil? lst) 0 (+ 1 (count-list (scons-tail lst)))))

;; Input: "`x"
(defun make-input () -> ptr
  (let ((buf (heap-array 3))
        (s0 (store-byte buf 96))
        (s1 (store-byte (ptr+ buf 1) 120))
        (s2 (store-byte (ptr+ buf 2) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-quasiquote (make-lexer input 2) table))
        (val (pcons-tail result)))
    (count-list val)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "2"); // (quasiquote x) has 2 elements
}

#[test]
fn test_quasiquote_first_element_is_symbol() {
    // Input: "`x" - verify first element is a symbol by checking symbol-id
    let source = r#"
(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64 lex-line: i64 lex-col: i64))
(defun make-lexer (src len: i64) -> ptr (share (Lexer src 0 len 1 1)))
(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))
(defun lexer-src (lex: ptr) -> ptr (. lex lex-src))
(defun lexer-len (lex: ptr) -> i64 (. lex lex-len))
(defun lexer-line (lex: ptr) -> i64 (. lex lex-line))
(defun lexer-col (lex: ptr) -> i64 (. lex lex-col))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (lexer-len lex))
      -1
      (zext i64 (load-byte (ptr+ (lexer-src lex) (lexer-pos lex))))))

(defun advance-char (lex: ptr) -> ptr
  (let ((pos (+ (lexer-pos lex) 1)))
    (share (Lexer (lexer-src lex) pos (lexer-len lex) (lexer-line lex) (+ (lexer-col lex) 1)))))

(defun advance-n (lex: ptr n: i64) -> ptr
  (if (<= n 0) lex (advance-n (advance-char lex) (- n 1))))

(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr t: ptr) -> ptr (share (PCons h t)))
(defun pcons-head (c: ptr) -> ptr (. c pcons-hd))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))

(defstruct SCons (scons-hd: ptr scons-tl: ptr scons-line: i64 scons-col: i64))
(defun scons (h: ptr t: ptr) -> ptr (share (SCons h t 0 0)))
(defun scons-loc (h: ptr t: ptr line col) -> ptr (share (SCons h t line col)))
(defun scons-head (c: ptr) -> ptr (. c scons-hd))
(defun scons-tail (c: ptr) -> ptr (. c scons-tl))

(defstruct Symbol (sym-id: i64 sym-name: ptr))
(defun symbol-id (s: ptr) -> i64 (. s sym-id))

(defstruct SymbolTable (st-data: ptr st-cap: i64 st-count: i64))
(defun make-symbol-table (cap) -> ptr
  (share (SymbolTable (heap-array-ptr (+ cap 1)) cap 0)))

(defun streq-loop (a: ptr b: ptr idx)
  (let ((ca (load-byte (ptr+ a idx)))
        (cb (load-byte (ptr+ b idx))))
    (if (= ca cb)
        (if (= ca 0) true (streq-loop a b (+ idx 1)))
        false)))
(defun streq (a: ptr b: ptr) (streq-loop a b 0))

(defun find-symbol-loop (data: ptr name count idx) -> i64
  (if (>= idx count) -1
      (let ((stored (aget-ptr data idx)))
        (if (streq stored name) idx
            (find-symbol-loop data name count (+ idx 1))))))
(defun find-symbol (table: ptr name) -> i64
  (find-symbol-loop (. table st-data) name (. table st-count) 0))
(defun add-symbol (table: ptr name) -> i64
  (let ((data (. table st-data))
        (count (. table st-count))
        (s (aset-ptr data count name)))
    count))
(defun intern (table: ptr name) -> ptr
  (let ((existing (find-symbol table name)))
    (if (>= existing 0)
        (share (Symbol existing name))
        (share (Symbol (add-symbol table name) name)))))

(defun alpha? (c: i64)
  (if (>= c 97) (if (<= c 122) true false) false))
(defun symbol-char? (c: i64) (alpha? c))

(defun count-symbol-len (lex: ptr len: i64) -> i64
  (let ((c (zext i64 (load-byte (ptr+ (lexer-src lex) (+ (lexer-pos lex) len))))))
    (if (symbol-char? c) (count-symbol-len lex (+ len 1)) len)))

(defun copy-bytes-loop (src: ptr dest: ptr n: i64 idx: i64) -> i64
  (if (>= idx n) n
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

(defun make-quasiquote-str () -> ptr
  (let ((buf (heap-array 11))
        (s0 (store-byte buf 113))
        (s1 (store-byte (ptr+ buf 1) 117))
        (s2 (store-byte (ptr+ buf 2) 97))
        (s3 (store-byte (ptr+ buf 3) 115))
        (s4 (store-byte (ptr+ buf 4) 105))
        (s5 (store-byte (ptr+ buf 5) 113))
        (s6 (store-byte (ptr+ buf 6) 117))
        (s7 (store-byte (ptr+ buf 7) 111))
        (s8 (store-byte (ptr+ buf 8) 116))
        (s9 (store-byte (ptr+ buf 9) 101))
        (sa (store-byte (ptr+ buf 10) 0)))
    buf))

(defun read-quasiquote (lex: ptr table: ptr) -> ptr
  (let ((start-lex (advance-char lex))
        (line (lexer-line lex))
        (col (lexer-col lex))
        (qq-str (make-quasiquote-str))
        (qq-sym (intern table qq-str))
        (val-result (read-symbol start-lex table))
        (val-lex (pcons-head val-result))
        (val (pcons-tail val-result)))
    (pcons val-lex (scons-loc qq-sym (scons-loc val nil line col) line col))))

(defun make-input () -> ptr
  (let ((buf (heap-array 3))
        (s0 (store-byte buf 96))
        (s1 (store-byte (ptr+ buf 1) 120))
        (s2 (store-byte (ptr+ buf 2) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-quasiquote (make-lexer input 2) table))
        (val (pcons-tail result))
        (first-elem (scons-head val)))
    ;; First symbol should have id 0 - if this fails, first-elem isn't a symbol
    (symbol-id first-elem)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "0"); // symbol-id of first interned symbol is 0
}

// =============================================================================
// Unquote Test
// =============================================================================

#[test]
fn test_read_unquote() {
    // Input: "~y" should produce a 2-element list (unquote y)
    let source = r#"
(defstruct Lexer (lex-src: ptr lex-pos: i64 lex-len: i64 lex-line: i64 lex-col: i64))
(defun make-lexer (src len: i64) -> ptr (share (Lexer src 0 len 1 1)))
(defun lexer-pos (lex: ptr) -> i64 (. lex lex-pos))
(defun lexer-src (lex: ptr) -> ptr (. lex lex-src))
(defun lexer-len (lex: ptr) -> i64 (. lex lex-len))
(defun lexer-line (lex: ptr) -> i64 (. lex lex-line))
(defun lexer-col (lex: ptr) -> i64 (. lex lex-col))

(defun peek-char (lex: ptr) -> i64
  (if (>= (lexer-pos lex) (lexer-len lex))
      -1
      (zext i64 (load-byte (ptr+ (lexer-src lex) (lexer-pos lex))))))

(defun advance-char (lex: ptr) -> ptr
  (let ((pos (+ (lexer-pos lex) 1)))
    (share (Lexer (lexer-src lex) pos (lexer-len lex) (lexer-line lex) (+ (lexer-col lex) 1)))))

(defun advance-n (lex: ptr n: i64) -> ptr
  (if (<= n 0) lex (advance-n (advance-char lex) (- n 1))))

(defstruct PCons (pcons-hd: ptr pcons-tl: ptr))
(defun pcons (h: ptr t: ptr) -> ptr (share (PCons h t)))
(defun pcons-head (c: ptr) -> ptr (. c pcons-hd))
(defun pcons-tail (c: ptr) -> ptr (. c pcons-tl))

(defstruct SCons (scons-hd: ptr scons-tl: ptr scons-line: i64 scons-col: i64))
(defun scons (h: ptr t: ptr) -> ptr (share (SCons h t 0 0)))
(defun scons-loc (h: ptr t: ptr line col) -> ptr (share (SCons h t line col)))
(defun scons-head (c: ptr) -> ptr (. c scons-hd))
(defun scons-tail (c: ptr) -> ptr (. c scons-tl))

(defstruct Symbol (sym-id: i64 sym-name: ptr))
(defun symbol-id (s: ptr) -> i64 (. s sym-id))

(defstruct SymbolTable (st-data: ptr st-cap: i64 st-count: i64))
(defun make-symbol-table (cap) -> ptr
  (share (SymbolTable (heap-array-ptr (+ cap 1)) cap 0)))

(defun streq-loop (a: ptr b: ptr idx)
  (let ((ca (load-byte (ptr+ a idx)))
        (cb (load-byte (ptr+ b idx))))
    (if (= ca cb)
        (if (= ca 0) true (streq-loop a b (+ idx 1)))
        false)))
(defun streq (a: ptr b: ptr) (streq-loop a b 0))

(defun find-symbol-loop (data: ptr name count idx) -> i64
  (if (>= idx count) -1
      (let ((stored (aget-ptr data idx)))
        (if (streq stored name) idx
            (find-symbol-loop data name count (+ idx 1))))))
(defun find-symbol (table: ptr name) -> i64
  (find-symbol-loop (. table st-data) name (. table st-count) 0))
(defun add-symbol (table: ptr name) -> i64
  (let ((data (. table st-data))
        (count (. table st-count))
        (s (aset-ptr data count name)))
    count))
(defun intern (table: ptr name) -> ptr
  (let ((existing (find-symbol table name)))
    (if (>= existing 0)
        (share (Symbol existing name))
        (share (Symbol (add-symbol table name) name)))))

(defun alpha? (c: i64)
  (if (>= c 97) (if (<= c 122) true false) false))
(defun symbol-char? (c: i64) (alpha? c))

(defun count-symbol-len (lex: ptr len: i64) -> i64
  (let ((c (zext i64 (load-byte (ptr+ (lexer-src lex) (+ (lexer-pos lex) len))))))
    (if (symbol-char? c) (count-symbol-len lex (+ len 1)) len)))

(defun copy-bytes-loop (src: ptr dest: ptr n: i64 idx: i64) -> i64
  (if (>= idx n) n
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

;; Create "unquote" string
(defun make-unquote-str () -> ptr
  (let ((buf (heap-array 8))
        (s0 (store-byte buf 117))
        (s1 (store-byte (ptr+ buf 1) 110))
        (s2 (store-byte (ptr+ buf 2) 113))
        (s3 (store-byte (ptr+ buf 3) 117))
        (s4 (store-byte (ptr+ buf 4) 111))
        (s5 (store-byte (ptr+ buf 5) 116))
        (s6 (store-byte (ptr+ buf 6) 101))
        (s7 (store-byte (ptr+ buf 7) 0)))
    buf))

;; Read ~y -> (unquote y) - simplified, assumes no @
(defun read-unquote (lex: ptr table: ptr) -> ptr
  (let ((start-lex (advance-char lex))
        (line (lexer-line lex))
        (col (lexer-col lex))
        (uq-str (make-unquote-str))
        (uq-sym (intern table uq-str))
        (val-result (read-symbol start-lex table))
        (val-lex (pcons-head val-result))
        (val (pcons-tail val-result)))
    (pcons val-lex (scons-loc uq-sym (scons-loc val nil line col) line col))))

(defun count-list (lst: ptr) -> i64
  (if (nil? lst) 0 (+ 1 (count-list (scons-tail lst)))))

;; Input: "~y"
(defun make-input () -> ptr
  (let ((buf (heap-array 3))
        (s0 (store-byte buf 126))
        (s1 (store-byte (ptr+ buf 1) 121))
        (s2 (store-byte (ptr+ buf 2) 0)))
    buf))

(defun test () -> i64
  (let ((table (make-symbol-table 100))
        (input (make-input))
        (result (read-unquote (make-lexer input 2) table))
        (val (pcons-tail result)))
    (count-list val)))
"#;

    let result = compile_and_call(source, "test").unwrap();
    assert_eq!(format_value(&result), "2"); // (unquote y) has 2 elements
}

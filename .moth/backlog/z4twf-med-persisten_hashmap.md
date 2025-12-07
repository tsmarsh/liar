# PersistentHashMap - Hash Array Mapped Trie

**Priority:** high
**Category:** lib/collections
**Dependencies:** bitops/popcount (done), arrays, trqhk (br/phi)

## Blocked By

- **trqhk** - Recursion requires proper `if` with br/phi
- Array operations need runtime support

## Summary

Implement HAMT (Hash Array Mapped Trie) for persistent key-value maps. O(log32 n) operations with structural sharing. Uses bitmap + popcount for sparse node compression.

## Data Structure
```lisp
;; Node types
(defstruct BitmapNode
  (bitmap: i64)     ;; which of 32 slots are occupied
  (array))          ;; compressed array (length = popcount(bitmap))

(defstruct ArrayNode
  (count: i64)      ;; number of non-nil children
  (array))          ;; full 32-element array

(defstruct CollisionNode
  (hash: i64)       ;; shared hash value
  (entries))        ;; list of (key, value) pairs

(defstruct MapEntry (key value))

(defstruct PersistentHashMap
  (count: i64)
  (root))           ;; BitmapNode, ArrayNode, or nil
```

## API
```lisp
;; Construction
(hash-map)                ;; empty
(hash-map k1 v1 k2 v2)    ;; from pairs

;; Access - O(log32 n)
(get m key)               ;; nil if not found
(get m key default)       ;; default if not found
(contains? m key)

;; Update - O(log32 n)
(assoc m key val)         ;; add/update
(dissoc m key)            ;; remove

;; Info - O(1)
(count m)
(empty? m)

;; Iteration
(keys m)                  ;; returns list
(vals m)                  ;; returns list
(entries m)               ;; returns list of MapEntry
```

## Implementation

**Location:** `lib/collections/hashmap.liar`
```lisp
(def BITS 5)
(def WIDTH 32)
(def MASK 0x1f)

(defstruct BitmapNode (bitmap array))
(defstruct ArrayNode (count array))
(defstruct CollisionNode (hash entries))
(defstruct MapEntry (key value))
(defstruct PersistentHashMap (count root))

(def EMPTY-MAP (PersistentHashMap count: 0 root: nil))

;; Hash function (simplified - real impl needs better hash)
(defun hash-code (x)
  ;; Mix bits for better distribution
  (let ((h (if (int? x) x (str-hash x))))
    (bit-xor h (bit-shift-right h 16))))

;; Bitmap index: which bit represents this hash fragment
(defun bit-pos (hash shift)
  (bit-shift-left 1 (bit-and (bit-shift-right hash shift) MASK)))

;; Array index: position in compressed array
(defun bitmap-index (bitmap bit)
  (popcount (bit-and bitmap (- bit 1))))

;; Get from HAMT
(defun get (m key)
  (if (nil? (. m root))
    nil
    (get-node (. m root) (hash-code key) key 0)))

(defun get-node (node hash key shift)
  (cond
    ((bitmap-node? node)
     (let ((bit (bit-pos hash shift))
           (bitmap (. node bitmap)))
       (if (= 0 (bit-and bitmap bit))
         nil  ;; not present
         (let ((idx (bitmap-index bitmap bit))
               (child (aget (. node array) idx)))
           (if (entry? child)
             (if (= (. child key) key) (. child value) nil)
             (get-node child hash key (+ shift BITS)))))))
    
    ((array-node? node)
     (let ((idx (bit-and (bit-shift-right hash shift) MASK))
           (child (aget (. node array) idx)))
       (if (nil? child)
         nil
         (get-node child hash key (+ shift BITS)))))
    
    ((collision-node? node)
     (find-in-collision (. node entries) key))
    
    (else nil)))

;; Assoc into HAMT
(defun assoc (m key val)
  (let ((hash (hash-code key))
        (new-root (assoc-node (. m root) hash key val 0)))
    (share (PersistentHashMap
      count: (+ (. m count) (if (contains? m key) 0 1))
      root: new-root))))

(defun assoc-node (node hash key val shift)
  (if (nil? node)
    ;; Create new entry
    (share (BitmapNode
      bitmap: (bit-pos hash shift)
      array: (array-of (share (MapEntry key: key value: val)))))
    
    (cond
      ((bitmap-node? node)
       (let ((bit (bit-pos hash shift))
             (idx (bitmap-index (. node bitmap) bit)))
         (if (= 0 (bit-and (. node bitmap) bit))
           ;; Add new entry
           (share (BitmapNode
             bitmap: (bit-or (. node bitmap) bit)
             array: (array-insert (. node array) idx
                      (share (MapEntry key: key value: val)))))
           ;; Existing slot
           (let ((child (aget (. node array) idx)))
             (if (entry? child)
               (if (= (. child key) key)
                 ;; Replace value
                 (share (BitmapNode
                   bitmap: (. node bitmap)
                   array: (array-set-copy (. node array) idx
                            (share (MapEntry key: key value: val)))))
                 ;; Hash collision at this level - push down
                 (share (BitmapNode
                   bitmap: (. node bitmap)
                   array: (array-set-copy (. node array) idx
                            (create-node (. child key) (. child value)
                                         key val (+ shift BITS))))))
               ;; Recurse into child node
               (share (BitmapNode
                 bitmap: (. node bitmap)
                 array: (array-set-copy (. node array) idx
                          (assoc-node child hash key val (+ shift BITS))))))))))
      
      ;; ArrayNode and CollisionNode cases...
      )))

;; Dissoc from HAMT  
(defun dissoc (m key)
  (if (nil? (. m root))
    m
    (let ((new-root (dissoc-node (. m root) (hash-code key) key 0)))
      (share (PersistentHashMap
        count: (- (. m count) (if (contains? m key) 1 0))
        root: new-root)))))
```

## Ownership Model

- All nodes `share`d
- `assoc`/`dissoc` path-copy affected nodes
- Unchanged subtrees shared between versions
- Bitmap compression: small nodes = small arrays = less copying

## Tests
```lisp
;; Basic ops
(let ((m (assoc (assoc (hash-map) :a 1) :b 2)))
  (assert (= (get m :a) 1))
  (assert (= (get m :b) 2))
  (assert (= (count m) 2)))

;; Structural sharing
(let ((m1 (assoc (hash-map) :a 1))
      (m2 (assoc m1 :b 2)))
  (assert (nil? (get m1 :b)))  ;; m1 unchanged
  (assert (= (get m2 :b) 2)))

;; Dissoc
(let ((m1 (assoc (assoc (hash-map) :a 1) :b 2))
      (m2 (dissoc m1 :a)))
  (assert (= (get m1 :a) 1))   ;; m1 unchanged
  (assert (nil? (get m2 :a)))
  (assert (= (get m2 :b) 2)))

;; Many keys (tests tree depth)
(let ((m (reduce (fn (m i) (assoc m i (* i 2))) (hash-map) (range 0 1000))))
  (assert (= (count m) 1000))
  (assert (= (get m 500) 1000)))

;; Hash collisions handled
```

## Acceptance Criteria

- [ ] BitmapNode with popcount indexing
- [ ] ArrayNode for dense nodes
- [ ] CollisionNode for hash collisions
- [ ] `get`/`assoc`/`dissoc` work correctly
- [ ] Structural sharing verified
- [ ] Ownership checker validates
- [ ] Performance: O(log32 n) not O(n)

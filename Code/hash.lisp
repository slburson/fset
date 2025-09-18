;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: hash.lisp
;;; Contents: Hashing function for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2024-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)

(defgeneric hash-value (x)
  (:documentation
    "Returns a fixnum providing an encoding of the contents of `x'.  The
result can be negative.  For best performance, it is recommended to use
fixnum arithmetic and return a fixnum; however, Common Lisp gives us no
way to enforce this constraint, so callers should use `hash-value-fixnum'
instead.

When writing methods for this generic function, in order to force fixnum
arithmetic, you may find `hash-mix', `hash-mixf', and `hash-multiply' useful.

This generic function is used in combination with `compare', so it is
important that for any two objects on which `compare' returns `:equal'
or `:unequal', this function returns the same hash value."))

(define-hash-function compare hash-value)

(declaim (inline hash-value-fixnum))
(defun hash-value-fixnum (x)
  "Returns the `hash-value' of `x', ensuring that the result is a fixnum."
  (let ((h (hash-value x)))
    (if (typep h 'fixnum) h
      (the fixnum (squash-bignum-hash h)))))

(defun squash-bignum-hash (h)
  (let ((len (integer-length h))
	(fixnum-len (integer-length most-positive-fixnum))
	(fh 0))
    (dotimes (i (ceiling len fixnum-len))
      (hash-mixf fh (ldb (byte fixnum-len (* fixnum-len i)) h)))
    (the fixnum (if (< h 0) (- fh) fh))))


(defmethod hash-value ((x identity-equality-mixin))
  (hash-mix (class-hash-value x) (slot-value x 'serial-number)))

(defmethod hash-value ((x identity-ordering-mixin))
  (hash-mix (class-hash-value x) (slot-value x 'serial-number)))

(defmethod hash-value ((s null))
  #32Rnil) ; = 24149.

(defmethod hash-value ((x real))
  (sxhash x))

(defmethod hash-value ((x integer))
  ;; It's fine if `x' is negative.
  (if (typep x 'fixnum) x
    (squash-bignum-hash x)))

(defmethod hash-value ((x complex))
  (sxhash x))

(defmethod hash-value ((x character))
  (sxhash x))

(defmethod hash-value ((x symbol))
  ;; None of SBCL, CCL, ABCL, Allegro, LispWorks, or CLASP include the package in the hash.  (Of these,
  ;; only on SBCL is the hash of a symbol different from that of its `symbol-name'.)  We _could_ mix
  ;; in the hash of the package, but how often does one really use symbols from multiple packages in
  ;; one set or map?  We'll leave that problem for `compare' — it handles it well.
  (symbol-hash-value x))

(defmethod hash-value ((x string))
  ;; We could, of course, do our own string hashing (using maybe SipHash? plenty to choose from:
  ;; https://en.wikipedia.org/wiki/List_of_hash_functions).  But the built-in string hash should
  ;; be well optimized for the implementation.  Let's go with it for now.
  (sxhash x))

(defmethod hash-value ((x list))
  ;; We assume that the list is longer (cdr-wise) than it is deep (car-wise).
  (list-hash-value x 4 12))

(defun list-hash-value (ls car-depth cdr-depth)
  ;; Since lists can be trees, they can have an arbitrary amount of stuff beneath them.
  ;; We bound the maximum depth to keep hashing effort from becoming excessive.
  (if (<= car-depth 0)
      0
    (do ((ls ls (cdr ls))
	 (result 0)
	 (mult 1 (hash-multiply mult 13))
	 (cdrs 0 (1+ cdrs)))
	((or (not (consp ls)) (>= cdrs cdr-depth))
	 (when (not (null ls))
	   (hash-mixf result (hash-value-fixnum ls))) ; defensive squashing
	 result)
      (let ((x (car ls))
	    ((x-hash (if (consp x)
			 (list-hash-value x (1- car-depth) (- cdr-depth 3))
		       (hash-value-fixnum x)))))
	(hash-mixf result (hash-multiply mult x-hash))))))

(defmethod hash-value ((x vector))
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (simple-vector-p x)
      (do ((len (length x))
	   (i 0 (1+ i))
	   (mult 1 (hash-multiply mult 13))
	   (result 0 (hash-mix result (hash-multiply mult (hash-value-fixnum (svref x i))))))
	  ((= i len) result))
    (do ((len (length x))
	 (i 0 (1+ i))
	 (mult 1 (hash-multiply mult 13))
	 (result 0 (hash-mix result (hash-multiply mult (hash-value-fixnum (aref x i))))))
	((= i len) result))))

(defmethod hash-value ((x pathname))
  (sxhash x))

;;; Arrays (that aren't vectors)
(defmethod hash-value ((x array))
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (do ((len (array-total-size x))
       (i 0 (1+ i))
       (mult 1 (hash-multiply mult 13))
       (result 0 (hash-mix result (hash-multiply mult (hash-value-fixnum (row-major-aref x i))))))
      ((= i len) result)))

;;; Fallback for generic sequences
(defmethod hash-value ((x sequence))
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (do ((len (length x))
       (i 0 (1+ i))
       (mult 1 (hash-multiply mult 13))
       (result 0 (hash-mix result (hash-multiply mult (hash-value-fixnum (elt x i))))))
      ((= i len) result)))

(defun zero (x)
  (declare (ignore x))
  0)

;;; `eql-compare' can be used as a value comparison function in cases where a better one
;;; is not readily available, and getting value comparison right is less important than
;;; performance.  In such a case, using `zero' as a hash function also makes sense.
(define-hash-function eql-compare zero)


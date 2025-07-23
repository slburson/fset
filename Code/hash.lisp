;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: hash.lisp
;;; Contents: Hashing function for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)

(defgeneric hash-value (x))

(define-hash-function compare hash-value)

;;; As of this writing, these methods are tentative.  They should be gone over for performance
;;; and hash quality (well-distributed-ness) in the major Lisp implementations.

(defmethod hash-value ((x identity-equality-mixin))
  ;; &&& Seems to work in SBCL
  (logxor (sxhash (class-of x)) (slot-value x 'serial-number)))

(defmethod hash-value ((x identity-ordering-mixin))
  ;; &&& Seems to work in SBCL
  (logxor (sxhash (class-of x)) (slot-value x 'serial-number)))

(defmethod hash-value ((x real))
  (sxhash x))

(defmethod hash-value ((x fixnum))
  ;; Seems reasonable for our purposes
  ;; &&& Oops -- should strip sign bit?
  x)

(defmethod hash-value ((x character))
  (sxhash x)) ; I guess

(defmethod hash-value ((x symbol))
  (sxhash x))

(defmethod hash-value ((x string))
  (sxhash x))

(defmethod hash-value ((x list))
  (logxor (hash-value (car x)) (logand most-positive-fixnum (* 11 (hash-value (cdr x))))))

(defmethod hash-value ((x vector))
  (if (simple-vector-p x)
      (do ((result 0 (logxor result (logand most-positive-fixnum (* mult (hash-value (svref x i))))))
	   (mult 1 (* mult 13))
	   (i 0 (1+ i))
	   (len (length x)))
	  ((= i len) result))
    (do ((result 0 (logxor result (logand most-positive-fixnum (* mult (hash-value (aref x i))))))
	 (mult 1 (* mult 13))
	 (i 0 (1+ i))
	 (len (length x)))
	((= i len) result))))


(defun zero (x)
  (declare (ignore x))
  0)

;;; `eql-compare' can be used as a value comparison function in cases where a better one
;;; is not readily available, and getting value comparison right is less important than
;;; performance.  In such a case, using `zero' as a hash function also makes sense.
(define-hash-function eql-compare zero)


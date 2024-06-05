;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: hash.lisp
;;; Contents: Hashing function for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(in-package :fset)

(defgeneric hash-value (x))

(defmethod hash-value ((x identity-ordering-mixin))
  ;; &&& Seems to work in SBCL
  (logxor (sxhash (class-of x)) (slot-value x 'serial-number)))

(defmethod hash-value ((x real))
  (sxhash x))

(defmethod hash-value ((x fixnum))
  ;; Seems reasonable for our purposes
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


;;; ================================================================================
;;; Sets

(defstruct (ch-set
	     (:include set)
	     (:constructor make-ch-set (contents))
	     (:predicate ch-set?)
	     (:print-function print-ch-set)
	     (:copier nil))
  contents)

(defparameter *empty-ch-set* (make-ch-set nil))


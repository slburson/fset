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

(defmacro define-comparison-slots (class &rest slots/accessors)
  "A handy macro for generating `compare' and `hash' methods for user classes.
The comparison or hashing is done by comparing or hashing the results of
calling each of `slots/accessors', in order, on the object(s).  For best
performance (at least on SBCL), it is recommended to supply slot names as
symbols for standard classes -- these will turn into `slot-value' forms --
but accessor names as functions (e.g. \"#'point-x\") for structure classes.
Arbitrary functions on the class may also be supplied.

If the symbol `:eql' is supplied as the last accessor, then if the comparisons
by the other supplied accessors all return `:equal' but `obj1' and `obj2' are
not eql, the generated `compare' method returns `:unequal'."
  `(progn
     (defmethod compare ((a ,class) (b ,class))
       (compare-slots a b . ,slots/accessors))
     (defmethod hash-value ((x ,class))
       (hash-slots x . ,(if (eq (last slots/accessors) ':eql) (butlast slots/accessors)
			  slots/accessors)))))

;;; &&& Add `(:cache slot/acc)' syntax for auto-caching
(defmacro hash-slots (obj &rest slots/accessors)
  (unless slots/accessors
    (error "At least one slot/accessor must be supplied"))
  (let ((x-var (gensym "X-")))
    (rlabels `(let ((,x-var ,obj))
		,(rec `(hash-value ,(call (car slots/accessors) x-var)) (cdr slots/accessors)))
      (rec (value accs)
	(if (null accs) value
	  (rec `(logxor (logand most-positive-fixnum (* 17 ,value))
			(hash-value ,(call (car accs) x-var)))
	       (cdr accs))))
      (call (fn arg)
	;; Makes the expansion more readable, if nothing else
	(cond ((and (listp fn)
		    (eq (car fn) 'function))
	       `(,(cadr fn) ,arg))
	      ((and (listp fn)
		    (eq (car fn) 'lambda))
	       `(,fn ,arg))
	      ((and (listp fn)
		    (eq (car fn) 'quote)
		    (symbolp (cadr fn)))
	       `(slot-value ,arg ,fn))
	      (t `(funcall ,fn ,arg)))))))


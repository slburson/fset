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


;;; ================================================================================
;;; Global constants

(defconstant champ-hash-bits-per-level 5)
(defconstant champ-node-radix (ash 1 champ-hash-bits-per-level))
(defconstant champ-hash-level-mask (1- champ-node-radix))


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


;;; ================================================================================
;;; Maps

(defstruct (ch-map
	     (:include map)
	     (:constructor make-ch-map (contents))
	     (:predicate ch-map?)
	     (:print-function print-ch-map)
	     (:copier nil))
  contents)

(defparameter *empty-ch-map* (make-ch-map nil))

(defstruct (ch-map-node
	     (:type vector))
  entry-mask
  subnode-mask
  hash-value)
(defconstant ch-map-node-header-size 3)

(defun ch-map-node-with (tree key key-hash value)
  (rlabels (rec tree key key-hash value 0)
    (rec (node key hash value depth)
      (let ((hash-bits (logand hash champ-hash-level-mask)))
	(if (null node)
	    (let ((n (make-array (+ 2 ch-map-node-header-size))))
	      (setf (ch-map-node-entry-mask n) (ash 1 hash-bits))
	      (setf (ch-map-node-subnode-mask n) 0)
	      (setf (ch-map-node-hash-value n) (logxor key-hash (hash-value value)))
	      (setf (svref n ch-map-node-header-size) key)
	      (setf (svref n (1+ ch-map-node-header-size)) value)
	      n)
	  (let ((entry-mask (ch-map-node-entry-mask node))
		((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		 ((entry-idx (+ ch-map-node-header-size (* 2 entry-raw-idx)))))
		(subnode-mask (ch-map-node-subnode-mask node))
		((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		 ((subnode-idx (- (length node) 1 subnode-raw-idx)))))
	    (if (logbitp hash-bits entry-mask)
		;; Entry found
		(let (((ex-key (svref node entry-idx))
		       (ex-val (svref node (1+ entry-idx)))))
		  (if (equal? key ex-key)
		      (if (equal? value ex-val)
			  node ;; Key found, value equal: nothing to do
			;; Key found, value differs: just update value
			(let ((n (vector-update node (+ entry-idx 1 ch-map-node-header-size) value)))
			  (setf (ch-map-node-hash-value n)
				(logxor (ch-map-node-hash-value node)
					(hash-value ex-val) (hash-value value)))
			  (setf (svref n (1+ entry-idx)) value)
			  n))
		    ;; Entry with different key found: make a subnode
		    (let ((ex-key-hash (ash (hash-value ex-key) (* (- champ-hash-bits-per-level) (1+ depth))))
			  (hash (ash hash (- champ-hash-bits-per-level)))
			  (n (make-array (1- (length node))))
			  ((n2 (if (= hash ex-key-hash)
				   ;; We're out of distinct hash bits.  Fall back to WB-trees.
				   (wb-map-tree-with (wb-map-tree-with nil ex-key ex-val) key value)
				 ;; Creates a little garbage; maybe hand-integrate later.
				 (rec (rec nil ex-key ex-key-hash ex-val (1+ depth))
				      key hash value (1+ depth))))))
		      (setf (ch-map-node-entry-mask n) (logand (ch-map-node-entry-mask node)
							       (lognot (ash 1 hash-bits))))
		      (assert (not (logbitp hash-bits (ch-map-node-subnode-mask node))))
		      (setf (ch-map-node-subnode-mask n) (logior (ch-map-node-subnode-mask node) (ash 1 hash-bits)))
		      (setf (ch-map-node-hash-value n) (logxor (ch-map-node-hash-value n) key-hash (hash-value value)))
		      ;; Copy up to entry being deleted
		      (dotimes (i (* 2 entry-raw-idx))
			(setf (svref n (+ i ch-map-node-header-size)) (svref node (+ i ch-map-node-header-size))))
		      ;; Copy from next entry up to subnode being inserted
		      (dotimes (i (- subnode-idx entry-idx 1))
			(setf (svref n (+ entry-idx i))
			      (svref node (+ entry-idx i 2))))
		      ;; Insert subnode (`subnode-idx' is where the subnode would have been in `node', had it
		      ;; been present; we subtract 2 for the deleted entry, but add 1 back because it's a new subnode)
		      (setf (svref n (1- subnode-idx)) n2)
		      ;; Copy remaining subnodes
		      (dotimes (i subnode-raw-idx)
			(setf (svref n (+ subnode-idx i))
			      (svref node (+ subnode-idx i 1))))
		      n)))
	      ;; No entry found: check for subnode
	      (if (logbitp hash-bits subnode-mask)
		  ;; Subnode found
		  (let ((subnode (svref node subnode-idx))
			((new-subnode (rec subnode key (ash hash (- champ-hash-bits-per-level)) value (1+ depth)))))
		    (if (eq new-subnode subnode)
			node ;; no change
		      ;; New subnode
		      (let ((n (vector-update node subnode-idx new-subnode)))
			(setf (ch-map-node-hash-value n)
			      (logxor (ch-map-node-hash-value node) (ch-map-node-hash-value subnode)
				      (ch-map-node-hash-value new-subnode)))
			n)))
		;; Neither entry nor subnode found: make new entry
		(let ((n (make-array (+ 2 (length node)))))
		  (dotimes (i entry-idx) ; includes header
		    (setf (svref n i) (svref node i)))
		  (setf (ch-map-node-entry-mask n) (logior (ch-map-node-entry-mask node) (ash 1 hash-bits)))
		  (setf (ch-map-node-hash-value n)
			(logxor (ch-map-node-hash-value node) key-hash (hash-value value)))
		  (setf (svref n entry-idx) key)
		  (setf (svref n (1+ entry-idx)) value)
		  (dotimes (i (- (length node) entry-idx))
		    (setf (svref n (+ entry-idx i 2)) (svref node (+ entry-idx i))))
		  n)))))))))

#|| For testing...
(defmethod hash-value ((x my-integer))
  (logand (my-integer-value x) 127))
||#

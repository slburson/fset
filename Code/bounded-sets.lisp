;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: bounded-sets.lisp
;;; Contents: Bounded sets (subsets of a specified set)
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; "Bounded" is certainly not an ideal term, but I couldn't find anything better
;;; in Wikipedia's pages on topology.  "Set-in-discrete-topology" is just too long.
(defstruct (bounded-set
	     (:constructor make-bounded-set-internal (universe bitmap))
	     (:predicate bounded-set?)
	     (:print-function print-bounded-set)
	     (:copier nil))
  "A \"bounded set\" is a subset (not necessarily proper) of a specified set,
called the \"universe\".  (Topologically, it is a set in the discrete topology
on the universe.)"
  (universe nil :read-only t)
  (bitmap 0 :read-only t))

(defun next-1-bit (n start-bit)
  "The bit index of the first 1 bit of `n' where the index is at least `start-bit'.
Null if there is no such bit.  `n' may be a bignum."
  (declare (optimize (speed 3) (safety 0))
	   (type unsigned-byte n)
	   (type (and fixnum unsigned-byte) start-bit))
  (if (typep n 'fixnum)
      (let ((n (logandc2 n (1- (the fixnum (ash 1 start-bit))))))
	(and (plusp n) (least-1-bit n)))
    ;; We scan the bignum in chunks, word-sized on 64-bit SBCL, fixnum-sized otherwise.
    (let ((chunk-len #+(and sbcl 64-bit) 64
		     #-(and sbcl 64-bit) (integer-length most-positive-fixnum))
	  (n-len (gen integer-length n))
	  ((start-chunk start-bit (floor start-bit chunk-len))))
      (do ((chunk-base (* start-chunk chunk-len) (+ chunk-base chunk-len))
	   (start-bit start-bit 0))
	  ((> chunk-base n-len) nil)
	(let ((cur-chunk (ldb (byte (- chunk-len start-bit) (+ chunk-base start-bit)) n)))
	  (when (plusp cur-chunk)
	    (return (+ chunk-base start-bit (least-1-bit cur-chunk)))))))))

(defmacro do-bit-indices-gen ((idx-var val &optional result) &body body)
  (let ((val-var (gensymx #:val-))
	(start-bit-var (gensymx #:start-bit-)))
    `(do ((,val-var ,val)
	  (,start-bit-var 0))
	 (nil)
       (declare (fixnum ,start-bit-var))
       (let ((,idx-var (next-1-bit ,val-var ,start-bit-var)))
	 (declare (type (or null fixnum) ,idx-var))
	 (unless ,idx-var
	   (return ,result))
	 (setq ,start-bit-var (1+ ,idx-var))
	 . ,body))))

(gmap:def-arg-type bit-indices (n)
  (let ((n-var (gensymx #:n-))
	(last-bit-var (gensymx #:last-bit-)))
    `(nil  ; unused; we have to do the computation in the predicate
       (fn (_x) (let ((next (next-1-bit ,n-var (1+ ,last-bit-var))))
		  (null (and next (setq ,last-bit-var next)))))
       (fn (_x) ,last-bit-var)
       nil
       ((,n-var ,n)
	 (,last-bit-var -1))
       nil
       (do-bit-indices-gen ,n)
       nil)))

(defun make-bounded-set (universe set)
  (unless (subset? set universe)
    (error "Attempt to create a bounded-set whose set is not a subset of its universe"))
  (make-bounded-set-internal universe (gmap (:result sum) (fn (x) (ash 1 (index universe x)))
					    (:arg set set))))

(defun bounded-set-contents (bs)
  (let ((u (bounded-set-universe bs)))
    (if (wb-set? u)
	(gmap (:result wb-set) (fn (i) (at-index u i)) (:arg bit-indices (bounded-set-bitmap bs)))
      (gmap (:result ch-set) (fn (i) (at-index u i)) (:arg bit-indices (bounded-set-bitmap bs))))))

(defmethod complement ((bs bounded-set))
  (make-bounded-set-internal (bounded-set-universe bs)
			     (logxor (1- (ash 1 (size (bounded-set-universe bs))))
				     (bounded-set-bitmap bs))))

(defmethod empty? ((bs bounded-set))
  (zerop (bounded-set-bitmap bs)))

(defmethod contains? ((bs bounded-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'contains? 'bounded-set)
  (let ((idx (index (bounded-set-universe bs) x)))
    (unless idx
      (error "NIU: `contains?' on a `bounded-set' called on a value not in its universe"))
    (logbitp idx (bounded-set-bitmap bs))))

(defmethod arb ((bs bounded-set))
  (if (zerop (bounded-set-bitmap bs))
      (values nil nil)
    (values (at-index (bounded-set-universe bs) (next-1-bit (bounded-set-bitmap bs) 0)) t)))

(defmethod size ((bs bounded-set))
  (logcount (bounded-set-bitmap bs)))

(defmethod with ((bs bounded-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'bounded-set)
  (let ((idx (index (bounded-set-universe bs) x)))
    (unless idx
      (error "NIU: `with' on a `bounded-set' called on a value not in its universe"))
    (make-bounded-set-internal (bounded-set-universe bs) (logior (bounded-set-bitmap bs) (ash 1 idx)))))

(defmethod less ((bs bounded-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'bounded-set)
  (let ((idx (index (bounded-set-universe bs) x)))
    (unless idx
      (error "NIU: `less' on a `bounded-set' called on a value not in its universe"))
    (make-bounded-set-internal (bounded-set-universe bs) (logandc2 (bounded-set-bitmap bs) (ash 1 idx)))))

(defmethod union ((bs1 bounded-set) (bs2 bounded-set) &key)
  (let ((u1 (bounded-set-universe bs1))
	(u2 (bounded-set-universe bs2)))
    (unless (equal? u1 u2)
      (error "Can't take the union of two bounded-sets with different universes"))
    (make-bounded-set-internal u1 (logior (bounded-set-bitmap bs1) (bounded-set-bitmap bs2)))))

(defmethod intersection ((bs1 bounded-set) (bs2 bounded-set) &key)
  (let ((u1 (bounded-set-universe bs1))
	(u2 (bounded-set-universe bs2)))
    (unless (equal? u1 u2)
      (error "Can't take the intersection of two bounded-sets with different universes"))
    (make-bounded-set-internal u1
			       (logand (bounded-set-bitmap bs1) (bounded-set-bitmap bs2)))))

(defmethod set-difference ((bs1 bounded-set) (bs2 bounded-set) &key)
  (let ((u1 (bounded-set-universe bs1))
	(u2 (bounded-set-universe bs2)))
    (unless (equal? u1 u2)
      (error "Can't take the set-difference of two bounded-sets with different universes"))
    (make-bounded-set-internal u1
			       (logandc2 (bounded-set-bitmap bs1) (bounded-set-bitmap bs2)))))

(defmethod subset? ((bs1 bounded-set) (bs2 bounded-set))
  (let ((u1 (bounded-set-universe bs1))
	(u2 (bounded-set-universe bs2)))
    (unless (equal? u1 u2)
      (error "Can't do `subset?' on two bounded-sets with different universes"))
    (zerop (logandc2 (bounded-set-bitmap bs1) (bounded-set-bitmap bs2)))))

(defmethod disjoint? ((bs1 bounded-set) (bs2 bounded-set))
  (let ((u1 (bounded-set-universe bs1))
	(u2 (bounded-set-universe bs2)))
    (unless (equal? u1 u2)
      (error "Can't do `disjoint?' on two bounded-sets with different universes"))
    (zerop (logand (bounded-set-bitmap bs1) (bounded-set-bitmap bs2)))))

(defmethod internal-do-set ((bs bounded-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-bit-indices-gen (idx (bounded-set-bitmap bs) (funcall value-fn))
    (funcall elt-fn (at-index (bounded-set-universe bs) idx))))

(defun print-bounded-set (bs stream level)
  (declare (ignore level))
  ;; I'm a lazy sod.  This is not readable since it doesn't include the universe.
  (format stream "#/")
  (write (bounded-set-contents bs) :stream stream))

(defmethod compare ((bs1 bounded-set) (bs2 bounded-set))
  ;; We don't constrain the bounded-sets to have the same universes, since the
  ;; FSet way is to let you mix absolutely any objects in sets.  However, if
  ;; the universes are different, we don't compare the contents.
  (let ((uni-comp (compare (bounded-set-universe bs1) (bounded-set-universe bs2))))
    (if (member uni-comp '(:less :greater))
	uni-comp
      (let ((bits-comp (compare (bounded-set-bitmap bs1) (bounded-set-bitmap bs2))))
	(if (member bits-comp '(:less :greater))
	    bits-comp
	  uni-comp)))))  ; in case it's `:unequal'

(define-convert-methods (set fset2:set) ((bs bounded-set) &key)
  (bounded-set-contents bs))


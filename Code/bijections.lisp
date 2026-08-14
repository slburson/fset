;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: bijections.lisp
;;; Contents: Bijections (unique maps)
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


(defstruct (bijection
	     (:include collection)
	     (:constructor nil)
	     (:predicate bijection?)
	     (:copier nil))
  "The abstract class for FSet bijections.  It is a structure class.  A bijection
is a one-to-one mapping, i.e., where both the keys and values are required to be
unique; an error is signalled if this constraint is violated.  Also, fast inverse
lookups are supported with `lookup-inv'.  A bijection cannot have a default, so
out-of-domain lookups always signal an error.")


(defstruct (ch-bijection
	     (:include bijection)
	     (:constructor make-ch-bijection (map0 map1 org))
	     (:predicate ch-bijection?)
	     (:print-function print-ch-bijection)
	     (:copier nil))
  (map0 nil :read-only t)
  (map1 nil :read-only t)
  (org nil :type hash-map-org :read-only t))

(deflex +empty-ch-bijection+ (make-ch-bijection nil nil +fset-default-hash-map-org+))

(declaim (inline empty-bijection))
(defun empty-bijection ()
  +empty-ch-bijection+)

(declaim (inline empty-ch-bijection))
(defun empty-ch-bijection (&key key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      +empty-ch-bijection+
    (empty-ch-custom-bijection (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))

(deflex +empty-ch-custom-bijection-cache+ (make-hash-table :test 'equal))

(defun empty-ch-custom-bijection (key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      +empty-ch-bijection+
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-ch-custom-bijection-cache+)))
	  (key-hash-fn-name (or (get key-compare-fn-name 'hash-function)
				(error "key-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       key-compare-fn-name)))
	  (val-hash-fn-name (or (get val-compare-fn-name 'hash-function)
				(error "val-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       val-compare-fn-name)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name))
	  ((key-hash-fn (symbol-function key-hash-fn-name))
	   (val-hash-fn (symbol-function val-hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-org (ch-bijection-org prev-instance)))
		 (and (eq key-compare-fn (hash-map-org-key-compare-fn prev-org))
		      (eq key-hash-fn (hash-map-org-key-hash-fn prev-org))
		      (eq val-compare-fn (hash-map-org-val-compare-fn prev-org))
		      (eq val-hash-fn (hash-map-org-val-hash-fn prev-org)))))
	  prev-instance
	(setf (gethash cache-key +empty-ch-custom-bijection-cache+)
	      (make-ch-bijection 0 nil (make-hash-map-org key-compare-fn-name key-compare-fn key-hash-fn
							  val-compare-fn-name val-compare-fn val-hash-fn)))))))


(defmethod key-compare-fn-name ((b ch-bijection))
  (hash-map-org-key-compare-fn-name (ch-bijection-org b)))

(defmethod val-compare-fn-name ((b ch-bijection))
  (hash-map-org-val-compare-fn-name (ch-bijection-org b)))

(defmethod empty? ((b ch-bijection))
  (null (ch-bijection-map0 b)))

(defmethod size ((b ch-bijection))
  (ch-map-tree-size (ch-bijection-map0 b)))

(defmethod arb ((b ch-bijection))
  (let ((tree (ch-bijection-map0 b)))
    (if tree
	(let ((key val (ch-map-tree-arb-pair tree)))
	  (values key (ch-set-tree-arb val) t))
      (values nil nil nil))))

(defmethod contains? ((b ch-bijection) key &optional (val nil val?))
  (let ((hmorg (ch-bijection-org b))
	((b-val? b-val (ch-map-tree-lookup (ch-bijection-map0 b) key
					   (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    (if val? (and b-val? (equal?-cmp b-val val (hash-map-org-val-compare-fn hmorg)))
      b-val?)))

(defmethod domain-contains? ((b ch-bijection) key)
  (let ((org (ch-bijection-org b)))
    (ch-map-tree-lookup (ch-bijection-map0 b) key
			(hash-map-org-key-hash-fn org) (hash-map-org-key-compare-fn org))))

(defmethod range-contains? ((b ch-bijection) val)
  (let ((org (ch-bijection-org b)))
    (ch-map-tree-lookup (ch-bijection-map1 b) val
			(hash-map-org-key-hash-fn org) (hash-map-org-key-compare-fn org))))

(define-condition bijection-constraint-violation (error)
    ((bijection :initarg :bijection :reader bijection-constraint-violation-bijection)
     (key :initarg :key :reader bijection-constraint-violation-key)
     (value :initarg :value :reader bijection-constraint-violation-value)
     (prev-key :initarg :prev-key :reader bijection-constraint-violation-prev-key))
  (:report (lambda (bcv stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Attempt to add mapping from ~S to ~S in ~A,~@
			       but that value was already mapped from ~S"
		       (bijection-constraint-violation-key bcv)
		       (bijection-constraint-violation-value bcv)
		       (bijection-constraint-violation-bijection bcv)
		       (bijection-constraint-violation-prev-key bcv))))))

(defmethod with ((b ch-bijection) key &optional (val nil val?))
  (check-three-arguments val? 'with 'ch-bijection)
  (let ((org (ch-bijection-org b))
	((map0-hash-fn (hash-map-org-key-hash-fn org))
	 (map0-cmp-fn (hash-map-org-key-compare-fn org))
	 (map1-hash-fn (hash-map-org-val-hash-fn org))
	 (map1-cmp-fn (hash-map-org-val-compare-fn org))
	 ((prev? prev-key (ch-map-tree-lookup (ch-bijection-map1 b) val map1-hash-fn map1-cmp-fn)))))
    (if prev?
	(if (equal?-cmp prev-key key map0-cmp-fn)
	    b
	  (error 'bijection-constraint-violation :bijection b :key key :value val :prev-key prev-key))
      (make-ch-bijection (ch-map-tree-with (ch-bijection-map0 b) key val
					   map0-hash-fn map0-cmp-fn map1-hash-fn map1-cmp-fn)
			 (ch-map-tree-with (ch-bijection-map1 b) val key
					   map1-hash-fn map1-cmp-fn map0-hash-fn map0-cmp-fn)
			 org))))

(defmethod less ((b ch-bijection) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-bijection)
  (let ((org (ch-bijection-org b))
	((map0-hash-fn (hash-map-org-key-hash-fn org))
	 (map0-cmp-fn (hash-map-org-key-compare-fn org))
	 (map1-hash-fn (hash-map-org-val-hash-fn org))
	 (map1-cmp-fn (hash-map-org-val-compare-fn org))
	 ((prev? prev-val (ch-map-tree-lookup (ch-bijection-map0 b) key map0-hash-fn map0-cmp-fn)))))
    (if prev?
	(make-ch-bijection (ch-map-tree-less (ch-bijection-map0 b) key map0-hash-fn map0-cmp-fn map1-hash-fn)
			   (ch-map-tree-less (ch-bijection-map1 b) prev-val map1-hash-fn map1-cmp-fn map0-hash-fn)
			   org)
      b)))

(define-condition bijection-domain-error (fset2:lookup-error)
    ((bijection :initarg :map :reader bijection-domain-error-bijection)
     (key :initarg :key :reader bijection-domain-error-key))
  (:report (lambda (bde stream)
	     (let ((*print-length* 8)
		   (*print-level* 3))
	       (format stream "Key ~S not found in bijection ~A"
		       (bijection-domain-error-key bde) (bijection-domain-error-bijection bde))))))

(define-methods (lookup fset2:lookup) ((b ch-bijection) key)
  (let ((org (ch-bijection-org b))
	((val? val bkey (ch-map-tree-lookup (ch-bijection-map0 b) key
					    (hash-map-org-key-hash-fn org) (hash-map-org-key-compare-fn org)))))
    (if val? (values val t bkey)
      (error 'bijection-domain-error :bijection b :key key))))

(defmethod lookup-inv ((b ch-bijection) val)
  (let ((org (ch-bijection-org b))
	((key? key bval (ch-map-tree-lookup (ch-bijection-map1 b) val
					    (hash-map-org-val-hash-fn org) (hash-map-org-val-compare-fn org)))))
    (if key? (values key t bval)
      (error 'bijection-domain-error :bijection (inverse b) :key val))))

(defmethod inverse ((b ch-bijection))
  (make-ch-bijection (ch-bijection-map1 b) (ch-bijection-map0 b) (hash-map-org-inverse (ch-bijection-org b))))

(define-convert-methods (map fset2:map) ((b ch-bijection) &key)
  (make-ch-map (ch-bijection-map0 b) (ch-bijection-org b) 'no-default))

(define-convert-methods (ch-map fset2:ch-map) ((b ch-bijection) &key key-compare-fn-name val-compare-fn-name default)
  (convert 'ch-map (make-ch-map (ch-bijection-map0 b) (ch-bijection-org b) 'no-default)
	   :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name :default default))

(defmethod convert ((to-type (eql 'bijection)) (m map) &key)
  (let ((map0 nil)
	(map1 nil)
	(org +fset-default-hash-map-org+)
	((map0-hash-fn (hash-map-org-key-hash-fn org))
	 (map0-cmp-fn (hash-map-org-key-compare-fn org))
	 (map1-hash-fn (hash-map-org-val-hash-fn org))
	 (map1-cmp-fn (hash-map-org-val-compare-fn org)))
	(transient-id (get-next-transient-id)))
    (do-map (key val m)
      (let ((prev? prev-key (ch-map-tree-lookup map1 val map1-hash-fn map1-cmp-fn)))
	(when prev?
	  (error 'bijection-constraint-violation :bijection (make-ch-bijection map0 map1 org)
						 :key key :value val :prev-key prev-key)))
      (setq map0 (ch-map-tree-with map0 key val map0-hash-fn map0-cmp-fn map1-hash-fn map1-cmp-fn transient-id))
      (setq map1 (ch-map-tree-with map1 val key map1-hash-fn map1-cmp-fn map0-hash-fn map0-cmp-fn transient-id)))
    (make-ch-bijection map0 map1 +fset-default-hash-map-org+)))

(defmethod convert ((to-type (eql 'ch-bijection)) (m map) &key key-compare-fn-name val-compare-fn-name)
  (let ((map0 nil)
	(map1 nil)
	(org (ch-map-org (empty-ch-map nil key-compare-fn-name val-compare-fn-name)))
	((map0-hash-fn (hash-map-org-key-hash-fn org))
	 (map0-cmp-fn (hash-map-org-key-compare-fn org))
	 (map1-hash-fn (hash-map-org-val-hash-fn org))
	 (map1-cmp-fn (hash-map-org-val-compare-fn org)))
	(transient-id (get-next-transient-id)))
    (do-map (key val m)
      (let ((prev? prev-key (ch-map-tree-lookup map1 val map1-hash-fn map1-cmp-fn)))
	(when prev?
	  (error 'bijection-constraint-violation :bijection (make-ch-bijection map0 map1 +fset-default-hash-map-org+)
						 :key key :value val :prev-key prev-key)))
      (setq map0 (ch-map-tree-with map0 key val map0-hash-fn map0-cmp-fn map1-hash-fn map1-cmp-fn transient-id))
      (setq map1 (ch-map-tree-with map1 val key map1-hash-fn map1-cmp-fn map0-hash-fn map0-cmp-fn transient-id)))
    (make-ch-bijection map0 map1 +fset-default-hash-map-org+)))

(defun print-ch-bijection (b stream level)
  (declare (ignore level))
  (pprint-logical-block
      (stream nil :prefix "##{||"
		  :suffix (let ((hmorg (ch-bijection-org b))
				((key-cf-name (hash-map-org-key-compare-fn-name hmorg))
				 (val-cf-name (hash-map-org-val-compare-fn-name hmorg))
				 ((key-default? (eq key-cf-name 'compare))
				  (val-default? (eq val-cf-name 'compare)))))
			    (format nil " ||}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]"
				    (and key-default? val-default?)
				    key-default? key-cf-name val-default? val-cf-name)))
    (do-ch-map-tree-pairs (x y (ch-bijection-map0 b))
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline ':fill stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))

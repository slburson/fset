;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: relations.lisp
;;; Contents: Relations (binary and general).
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


(defstruct (relation
	    (:include collection)
	    (:constructor nil)
	    (:predicate relation?)
	    (:copier nil))
  "The abstract class for FSet relations.  It is a structure class.")

(defgeneric arity (rel)
  (:documentation "Returns the arity of the relation `rel'."))

(defstruct (2-relation
	    (:include relation)
	    (:constructor nil)
	    (:predicate 2-relation?)
	    (:copier nil))
  "The abstract class for FSet binary relations.  It is a structure class.")

(defmethod arity ((br 2-relation))
  2)


;;; ================
;;; WB-2-relation

(defstruct (wb-2-relation
	    (:include 2-relation)
	    (:constructor make-wb-2-relation (size map0 map1 org))
	    (:predicate wb-2-relation?)
	    (:print-function print-wb-2-relation)
	    (:copier nil))
  "A class of functional binary relations represented as pairs of weight-
balanced binary trees.  This is the default implementation of binary relations
in FSet.  The inverse is constructed lazily, and maintained incrementally once
constructed."
  (size 0 :type integer :read-only t)
  (map0 nil :read-only t)
  (map1 nil) ; a cache, so we leave it mutable
  (org nil :type tree-map-org :read-only t))

(defparameter *empty-wb-2-relation* (make-wb-2-relation 0 nil nil (wb-map-org *empty-wb-map*)))

(declaim (inline empty-2-relation))
(defun empty-2-relation ()
  *empty-wb-2-relation*)

(defmethod empty-instance-function ((class-name (eql '2-relation)))
  'empty-2-relation)

(declaim (inline empty-wb-2-relation))
(defun empty-wb-2-relation (&optional key-compare-fn-name val-compare-fn-name)
  "In this case, \"keys\" are the left values, and \"values\" are the right
values."
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      *empty-wb-2-relation*
    (empty-wb-custom-2-relation (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))

(deflex +empty-wb-custom-2-relation-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-2-relation (key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      *empty-wb-2-relation*
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-wb-custom-2-relation-cache+)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name)))
      (if (and prev-instance
	       (let ((prev-org (wb-2-relation-org prev-instance)))
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn prev-org))
		      (eq val-compare-fn (tree-map-org-val-compare-fn prev-org)))))
	  prev-instance
	(setf (gethash cache-key +empty-wb-custom-2-relation-cache+)
	      (make-wb-2-relation 0 nil nil (make-tree-map-org key-compare-fn-name key-compare-fn
							       val-compare-fn-name val-compare-fn)))))))

(defmethod empty-instance-function ((class-name (eql 'wb-2-relation)))
  'empty-wb-2-relation)

(defmethod empty-relation-like ((br wb-2-relation))
  (let ((org (wb-2-relation-org br)))
    (empty-wb-2-relation (tree-map-org-key-compare-fn-name org) (tree-map-org-val-compare-fn-name org))))

(defmethod empty? ((br wb-2-relation))
  (zerop (wb-2-relation-size br)))

(defmethod size ((br wb-2-relation))
  (wb-2-relation-size br))

(defmethod arb ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Arb-Pair tree)))
	  (values key (WB-Set-Tree-Arb val) t))
      (values nil nil nil))))

(defmethod contains? ((br wb-2-relation) x &optional (y nil y?))
  (check-three-arguments y? 'contains? 'wb-2-relation)
  (let ((org (wb-2-relation-org br))
	((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x (tree-map-org-key-compare-fn org)))))
    (and found? (WB-Set-Tree-Contains? set-tree y (tree-map-org-val-compare-fn org)))))

;;; &&& Aaagh -- not sure this makes sense -- (setf (lookup rel x) ...) doesn't do
;;; the right thing at all, relative to this.  Maybe the setf expander for `lookup'/`@'
;;; should call an internal form of `with' that does something different on a
;;; relation...
;;; [Later] No, I don't think this is a problem.  (setf (lookup ...) ...) just doesn't
;;; make sense on a relation, any more than it does on a set.
(defmethod lookup ((br wb-2-relation) x)
  "Returns the set of values that the relation pairs `x' with."
  (let ((org (wb-2-relation-org br))
	((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x (tree-map-org-key-compare-fn org)))))
    ;; We don't go through `empty-wb-set' here, because that would retrieve the current
    ;; `symbol-function' of the `key-compare-fn-name', which might have been changed since
    ;; the relation was created.
    (if found? (make-wb-set set-tree (make-tree-set-org (tree-map-org-val-compare-fn-name org)
							(tree-map-org-val-compare-fn org)))
      *empty-wb-set*)))

(defgeneric lookup-inv (2-relation y)
  (:documentation "Does an inverse lookup on a binary relation."))

(defmethod lookup-inv ((br wb-2-relation) y)
  (get-inverse br)
  (let ((org (wb-2-relation-org br))
	((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map1 br) y (tree-map-org-val-compare-fn org)))))
    (if found? (make-wb-set set-tree (make-tree-set-org (tree-map-org-key-compare-fn-name org)
							(tree-map-org-key-compare-fn org)))
      *empty-wb-set*)))

(defmethod domain ((br wb-2-relation))
  (let ((org (wb-2-relation-org br)))
    (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map0 br))
		 (make-tree-set-org (tree-map-org-key-compare-fn-name org)
				    (tree-map-org-key-compare-fn org)))))

(defmethod range ((br wb-2-relation))
  (get-inverse br)
  (let ((org (wb-2-relation-org br)))
    (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map1 br))
		 (make-tree-set-org (tree-map-org-val-compare-fn-name org)
				    (tree-map-org-val-compare-fn org)))))

(defmethod at-rank ((br wb-2-relation) rank)
  (let ((tree (wb-2-relation-map0 br))
	((size (WB-Map-Tree-Size tree))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank br)))
    (let ((key set-tree (WB-Map-Tree-Rank-Pair tree rank)))
      (values key (make-wb-set set-tree)))))

(defun get-inverse (br)
  ;; Make sure this thread sees a fully initialized map tree, if some other thread has just
  ;; created it.  Not certain this is necessary and it seems like it might be a bit expensive,
  ;; but for safety I'm leaving it in.
  (read-memory-barrier)
  (let ((m0 (wb-2-relation-map0 br))
	(m1 (wb-2-relation-map1 br))
	(org (wb-2-relation-org br)))
    (when (and m0 (null m1))
      (Do-WB-Map-Tree-Pairs (x s m0)
	(Do-WB-Set-Tree-Members (y s)
	  (let ((ignore prev (WB-Map-Tree-Lookup m1 y (tree-map-org-val-compare-fn org))))
	    (declare (ignore ignore))
	    ;; The `val-cmp-fn' for `WB-Map-Tree-With' is comparing set trees, not elements.
	    ;; `eql-compare' suffices for this (actually, in this case, we could use `(constantly ':unequal)',
	    ;; since `x' values are unique).
	    (setq m1 (WB-Map-Tree-With m1 y (WB-Set-Tree-With prev x (tree-map-org-key-compare-fn org))
				       (tree-map-org-val-compare-fn org) #'eql-compare)))))
      ;; Make sure other threads see a fully initialized map tree.
      (write-memory-barrier)
      (setf (wb-2-relation-map1 br) m1))
    m1))

(defgeneric inverse (2-relation)
  (:documentation "The inverse of a binary relation."))

;;; This is so fast (once the inverse is constructed) we almost don't need
;;; `lookup-inv'.  Maybe we should just put a compiler optimizer on
;;; `(lookup (inverse ...) ...)'?
(defmethod inverse ((br wb-2-relation))
  (get-inverse br)
  (make-wb-2-relation (wb-2-relation-size br) (wb-2-relation-map1 br)
		      (wb-2-relation-map0 br)
		      (let ((org (wb-2-relation-org br)))
			(make-tree-map-org (tree-map-org-val-compare-fn-name org) (tree-map-org-val-compare-fn org)
					   (tree-map-org-key-compare-fn-name org) (tree-map-org-key-compare-fn org)))))

(defmethod least ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key vals (WB-Map-Tree-Least-Pair tree))
	      (org (wb-2-relation-org br)))
	  (values key (make-wb-set vals (make-tree-set-org (tree-map-org-val-compare-fn-name org)
							   (tree-map-org-val-compare-fn org)))
		  t))
      (values nil nil nil))))

(defmethod greatest ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key vals (WB-Map-Tree-Greatest-Pair tree))
	      (org (wb-2-relation-org br)))
	  (values key (make-wb-set vals (make-tree-set-org (tree-map-org-val-compare-fn-name org)
							   (tree-map-org-val-compare-fn org)))
		  t))
      (values nil nil nil))))

(defmethod with ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((org (wb-2-relation-org br))
	((map0-cmp-fn (tree-map-org-key-compare-fn org))
	 (map1-cmp-fn (tree-map-org-val-compare-fn org))
	 ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x map0-cmp-fn))))
	(map1 (wb-2-relation-map1 br)))
    (if found?
	(let ((new-set-tree (WB-Set-Tree-With set-tree y map1-cmp-fn)))
	  (if (eq new-set-tree set-tree)
	      br			; `y' was already there
	    (make-wb-2-relation (1+ (wb-2-relation-size br))
				(WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree map0-cmp-fn #'eql-compare)
				(and map1
				     (let ((ignore set-tree-1 (WB-Map-Tree-Lookup map1 y map1-cmp-fn)))
				       (declare (ignore ignore))
				       (WB-Map-Tree-With map1 y (WB-Set-Tree-With set-tree-1 x map0-cmp-fn)
							 map1-cmp-fn #'eql-compare)))
				org)))
      (make-wb-2-relation (1+ (wb-2-relation-size br))
			  (WB-Map-Tree-With (wb-2-relation-map0 br) x (WB-Set-Tree-With nil y map1-cmp-fn)
					    map0-cmp-fn #'eql-compare)
			  (and map1
			       (let ((ignore set-tree-1 (WB-Map-Tree-Lookup map1 y map1-cmp-fn)))
				 (declare (ignore ignore))
				 (WB-Map-Tree-With map1 y (WB-Set-Tree-With set-tree-1 x map0-cmp-fn)
						   map1-cmp-fn #'eql-compare)))
			  org))))

(defmethod less ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((org (wb-2-relation-org br))
	((map0-cmp-fn (tree-map-org-key-compare-fn org))
	 (map1-cmp-fn (tree-map-org-val-compare-fn org))
	 ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x map0-cmp-fn))))
	(map1 (wb-2-relation-map1 br)))
    (if (not found?)
	br
      (let ((new-set-tree (WB-Set-Tree-Less set-tree y map1-cmp-fn)))
	(if (eq new-set-tree set-tree)
	    br
	  (make-wb-2-relation (1- (wb-2-relation-size br))
			      (if new-set-tree
				  (WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree map0-cmp-fn #'eql-compare)
				(WB-Map-Tree-Less (wb-2-relation-map0 br) x map0-cmp-fn))
			      (and map1
				   (let ((ignore set-tree (WB-Map-Tree-Lookup map1 y map1-cmp-fn))
					 ((new-set-tree (WB-Set-Tree-Less set-tree x map0-cmp-fn))))
				     (declare (ignore ignore))
				     (if new-set-tree
					 (WB-Map-Tree-With map1 y new-set-tree map1-cmp-fn #'eql-compare)
				       (WB-Map-Tree-Less map1 y map1-cmp-fn))))
			      org))))))

(defmethod union ((br1 2-relation) (br2 2-relation) &key)
  "Fallback method for mixed implementations."
  (let ((result br1))
    (do-2-relation (x y br2)
      (setq result (with result x y)))
    result))

(defmethod union ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (if-same-wb-2-relation-orgs (br1 br2 org)
      (let ((new-size (+ (wb-2-relation-size br1) (wb-2-relation-size br2)))
	    (map0-cmp-fn (tree-map-org-key-compare-fn org))
	    (map1-cmp-fn (tree-map-org-val-compare-fn org))
	    ((new-map0 (WB-Map-Tree-Union (wb-2-relation-map0 br1) (wb-2-relation-map0 br2)
					  (lambda (s1 s2)
					    (let ((s (WB-Set-Tree-Union s1 s2 map1-cmp-fn)))
					      (decf new-size
						    (- (+ (WB-Set-Tree-Size s1) (WB-Set-Tree-Size s2))
						       (WB-Set-Tree-Size s)))
					      s))
					  map0-cmp-fn))
	     (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			    (progn
			      (get-inverse br1)
			      (get-inverse br2)
			      (WB-Map-Tree-Union (wb-2-relation-map1 br1) (wb-2-relation-map1 br2)
						 (fn (a b) (WB-Set-Tree-Union a b map0-cmp-fn)) map1-cmp-fn))))))
	(make-wb-2-relation new-size new-map0 new-map1 org))
    (call-next-method)))

(defmethod intersection ((br1 2-relation) (br2 2-relation) &key)
  "Fallback method for mixed implementations."
  (let ((result (empty-relation-like br1)))
    (do-2-relation (x y br1)
      (when (contains? br2 x y)
	(includef result x y)))
    result))

(defmethod intersection ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (if-same-wb-2-relation-orgs (br2 br2 org)
      (let ((new-size 0)
	    (map0-cmp-fn (tree-map-org-key-compare-fn org))
	    (map1-cmp-fn (tree-map-org-val-compare-fn org))
	    ((new-map0 (WB-Map-Tree-Intersect (wb-2-relation-map0 br1)
					      (wb-2-relation-map0 br2)
					      (lambda (s1 s2)
						(let ((s (WB-Set-Tree-Intersect s1 s2 map1-cmp-fn)))
						  (incf new-size (WB-Set-Tree-Size s))
						  (values s (and (null s) ':no-value))))
					      map0-cmp-fn))
	     (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			    (progn
			      (get-inverse br1)
			      (get-inverse br2)
			      (WB-Map-Tree-Intersect (wb-2-relation-map1 br1) (wb-2-relation-map1 br2)
						     (lambda (s1 s2)
						       (let ((s (WB-Set-Tree-Intersect s1 s2 map0-cmp-fn)))
							 (values s (and (null s) ':no-value))))
						     map1-cmp-fn))))))
	(make-wb-2-relation new-size new-map0 new-map1 org))
    (call-next-method)))

(defgeneric join (relation-a column-a relation-b column-b)
  (:documentation
    "A relational equijoin, matching up `column-a' of `relation-a' with `column-b' of
`relation-b'.  For a binary relation, the columns are named 0 (domain) and 1 (range)."))

(defmethod join ((bra wb-2-relation) cola (brb wb-2-relation) colb)
  (let ((org-a (wb-2-relation-org bra))
	(org-b (wb-2-relation-org brb))
	((map0a map1a cmp-fn-0a cmp-fn-1a cmp-fn-0a-name
	   (ecase cola
	     (1 (values (wb-2-relation-map0 bra) (wb-2-relation-map1 bra)
			(tree-map-org-key-compare-fn org-a) (tree-map-org-val-compare-fn org-a)
			(tree-map-org-key-compare-fn-name org-a)))
	     (0 (progn
		  (get-inverse bra)
		  (values (wb-2-relation-map1 bra) (wb-2-relation-map0 bra)
			  (tree-map-org-val-compare-fn org-a) (tree-map-org-key-compare-fn org-a)
			  (tree-map-org-val-compare-fn-name org-a))))))
	 (map0b map1b cmp-fn-0b cmp-fn-1b cmp-fn-1b-name
	   (ecase colb
	     (0 (values (wb-2-relation-map0 brb) (wb-2-relation-map1 brb)
			(tree-map-org-key-compare-fn org-b) (tree-map-org-val-compare-fn org-b)
			(tree-map-org-val-compare-fn-name org-b)))
	     (1 (progn
		  (get-inverse brb)
		  (values (wb-2-relation-map1 brb) (wb-2-relation-map0 brb)
			  (tree-map-org-val-compare-fn org-b) (tree-map-org-key-compare-fn org-b)
			  (tree-map-org-key-compare-fn-name org-b)))))))
	(new-map0 nil)
	(new-map1 nil)
	(new-size 0))
    (Do-WB-Map-Tree-Pairs (x ys map0a)
      (Do-WB-Set-Tree-Members (y ys)
	(let ((s? s (WB-Map-Tree-Lookup map0b y cmp-fn-0b)))
	  (when s?
	    (let ((ignore prev (WB-Map-Tree-Lookup new-map0 x cmp-fn-0a))
		  ((new (WB-Set-Tree-Union prev s cmp-fn-1b))))
	      (declare (ignore ignore))
	      (incf new-size (- (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev)))
	      (setq new-map0 (WB-Map-Tree-With new-map0 x new cmp-fn-0a #'eql-compare)))))))
    (when (or map1a map1b)
      (when (null map1b)
	(setq map1b (get-inverse brb)))
      (when (null map1a)
	(setq map1a (get-inverse bra)))
      (Do-WB-Map-Tree-Pairs (x ys map1b)
	(Do-WB-Set-Tree-Members (y ys)
	  (let ((s? s (WB-Map-Tree-Lookup map1a y cmp-fn-1a)))
	    (when s?
	      (let ((ignore prev (WB-Map-Tree-Lookup new-map1 x cmp-fn-1b)))
		(declare (ignore ignore))
		(setq new-map1
		      (WB-Map-Tree-With new-map1 x (WB-Set-Tree-Union prev s cmp-fn-0a)
					cmp-fn-1b #'eql-compare))))))))
    (make-wb-2-relation new-size new-map0 new-map1
			(make-tree-map-org cmp-fn-0a-name cmp-fn-0a cmp-fn-1b-name cmp-fn-1b))))


(defmethod compose ((rel wb-2-relation) (fn function) &key val-compare-fn-name)
  (2-relation-fn-compose rel fn val-compare-fn-name))

(defmethod compose ((rel wb-2-relation) (fn symbol) &key val-compare-fn-name)
  (2-relation-fn-compose rel (coerce fn 'function) val-compare-fn-name))

(defmethod compose ((rel wb-2-relation) (fn map) &key val-compare-fn-name)
  (2-relation-fn-compose rel fn (or val-compare-fn-name
				    (tree-map-org-val-compare-fn-name (wb-2-relation-org rel)))))

(defmethod compose ((rel wb-2-relation) (fn seq) &key val-compare-fn-name)
  (2-relation-fn-compose rel fn val-compare-fn-name))

(defmethod compose ((rel1 wb-2-relation) (rel2 wb-2-relation) &key)
  (join rel1 1 rel2 0))

(defun 2-relation-fn-compose (rel fn val-compare-fn-name)
  (let ((new-size 0)
	(org (wb-2-relation-org (empty-wb-custom-2-relation (tree-map-org-key-compare-fn-name (wb-2-relation-org rel))
							    (or val-compare-fn-name 'compare))))
	((new-map0 (gmap (:result wb-map)
			 (fn (x ys)
			   (let ((result nil))
			     (Do-WB-Set-Tree-Members (y ys)
			       (setq result (WB-Set-Tree-With result (@ fn y) (tree-map-org-val-compare-fn org))))
			     (incf new-size (WB-Set-Tree-Size result))
			     (values x result)))
			(:arg wb-map (make-wb-map (wb-2-relation-map0 rel) +fset-default-tree-map-org+ nil))))))
    (make-wb-2-relation new-size (wb-map-contents new-map0) nil org)))

(define-wb-set-method image ((fn 2-relation) (s wb-set))
  (make s (wb-set-image fn (contents s) (compare-fn s))))

(defmethod image ((fn 2-relation) (s ch-set))
  (ch-set-image fn s))


(defgeneric internal-do-2-relation (br elt-fn value-fn))

(defmethod internal-do-2-relation ((br wb-2-relation) elt-fn value-fn)
  (Do-WB-Map-Tree-Pairs (x y-set (wb-2-relation-map0 br) (funcall value-fn))
    (Do-WB-Set-Tree-Members (y y-set)
      (funcall elt-fn x y))))

(defmethod convert ((to-type (eql '2-relation)) (br 2-relation) &key)
  br)

(defmethod convert ((to-type (eql 'wb-2-relation)) (br wb-2-relation) &key key-compare-fn-name val-compare-fn-name)
  (let ((org (wb-2-relation-org (empty-wb-2-relation key-compare-fn-name val-compare-fn-name)))
	((key-compare-fn (tree-map-org-key-compare-fn org))
	 (val-compare-fn (tree-map-org-val-compare-fn org))))
    (if (and (eq key-compare-fn (tree-map-org-key-compare-fn (wb-2-relation-org br)))
	     (eq val-compare-fn (tree-map-org-val-compare-fn (wb-2-relation-org br))))
	br
      (let ((map0 nil)
	    (size 0))
	(Do-WB-Map-Tree-Pairs (x ys (wb-2-relation-map0 br))
	  (Do-WB-Set-Tree-Members (y ys)
	    (let ((ignore prev (WB-Map-Tree-Lookup map0 x key-compare-fn)))
	      (declare (ignore ignore))
	      (setq map0 (WB-Map-Tree-With map0 x (WB-Set-Tree-With prev y val-compare-fn)
					   key-compare-fn #'eql-compare)))
	    (incf size)))
	(make-wb-2-relation size map0 nil org)))))

(defmethod convert ((to-type (eql 'list)) (br 2-relation) &key (pair-fn #'cons))
  (gmap (:result list) pair-fn (:arg 2-relation br)))

(defmethod convert ((to-type (eql 'set)) (br 2-relation) &key (pair-fn #'cons))
  (convert 'wb-set br :pair-fn pair-fn))

(defmethod convert ((to-type (eql 'wb-set)) (br 2-relation) &key (pair-fn #'cons) compare-fn-name)
  (let ((set-org (wb-set-org (empty-wb-set compare-fn-name)))
	(result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-2-relation (x y br)
      (setq result (WB-Set-Tree-With result (funcall pair-fn x y) (tree-set-org-compare-fn set-org))))
    (make-wb-set result set-org)))

;;; I've made the default conversions between maps and 2-relations use the
;;; same pairs; that is, the conversion from a map to a 2-relation yields a
;;; functional relation with the same mappings, and the inverse conversion
;;; requires a functional relation and yields a map with the same mappings.
;;; This is mathematically elegant, but I wonder if the other kind of conversion
;;; -- where the map's range is set-valued -- is not more useful in practice,
;;; and maybe more deserving of being the default.
(defmethod convert ((to-type (eql '2-relation)) (m map) &key from-type)
  "If `from-type' is the symbol `map-to-sets', the range elements must all be
sets, and the result pairs each domain element with each member of the
corresponding range set.  Otherwise, the result pairs each domain element
with the corresponding range element directly."
  (if (eq from-type 'map-to-sets)
      (wb-map-to-sets-to-wb-2-relation m nil nil)
    (wb-map-to-wb-2-relation m nil nil)))

(defmethod convert ((to-type (eql 'wb-2-relation)) (m map) &key from-type key-compare-fn-name val-compare-fn-name)
  "If `from-type' is the symbol `map-to-sets', the range elements must all be
sets, and the result pairs each domain element with each member of the
corresponding range set.  If `from-type' is not `map-to-sets', the result
pairs each domain element with the corresponding range element directly.

The key and value compare-fns of the result are taken from `m' unless
explicitly overridden in the call."
  (if (eq from-type 'map-to-sets)
      (wb-map-to-sets-to-wb-2-relation m key-compare-fn-name val-compare-fn-name)
    (wb-map-to-wb-2-relation m key-compare-fn-name val-compare-fn-name)))

(defun wb-map-to-sets-to-wb-2-relation (m key-compare-fn-name val-compare-fn-name)
  (let ((size 0)
	(val-compare-fn-name (or val-compare-fn-name (and (nonempty? m)
							  (let ((k v (least m)))
							    (declare (ignore k))
							    (check-type v wb-set)
							    (tree-set-org-compare-fn-name (wb-set-org v))))))
	((compose-org (wb-2-relation-org (empty-wb-2-relation (tree-map-org-key-compare-fn-name (wb-map-org m))
							     val-compare-fn-name)))
	 (new-tree
	   (WB-Map-Tree-Compose (wb-map-contents m)
					(fn (s)
					  (let ((s (wb-set-contents
						     (convert 'wb-set s :compare-fn-name val-compare-fn-name))))
					    (incf size (WB-Set-Tree-Size s))
					    s))))))
    (convert 'wb-2-relation (make-wb-2-relation size new-tree nil compose-org)
	     :key-compare-fn-name (or key-compare-fn-name (tree-map-org-key-compare-fn-name compose-org))
	     :val-compare-fn-name val-compare-fn-name)))

(defun wb-map-to-wb-2-relation (m key-compare-fn-name val-compare-fn-name)
  (declare (optimize (debug 3)))
  (let ((map-org (wb-map-org m))
	((new-tree (WB-Map-Tree-Compose (wb-map-contents m)
					(fn (x) (WB-Set-Tree-With nil x (tree-map-org-val-compare-fn map-org)))))))
    (convert 'wb-2-relation (make-wb-2-relation (size m) new-tree nil map-org)
	     :key-compare-fn-name (or key-compare-fn-name (tree-map-org-key-compare-fn-name map-org))
	     :val-compare-fn-name (or val-compare-fn-name (tree-map-org-val-compare-fn-name map-org)))))

(defmethod convert ((to-type (eql '2-relation)) (lst list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (declare (optimize (debug 3)))
  (let ((key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (gmap (:result 2-relation) (fn (pr) (values (funcall key-fn pr) (funcall value-fn pr)))
	  (:list lst))))

(defmethod convert ((to-type (eql 'wb-2-relation)) (lst list)
		    &key (key-fn #'car) (value-fn #'cdr) key-compare-fn-name val-compare-fn-name)
  (let ((key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (gmap (:result 2-relation :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name)
	  (fn (pr) (values (funcall key-fn pr) (funcall value-fn pr)))
	  (:list lst))))

(defmethod convert ((to-type (eql '2-relation)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr))
  (let ((key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (gmap (:result 2-relation) (fn (pr) (values (funcall key-fn pr) (funcall value-fn pr)))
	  (:seq s))))

(defmethod convert ((to-type (eql 'wb-2-relation)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) key-compare-fn-name val-compare-fn-name)
  (let ((key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (gmap (:result 2-relation :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name)
	  (fn (pr) (values (funcall key-fn pr) (funcall value-fn pr)))
	  (:seq s))))

(defmethod convert ((to-type (eql 'map)) (br wb-2-relation) &key)
  "This conversion requires the relation to be functional, and returns
a map representing the function; that is, the relation must map each
domain value to a single range value, and the returned map maps that
domain value to that range value."
  (2-relation-to-wb-map br nil nil))

(defmethod convert ((to-type (eql 'wb-map)) (br wb-2-relation) &key key-compare-fn-name val-compare-fn-name)
  "This conversion requires the relation to be functional, and returns
a map representing the function; that is, the relation must map each
domain value to a single range value, and the returned map maps that
domain value to that range value."
  (2-relation-to-wb-map br key-compare-fn-name val-compare-fn-name))

(defun 2-relation-to-wb-map (br key-compare-fn-name val-compare-fn-name)
  (let ((m nil)
	(br-org (wb-2-relation-org br))
	((org (wb-2-relation-org (empty-wb-2-relation
				   (or key-compare-fn-name (tree-map-org-key-compare-fn-name br-org))
				   (or val-compare-fn-name (tree-map-org-val-compare-fn-name br-org)))))
	 ((key-compare-fn (tree-map-org-key-compare-fn org))
	  (val-compare-fn (tree-map-org-val-compare-fn org)))))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (let ((sz (WB-Set-Tree-Size s)))
	(unless (= 1 sz)
	  (error "2-relation maps ~A to ~D values" x sz))
	(setq m (WB-Map-Tree-With m x (WB-Set-Tree-Arb s) key-compare-fn val-compare-fn))))
    (make-wb-map m org nil)))

(defmethod convert ((to-type (eql 'map-to-sets)) (br wb-2-relation) &key)
  "This conversion returns a map mapping each domain value to the set of
corresponding range values."
  (make-wb-map (WB-Map-Tree-Compose (wb-2-relation-map0 br) #'make-wb-set)
	       +fset-default-tree-map-org+ nil))

(defgeneric conflicts (2-relation)
  (:documentation
    "Returns a 2-relation containing only those pairs of `2-relation' whose domain value
is mapped to multiple range values."))

(defmethod conflicts ((br wb-2-relation))
  (let ((m0 nil)
	(key-compare-fn (tree-map-org-key-compare-fn (wb-2-relation-org br)))
	(size 0))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (when (> (WB-Set-Tree-Size s) 1)
	(setq m0 (WB-Map-Tree-With m0 x s key-compare-fn #'eql-compare))
	(incf size (WB-Set-Tree-Size s))))
    (make-wb-2-relation size m0 nil (wb-2-relation-org br))))

(defun print-wb-2-relation (br stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{+"
				    :suffix (let ((tmorg (wb-2-relation-org br))
						  ((key-cf-name (tree-map-org-key-compare-fn-name tmorg))
						   (val-cf-name (tree-map-org-val-compare-fn-name tmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare)))))
					      (format nil " +}~:[[~:[~S~;~*~];~:[~S~;~*~]]~;~4*~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name)))
    (do-2-relation (x y br)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write (list x y) :stream stream))))

(defmethod iterator ((rel wb-2-relation) &key)
  (let ((outer (Make-WB-Map-Tree-Iterator-Internal (wb-2-relation-map0 rel)))
	(cur-dom-elt nil)
	(inner nil))
    (flet ((done? ()
	     (and (WB-Map-Tree-Iterator-Done? outer)
		  (or (null inner) (WB-Set-Tree-Iterator-Done? inner)))))
      (lambda (op)
	(ecase op
	  (:get (if (done?) (values nil nil nil)
		  (progn
		    (when (or (null inner) (WB-Set-Tree-Iterator-Done? inner))
		      (let ((dom-elt inner-tree (WB-Map-Tree-Iterator-Get outer)))
			(setq cur-dom-elt dom-elt)
			(assert inner-tree)	; must be nonempty
			(setq inner (Make-WB-Set-Tree-Iterator-Internal inner-tree))))
		    (values cur-dom-elt (WB-Set-Tree-Iterator-Get inner) t))))
	  (:done? (done?))
	  (:more? (not (done?))))))))

(defmethod fun-iterator ((rel wb-2-relation) &key from-end?)
  (rlabels (walk-outer (if from-end? (WB-Map-Tree-Rev-Fun-Iter (wb-2-relation-map0 rel))
			 (WB-Map-Tree-Fun-Iter (wb-2-relation-map0 rel))))
    (walk-outer (outer)
      (if (funcall outer ':empty?) outer
	(let ((cur-dom-elt inner-tree (funcall outer ':first)))
	  (walk-inner (funcall outer ':rest) cur-dom-elt
		      (if from-end? (WB-Set-Tree-Rev-Fun-Iter inner-tree)
			(WB-Set-Tree-Fun-Iter inner-tree))))))
    (walk-inner (outer cur-dom-elt inner)
      (if (funcall inner ':empty?) (walk-outer outer)
	(lambda (op)
	  (ecase op
	    (:first (values cur-dom-elt (funcall inner ':first) t))
	    (:rest (walk-inner outer cur-dom-elt (funcall inner ':rest)))
	    (:empty? nil)
	    (:more? t)))))))

(define-cross-type-compare-methods relation)

(defmethod compare ((a wb-2-relation) (b wb-2-relation))
  (if-same-wb-2-relation-orgs (a b org)
      (let ((a-size (wb-2-relation-size a))
	    (b-size (wb-2-relation-size b)))
	(cond ((< a-size b-size) ':less)
	      ((> a-size b-size) ':greater)
	      (t
	       (WB-Map-Tree-Compare (wb-2-relation-map0 a) (wb-2-relation-map0 b)
				    (tree-map-org-key-compare-fn org)
				    (fn (a b) (WB-Set-Tree-Compare a b (tree-map-org-val-compare-fn org)))))))
    (let ((a-kcfn-name (tree-map-org-key-compare-fn-name (wb-2-relation-org a)))
	  (a-vcfn-name (tree-map-org-val-compare-fn-name (wb-2-relation-org a)))
	  (b-kcfn-name (tree-map-org-key-compare-fn-name (wb-2-relation-org b)))
	  (b-vcfn-name (tree-map-org-val-compare-fn-name (wb-2-relation-org b)))
	  ((name-comp (compare (list a-kcfn-name b-vcfn-name) (list a-kcfn-name b-vcfn-name)))))
      (ecase name-comp
	((:less :greater)
	  name-comp)
	(:equal
	  (compare (convert 'wb-2-relation a :key-compare-fn-name a-kcfn-name :val-compare-fn-name a-vcfn-name)
		   (convert 'wb-2-relation b :key-compare-fn-name b-kcfn-name :val-compare-fn-name b-vcfn-name)))
	(:unequal
	  (error "Can't compare wb-2-relations with uninterned compare-fn-names with same symbol-name"))))))

(defmethod hash-value ((rel wb-2-relation))
  ;; As with the other WB types, we do bounded-effort hashing without caching.
  (let ((result 0)
	(i 0)
	(mult 1))
    (Do-WB-Map-Tree-Pairs (x ys (wb-2-relation-map0 rel))
      (hash-mixf result (hash-multiply mult (hash-value-fixnum x)))
      (setq mult (hash-multiply mult 13))
      (let ((j 0))
	(Do-WB-Set-Tree-Members (y ys)
	  (hash-mixf result (hash-multiply mult (hash-value-fixnum y)))
	  (setq mult (hash-multiply mult 3))
	  (when (= (incf j) 8)
	    (return))))
      (when (= (incf i) 8)
	(return)))
    result))


(defgeneric transitive-closure (2-relation set)
  (:documentation
    "The transitive closure of the set over the relation.  The relation may
also be supplied as a function returning a set."))

(defmethod transitive-closure ((fn function) (s set))
  (set-transitive-closure fn s))

(defmethod transitive-closure ((r 2-relation) (s set))
  (set-transitive-closure r s))

(defun set-transitive-closure (r s)
  (let ((workset (set-difference
		   (reduce #'union (image r s) :initial-value (empty-set-like s))
		   s))
	(result s))
    (while (nonempty? workset)
      (let ((x (arb workset)))
	(excludef workset x)
	(includef result x)
	(unionf workset (set-difference (@ r x) result))))
    result))


;;; ================================================================================
;;; List relations

;;; A list relation is a general relation (i.e. of arbitrary arity >= 2) whose tuples are
;;; in list form.  List relations support a `query' operation that takes, along with the
;;; relation, a list, of length equal to the arity, called the "pattern".  For each
;;; position, if the pattern contains the symbol `fset:?', the query is not constrained
;;; by that position; otherwise, the result set contains only those tuples with the same
;;; value in that position as the pattern has.  There is also a `query-multi' operation
;;; that is similar, but the pattern contains sets of possible values (or `?') in each
;;; position, and the result set contains all tuples with any of those values in that
;;; position.


(defstruct (list-relation
	    (:include relation)
	    (:constructor nil)
	    (:predicate list-relation?)
	    (:copier nil))
  "The abstract class for FSet list relations.  It is a structure class.
A list relation is a general relation (i.e. of arbitrary arity >= 1) whose
tuples are in list form."
  (arity nil :type (or null integer) :read-only t))

(defstruct (tree-list-relation-org
	     (:constructor make-raw-tree-list-relation-org (compare-fn-name compare-fn
							    tuple-compare-fn index-compare-fn))
	     (:predicate tree-list-relation-org?)
	     (:copier nil))
  (compare-fn-name nil :type symbol :read-only t)
  (compare-fn nil :type function :read-only t)
  (tuple-compare-fn nil :type function :read-only t)
  (index-compare-fn nil :type function :read-only t))

(defun make-tree-list-relation-org (compare-fn-name)
  (let ((compare-fn (symbol-function compare-fn-name))
	((tuple-compare-fn (fn (x y)
			     (let ((unequal? nil))
			       (or (gmap :or (fn (ex ey)
					       (let ((comp (funcall compare-fn ex ey)))
						 (ecase comp
						   ((:less :greater) comp)
						   (:unequal
						     (setq unequal? t)
						     nil)
						   (:equal nil))))
					 (:arg list x)
					 (:arg list y))
				   (if unequal? ':unequal ':equal)))))
	 ;; This just compares pattern masks, which are integers.
	 (index-compare-fn (fn (x y) (cond ((< x y) ':less)
					   ((> x y) ':greater)
					   (t ':equal))))))
    (make-raw-tree-list-relation-org compare-fn-name compare-fn tuple-compare-fn index-compare-fn)))

(defstruct (wb-list-relation
	    (:include list-relation)
	    (:constructor make-wb-list-relation (arity tuples indices org))
	    (:predicate wb-list-relation?)
	    (:print-function print-wb-list-relation)
	    (:copier nil))
  "A class of functional relations of arbitrary arity >= 1, whose tuples
are in list form."
  (tuples nil :read-only t) ; internal tree
  ;; A map from pattern mask to map from reduced tuple to set of tuples (maps and sets as internal trees).
  ;; Mutable, since it's a cache.
  indices
  (org nil :type tree-list-relation-org :read-only t))


(defun empty-list-relation (&optional arity)
  "We allow the arity to be temporarily unspecified; it will be taken from
the first tuple added."
  (unless (or (null arity) (and (integerp arity) (>= arity 1)))
    (error "Invalid arity"))
  (empty-wb-list-relation arity))

(defun empty-wb-list-relation (arity &optional compare-fn-name)
  "We allow the arity to be temporarily unspecified; it will be taken from
the first tuple added.  If `compare-fn-name' is provided, the function must
be able to compare both tuples \(lists\) and their elements."
  ;; -- Or, we could construct the tuple comparator from the element comparator...
  ;; If arity = 1 it's just a set... but what the heck...
  (unless (or (null arity) (and (integerp arity) (>= arity 1)))
    (error "Invalid arity"))
  (make-wb-list-relation arity nil nil (make-tree-list-relation-org (or compare-fn-name 'compare))))


(defmethod arity ((rel list-relation))
  "Will return `nil' if the arity is not yet specified; see `empty-list-relation'."
  (list-relation-arity rel))

(defmethod empty? ((rel wb-list-relation))
  (null (wb-list-relation-tuples rel)))

(defmethod size ((rel wb-list-relation))
  (WB-Set-Tree-Size (wb-list-relation-tuples rel)))

(defmethod arb ((rel wb-list-relation))
  (WB-Set-Tree-Arb (wb-list-relation-tuples rel)))

(defmethod at-rank ((rel wb-list-relation) rank)
  (let ((tuples (wb-list-relation-tuples rel))
	((size (WB-Set-Tree-Size tuples))))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank rel)))
    (WB-Set-Tree-Rank-Element tuples rank)))

(defmethod convert ((to-type (eql 'set)) (rel wb-list-relation) &key)
  (convert 'wb-set rel))

(defmethod convert ((to-type (eql 'wb-set)) (rel wb-list-relation) &key)
  (make-wb-set (wb-list-relation-tuples rel)
	       (wb-list-relation-result-org rel)))

(defun wb-list-relation-result-org (rel)
  (let ((org (wb-list-relation-org rel)))
    (if (eq (tree-list-relation-org-compare-fn-name org) 'compare)
	(wb-set-org *empty-wb-set*)
      (make-tree-set-org '?? (tree-list-relation-org-tuple-compare-fn org)))))

(defmethod contains? ((rel wb-list-relation) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'contains? 'wb-list-relation)
  (WB-Set-Tree-Contains? (wb-list-relation-tuples rel) tuple
			 (tree-list-relation-org-tuple-compare-fn (wb-list-relation-org rel))))

(defgeneric query (relation pattern &optional metapattern)
  (:documentation
    "Along with the relation, takes `pattern', which is a list of length
less than or equal to the arity.  Returns all tuples whose elements match
those of the pattern, starting from the left end of both, where pattern
elements equal to `fset:?' (the symbol itself, not its value) match any
tuple value.  If the pattern's length is less than the arity, the missing
positions also match any tuple value.  (The symbol `?' is exported from
`fset:' for this purpose.)

The `metapattern' parameter was used in an older API; its use is now
deprecated."))

(defmethod query ((rel wb-list-relation) pattern &optional (metapattern nil metapattern?))
  ;; Backward compatibility for the old API.  Deprecated.
  (when metapattern?
    (setq pattern (gmap (:result list) (fn (p m) (if m p '?))
			(:arg list pattern) (:arg list metapattern))))
  (let ((arity (wb-list-relation-arity rel)))
    (if (null arity)
	;; We don't know the arity yet, which means there are no tuples.
	(wb-set)
      (progn
	(unless (and pattern (<= (length pattern) arity))
	  (error "Pattern is of the wrong length"))
	(let ((pattern mask (prepare-pattern arity pattern)))
	  (if (= mask (1- (ash 1 arity)))
	      (if (contains? rel pattern) (wb-set pattern) (wb-set))
	    (let ((reduced-tuple (reduced-tuple pattern mask))
		  (org (wb-list-relation-org rel))
		  ((tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))
		   (index? index (WB-Map-Tree-Lookup (wb-list-relation-indices rel) mask
						     (tree-list-relation-org-index-compare-fn org)))))
	      (if index?
		  (make-wb-set (nth-value 1 (WB-Map-Tree-Lookup index reduced-tuple tuple-compare-fn))
			       (wb-list-relation-result-org rel))
		(let ((index-results
			(gmap (:result list :filterp (fn (x) (not (eq x 'none))))
			      (fn (index i pat-elt)
				(if (logbitp i mask)
				    (nth-value 1 (WB-Map-Tree-Lookup index (list pat-elt) tuple-compare-fn))
				  'none))
			      (:arg seq (get-indices rel mask))
			      (:arg index 0)
			      (:arg list pattern))))
		  ;; &&& We also want to build composite indices under some
		  ;; circumstances -- e.g. if the result set is much smaller
		  ;; than the smallest of `index-results'.
		  (if index-results
		      (make-wb-set (reduce (fn (a b) (WB-Set-Tree-Intersect a b tuple-compare-fn))
					   (sort index-results #'< :key #'WB-Set-Tree-Size))
				   (wb-list-relation-result-org rel))
		    ;; Completely uninstantiated pattern
		    (convert 'wb-set rel)))))))))))

(defgeneric query-multi (rel pattern &optional metapattern)
  (:documentation
    "Like `query' (q.v.), except that `pattern' is a list where the elements that
aren't `fset:?' are sets of values rather than single values.  Returns all tuples
in the relation for which each value is a member of the corresponding set in the
pattern."))

(defmethod query-multi ((rel wb-list-relation) (pattern list) &optional (metapattern nil metapattern?))
  ;; Backward compatibility for the old API.  Deprecated.
  (when metapattern?
    (setq pattern (gmap (:result list) (fn (p m) (if m p '?))
			(:arg list pattern) (:arg list metapattern))))
  (let ((arity (wb-list-relation-arity rel)))
    (if (null arity)
	;; We don't know the arity yet, which means there are no tuples.
	(set)
      (progn
	(unless (<= (length pattern) arity)
	  (error "Pattern is of the wrong length"))
	(let ((pattern mask (prepare-pattern arity pattern)))
	  (if (gmap :and (fn (s) (or (eq s '?) (= (size s) 1)))
		    (:arg list pattern))
	      (query rel (mapcar (fn (s) (if (eq s '?) s (arb s))) pattern))
	    (let ((org (wb-list-relation-org rel))
		  ((tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))
		   ((index-results
		      (gmap (:result list :filterp (fn (x) (not (eq x 'none))))
			    (fn (index i pat-elt)
			      (if (logbitp i mask)
				  (gmap (:result nil nil (fn (a b) (WB-Set-Tree-Union a b tuple-compare-fn)))
					(fn (pat-elt-elt)
					  (nth-value 1 (WB-Map-Tree-Lookup index (list pat-elt-elt) tuple-compare-fn)))
					(:arg set pat-elt))
				'none))
			    (:arg seq (get-indices rel mask))
			    (:arg index 0)
			    (:arg list pattern))))))
	      (if index-results
		  (make-wb-set (reduce (fn (a b) (WB-Set-Tree-Intersect a b tuple-compare-fn))
				       (sort index-results #'< :key #'WB-Set-Tree-Size))
			       (wb-list-relation-result-org rel))
		(convert 'wb-set rel)))))))))

(defgeneric query-multi-restricted (rel pattern restrict-set)
  (:documentation
    "Queries the relation for tuples that match `pattern' and contain some element
of `restrict-set'.  `pattern' is a list where each element is either the symbol
`fset:?' or a set of values.  A tuple matches the pattern if, for each position,
either the element of `pattern' at that position is `fset:?' or the tuple element
at that position is an element of that set of values.  Returns the set of tuples
in the relation that match the pattern and contain at least one element of
`restrict-set'.  For convenience, the function accepts the full set for `restrict-set',
in which case there is no restriction."))

(defmethod query-multi-restricted ((rel wb-list-relation) (pattern list) restrict-set)
  (let ((arity (wb-list-relation-arity rel)))
    (if (null arity)
	(set)
      (let ((pattern (prepare-pattern arity pattern)))
	(if (and (not (contains? pattern '?))
		 (gmap :and (fn (x) (disjoint? x restrict-set))
		       (:arg list pattern)))
	    (wb-set)
	  (if (equal? restrict-set (full-set))
	      (query-multi rel pattern)
	    (let ((indices (get-indices rel (1- (ash 1 arity))))
		  (org (wb-list-relation-org rel))
		  ((tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))
		   ((full-results restricted-results
		      (gmap (:result values seq seq)
			    (fn (pat-elt index)
			      (let ((restricted (if (eq pat-elt '?) restrict-set
						  (intersection pat-elt restrict-set))))
				(values (if (eq pat-elt '?) '?
					  (gmap (:result nil nil (fn (a b) (WB-Set-Tree-Union a b tuple-compare-fn)))
						(fn (pat-elt-elt)
						  (nth-value 1 (WB-Map-Tree-Lookup index (list pat-elt-elt)
										   tuple-compare-fn)))
						(:arg set pat-elt)))
					(gmap (:result nil nil (fn (a b) (WB-Set-Tree-Union a b tuple-compare-fn)))
					      (fn (pat-elt-elt)
						(nth-value 1 (WB-Map-Tree-Lookup index (list pat-elt-elt)
										 tuple-compare-fn)))
					      (:arg set restricted)))))
			    (:arg list pattern)
			    (:arg seq indices))))))
	      ;; Hmm.  The simpler approach would be just to call `query-multi', and then intersect
	      ;; its result with each of `restricted-results', unioning the intersections.  The
	      ;; potential downside of that is that the full results might be much larger than any
	      ;; of the partial results we get this way.  That is, each element of `restricted-results'
	      ;; might be much smaller than any of `full-results' or even their intersection, so that
	      ;; materializing that intersection might be much more expensive than doing this.
	      (make-wb-set (gmap (:result nil nil (fn (a b) (WB-Set-Tree-Union a b tuple-compare-fn)))
				 (fn (i)
				   (let ((results (with full-results i (@ restricted-results i))))
				     (reduce (fn (a b)
					       (WB-Set-Tree-Intersect a b tuple-compare-fn))
					     (sort (filter (fn (x) (not (eq x '?))) results)
						   #'< :key #'WB-Set-Tree-Size))))
				 (:index 0 (length pattern)))
			   (wb-list-relation-result-org rel)))))))))

(defun prepare-pattern (rel-arity pattern)
  (unless (<= (length pattern) rel-arity)
    (error "Pattern is too long for arity ~D" rel-arity))
  (let ((mask (pattern-mask pattern)))
    (if (< (length pattern) rel-arity)
	(let ((qs (make-list (- rel-arity (length pattern)) :initial-element '?)))
	  (values (append pattern qs) mask))
      (values pattern mask))))

(defun pattern-mask (pattern)
  (gmap (:result sum) (fn (pat-elt i)
			(if (eq pat-elt '?) 0 (ash 1 i)))
	(:arg list pattern)
	(:index 0)))

(defmethod get-indices ((rel wb-list-relation) mask)
  "Returns a seq of size equal to the arity of `rel'.  For each 1 bit in `mask',
the corresponding result element is the index to use for that tuple position.
Indices are returned as internal wb-map trees."
  ;; First we see what indices exist on each position.
  (let ((org (wb-list-relation-org rel))
	((index-compare-fn (tree-list-relation-org-index-compare-fn org))
	 (tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))
	 ((ex-inds (gmap (:result seq)
			 (fn (i) (and (logbitp i mask)
				      (nth-value 1 (WB-Map-Tree-Lookup (wb-list-relation-indices rel) (ash 1 i)
								       index-compare-fn))))
			 (:arg index 0 (arity rel))))
	  ((unindexed (gmap (:result list)
			    (lambda (index i)
			      (and (logbitp i mask) (null index)))
			    (:arg seq ex-inds)
			    (:arg index 0)))))))
    ;; Now, if there were any instantiated positions for which an index did
    ;; not exist, construct indices for them.
    (if (gmap :and #'null (:arg list unindexed))
	ex-inds
      (let ((new-indices (make-array (arity rel) :initial-element nil)))
	;; Populate the new indices
	(do-wb-set-tree-members (tuple (wb-list-relation-tuples rel))
	  (gmap nil (fn (tuple-elt unind i)
		      (when unind
			;; If we called `reduced-tuple', we'd get `(list tuple-elt)'.
			;; (includef (@ (svref new-indices i) (list tuple-elt)) tuple)
			(let ((key (list tuple-elt))
			      ((ignore rt-set (WB-Map-Tree-Lookup (svref new-indices i) key tuple-compare-fn))))
			  (declare (ignore ignore))
			  (setf (svref new-indices i)
				(WB-Map-Tree-With (svref new-indices i) key
						  (WB-Set-Tree-With rt-set tuple tuple-compare-fn)
						  tuple-compare-fn #'eql-compare)))))
		(:arg list tuple)
		(:arg list unindexed)
		(:arg index 0)))
	;; Store them back in the relation.  This is unusual in that we're modifying an FSet
	;; collection, but note that we're just caching information about the contents.
	;; In particular, if two threads do this at the same time, all that goes wrong is
	;; some duplication of work.
	(let ((indices (wb-list-relation-indices rel)))
	  (gmap nil (fn (unind i new-index)
		      (when unind
			;; (setf (@ indices (ash 1 i)) new-index)
			(setq indices (WB-Map-Tree-With indices (ash 1 i) new-index index-compare-fn #'eql-compare))))
		(:arg list unindexed)
		(:arg index 0)
		(:arg vector new-indices))
	  (setf (wb-list-relation-indices rel) indices))
	(gmap (:result seq) (fn (ex-ind new-index) (or ex-ind new-index))
	      (:arg seq ex-inds)
	      (:arg vector new-indices))))))

(defmethod with ((rel wb-list-relation) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-list-relation)
  (let ((arity (or (wb-list-relation-arity rel)
		   (length tuple)))
	(org (wb-list-relation-org rel))
	((index-compare-fn (tree-list-relation-org-index-compare-fn org))
	 (tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not equal arity, ~D"
	     (length tuple) arity))
    (if (WB-Set-Tree-Contains? (wb-list-relation-tuples rel) tuple tuple-compare-fn)
	rel
      (make-wb-list-relation arity (WB-Set-Tree-With (wb-list-relation-tuples rel) tuple tuple-compare-fn)
			     ;; (image (lambda (mask rt-map)
			     ;; 	      (let ((rt (reduced-tuple tuple mask)))
			     ;; 		(values mask (with rt-map rt (with (@ rt-map rt) tuple)))))
			     ;; 	    (wb-list-relation-indices rel))
			     (let ((result nil))
			       (Do-WB-Map-Tree-Pairs (mask rt-map (wb-list-relation-indices rel) result)
				 (let ((rt (reduced-tuple tuple mask))
				       ((ignore rt-set (WB-Map-Tree-Lookup rt-map rt tuple-compare-fn))
					((new-rt-set (WB-Set-Tree-With rt-set tuple tuple-compare-fn))
					 ((new-inner-map
					    (WB-Map-Tree-With rt-map rt new-rt-set tuple-compare-fn #'eql-compare))))))
				   (declare (ignore ignore))
				   (setq result (WB-Map-Tree-With result mask new-inner-map
								  index-compare-fn #'eql-compare)))))
			     org))))

(defmethod less ((rel wb-list-relation) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-list-relation)
  (let ((arity (or (wb-list-relation-arity rel)
		   (length tuple)))
	(org (wb-list-relation-org rel))
	((index-compare-fn (tree-list-relation-org-index-compare-fn org))
	 (tuple-compare-fn (tree-list-relation-org-tuple-compare-fn org))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not equal arity, ~D"
	     (length tuple) arity))
    (if (not (WB-Set-Tree-Contains? (wb-list-relation-tuples rel) tuple tuple-compare-fn))
	rel
      (make-wb-list-relation arity (WB-Set-Tree-Less (wb-list-relation-tuples rel) tuple tuple-compare-fn)
			     ;; (image (lambda (mask rt-map)
			     ;; 	      (let ((rt (reduced-tuple tuple mask)))
			     ;; 		(values mask (with rt-map rt (less (@ rt-map rt) tuple)))))
			     ;; 	    (wb-list-relation-indices rel))
			     (let ((result nil))
			       (Do-WB-Map-Tree-Pairs (mask rt-map (wb-list-relation-indices rel) result)
				 (let ((rt (reduced-tuple tuple mask))
				       ((ignore rt-set (WB-Map-Tree-Lookup rt-map rt tuple-compare-fn))
					((new-rt-set (WB-Set-Tree-Less rt-set tuple tuple-compare-fn))
					 ((new-inner-map
					    (WB-Map-Tree-With rt-map rt new-rt-set tuple-compare-fn #'eql-compare))))))
				   (declare (ignore ignore))
				   (setq result (WB-Map-Tree-With result mask new-inner-map
								  index-compare-fn #'eql-compare)))))
			     org))))

(defun reduced-tuple (tuple mask)
  "Returns a list of those members of `tuple' corresponding to instantiated
positions in the original pattern."
  (declare (fixnum mask))
  (if (= mask (1- (ash 1 (length tuple))))
      tuple
    (do ((mask mask (ash mask -1))
	 (tuple tuple (cdr tuple))
	 (result nil))
	((zerop mask)
	 (nreverse result))
      (when (logbitp 0 mask)
	(push (car tuple) result)))))


(defgeneric internal-do-list-relation (rel elt-fn value-fn))

(defmacro do-list-relation ((tuple rel &optional value) &body body)
  `(block nil
     (internal-do-list-relation ,rel (lambda (,tuple) . ,body)
				(lambda () ,value))))

(defmethod internal-do-list-relation ((rel wb-list-relation) elt-fn value-fn)
  (Do-WB-Set-Tree-Members (tuple (wb-list-relation-tuples rel)
				 (funcall value-fn))
    (funcall elt-fn tuple)))

(defun print-wb-list-relation (rel stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream "#{* ")
      (let ((i 0))
	(do-list-relation (tuple rel)
	  (when (> i 0)
	    (format stream " "))
	  (when (and *print-length* (>= i *print-length*))
	    (format stream "...")
	    (return))
	  (incf i)
	  (let ((*print-level* (and *print-level* (1- *print-level*))))
	    (write tuple :stream stream)))
	(when (> i 0)
	  (format stream " ")))
      (format stream "*}~@[^~D~]" (arity rel)))))


;;; --------------------------------------------------------------------------------
;;; Assertion DB

;;; A simple, functional in-memory assertion "database" with pattern query capability.
;;; Contains one `list-relation' for each arity of tuples that have been added to it.

(defstruct (assertion-db
	     (:constructor nil)
	     (:predicate assertion-db?)
	     (:copier nil)))

(defstruct (wb-assertion-db
	     (:include assertion-db)
	     (:constructor make-wb-assertion-db (list-rels))
	     (:predicate wb-assertion-db?)
	     (:print-function print-wb-assertion-db)
	     (:copier nil))
  (list-rels nil :read-only t))

(defun empty-assertion-db ()
  (empty-wb-assertion-db))

(defun empty-wb-assertion-db ()
  (make-wb-assertion-db (map :default (empty-list-relation))))

(defmethod with ((adb assertion-db) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'assertion-db)
  (let ((arity (length tuple)))
    (make-wb-assertion-db (with (wb-assertion-db-list-rels adb) arity
				(with (@ (wb-assertion-db-list-rels adb) arity)
				      tuple)))))

(defmethod less ((adb assertion-db) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'assertion-db)
  (let ((arity (length tuple)))
    (make-wb-assertion-db (with (wb-assertion-db-list-rels adb) arity
				(less (@ (wb-assertion-db-list-rels adb) arity)
				      tuple)))))

;;; A little bit unkosher to reuse the optional `metapattern' parameter of `query' on
;;; `list-relation' like this, but use of that parameter is deprecated, so I think I
;;; can get away with it.
(defmethod query ((adb assertion-db) pattern &optional override-arity)
  (when (and override-arity
	     (not (and (integerp override-arity) (> override-arity 0) (>= override-arity (length pattern)))))
    (error "Invalid `override-arity': ~A" override-arity))
  (let ((arity (or override-arity (length pattern))))
    (query (@ (wb-assertion-db-list-rels adb) arity) pattern)))

(defun print-wb-assertion-db (adb stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream "#<Assertion DB, ~D arit~:@P> " (size (wb-assertion-db-list-rels adb))))))


;;; --------------------------------------------------------------------------------
;;; Query registry

#||

Okay, this is a start, but:

() Don't we want to do better meta-indexing, so adding a tuple doesn't require
iterating through all the indices?  [Eh, maybe not]

() I'm not creating composite indices yet.  The plan is straightforward -- create
one when the size of the final result set is <= the square root of the size of
the smallest index set.  This is easy, but how do subsequent queries find the
composite index?  [Why "square root"?  Maybe a fixed fraction like 1/8?]

||#


;;; A query registry to be used with `list-relation'.  Register queries with `with',
;;; supplying a pattern.  The queries themselves are uninterpreted except that they are
;;; kept in sets (so bare CL closures are not a good choice).  `lookup' returns the set of
;;; queries that match the supplied tuple.  A single query registry can now be used with
;;; several list-relations of different arities, but the client has to check whether the
;;; arity of each returned query matches that of the tuple.
(defstruct (query-registry
	     (:constructor make-query-registry (list-relations))
	     (:predicate query-registry?)
	     (:copier nil))
  ;; A map from pattern mask to a list-relation holding lists of the form
  ;; `(,query . ,masked-tuple).
  (list-relations nil :read-only t))

(defun empty-query-registry ()
  (make-query-registry (empty-map (empty-list-relation))))

(defmethod with ((reg query-registry) pattern &optional (query nil query?))
  (check-three-arguments query? 'with 'query-registry)
  (let ((mask (pattern-mask pattern)))
    (make-query-registry (with (query-registry-list-relations reg)
			       mask (with (@ (query-registry-list-relations reg) mask)
					  (cons query (reduced-tuple pattern mask)))))))

(defmethod less ((reg query-registry) pattern &optional (query nil query?))
  (check-three-arguments query? 'less 'query-registry)
  (let ((mask (pattern-mask pattern)))
    (if (empty? (@ (query-registry-list-relations reg) mask))
	reg
      (make-query-registry (with (query-registry-list-relations reg)
				 mask (less (@ (query-registry-list-relations reg) mask)
					    (cons query (reduced-tuple pattern mask))))))))

(defmethod all-queries ((reg query-registry))
  (gmap (:result union)
	(fn (_mask rel)
	  (image #'car (convert 'set rel)))
	(:arg map (query-registry-list-relations reg))))

(defmacro do-all-queries ((query reg &optional value) &body body)
  `(block nil
     (internal-do-all-queries ,reg (lambda (,query) . ,body)
			      (lambda () ,value))))

(defmethod internal-do-all-queries ((reg query-registry) elt-fn value-fn)
  (do-map (mask rel (query-registry-list-relations reg) (funcall value-fn))
    (declare (ignore mask))
    (do-list-relation (tuple rel)
      (funcall elt-fn (car tuple)))))

(defmethod lookup ((reg query-registry) tuple)
  "Returns all queries in `reg' whose patterns match `tuple'."
  (gmap (:result union)
	(fn (mask list-rel)
	  (let ((msk-tup (masked-tuple tuple mask)))
	    (if msk-tup (image #'car (query list-rel (cons '? msk-tup)))
	      (set))))
	(:arg map (query-registry-list-relations reg))))

(defmethod lookup-multi ((reg query-registry) set-tuple)
  "Here `set-tuple' contains a set of values in each position.  Returns
all queries in `reg' whose patterns match any member of each set."
  (gmap (:result union)
	(fn (mask list-rel)
	  (let ((msk-tup (masked-tuple set-tuple mask)))
	    (if msk-tup (image #'car (query-multi list-rel (cons '? msk-tup)))
	      (set))))
	(:arg map (query-registry-list-relations reg))))

(defun masked-tuple (tuple mask)
  (do ((mask mask (ash mask -1))
       (tuple tuple (cdr tuple))
       (result nil))
      ((= mask 0) (nreverse result))
    (when (null tuple)
      (return nil))  ; tuple is too short for mask
    (when (logbitp 0 mask)
      (push (car tuple) result))))


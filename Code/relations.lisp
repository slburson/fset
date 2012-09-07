;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: relations.lisp
;;; Contents: Relations (binary and general).
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


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

(defstruct (wb-2-relation
	    (:include 2-relation)
	    (:constructor make-wb-2-relation (size map0 map1))
	    (:predicate wb-2-relation?)
	    (:print-function print-wb-2-relation)
	    (:copier nil))
  "A class of functional binary relations represented as pairs of weight-
balanced binary trees.  This is the default implementation of binary relations
in FSet.  The inverse is constructed lazily, and maintained thereafter."
  size
  map0
  map1)

(defparameter *empty-wb-2-relation* (make-wb-2-relation 0 nil nil))

(defun empty-2-relation ()
  *empty-wb-2-relation*)
(declaim (inline empty-2-relation))

(defun empty-wb-2-relation ()
  *empty-wb-2-relation*)
(declaim (inline empty-wb-2-relation))

(defmethod empty? ((br wb-2-relation))
  (zerop (wb-2-relation-size br)))

(defmethod size ((br wb-2-relation))
  (wb-2-relation-size br))

(defmethod arb ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Arb-Pair tree)))
	  (values key (WB-Set-Tree-Arb val)) t)
      (values nil nil nil))))

;;; Must pass the pair as a cons -- the generic function doesn't allow us to
;;; add a parameter.  (&&& Actually we should do the same thing we're doing
;;; with `with' and `less'.)
(defmethod contains? ((br wb-2-relation) pr)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) (car pr))))
    (and found? (WB-Set-Tree-Member? set-tree (cdr pr)))))

;;; Returns the range set.
;;; &&& Aaagh -- not sure this makes sense -- (setf (lookup rel x) ...) doesn't do
;;; the right thing at all, relative to this.  Maybe the setf expander for `lookup'/`@'
;;; should call an internal form of `with' that does something different on a
;;; relation...  Yes, I think this operation should be renamed, and `setf-lookup'
;;; should not exist on a relation, as `lookup' should not.
(defmethod lookup ((br wb-2-relation) x)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x)))
    (if found? (make-wb-set set-tree)
      *empty-wb-set*)))

(defgeneric lookup-inv (2-relation y)
  (:documentation "Does an inverse lookup on a binary relation."))

(defmethod lookup-inv ((br wb-2-relation) y)
  (get-inverse br)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map1 br) y)))
    (if found? (make-wb-set set-tree)
      *empty-wb-set*)))

(defmethod domain ((br wb-2-relation))
  (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map0 br))))

(defmethod range ((br wb-2-relation))
  (get-inverse br)
  (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map1 br))))

(defun get-inverse (br)
  (let ((m0 (wb-2-relation-map0 br))
	(m1 (wb-2-relation-map1 br)))
    (when (and m0 (null m1))
      (Do-WB-Map-Tree-Pairs (x s m0)
	(Do-WB-Set-Tree-Members (y s)
	  (let ((ignore prev (WB-Map-Tree-Lookup m1 y)))
	    (declare (ignore ignore))
	    (setq m1 (WB-Map-Tree-With m1 y (WB-Set-Tree-With prev x))))))
      ;;; Look Ma, no locking!  Assuming the write is atomic.
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
		      (wb-2-relation-map0 br)))

(defmethod least ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Least-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod greatest ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Greatest-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod with ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x))
	(map1 (wb-2-relation-map1 br)))
    (if found?
	(let ((new-set-tree (WB-Set-Tree-With set-tree y)))
	  (if (eq new-set-tree set-tree)
	      br			; `y' was already there
	    (make-wb-2-relation (1+ (wb-2-relation-size br))
			     (WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree)
			     (and map1
				  (let ((ignore set-tree-1
					  (WB-Map-Tree-Lookup map1 y)))
				    (declare (ignore ignore))
				    (WB-Map-Tree-With
				      map1 y (WB-Set-Tree-With set-tree-1 x)))))))
      (make-wb-2-relation (1+ (wb-2-relation-size br))
		       (WB-Map-Tree-With (wb-2-relation-map0 br) x
					 (WB-Set-Tree-With nil y))
		       (and map1
			    (let ((ignore set-tree-1
				    (WB-Map-Tree-Lookup map1 y)))
			      (declare (ignore ignore))
			      (WB-Map-Tree-With
				map1 y (WB-Set-Tree-With set-tree-1 x))))))))

(defmethod less ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x))
	(map1 (wb-2-relation-map1 br)))
    (if (not found?)
	br
      (let ((new-set-tree (WB-Set-Tree-Less set-tree y)))
	(if (eq new-set-tree set-tree)
	    br
	  (make-wb-2-relation (1- (wb-2-relation-size br))
			   (if new-set-tree
			       (WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree)
			     (WB-Map-Tree-Less (wb-2-relation-map0 br) x))
			   (and map1
				(let ((ignore set-tree
					(WB-Map-Tree-Lookup map1 y))
				      ((new-set-tree (WB-Set-Tree-Less set-tree x))))
				  (declare (ignore ignore))
				  (if new-set-tree
				      (WB-Map-Tree-With map1 y new-set-tree)
				    (WB-Map-Tree-Less map1 y))))))))))

(defmethod union ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (let ((new-size 0)
	((new-map0 (WB-Map-Tree-Union (wb-2-relation-map0 br1) (wb-2-relation-map0 br2)
				      (lambda (s1 s2)
					(let ((s (WB-Set-Tree-Union s1 s2)))
					  (incf new-size (WB-Set-Tree-Size s))
					  s))))
	 (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			(progn
			  (get-inverse br1)
			  (get-inverse br2)
			  (WB-Map-Tree-Union (wb-2-relation-map1 br1)
					     (wb-2-relation-map1 br2)
					     #'WB-Set-Tree-Union))))))
    (make-wb-2-relation new-size new-map0 new-map1)))

(defmethod intersection ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (let ((new-size 0)
	((new-map0 (WB-Map-Tree-Intersect (wb-2-relation-map0 br1)
					  (wb-2-relation-map0 br2)
					  (lambda (ignore s1 s2)
					    (declare (ignore ignore))
					    (let ((s (WB-Set-Tree-Intersect s1 s2)))
					      (incf new-size (WB-Set-Tree-Size s))
					      s))))
	 (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			(progn
			  (get-inverse br1)
			  (get-inverse br2)
			  (WB-Map-Tree-Intersect (wb-2-relation-map1 br1)
						 (wb-2-relation-map1 br2)
						 #'WB-Set-Tree-Intersect))))))
    (make-wb-2-relation new-size new-map0 new-map1)))

(defgeneric join (relation-a column-a relation-b column-b)
  (:documentation
    "A relational equijoin, matching up `column-a' of `relation-a' with `column-b' of
`relation-b'.  For a binary relation, the columns are named 0 (domain) and 1 (range)."))

(defmethod join ((bra wb-2-relation) cola (brb wb-2-relation) colb)
  (let ((map0a map1a (ecase cola
		       (1 (values (wb-2-relation-map0 bra) (wb-2-relation-map1 bra)))
		       (0 (progn
			    (get-inverse bra)
			    (values (wb-2-relation-map1 bra)
				    (wb-2-relation-map0 bra))))))
	(map0b map1b (ecase colb
		       (0 (values (wb-2-relation-map0 brb) (wb-2-relation-map1 brb)))
		       (1 (progn
			    (get-inverse brb)
			    (values (wb-2-relation-map1 brb)
				    (wb-2-relation-map0 brb))))))
	(new-map0 nil)
	(new-map1 nil)
	(new-size 0))
    (Do-WB-Map-Tree-Pairs (x ys map0a)
      (Do-WB-Set-Tree-Members (y ys)
	(let ((ignore s (WB-Map-Tree-Lookup map0b y)))
	  (declare (ignore ignore))
	  (when s
	    (let ((ignore prev (WB-Map-Tree-Lookup new-map0 x))
		  ((new (WB-Set-Tree-Union prev s))))
	      (declare (ignore ignore))
	      (incf new-size (- (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev)))
	      (setq new-map0 (WB-Map-Tree-With new-map0 x new)))))))
    (when (or map1a map1b)
      (when (null map1b)
	(setq map1b (get-inverse brb)))
      (when (null map1a)
	(setq map1a (get-inverse bra)))
      (Do-WB-Map-Tree-Pairs (x ys map1b)
	(Do-WB-Set-Tree-Members (y ys)
	  (let ((ignore s (WB-Map-Tree-Lookup map1a y)))
	    (declare (ignore ignore))
	    (when s
	      (let ((ignore prev (WB-Map-Tree-Lookup new-map1 x)))
		(declare (ignore ignore))
		(setq new-map1
		      (WB-Map-Tree-With new-map1 x (WB-Set-Tree-Union prev s)))))))))
    (make-wb-2-relation new-size new-map0 new-map1)))


(defmethod compose ((rel wb-2-relation) (fn function))
  (2-relation-fn-compose rel fn))

(defmethod compose ((rel wb-2-relation) (fn symbol))
  (2-relation-fn-compose rel (coerce fn 'function)))

(defmethod compose ((rel wb-2-relation) (fn map))
  (2-relation-fn-compose rel fn))

(defmethod compose ((rel wb-2-relation) (fn seq))
  (2-relation-fn-compose rel fn))

(defmethod compose ((rel1 wb-2-relation) (rel2 wb-2-relation))
  (join rel1 1 rel2 0))

(defun 2-relation-fn-compose (rel fn)
  (let ((new-size 0)
	((new-map0 (gmap :wb-map (lambda (x ys)
				   (let ((result nil))
				     (Do-WB-Set-Tree-Members (y ys)
				       (setq result (WB-Set-Tree-With result (@ fn y))))
				     (incf new-size (WB-Set-Tree-Size result))
				     (values x result)))
			(:wb-map (make-wb-map (wb-2-relation-map0 rel)))))))
    (make-wb-2-relation new-size
			(wb-map-contents new-map0)
			nil)))


(defgeneric internal-do-2-relation (br elt-fn value-fn))

(defmacro do-2-relation ((key val br &optional value) &body body)
  `(block nil
     (internal-do-2-relation ,br (lambda (,key ,val) . ,body)
			      (lambda () ,value))))

(defmethod internal-do-2-relation ((br wb-2-relation) elt-fn value-fn)
  (Do-WB-Map-Tree-Pairs (x y-set (wb-2-relation-map0 br) (funcall value-fn))
    (Do-WB-Set-Tree-Members (y y-set)
      (funcall elt-fn x y))))

(defmethod convert ((to-type (eql '2-relation)) (br 2-relation) &key)
  br)

(defmethod convert ((to-type (eql 'wb-2-relation)) (br wb-2-relation) &key)
  br)

(defmethod convert ((to-type (eql 'set)) (br 2-relation) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-2-relation (x y br)
      (setq result (WB-Set-Tree-With result (funcall pair-fn x y))))
    (make-wb-set result)))

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
      (map-to-sets-to-wb-2-relation m)
    (map-to-wb-2-relation m)))

(defmethod convert ((to-type (eql 'wb-2-relation)) (m map) &key from-type)
  "If `from-type' is the symbol `map-to-sets', the range elements must all be
sets, and the result pairs each domain element with each member of the
corresponding range set.  Otherwise, the result pairs each domain element
with the corresponding range element directly."
  (if (eq from-type 'map-to-sets)
      (map-to-sets-to-wb-2-relation m)
    (map-to-wb-2-relation m)))

(defun map-to-sets-to-wb-2-relation (m)
  (let ((size 0)
	((new-tree (WB-Map-Tree-Compose
		     (wb-map-contents m)
		     #'(lambda (s)
			 (let ((s (wb-set-contents (convert 'wb-set s))))
			   (incf size (WB-Set-Tree-Size s))
			   s))))))
    (make-wb-2-relation size new-tree nil)))

(defun map-to-wb-2-relation (m)
  (let ((new-tree (WB-Map-Tree-Compose (wb-map-contents m)
				       #'(lambda (x) (WB-Set-Tree-With nil x)))))
    (make-wb-2-relation (size m) new-tree nil)))

(defmethod convert ((to-type (eql '2-relation)) (alist list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (list-to-wb-2-relation alist key-fn value-fn))

(defmethod convert ((to-type (eql 'wb-2-relation)) (alist list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (list-to-wb-2-relation alist key-fn value-fn))

(defun list-to-wb-2-relation (alist key-fn value-fn)
  (let ((m0 nil)
	(size 0)
	(key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (dolist (pr alist)
      (let ((k (funcall key-fn pr))
	    (v (funcall value-fn pr))
	    ((found? prev (WB-Map-Tree-Lookup m0 k))
	     ((new (WB-Set-Tree-With prev v)))))
	(declare (ignore found?))
	(when (> (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev))
	  (incf size)
	  (setq m0 (WB-Map-Tree-With m0 k new)))))
    (make-wb-2-relation size m0 nil)))

(defmethod convert ((to-type (eql '2-relation))
		    (s seq)
		    &key key-fn (value-fn #'identity))
  (convert 'wb-2-relation s :key-fn key-fn :value-fn value-fn))

(defmethod convert ((to-type (eql 'wb-2-relation))
		    (s seq)
		    &key key-fn (value-fn #'identity))
  (let ((m0 nil)
	(size 0)
	(key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (do-seq (row s)
      (let ((k (funcall key-fn row))
	    (v (funcall value-fn row))
	    ((found? prev (WB-Map-Tree-Lookup m0 k))
	     ((new (WB-Set-Tree-With prev v)))))
	(declare (ignore found?))
	(when (> (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev))
	  (incf size)
	  (setq m0 (WB-Map-Tree-With m0 k new)))))
    (make-wb-2-relation size m0 nil)))

(defmethod convert ((to-type (eql 'map)) (br wb-2-relation) &key)
  "This conversion requires the relation to be functional, and returns
a map representing the function; that is, the relation must map each
domain value to a single range value, and the returned map maps that
domain value to that range value."
  (2-relation-to-wb-map br))

(defmethod convert ((to-type (eql 'wb-map)) (br wb-2-relation) &key)
  "This conversion requires the relation to be functional, and returns
a map representing the function; that is, the relation must map each
domain value to a single range value, and the returned map maps that
domain value to that range value."
  (2-relation-to-wb-map br))

(defun 2-relation-to-wb-map (br)
  (let ((m nil))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (let ((sz (WB-Set-Tree-Size s)))
	(unless (= 1 sz)
	  (error "2-relation maps ~A to ~D values" x sz))
	(setq m (WB-Map-Tree-With m x (WB-Set-Tree-Arb s)))))
    (make-wb-map m)))

(defmethod convert ((to-type (eql 'map-to-sets)) (br wb-2-relation) &key)
  "This conversion returns a map mapping each domain value to the set of
corresponding range values."
  (make-wb-map (WB-Map-Tree-Compose (wb-2-relation-map0 br) #'make-wb-set)))

(defgeneric conflicts (2-relation)
  (:documentation
    "Returns a 2-relation containing only those pairs of `2-relation' whose domain value
is mapped to multiple range values."))

(defmethod conflicts ((br wb-2-relation))
  (let ((m0 nil)
	(size 0))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (when (> (WB-Set-Tree-Size s) 1)
	(setq m0 (WB-Map-Tree-With m0 x s))
	(incf size (WB-Set-Tree-Size s))))
    (make-wb-2-relation size m0 nil)))

(defun print-wb-2-relation (br stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream "#{+ ")
      (let ((i 0))
	(do-2-relation (x y br)
	  (when (> i 0)
	    (format stream " "))
	  (when (and *print-length* (>= i *print-length*))
	    (format stream "...")
	    (return))
	  (incf i)
	  (let ((*print-level* (and *print-level* (1- *print-level*))))
	    (write (list x y) :stream stream)))
	(when (> i 0)
	  (format stream " ")))
      (format stream "+}"))))

(defmethod iterator ((rel wb-2-relation) &key)
  (let ((outer (Make-WB-Map-Tree-Iterator-Internal (wb-2-relation-map0 rel)))
	(cur-dom-elt nil)
	(inner nil))
    (lambda (op)
      (ecase op
	(:get (if (WB-Map-Tree-Iterator-Done? outer)
		  (values nil nil nil)
		(progn
		  (when (or (null inner) (WB-Set-Tree-Iterator-Done? inner))
		    (let ((dom-elt inner-tree (WB-Map-Tree-Iterator-Get outer)))
		      (setq cur-dom-elt dom-elt)
		      (assert inner-tree)	; must be nonempty
		      (setq inner (Make-WB-Set-Tree-Iterator-Internal inner-tree))))
		  (values cur-dom-elt (WB-Set-Tree-Iterator-Get inner) t))))
	(:done? (WB-Map-Tree-Iterator-Done? outer))
	(:more? (not (WB-Map-Tree-Iterator-Done? outer)))))))

(def-gmap-arg-type :2-relation (rel)
  "Yields each pair of `rel', as two values."
  `((iterator ,rel)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    (:values 2 #'(lambda (it) (declare (type function it)) (funcall it ':get)))))

(def-gmap-arg-type :wb-2-relation (rel)
  "Yields each pair of `rel', as two values."
  `((iterator ,rel)
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    (:values 2 #'(lambda (it) (declare (type function it)) (funcall it ':get)))))

(def-gmap-res-type :2-relation (&key filterp)
  "Consumes two values from the mapped function; returns a 2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (alist x y) (cons (cons x y) alist)))
	#'(lambda (alist) (list-to-wb-2-relation alist #'car #'cdr))
	,filterp))

(def-gmap-res-type :wb-2-relation (&key filterp)
  "Consumes two values from the mapped function; returns a 2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (alist x y) (cons (cons x y) alist)))
	#'(lambda (alist) (list-to-wb-2-relation alist #'car #'cdr))
	,filterp))


(define-cross-type-compare-methods relation)

(defmethod compare ((a wb-2-relation) (b wb-2-relation))
  (WB-Map-Tree-Compare (wb-2-relation-map0 a) (wb-2-relation-map0 b)
		       #'WB-Set-Tree-Compare))

(defmethod verify ((br wb-2-relation))
  ;; Slow, but thorough.
  (and (WB-Map-Tree-Verify (wb-2-relation-map0 br))
       (WB-Map-Tree-Verify (wb-2-relation-map1 br))
       (let ((size 0))
	 (and (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br) t)
		(WB-Set-Tree-Verify s)
		(incf size (WB-Set-Tree-Size s))
		(or (null (wb-2-relation-map1 br))
		    (Do-WB-Set-Tree-Members (y s)
		      (let ((ignore s1 (WB-Map-Tree-Lookup (wb-2-relation-map1 br) y)))
			(declare (ignore ignore))
			(unless (WB-Set-Tree-Member? s1 x)
			  (format *debug-io* "Map discrepancy in wb-2-relation")
			  (return nil))))))
	      (or (= size (wb-2-relation-size br))
		  (progn (format *debug-io* "Size discrepancy in wb-2-relation")
			 nil))))
       (or (null (wb-2-relation-map1 br))
	   (let ((size 0))
	     (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map1 br))
	       (declare (ignore x))
	       (WB-Set-Tree-Verify s)
	       (incf size (WB-Set-Tree-Size s)))
	     (or (= size (wb-2-relation-size br))
		 (progn (format *debug-io*  "Size discrepancy in wb-2-relation")
			nil))))))


(defgeneric transitive-closure (2-relation set)
  (:documentation
    "The transitive closure of the set over the relation.  The relation may
also be supplied as a function returning a set."))

(defmethod transitive-closure ((fn function) (s set))
  (set-transitive-closure fn s))

(defmethod transitive-closure ((r 2-relation) (s set))
  (set-transitive-closure r s))

(defun set-transitive-closure (r s)
  ;; This could probably use a little more work.
  (let ((workset (set-difference
		   (reduce #'union (image r (convert 'seq s)) :initial-value (set))
		   s))
	(result s))
    (while (nonempty? workset)
      (let ((x (arb workset)))
	(removef workset x)
	(adjoinf result x)
	(unionf workset (set-difference (@ r x) result))))
    result))


(defmacro 2-relation (&rest args)
  "Constructs a 2-relation of the default implementation according to the supplied
argument subforms.  Each argument subform can be a list of the form (`key-expr'
`value-expr'), denoting a mapping from the value of `key-expr' to the value of
`value-expr'; or a list of the form ($ `expression'), in which case the
expression must evaluate to a 2-relation, all of whose mappings will be
included in the result."
  (expand-2-relation-constructor-form '2-relation args))

(defmacro wb-2-relation (&rest args)
  "Constructs a wb-2-relation according to the supplied argument subforms.
Each argument subform can be a list of the form (`key-expr' `value-expr'),
denoting a mapping from the value of `key-expr' to the value of `value-expr';
or a list of the form ($ `expression'), in which case the expression must
evaluate to a 2-relation, all of whose mappings will be included in the
result."
  (expand-2-relation-constructor-form '2-relation args))

(defun expand-2-relation-constructor-form (type-name args)
  (let ((empty-form (ecase type-name
		      (2-relation '(empty-2-relation))
		      (wb-2-relation '(empty-wb-2-relation)))))
    (labels ((recur (args result)
	       (cond ((null args) result)
		     ((not (and (listp (car args))
				(= (length (car args)) 2)))
		      (error "Arguments to ~S must all be pairs expressed as 2-element~@
			      lists, or ($ x) subforms -- not ~S"
			     type-name (car args)))
		     ((eq (caar args) '$)
		      (if (eq result empty-form)
			  (recur (cdr args) (cadar args))
			(recur (cdr args) `(union ,result ,(cadar args)))))
		     (t
		      (recur (cdr args) `(with ,result ,(caar args) ,(cadar args)))))))
      (recur args empty-form))))


;;; ================================================================================
;;; List relations

;;; A list relation is a general relation (i.e. of arbitrary arity >= 2) whose
;;; tuples are in list form.  List relations support a `query' operation that
;;; takes, along with the relation, two lists, each of length equal to the
;;; arity, called the "pattern" and "metapattern".  For each position, if the
;;; metapattern contains `nil', the query is not constrained by that position
;;; (the corresponding position in the pattern is ignored); if the metapattern
;;; contains `t' or `:single', then the result set contains only those tuples
;;; with the same value in that position as the pattern has.  The difference
;;; between `t' and `:single' has to do with indexing.  For each metapattern
;;; that is actually used, an index is constructed if not previously present,
;;; and then is maintained incrementally.  If the metapattern has `t' in a
;;; location, the resulting index will contain all values for that location;
;;; if it has `:single', the resulting index will contain only those values
;;; that have actually appeared in a query pattern with this metapattern.



(defstruct (list-relation
	    (:include relation)
	    (:constructor nil)
	    (:predicate list-relation?)
	    (:copier nil))
  "The abstract class for FSet list relations.  It is a structure class.
A list relation is a general relation (i.e. of arbitrary arity >= 2) whose
tuples are in list form.")

(defstruct (wb-list-relation
	    (:include list-relation)
	    (:constructor make-wb-list-relation (arity tuples indices))
	    (:predicate wb-list-relation?)
	    (:print-function print-wb-list-relation)
	    (:copier nil))
  "A class of functional relations of arbitrary arity >= 2, whose tuples
are in list form."
  arity
  tuples
  ;; a map from augmented metapattern to map from reduced tuple to set of tuples
  indices)


(defun empty-list-relation (&optional arity)
  "We allow the arity to be temporarily unspecified; it will be taken from
the first tuple added, or the first query."
  (unless (or (null arity) (and (integerp arity) (>= arity 1)))
    (error "Invalid arity"))
  (empty-wb-list-relation arity))

(defun empty-wb-list-relation (arity)
  "We allow the arity to be temporarily unspecified; it will be taken from
the first tuple added, or the first query."
  ;; If arity = 1 it's just a set... but what the heck...
  (unless (or (null arity) (and (integerp arity) (>= arity 1)))
    (error "Invalid arity"))
  (make-wb-list-relation arity (set) (map)))

(defmethod arity ((rel wb-list-relation))
  "Will return `nil' if the arity is not yet specified; see `empty-list-relation'."
  (wb-list-relation-arity rel))

(defmethod empty? ((rel wb-list-relation))
  (empty? (wb-list-relation-tuples rel)))

(defmethod size ((rel wb-list-relation))
  (size (wb-list-relation-tuples rel)))

(defmethod arb ((rel wb-list-relation))
  (arb (wb-list-relation-tuples rel)))

(defmethod contains? ((rel wb-list-relation) tuple)
  (contains? (wb-list-relation-tuples rel) tuple))

(defgeneric query (relation pattern metapattern)
  (:documentation
    "Along with the relation, takes two lists, each of length equal to the
arity, called the `pattern' and `metapattern'; returns a set of tuples
satisfying the query.  For each position, if the metapattern contains `nil',
the query is not constrained by that position (the corresponding position in
the pattern is ignored); if the metapattern contains `t' or `:single', then
the result set contains only those tuples with the same value in that
position as the pattern has.  The difference between `t' and `:single' has
to do with indexing.  For each metapattern that is actually used, an index
is constructed if not previously present, and then is maintained
incrementally.  If the metapattern has `t' in a location, the resulting
index will contain all values for that location; if it has `:single', the
resulting index will contain only those values that have actually appeared
in a query pattern with this metapattern."))

;;; `:single' is implemented, but not necessarily well enough that you'd want to
;;; use it.
(defmethod query ((rel wb-list-relation) (pattern list) (metapattern list))
  (let ((arity (wb-list-relation-arity rel)))
    (if (null arity)
	;; We don't know the arity yet, which means there are no tuples.
	(set)
      (progn
	(unless (and (= (length pattern) arity)
		     (= (length metapattern) arity))
	  (error "Pattern or metapattern is of the wrong length"))
	(if (every #'identity metapattern)
	    (if (contains? rel pattern) (set pattern) (set))
	  (let ((augmented-mp (augmented-mp pattern metapattern))
		((reduced-tuple (reduced-tuple pattern augmented-mp))
		 (index (@ (wb-list-relation-indices rel) augmented-mp))))
	    (if index
		(@ index reduced-tuple)
	      (progn
		
		(let ((index-results
			(remove nil (mapcar (lambda (index mp-elt pat-elt)
					      (and index
						   (@ index (and (eq mp-elt t)
								 (list pat-elt)))))
					    (get-indices rel augmented-mp)
					    augmented-mp pattern))))
		  ;; &&& We also want to build composite indices under some
		  ;; circumstances -- e.g. if the result set is much smaller
		  ;; than the smallest of `index-results'.
		  (if index-results
		      (reduce #'intersection
			      (sort index-results #'> :key #'size))
		    (wb-list-relation-tuples rel)))))))))))

;;; &&& Another nail in the coffin of `:single'... should just rip it out...
(defgeneric query-multi (rel pattern metapattern)
  (:documentation
   "Like `query' (q.v.), except that `pattern' is a list of sets of values
rather than a list of values.  Returns all tuples in the relation for which
each value is a member of the corresponding set in the pattern.  `:single'
in the metapattern is not accepted."))

(defmethod query-multi ((rel wb-list-relation) (pattern list) (metapattern list))
  (let ((arity (wb-list-relation-arity rel)))
    (if (null arity)
	;; We don't know the arity yet, which means there are no tuples.
	(set)
      (progn
	(unless (and (= (length pattern) arity)
		     (= (length metapattern) arity))
	  (error "Pattern or metapattern is of the wrong length"))
	;; Without :single, the augmented-mp is just the metapattern.
	(when (member ':single metapattern)
	  (error "~S doesn't take ~S" 'query-multi ':single))
	(if (every (fn (s) (= (size s) 1)) pattern)
	    (query rel (mapcar #'arb pattern) metapattern)
	  (let ((index-results
		  (remove nil
			  (mapcar (lambda (index pat-elt)
				    (and index
					 (gmap :union
					       (fn (pat-elt-elt)
						 (@ index (list pat-elt-elt)))
					       (:set pat-elt))))
				  (get-indices rel metapattern)
				  pattern))))
	    (if index-results
		(reduce #'intersection
			(sort index-results #'> :key #'size))
	      (wb-list-relation-tuples rel))))))))

(defun get-indices (rel augmented-mp)
  "Returns a list giving the index to use for each element of `augmented-mp'."
  (flet ((make-mp (i elt)
	   (let ((mp nil)
		 (arity (wb-list-relation-arity rel)))
	     (dotimes (j arity)
	       (push (and (= i (- arity j 1)) elt) mp))
	     mp)))
    ;; First we see what indices exist on each position.
    (let ((ex-inds (gmap :list
			 (lambda (mp-elt i)
			   (and mp-elt (or (@ (wb-list-relation-indices rel)
					      (make-mp i mp-elt))
					   (and (not (eq mp-elt t))
						(@ (wb-list-relation-indices rel)
						   (make-mp i t))))))
			 (:list augmented-mp)
			 (:index 0)))
	  ((unindexed (mapcar (lambda (index mp-elt)
				(and (null index) mp-elt))
			      ex-inds augmented-mp))))
      ;; Now, if there were any instantiated positions for which an index did
      ;; not exist, construct indices for them.
      (unless (every #'null unindexed)
	(let ((saved-mps (gmap :list (lambda (unind i)
				       (and unind (make-mp i unind)))
			       (:list unindexed)
			       (:index 0)))
	      (new-indices (make-array (length augmented-mp)
				       :initial-element (empty-map (set)))))
	  (do-set (tuple (wb-list-relation-tuples rel))
	    (gmap nil (lambda (tuple-elt unind saved-mp i)
			(when (and unind
				   (or (eq unind t)
				       (equal? tuple-elt (cdr unind))))
			  (adjoinf (@ (svref new-indices i)
				      (reduced-tuple tuple saved-mp))
				   tuple)))
		  (:list tuple)
		  (:list unindexed)
		  (:list saved-mps)
		  (:index 0)))
	  (gmap nil (lambda (saved-mp new-index)
		      (when saved-mp
			(setf (@ (wb-list-relation-indices rel) saved-mp) new-index)))
		(:list saved-mps)
		(:vector new-indices))
	  (setq ex-inds (gmap :list (lambda (ex-ind saved-mp new-index)
				      (or ex-ind (and saved-mp new-index)))
			      (:list ex-inds)
			      (:list saved-mps)
			      (:vector new-indices)))))
      ;; &&& If we just built a complete index that subsumes any single-value indices,
      ;; need to discard the latter.
      ;; &&& Also, if the total size of the single-value indices we build for any
      ;; position gets large enough, we should replace them all with a complete index.
      ex-inds)))

(defmethod with ((rel wb-list-relation) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-list-relation)
  (let ((arity (or (wb-list-relation-arity rel)
		   (length tuple))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not equal arity, ~D"
	     (length tuple) arity))
    (if (contains? (wb-list-relation-tuples rel) tuple)
	rel
      (make-wb-list-relation arity (with (wb-list-relation-tuples rel) tuple)
			     ;; Hmm, methinks we need to index the index map...
			     (image (lambda (aug-mp rt-map)
				      (if (augmented-mp-matches? aug-mp tuple)
					  (let ((rt (reduced-tuple tuple aug-mp)))
					    (values aug-mp
						    (with rt-map rt
							  (with (@ rt-map rt) tuple))))
					(values aug-mp rt-map)))
				    (wb-list-relation-indices rel))))))

(defmethod less ((rel wb-list-relation) tuple &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-list-relation)
  (let ((arity (or (wb-list-relation-arity rel)
		   (length tuple))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not equal arity, ~D"
	     (length tuple) arity))
    (if (not (contains? (wb-list-relation-tuples rel) tuple))
	rel
      (make-wb-list-relation arity (less (wb-list-relation-tuples rel) tuple)
			     (image (lambda (aug-mp rt-map)
				      (if (augmented-mp-matches? aug-mp tuple)
					  (let ((rt (reduced-tuple tuple aug-mp)))
					    (values aug-mp
						    (with rt-map rt
							  (less (@ rt-map rt) tuple))))
					(values aug-mp rt-map)))
				    (wb-list-relation-indices rel))))))

;;; &&& I suppose that instead of consing these things up all the time we could
;;; have a special pattern object with special compare methods against lists that
;;; would compare only the desired positions.  L8r...
(defun reduced-tuple (tuple augmented-mp)
  "Returns a list of those members of `tuple' corresponding to instantiated
positions in the original pattern."
  (if (every (lambda (x) (eq x t)) augmented-mp) tuple
    (gmap (:list :filterp #'identity)		; omits nil
	  (lambda (pat-elt mp-elt)
	    (and (eq mp-elt t) pat-elt))
	  (:list tuple)
	  (:list augmented-mp))))

(defun augmented-mp (pattern metapattern)
  "Returns a list, of the same length as the pattern, which is like the
metapattern except that each `:single' has been replaced by a cons of
`:single' and the corresponding pattern element."
  (if (not (member ':single metapattern)) metapattern
    (mapcar (lambda (pat-elt mp-elt)
	      (if (eq mp-elt ':single) (cons ':single pat-elt)
		mp-elt))
	    pattern metapattern)))

(defun augmented-mp-matches? (augmented-mp tuple)
  (every (lambda (mp-elt tuple-elt)
	   (or (eq mp-elt nil) (eq mp-elt t)
	       (and (consp mp-elt) (eq (car mp-elt) ':single)
		    (equal? tuple-elt (cdr mp-elt)))))
	 augmented-mp tuple))



(defgeneric internal-do-list-relation (rel elt-fn value-fn))

(defmacro do-list-relation ((tuple rel &optional value) &body body)
  `(block nil
     (internal-do-list-relation ,rel (lambda (,tuple) . ,body)
				(lambda () ,value))))

(defmethod internal-do-list-relation ((rel wb-list-relation) elt-fn value-fn)
  (Do-WB-Set-Tree-Members (tuple (wb-set-contents (wb-list-relation-tuples rel))
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

#||

Okay, this is a start, but:

() Don't we want to do better meta-indexing, so adding a tuple doesn't require
iterating through all the indices?

() I'm not creating composite indices yet.  The plan is straightforward -- create
one when the size of the final result set is <= the square root of the size of
the smallest index set.  This is easy, but how do subsequent queries find the
composite index?

[Later]  I think that for now, the single-value index feature is an unnecessary
complication.  Without it, there either exists an index on a column, or not.

As for composite indices, I think the right way to find them will be with a
discrimination tree (or DAG), but I'm not going to bother with them yet either.

||#


;;; A query registry to be used with `list-relation'.  Register queries with
;;; `with-query', supplying a pattern and metapattern.  The queries themselves
;;; are uninterpreted except that they are kept in sets (so CL closures are not
;;; a good choice).  `lookup' returns the set of queries that match the supplied
;;; tuple.
(defstruct (query-registry
	     (:constructor make-query-registry (arity indices key-index)))
  arity
  ;; A map from augmented metapattern to map from reduced tuple to set of queries.
  ;; &&& Not worrying for now whether this does anything reasonable with `:single'.
  indices
  ;; A map from every "key", i.e., value used in an instantiated position in a
  ;; pattern, to map from augmented metapattern to set of reduced tuples in which
  ;; they were used.
  key-index)

(defun empty-query-registry (&optional arity)
  (unless (or (null arity) (and (integerp arity) (>= arity 1)))
    (error "Invalid arity"))
  (make-query-registry arity (empty-map (empty-map (set)))
		       (empty-map (empty-map (set)))))

(defmethod arity ((reg query-registry))
  (query-registry-arity reg))

(defmethod with-query ((reg query-registry) (pattern list) (metapattern list) query)
  (let ((arity (or (query-registry-arity reg)
		   (length pattern))))
    (unless (and (= (length pattern) arity)
		 (= (length metapattern) arity))
      (error "Pattern or metapattern is of the wrong length"))
    (let ((augmented-mp (augmented-mp pattern metapattern))
	  ((reduced-tuple (reduced-tuple pattern augmented-mp))
	   ((prev-1 (@ (query-registry-indices reg) augmented-mp))
	    ((prev-2 (@ prev-1 reduced-tuple)))
	    (aug->red (map (augmented-mp (set reduced-tuple)) :default (set))))))
      (make-query-registry arity
			   (with (query-registry-indices reg) augmented-mp
				 (with prev-1 reduced-tuple
				       (with prev-2 query)))
			   (map-union (query-registry-key-index reg)
				      (gmap (:map :default (empty-map (set)))
					    (fn (key) (values key aug->red))
					    (:list reduced-tuple))
				      (lambda (x y) (map-union x y #'union)))))))

(defmethod less-query ((reg query-registry) (pattern list) (metapattern list) query)
  (let ((arity (or (query-registry-arity reg)
		   (length pattern))))
    (unless (and (= (length pattern) arity)
		 (= (length metapattern) arity))
      (error "Pattern or metapattern is of the wrong length"))
    (let ((augmented-mp (augmented-mp pattern metapattern))
	  ((reduced-tuple (reduced-tuple pattern augmented-mp))
	   ((prev-1 (@ (query-registry-indices reg) augmented-mp))
	    ((prev-2 (@ prev-1 reduced-tuple))))))
      (make-query-registry arity
			   (with (query-registry-indices reg) augmented-mp
				 (with prev-1 reduced-tuple
				       (less prev-2 query)))
			   ;; &&& For now.
			   (query-registry-key-index reg)))))

(defmethod all-queries ((reg query-registry))
  (gmap :union (fn (_aug-mp submap)
		 (gmap :union (fn (_red-tup queries)
				queries)
		       (:map submap)))
	(:map (query-registry-indices reg))))

(defmethod lookup ((reg query-registry) tuple)
  "Returns all queries in `reg' whose patterns match `tuple'."
  (let ((arity (or (query-registry-arity reg)
		   (length tuple))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not match arity, ~D"
	     (length tuple) arity))
    (gmap :union (lambda (aug-mp rt-map)
		   (@ rt-map (reduced-tuple tuple aug-mp)))
	  (:map (query-registry-indices reg)))))

(defmethod lookup-multi ((reg query-registry) set-tuple)
  "Here `set-tuple' contains a set of values in each position.  Returns
all queries in `reg' whose patterns match any member of the cartesian
product of the sets."
  (let ((arity (or (query-registry-arity reg)
		   (length set-tuple))))
    (unless (and (listp set-tuple) (= (length set-tuple) arity))
      (error "Length of tuple, ~D, does not match arity, ~D"
	     (length set-tuple) arity))
    ;; Ugh.  At least, computing the cartesian product of the reduced set-tuple
    ;; will frequently be faster than computing that of the original.  Still,
    ;; maybe we &&& need to redesign the indexing scheme here...
    (gmap :union (lambda (aug-mp rt-map)
		   (gmap :union (fn (tuple)
				  (@ rt-map tuple))
			 (:seq (cartesian-product (reduced-tuple set-tuple aug-mp)))))
	  (:map (query-registry-indices reg)))))

;;; Since all the members are known to be distinct, we return a seq rather
;;; than pay the setification cost... a little inelegant, though.
(defmethod cartesian-product ((sets list))
  (if (null sets)
      (seq nil)
    (gmap :concat (fn (tail)
		    (gmap :seq (fn (x) (cons x tail))
			  (:set (car sets))))
	  (:seq (cartesian-product (cdr sets))))))

(defmethod forward-key ((reg query-registry) from-key to-key)
  "Returns a new query-registry in which all queries whose patterns used
`from-key' (in an instantiated position) now use `to-key' in that position
instead."
  (let ((key-idx-submap (@ (query-registry-key-index reg) from-key))
	;; We'll generate garbage maintaining the map, but then the tuple instances
	;; will be shared.
	(subst-cache (map)))
    (flet ((get-subst (tuple)
	     (or (@ subst-cache tuple)
		 (setf (@ subst-cache tuple)
		       (substitute to-key from-key tuple)))))
      (make-query-registry
	(query-registry-arity reg)
	(image (fn (aug-mp submap)
		 (let ((red-tups (@ key-idx-submap aug-mp)))
		   (values aug-mp
			   (map-union (restrict-not submap red-tups)
				      (gmap (:map :default (set))
					    (fn (tup)
					      (let ((new-tup (get-subst tup)))
						(values new-tup
							(union (@ submap tup)
							       (@ submap new-tup)))))
					    (:set red-tups))
				      #'union))))
	       (query-registry-indices reg))
	;; Hehe, this is fun :-)  We need to update the indices for the other
	;; keys that occur along with `from-key' in tuples, and we don't want to
	;; walk the whole index to find them; but we already know what tuples are
	;; affected (the ones in `key-idx-submap'), so we work off of that.  Doing
	;; this functionally was interesting.
	(map-union (reduce (fn (kidx aug-mp tups)
			     (let ((to-update
				     (reduce (fn (x y) (map-union x y #'union))
					     (image (fn (tup)
						      (gmap :map
							    (fn (x) (values x (set tup)))
							    (:set (less (convert 'set tup)
									from-key))))
						    tups))))
			       (reduce (fn (kidx key tups)
					 (let ((prev-1 (@ kidx key))
					       ((prev-2 (@ prev-1 aug-mp))))
					   (with kidx key
						 (with prev-1 aug-mp
						       (union (set-difference prev-2 tups)
							      (image #'get-subst
								     tups))))))
				       to-update :initial-value kidx)))
			   key-idx-submap
			   :initial-value (less (query-registry-key-index reg) from-key))
		   (map (to-key (compose key-idx-submap
					 (fn (tups)
					   (image #'get-subst tups))))
			:default (empty-map (set)))
		   (fn (x y) (map-union x y #'union)))))))

(defmethod lookup-restricted ((reg query-registry) tuple key)
  "Returns all queries in `reg' whose patterns match `tuple' and which use
`key' (in an instantiated position) in their patterns."
  (let ((arity (or (query-registry-arity reg)
		   (length tuple))))
    (unless (and (listp tuple) (= (length tuple) arity))
      (error "Length of tuple, ~D, does not match arity, ~D"
	     (length tuple) arity))
    (gmap :union (lambda (aug-mp rt-map)
		   (@ rt-map (reduced-tuple tuple aug-mp)))
	  (:map (let ((key-idx-submap (@ (query-registry-key-index reg) key)))
		  (image (fn (aug-mp rt-map)
			   (values aug-mp (restrict rt-map (@ key-idx-submap aug-mp))))
			 (query-registry-indices reg)))))))

(defmethod lookup-multi-restricted ((reg query-registry) set-tuple keys)
  "Here `set-tuple' contains a set of values in each position.  Returns
all queries in `reg' whose patterns match any member of the cartesian
product of the sets and which use a member of `keys' in their patterns."
  (let ((arity (or (query-registry-arity reg)
		   (length set-tuple))))
    (unless (and (listp set-tuple) (= (length set-tuple) arity))
      (error "Length of tuple, ~D, does not match arity, ~D"
	     (length set-tuple) arity))
    (gmap :union (lambda (aug-mp rt-map)
		   (gmap :union (fn (tuple)
				  (@ rt-map tuple))
			 (:seq (cartesian-product (reduced-tuple set-tuple aug-mp)))))
	  (:map (let ((key-idx-submap
			(reduce (fn (x y) (map-union x y #'union))
				(image (query-registry-key-index reg) keys))))
		  (image (fn (aug-mp rt-map)
			   (values aug-mp (restrict rt-map (@ key-idx-submap aug-mp))))
			 (query-registry-indices reg)))))))

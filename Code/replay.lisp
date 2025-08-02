;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: replay.lisp
;;; Contents: Replay sets and maps.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)

;;; ================================================================================
;;; Replay sets

(defstruct (wb-replay-set
	     (:include replay-set)
	     (:constructor make-wb-replay-set (contents ordering org))
	     (:predicate wb-replay-set?)
	     (:print-function print-wb-replay-set)
	     (:copier nil))
  "A replay set is like a set, except that its iteration order is the order in which members
were added to it.  It does not support all set operations, but you can convert it to a set.
Note that in the current implementation, `less' on a replay set takes O(n) time.  Also, two
replay sets are equal only if they both contain the same elements and have the same iteration
order; if you just want to compare the contents, convert them to ordinary sets first.  Replay
sets are printed as \"#{= ... }\"."
  (contents nil :read-only t)
  (ordering nil :read-only t)
  (org nil :type tree-set-org :read-only t))

(defparameter *empty-wb-replay-set* (make-wb-replay-set nil nil +fset-default-tree-set-org+))

(defun empty-replay-set ()
  "Returns an empty replay set of the default implementation."
  *empty-wb-replay-set*)

(defmethod empty-instance-function ((class-name (eql 'replay-set)))
  'empty-replay-set)

(declaim (inline empty-wb-replay-set))
(defun empty-wb-replay-set (&optional compare-fn-name)
  (if (null compare-fn-name)
      *empty-wb-replay-set*
    (empty-wb-custom-replay-set compare-fn-name)))

(deflex +empty-wb-custom-replay-set-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-replay-set (compare-fn-name)
  (assert (and (symbolp compare-fn-name) (not (null compare-fn-name))))
  (if (eq compare-fn-name 'compare)
      *empty-wb-replay-set*
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-replay-set-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (tree-set-org-compare-fn (wb-replay-set-org prev-instance))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-replay-set-cache+)
	      (make-wb-replay-set nil nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defmethod empty-instance-function ((class-name (eql 'wb-replay-set)))
  'empty-wb-replay-set)

(defmethod compare-fn ((s wb-replay-set))
  (tree-set-org-compare-fn (wb-replay-set-org s)))

(defmethod empty? ((s wb-replay-set))
  (null (wb-replay-set-contents s)))

(defmethod size ((s wb-replay-set))
  (wb-set-tree-size (wb-replay-set-contents s)))

(defmethod arb ((s wb-replay-set))
  (let ((tree (wb-replay-set-contents s)))
    (if tree (values (wb-set-tree-arb tree) t)
      (values nil nil))))

(defmethod first ((s wb-replay-set))
  (let ((val? val (WB-Seq-Tree-Subscript (wb-replay-set-ordering s) 0)))
    (values val val?)))

(defmethod last ((s wb-replay-set))
  (let ((tree (wb-replay-set-ordering s))
	((val? val (WB-Seq-Tree-Subscript tree (1- (WB-Seq-Tree-Size tree))))))
    (values val val?)))

(defmethod least ((s wb-replay-set))
  (let ((tree (wb-replay-set-contents s)))
    (if tree (values (WB-Set-Tree-Least tree) t)
      (values nil nil))))

(defmethod greatest ((s wb-replay-set))
  (let ((tree (wb-replay-set-contents s)))
    (if tree (values (WB-Set-Tree-Greatest tree) t)
        (values nil nil))))

(defmethod at-index ((m wb-replay-set) index)
  (let ((ordering (wb-replay-set-ordering m))
	((size (wb-seq-tree-size ordering))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
	     :format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index m)))
    (let ((ignore val (wb-seq-tree-subscript ordering index)))
      (declare (ignore ignore))
      val)))

(defmethod contains? ((s wb-replay-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-set)
  (wb-set-tree-member? (wb-replay-set-contents s) x (tree-set-org-compare-fn (wb-replay-set-org s))))

(defmethod lookup ((s wb-replay-set) value)
  (wb-set-tree-find-equal (wb-replay-set-contents s) value
			  (tree-set-org-compare-fn (wb-replay-set-org s))))

(defmethod compare ((set1 replay-set) (set2 replay-set))
  "Fallback method for mixed implementations."
  (let ((size1 (size set1))
	(size2 (size set2))
	(compare-fn-1 (compare-fn set1))
	(compare-fn-2 (compare-fn set2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  ((gmap :and (fn (e1 e2) (let ((equal?-1 (equal?-cmp e1 e2 compare-fn-1))
					(equal?-2 (equal?-cmp e1 e2 compare-fn-2)))
				    (unless (eq equal?-1 equal?-2)
				      (error "Inconsistent equality in `compare' method for replay-sets"))
				    equal?-1))
		 (:arg seq (convert 'seq set1))
		 (:arg seq (convert 'seq set2)))
	   ':equal)
	  (t ':unequal))))

(defmethod compare ((set1 wb-replay-set) (set2 wb-replay-set))
  (let ((tsorg1 (wb-replay-set-org set1))
	(tsorg2 (wb-replay-set-org set2)))
    (if (eq (tree-set-org-compare-fn tsorg1) (tree-set-org-compare-fn tsorg2))
	(let ((comp (wb-set-tree-compare (wb-replay-set-contents set1) (wb-replay-set-contents set2)
					 (tree-set-org-compare-fn tsorg1))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (wb-replay-set-ordering set1) (wb-replay-set-ordering set2)
						 (tree-set-org-compare-fn tsorg1))))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(if (or (eq comp ':unequal) (eq ord-comp ':unequal))
		    ':unequal
		  ':equal)))))
      (call-next-method))))

(defmethod convert ((to-type (eql 'list)) (s wb-replay-set) &key)
  (convert 'list (convert 'seq s)))
(defmethod convert ((to-type (eql 'vector)) (s wb-replay-set) &key)
  (convert 'vector (convert 'seq s)))

(defmethod convert ((to-type (eql 'seq)) (s wb-replay-set) &key)
  (make-wb-seq (wb-replay-set-ordering s)))
(defmethod convert ((to-type (eql 'wb-seq)) (s wb-replay-set) &key)
  (make-wb-seq (wb-replay-set-ordering s)))

(defmethod convert ((to-type (eql 'set)) (s wb-replay-set) &key)
  (make-wb-set (wb-replay-set-contents s)))
(defmethod convert ((to-type (eql 'wb-set)) (s wb-replay-set) &key compare-fn-name)
  (convert 'wb-set (make-wb-set (wb-replay-set-contents s)) :compare-fn-name compare-fn-name))

(defmethod convert ((to-type (eql 'replay-set)) (s replay-set) &key)
  s)

(defmethod convert ((to-type (eql 'wb-replay-set)) (s replay-set) &key compare-fn-name)
  (let ((prototype (empty-wb-set compare-fn-name))
	((compare-fn (wb-set-compare-fn prototype))))
    (let ((contents nil))
      (do-wb-set-tree-members (x (wb-replay-set-contents s))
	(setq contents (wb-set-tree-with contents x compare-fn)))
      (make-wb-replay-set contents (wb-replay-set-ordering s) (wb-custom-set-org prototype)))))

(defmethod convert ((to-type (eql 'wb-replay-set)) (s wb-replay-set) &key compare-fn-name)
  (let ((prototype (empty-wb-set compare-fn-name))
	((compare-fn (wb-set-compare-fn prototype))))
    (if (eq (tree-set-org-compare-fn (wb-replay-set-org s)) compare-fn)
	s
      (let ((contents nil))
	(do-wb-set-tree-members (x (wb-replay-set-contents s))
	  (setq contents (wb-set-tree-with contents x compare-fn)))
	(make-wb-replay-set contents (wb-replay-set-ordering s) (wb-custom-set-org prototype))))))

(defmethod convert ((to-type (eql 'replay-set)) (s wb-set) &key)
  (make-wb-replay-set (wb-set-contents s) (wb-seq-contents (convert 'wb-seq s)) (wb-set-org s)))
(defmethod convert ((to-type (eql 'wb-replay-set)) (s wb-set) &key compare-fn-name)
  (let ((s (convert 'wb-set s :compare-fn-name compare-fn-name)))
    (make-wb-replay-set (wb-set-contents s) (wb-seq-contents (convert 'wb-seq s)) (wb-set-org s))))

(defmethod convert ((to-type (eql 'replay-set)) (l list) &key)
  (wb-replay-set-from-iterable l 'compare))
(defmethod convert ((to-type (eql 'wb-replay-set)) (l list) &key compare-fn-name)
  (wb-replay-set-from-iterable l compare-fn-name))

(defmethod convert ((to-type (eql 'replay-set)) (s seq) &key)
  (wb-replay-set-from-iterable s 'compare))
(defmethod convert ((to-type (eql 'wb-replay-set)) (s seq) &key compare-fn-name)
  (wb-replay-set-from-iterable s compare-fn-name))

(defmethod convert ((to-type (eql 'replay-set)) (s sequence) &key)
  (wb-replay-set-from-iterable s 'compare))
(defmethod convert ((to-type (eql 'wb-replay-set)) (s sequence) &key compare-fn-name)
  (wb-replay-set-from-iterable s compare-fn-name))

(defun wb-replay-set-from-iterable (s compare-fn-name)
  (let ((prototype (empty-wb-set compare-fn-name))
	((compare-fn (wb-set-compare-fn prototype)))
	(contents nil)
	(ordering nil))
    ;; Make sure the ordering doesn't wind up with duplicates!
    (do-elements (x s)
      (let ((new-contents (wb-set-tree-with contents x compare-fn)))
	(unless (eq new-contents contents)
	  (setq ordering (wb-seq-tree-append ordering x))
	  (setq contents new-contents))))
    (make-wb-replay-set contents ordering (wb-set-org prototype))))

(defmethod with ((s wb-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	((new-contents (wb-set-tree-with contents value #'compare))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents (wb-seq-tree-append (wb-replay-set-ordering s) value)
			  (wb-replay-set-org s)))))

(defmethod union ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: it adds the members
of `s2' to `s1', so the ordering of the result will be that of `s1' with any
new members appended."
  (cond ((empty? s2) s1)
	((empty? s1)
	 (if (wb-replay-set? s2) s2
	   (convert 'wb-replay-set s2)))
	(t
	 (let ((compare-fn (tree-set-org-compare-fn (wb-replay-set-org s1)))
	       (contents (wb-replay-set-contents s1))
	       (ordering (wb-replay-set-ordering s1)))
	   ;; This is O(n log m), rather than the usual O(m + n).
	   (do-set (x s2)
	     (let ((tmp (wb-set-tree-with contents x compare-fn)))
	       (unless (eq tmp contents)
		 (setq contents tmp)
		 (setq ordering (wb-seq-tree-append ordering x)))))
	   (make-wb-replay-set contents ordering (wb-replay-set-org s1))))))

(defmethod intersection ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: the ordering of the
result is that of `s1', filtered by membership in `s2'."
  (cond ((empty? s2) (empty-wb-replay-set))
	((empty? s1) s1)
	(t
	 (make-wb-replay-set (wb-set-contents (intersection (convert 'wb-set s1) (convert 'wb-set s2)))
			     (wb-seq-contents (filter s2 (convert 'wb-seq s1)))
			     (wb-replay-set-org s1)))))

;;; WARNING: linear-time operation!
(defmethod less ((s wb-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	(compare-fn (tree-set-org-compare-fn (wb-replay-set-org s)))
	((new-contents (wb-set-tree-less contents value compare-fn))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents
			  (let ((tree (wb-replay-set-ordering s)))
			    (wb-seq-tree-remove tree
						(or (position value (make-wb-seq tree) :test (equal?-fn compare-fn))
						    (error "Bug in `less' on `wb-replay-set'"))))
			  (wb-replay-set-org s)))))

(defmethod iterator ((s wb-replay-set) &key)
  (make-wb-seq-tree-iterator (wb-replay-set-ordering s)))

(defmethod internal-do-set ((s wb-replay-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (wb-replay-set-ordering s) (funcall value-fn))
    (funcall elt-fn x)))

(defun print-wb-replay-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{=" :suffix " }")
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

;;; A bit faster than `set', if you know it's a `wb-replay-set'.
(gmap:def-gmap-arg-type wb-replay-set (set)
  "Yields the elements of `set'."
  `((make-wb-seq-tree-iterator-internal (wb-replay-set-ordering ,set))
    #'wb-seq-tree-iterator-done?
    #'wb-seq-tree-iterator-get))

(gmap:def-gmap-res-type append-unique ()
  "Returns a list of the unique elements of the lists returned by the
mapped function, in the order in which they were first encountered."
  `((empty-replay-set)
    #'(lambda (rs new-elts)
	(dolist (x new-elts)
	  (includef rs x))
	rs)
    #'(lambda (rs) (convert 'list rs))))


;;; ================================================================================
;;; Replay maps

(defstruct (wb-replay-map
	     (:include replay-map)
	     (:constructor make-wb-replay-map (contents ordering org &optional default))
	     (:predicate wb-replay-map?)
	     (:print-function print-wb-replay-map)
	     (:copier nil))
  "A replay map is like a map, except that its iteration order is the order in which keys
were first added to it.  It does not support all map operations, but you can convert it
to a map.  Note that in the current implementation, `less' on a replay map takes O(n) time.
Also, two replay maps are equal only if they both contain the same pairs and have the same
iteration order; if you just want to compare the contents, convert them to ordinary maps
first.  Replay maps are printed as \"#{=| ... |}\"."
  (contents nil :read-only t)
  (ordering nil :read-only t)
  (org nil :type tree-map-org :read-only t))

(defparameter *empty-wb-replay-map* (make-wb-replay-map nil nil +fset-default-tree-map-org+))

(defun empty-replay-map (&optional default)
  (if default (make-wb-replay-map nil nil +fset-default-tree-map-org+ default)
    *empty-wb-replay-map*))

(defmethod empty-instance-function ((class-name (eql 'replay-map)))
  'empty-replay-map)

(defun empty-wb-replay-map (&optional default key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  *empty-wb-replay-map*
	(make-wb-replay-map nil nil +fset-default-tree-map-org+ default))
    (empty-wb-custom-replay-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))

(deflex +empty-wb-custom-replay-map-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-replay-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and (symbolp key-compare-fn-name) (not (null key-compare-fn-name))))
  (assert (and (symbolp val-compare-fn-name) (not (null val-compare-fn-name))))
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) *empty-wb-replay-map*
	(make-wb-replay-map nil nil +fset-default-tree-map-org+ default))
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-wb-custom-replay-map-cache+)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name)))
      (if (and prev-instance
	       (let ((prev-comp (wb-replay-map-org prev-instance)))
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn prev-comp))
		      (eq val-compare-fn (tree-map-org-val-compare-fn prev-comp))
		      (equal?-cmp default (map-default prev-instance) val-compare-fn))))
	  prev-instance
	(setf (gethash cache-key +empty-wb-custom-replay-map-cache+)
	      (make-wb-replay-map nil nil (make-tree-map-org key-compare-fn-name key-compare-fn
							     val-compare-fn-name val-compare-fn)
				  default))))))

(defmethod empty-instance-function ((class-name (eql 'wb-replay-map)))
  'empty-wb-replay-map)

(defmethod with-default ((m wb-replay-map) new-default)
  (make-wb-replay-map (wb-replay-map-contents m) (wb-replay-map-ordering m) (wb-replay-map-org m) new-default))

(defmethod key-compare-fn ((m wb-replay-map))
  (tree-map-org-key-compare-fn (wb-replay-map-org m)))

(defmethod val-compare-fn ((m wb-replay-map))
  (tree-map-org-val-compare-fn (wb-replay-map-org m)))

(defmethod key-compare-fn-name ((m wb-replay-map))
  (tree-map-org-key-compare-fn-name (wb-replay-map-org m)))

(defmethod val-compare-fn-name ((m wb-replay-map))
  (tree-map-org-val-compare-fn-name (wb-replay-map-org m)))

(defmethod empty? ((m wb-replay-map))
  (null (wb-replay-map-contents m)))

(defmethod arb ((m wb-replay-map))
  (let ((tree (wb-replay-map-contents m)))
    (if tree
	(let ((key val (wb-map-tree-arb-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod first ((m wb-replay-map))
  (let ((key? key (wb-seq-tree-subscript (wb-replay-map-ordering m) 0)))
    (values (if key? key (map-default m))
	    (and key? (let ((ignore val (wb-map-tree-lookup (wb-replay-map-contents m) key
							    (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
			(declare (ignore ignore))
			val))
	    key?)))

(defmethod last ((m wb-replay-map))
  (let ((tree (wb-replay-map-ordering m))
	((key? key (wb-seq-tree-subscript tree (1- (wb-seq-tree-size tree))))))
    (values (if key? key (map-default m))
	    (and key? (let ((ignore val (wb-map-tree-lookup (wb-replay-map-contents m) key
							    (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
			(declare (ignore ignore))
			val))
	    key?)))

(defmethod least ((m wb-replay-map))
  (let ((tree (wb-replay-map-contents m)))
    (if tree
	(let ((key val (wb-map-tree-least-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod greatest ((m wb-replay-map))
  (let ((tree (wb-replay-map-contents m)))
    (if tree
	(let ((key val (wb-map-tree-greatest-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod at-index ((m wb-replay-map) index)
  (let ((ordering (wb-replay-map-ordering m))
	((size (wb-seq-tree-size ordering))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
	     :format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index m)))
    (let ((ignore1 key (wb-seq-tree-subscript ordering index))
	  ((ignore2 val (wb-map-tree-lookup (wb-replay-map-contents m) key
					    (tree-map-org-key-compare-fn (wb-replay-map-org m))))))
      (declare (ignore ignore1 ignore2))
      (values key val))))

(defmethod size ((m wb-replay-map))
  (WB-Map-Tree-Size (wb-replay-map-contents m)))

(defmethod convert ((to-type (eql 'replay-map)) (m replay-map) &key)
  m)

;;; Eventually, we may have `ch-replay-map'...
(defmethod convert ((to-type (eql 'wb-replay-map)) (m replay-map)
		    &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (let ((prototype (empty-wb-map default key-compare-fn-name val-compare-fn-name))
	((proto-tmorg (wb-map-org prototype))
	 ((key-compare-fn (tree-map-org-key-compare-fn proto-tmorg))
	  (val-compare-fn (tree-map-org-val-compare-fn proto-tmorg)))))
    (let ((contents nil))
      (do-map (k v m)
	(setq contents (WB-Map-Tree-With contents k v key-compare-fn val-compare-fn)))
      (make-wb-replay-map contents (wb-replay-map-ordering m) proto-tmorg (if default? default (map-default m))))))

(defmethod convert ((to-type (eql 'wb-replay-map)) (m wb-replay-map)
		    &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (let ((prototype (empty-wb-map default key-compare-fn-name val-compare-fn-name))
	((proto-tmorg (wb-map-org prototype))
	 ((key-compare-fn (tree-map-org-key-compare-fn proto-tmorg))
	  (val-compare-fn (tree-map-org-val-compare-fn proto-tmorg))))
	(m-tmorg (wb-replay-map-org m)))
    (if (or (eq m-tmorg proto-tmorg)
	    (and (eq key-compare-fn (tree-map-org-key-compare-fn m-tmorg))
		 (eq val-compare-fn (tree-map-org-val-compare-fn m-tmorg))))
	m
      (let ((contents nil))
	(do-map (k v m)
	  (setq contents (WB-Map-Tree-With contents k v key-compare-fn val-compare-fn)))
	(make-wb-replay-map contents (wb-replay-map-ordering m) proto-tmorg (if default? default (map-default m)))))))

(defmethod convert ((to-type (eql 'map)) (m wb-replay-map) &key (default nil default?))
  (make-wb-map (wb-replay-map-contents m) (wb-replay-map-org m) (if default? default (map-default m))))
(defmethod convert ((to-type (eql 'wb-map)) (m wb-replay-map)
		    &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  (convert 'wb-map (make-wb-map (wb-replay-map-contents m) (wb-replay-map-org m)
				(if default? default (map-default m)))
	   :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'list)) (m wb-replay-map) &key (pair-fn #'cons))
  (let ((result nil))
    (do-wb-seq-tree-members (x (wb-replay-map-ordering m))
      (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) x
					  (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
	(declare (ignore val?))
	(push (funcall pair-fn x val) result)))
    (nreverse result)))
(defmethod convert ((to-type (eql 'vector)) (m wb-replay-map) &key (pair-fn #'cons))
  (convert 'vector (convert 'list m :pair-fn pair-fn)))
(defmethod convert ((to-type (eql 'seq)) (m wb-replay-map) &key (pair-fn #'cons))
  (convert 'seq (convert 'list m :pair-fn pair-fn)))

(defmethod convert ((to-type (eql 'replay-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (wb-replay-map-from-iterable list key-fn value-fn default 'compare 'compare))
(defmethod convert ((to-type (eql 'wb-replay-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (wb-replay-map-from-iterable list key-fn value-fn default key-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'replay-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (wb-replay-map-from-iterable s key-fn value-fn default 'compare 'compare))
(defmethod convert ((to-type (eql 'wb-replay-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (wb-replay-map-from-iterable s key-fn value-fn default key-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'replay-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (wb-replay-map-from-iterable s key-fn value-fn default 'compare 'compare))
(defmethod convert ((to-type (eql 'wb-replay-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (wb-replay-map-from-iterable s key-fn value-fn default key-compare-fn-name val-compare-fn-name))

(defun wb-replay-map-from-iterable (s key-fn value-fn default key-compare-fn-name val-compare-fn-name)
  (let ((prototype (empty-wb-map default key-compare-fn-name val-compare-fn-name))
	((proto-tmorg (wb-map-org prototype))
	 ((key-compare-fn (tree-map-org-key-compare-fn proto-tmorg))
	  (val-compare-fn (tree-map-org-val-compare-fn proto-tmorg))))
	(contents nil)
	(ordering nil))
    (do-elements (pr s)
      (let ((key (funcall key-fn pr))
	    (val (funcall value-fn pr)))
	(let ((new-contents (wb-map-tree-with contents key val key-compare-fn val-compare-fn)))
	  (unless (eq new-contents contents)
	    (setq ordering (wb-seq-tree-append ordering key))
	    (setq contents new-contents)))))
    (make-wb-replay-map contents ordering proto-tmorg default)))

(defmethod lookup ((m wb-replay-map) key)
  (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) key
				      (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
    (values (if val? val (map-default m)) val?)))

(defmethod domain-contains? ((m wb-replay-map) x)
  (wb-map-tree-lookup (wb-replay-map-contents m) x (tree-map-org-key-compare-fn (wb-replay-map-org m))))

(defmethod compare ((map1 replay-map) (map2 replay-map))
  "Fallback method for mixed implementations."
  (let ((size1 (size map1))
	(size2 (size map2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  ((let ((kcf1 (key-compare-fn map1))
		 (vcf1 (val-compare-fn map1))
		 (kcf2 (key-compare-fn map2))
		 (vcf2 (val-compare-fn map2)))
	     (gmap :and (fn (k1 k2)
			  (let ((eqk1? (equal?-cmp k1 k2 kcf1))
				(eqk2? (equal?-cmp k1 k2 kcf2)))
			    (unless (eqv eqk1? eqk2?)
			      (error "Can't compare replay-maps with incompatible key-compare-fns, ~A vs. ~A"
				     (tree-map-org-key-compare-fn-name (wb-replay-map-org map1))
				     (tree-map-org-key-compare-fn-name (wb-replay-map-org map2))))
			    (and eqk1?
				 (let ((v1 (lookup map1 k1))
				       (v2 (lookup map2 k2))
				       ((eqv1? (equal?-cmp v1 v2 vcf1))
					(eqv2? (equal?-cmp v1 v2 vcf2))))
				   (unless (eqv eqv1? eqv2?)
				     (error "Can't compare replay-maps with incompatible val-compare-fns, ~A vs. ~A"
					    (tree-map-org-val-compare-fn-name (wb-replay-map-org map1))
					    (tree-map-org-val-compare-fn-name (wb-replay-map-org map2))))
				   eqv1?))))
		   (:arg seq (key-ordering map1))
		   (:arg seq (key-ordering map2))))
	   ':equal)
	  (t ':unequal))))

(defmethod compare ((map1 wb-replay-map) (map2 wb-replay-map))
  (let ((tmorg1 (wb-replay-map-org map1))
	(tmorg2 (wb-replay-map-org map2)))
    (if (or (eq tmorg1 tmorg2)
	    (and (eq (tree-map-org-key-compare-fn tmorg1) (tree-map-org-key-compare-fn tmorg2))
		 (eq (tree-map-org-val-compare-fn tmorg1) (tree-map-org-val-compare-fn tmorg2))))
	(let ((comp (wb-map-tree-compare (wb-replay-map-contents map1) (wb-replay-map-contents map2)
					 (tree-map-org-key-compare-fn tmorg1) (tree-map-org-val-compare-fn tmorg1))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (wb-replay-map-ordering map1) (wb-replay-map-ordering map2)
						 (tree-map-org-key-compare-fn tmorg1))))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(if (or (eq comp ':unequal) (eq ord-comp ':unequal))
		    ':unequal
		  ':equal)))))
      (call-next-method))))

(defmethod with ((m wb-replay-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'wb-replay-map)
  (let ((contents (wb-replay-map-contents m))
	(tmorg (wb-replay-map-org m))
	((new-contents (wb-map-tree-with contents key value (tree-map-org-key-compare-fn tmorg)
					 (tree-map-org-val-compare-fn tmorg)))))
    (if (eq new-contents contents)
	m
      (if (= (wb-map-tree-size new-contents) (wb-map-tree-size contents))
	  ;; New value for existing key.
	  (make-wb-replay-map new-contents (wb-replay-map-ordering m) tmorg (map-default m))
	(make-wb-replay-map new-contents (wb-seq-tree-append (wb-replay-map-ordering m) key)
			    tmorg (map-default m))))))

;;; WARNING: linear-time operation!
(defmethod less ((m wb-replay-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-replay-map)
  (let ((contents (wb-replay-map-contents m))
	(tmorg (wb-replay-map-org m))
	((new-contents (wb-map-tree-less contents key (tree-map-org-key-compare-fn tmorg)))))
    (if (eq new-contents contents)
	m
      (make-wb-replay-map new-contents
			  (let ((tree (wb-replay-map-ordering m)))
			    (wb-seq-tree-remove tree (or (position key (make-wb-seq tree))
							 (error "Bug in `less' on `wb-replay-map'"))))
			  tmorg (map-default m)))))

(defmethod domain ((m wb-replay-map))
  "The domain of a replay map is a replay set."
  (let ((tmorg (wb-replay-map-org m))
	((set-prototype (empty-wb-set (tree-map-org-key-compare-fn-name tmorg)))))
    (make-wb-replay-set (wb-map-tree-domain (wb-replay-map-contents m)) (wb-replay-map-ordering m)
			(wb-set-org set-prototype))))

(defmethod key-ordering ((m wb-replay-map))
  (make-wb-seq (wb-replay-map-ordering m)))

(defmethod iterator ((m wb-replay-map) &key)
  (let ((iter (make-wb-seq-tree-iterator-internal (wb-replay-map-ordering m)))
	(contents (wb-replay-map-contents m))
	(kcf (tree-map-org-key-compare-fn (wb-replay-map-org m))))
    (lambda (op)
      (ecase op
	(:get (let ((key key? (wb-seq-tree-iterator-get iter)))
		(if (not key?)
		    (values nil nil nil)
		  (values key (wb-map-tree-lookup contents key kcf) t))))
	(:done? (wb-seq-tree-iterator-done? iter))
	(:more? (not (wb-seq-tree-iterator-done? iter)))))))

(defmethod internal-do-map ((m wb-replay-map) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (wb-replay-map-ordering m) (funcall value-fn))
    (funcall elt-fn x (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) x
							  (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
			(assert val? () "Bug in `wb-replay-map' iteration")
			val))))

(defun print-wb-replay-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{=|"
				    :suffix (let ((tmorg (wb-map-org map))
						  ((key-cf-name (tree-map-org-key-compare-fn-name tmorg))
						   (val-cf-name (tree-map-org-val-compare-fn-name tmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare)))))
					      (format nil " |}~:[[~:[~S~;~*~]:~:[~S~;~*~]]~;~]~@[/~S~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name
						      (map-default map))))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))

(gmap:def-gmap-arg-type wb-replay-map (map)
  "Yields each pair of `map', as two values."
  (let ((map-var (gensymx #:map-))
	(compare-fn-var (gensymx #:compare-fn-)))
    `((make-wb-seq-tree-iterator-internal (wb-replay-map-ordering ,map-var))
      #'wb-seq-tree-iterator-done?
      (:values 2 #'(lambda (it) (let ((key key? (wb-seq-tree-iterator-get it)))
				  (if (not key?)
				      (values nil nil)
				    (values key (let ((val? val
							(wb-map-tree-lookup (wb-replay-map-contents ,map-var) key
									    ,compare-fn-var)))
						  (assert val? () "Bug in `wb-replay-map' GMap arg type")
						  val))))))
      nil
      ((,map-var ,map)
       ((,compare-fn-var (tree-map-org-key-compare-fn (wb-replay-map-org ,map-var))))))))

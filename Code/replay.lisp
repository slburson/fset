;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: replay.lisp
;;; Contents: Replay sets and maps.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)

;;; ================================================================================
;;; Replay sets

;;; We have some implementation-generic methods.
;;; `empty-replay-set' is below.

(defmethod convert ((to-type (eql 'replay-set)) (s replay-set) &key)
  s)

(defmethod convert ((to-type (eql 'list)) (s replay-set) &key)
  (convert 'list (convert 'seq s)))
(defmethod convert ((to-type (eql 'vector)) (s replay-set) &key)
  (convert 'vector (convert 'seq s)))

(defmethod convert ((to-type (eql 'seq)) (s replay-set) &key)
  (make-wb-seq (replay-set-ordering s) nil))
(defmethod convert ((to-type (eql 'fset2:seq)) (s replay-set) &key)
  (make-wb-seq (replay-set-ordering s) nil))
(defmethod convert ((to-type (eql 'wb-seq)) (s replay-set) &key)
  (make-wb-seq (replay-set-ordering s) nil))
(defmethod convert ((to-type (eql 'fset2:wb-seq)) (s replay-set) &key)
  (make-wb-seq (replay-set-ordering s) nil))

(defmethod convert ((to-type (eql 'replay-set)) (l list) &key)
  (gmap (:result replay-set) nil (:arg list l)))
(defmethod convert ((to-type (eql 'replay-set)) (s seq) &key)
  (gmap (:result replay-set) nil (:arg seq s)))
(defmethod convert ((to-type (eql 'replay-set)) (s sequence) &key)
  (gmap (:result replay-set) nil (:arg sequence s)))

(defmethod iterator ((s replay-set) &key)
  (make-wb-seq-tree-iterator (replay-set-ordering s)))

(defmethod internal-do-set ((s replay-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (replay-set-ordering s) (funcall value-fn))
    (funcall elt-fn x)))


;;; ================
;;; WB-replay-sets

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
  (org nil :type tree-set-org :read-only t))

(defparameter +empty-wb-replay-set+ (make-wb-replay-set nil nil +fset-default-tree-set-org+))

(declaim (inline empty-wb-replay-set fset2:empty-wb-replay-set))
(defun empty-wb-replay-set (&optional compare-fn-name)
  (if (null compare-fn-name)
      +empty-wb-replay-set+
    (empty-wb-custom-replay-set compare-fn-name)))
(defun fset2:empty-wb-replay-set (&key compare-fn-name)
  (if (null compare-fn-name)
      +empty-wb-replay-set+
    (empty-wb-custom-replay-set compare-fn-name)))

(deflex +empty-wb-custom-replay-set-cache+ (make-hash-table :test 'eq))

(defun empty-wb-custom-replay-set (compare-fn-name)
  (assert (and (symbolp compare-fn-name) (not (null compare-fn-name))))
  (if (eq compare-fn-name 'compare)
      +empty-wb-replay-set+
    (let ((prev-instance (gethash compare-fn-name +empty-wb-custom-replay-set-cache+))
	  (compare-fn (symbol-function compare-fn-name)))
      (if (and prev-instance
	       (eq compare-fn (tree-set-org-compare-fn (wb-replay-set-org prev-instance))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-wb-custom-replay-set-cache+)
	      (make-wb-replay-set nil nil (make-tree-set-org compare-fn-name compare-fn)))))))

(defmethod compare-fn ((s wb-replay-set))
  (tree-set-org-compare-fn (wb-replay-set-org s)))

(defmethod compare-fn-name ((s wb-replay-set))
  (tree-set-org-compare-fn-name (wb-replay-set-org s)))

(defmethod empty? ((s wb-replay-set))
  (null (wb-replay-set-contents s)))

(defmethod size ((s wb-replay-set))
  (wb-set-tree-size (wb-replay-set-contents s)))

(defmethod arb ((s wb-replay-set))
  (let ((tree (wb-replay-set-contents s)))
    (if tree (values (wb-set-tree-arb tree) t)
      (values nil nil))))

(defmethod first ((s wb-replay-set))
  (let ((val? val (WB-Seq-Tree-Subscript (replay-set-ordering s) 0)))
    (values val val?)))

(defmethod last ((s wb-replay-set))
  (let ((tree (replay-set-ordering s))
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

(defmethod index ((s wb-replay-set) x)
  (let ((idx 0)
	(compare-fn (tree-set-org-compare-fn (wb-replay-set-org s))))
    (do-wb-set-tree-members (e (wb-replay-set-contents s))
      (when (equal?-cmp e x compare-fn)
	(return idx)))))

(defmethod at-index ((m wb-replay-set) index)
  (let ((ordering (replay-set-ordering m))
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
  (check-two-arguments y? 'contains? 'wb-replay-set)
  (wb-set-tree-contains? (wb-replay-set-contents s) x (tree-set-org-compare-fn (wb-replay-set-org s))))

(define-methods (lookup fset2:lookup) ((s wb-replay-set) value)
  (wb-set-tree-find-equal (wb-replay-set-contents s) value
			  (tree-set-org-compare-fn (wb-replay-set-org s))))

(defmethod compare ((set1 wb-replay-set) (set2 wb-replay-set))
  (let ((tsorg1 (wb-replay-set-org set1))
	(tsorg2 (wb-replay-set-org set2)))
    (if (or (eq tsorg1 tsorg2)
	    (eq (tree-set-org-compare-fn tsorg1) (tree-set-org-compare-fn tsorg2)))
	(let ((compare-fn (tree-set-org-compare-fn tsorg1))
	      ((comp (wb-set-tree-compare (wb-replay-set-contents set1) (wb-replay-set-contents set2) compare-fn))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (replay-set-ordering set1) (replay-set-ordering set2) compare-fn)))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(if (or (eq comp ':unequal) (eq ord-comp ':unequal))
		    ':unequal
		  ':equal)))))
      (let ((s1-cfn-name (tree-set-org-compare-fn-name tsorg1))
	    (s2-cfn-name (tree-set-org-compare-fn-name tsorg2))
	    ((name-comp (compare s1-cfn-name s2-cfn-name))))
	(ecase name-comp
	  ((:less :greater)
	    name-comp)
	  (:equal
	    (compare (convert 'wb-set set1 :compare-fn-name s1-cfn-name)
		     (convert 'wb-set set2 :compare-fn-name s2-cfn-name)))
	  (:unequal
	    (error "Can't compare wb-sets with uninterned compare-fn-names with same symbol-name")))))))

(defmethod hash-value ((rs wb-replay-set))
  (let ((result 0)
	(i 0)
	(mult 1))
    (do-wb-seq-tree-members (x (replay-set-ordering rs))
      (hash-mixf result (hash-multiply mult (hash-value-fixnum x)))
      (setq mult (hash-multiply mult 13))
      (when (= (incf i) 32)
	(return)))
    result))

(defmethod convert ((to-type (eql 'set)) (s wb-replay-set) &key)
  (make-wb-set (wb-replay-set-contents s) (wb-replay-set-org s)))
(defmethod convert ((to-type (eql 'fset2:set)) (s wb-replay-set) &key)
  (make-wb-set (wb-replay-set-contents s) (wb-replay-set-org s)))

(defmethod convert ((to-type (eql 'wb-set)) (s wb-replay-set) &key compare-fn-name)
  (convert 'wb-set (make-wb-set (wb-replay-set-contents s)) :compare-fn-name compare-fn-name))

(defmethod convert ((to-type (eql 'wb-replay-set)) (s replay-set) &key compare-fn-name)
  ;; If this method is called, `s' is not a `wb-replay-set'.
  (let ((prototype (empty-wb-set compare-fn-name))
	((compare-fn (wb-set-compare-fn prototype))))
    (let ((contents nil))
      (do-wb-seq-tree-members (x (replay-set-ordering s))
	(setq contents (wb-set-tree-with contents x compare-fn)))
      (make-wb-replay-set contents (replay-set-ordering s) (wb-set-org prototype)))))

(defmethod convert ((to-type (eql 'wb-replay-set)) (s wb-replay-set) &key compare-fn-name)
  (let ((prototype (empty-wb-set compare-fn-name))
	((compare-fn (wb-set-compare-fn prototype))))
    (if (eq (tree-set-org-compare-fn (wb-replay-set-org s)) compare-fn)
	s
      (let ((contents nil))
	(do-wb-set-tree-members (x (wb-replay-set-contents s))
	  (setq contents (wb-set-tree-with contents x compare-fn)))
	(make-wb-replay-set contents (replay-set-ordering s) (wb-set-org prototype))))))

(defmethod convert ((to-type (eql 'wb-replay-set)) (l list) &key compare-fn-name)
  (gmap (:result wb-replay-set :compare-fn-name compare-fn-name) nil (:arg list l)))

(defmethod convert ((to-type (eql 'wb-replay-set)) (s seq) &key compare-fn-name)
  (gmap (:result wb-replay-set :compare-fn-name compare-fn-name) nil (:arg seq s)))

(defmethod convert ((to-type (eql 'wb-replay-set)) (s sequence) &key compare-fn-name)
  (gmap (:result wb-replay-set :compare-fn-name compare-fn-name) nil (:arg sequence s)))

(defmethod with ((s wb-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	((new-contents (wb-set-tree-with contents value (wb-set-compare-fn s)))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents (wb-seq-tree-append (replay-set-ordering s) value)
			  (wb-replay-set-org s)))))

(defmethod union ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: it adds the members
of `s2' to `s1', so the ordering of the result will be that of `s1' with any
new members appended."
  (if (empty? s2) s1
    (let ((compare-fn (tree-set-org-compare-fn (wb-replay-set-org s1)))
	  (contents (wb-replay-set-contents s1))
	  (ordering (replay-set-ordering s1)))
      ;; This is O(n log m), rather than the usual O(m + n).
      (do-set (x s2)
	(let ((tmp (wb-set-tree-with contents x compare-fn)))
	  (unless (eq tmp contents)
	    (setq contents tmp)
	    (setq ordering (wb-seq-tree-append ordering x)))))
      (make-wb-replay-set contents ordering (wb-replay-set-org s1)))))

(defmethod intersection ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: the ordering of the
result is that of `s1', filtered by membership in `s2'."
  (cond ((empty? s2) (empty-wb-replay-set))
	((empty? s1) s1)
	(t
	 (make-wb-replay-set (wb-set-contents (intersection (convert 'wb-set s1) s2))
			     (wb-seq-contents (filter s2 (convert 'wb-seq s1)))
			     (wb-replay-set-org s1)))))

(defmethod less ((s wb-replay-set) value &optional (arg2 nil arg2?))
  "WARNING: linear-time operation!"
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	(compare-fn (tree-set-org-compare-fn (wb-replay-set-org s)))
	((new-contents (wb-set-tree-less contents value compare-fn))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents
			  (let ((tree (replay-set-ordering s)))
			    (wb-seq-tree-remove tree
						(or (position value (make-wb-seq tree nil) :test (equal?-fn compare-fn))
						    (error "Bug in `less' on `wb-replay-set'"))))
			  (wb-replay-set-org s)))))

(defun print-wb-replay-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{="
				    :suffix (let ((tsorg (wb-replay-set-org set))
						  ((compare-fn-name (tree-set-org-compare-fn-name tsorg))))
					      (if (eq compare-fn-name 'compare) " }"
						(format nil " }[~S]" compare-fn-name))))
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))


;;; ================
;;; CH-replay-sets

(defstruct (ch-replay-set
	     (:include replay-set)
	     (:constructor make-ch-replay-set (contents ordering org))
	     (:predicate ch-replay-set?)
	     (:print-function print-ch-replay-set)
	     (:copier nil))
  "A replay set is like a set, except that its iteration order is the order in which members
were added to it.  It does not support all set operations, but you can convert it to a set.
Note that in the current implementation, `less' on a replay set takes O(n) time.  Also, two
replay sets are equal only if they both contain the same elements and have the same iteration
order; if you just want to compare the contents, convert them to ordinary sets first.  Replay
sets are printed as \"#{= ... }\"."
  (contents nil :read-only t)
  (org nil :type hash-set-org :read-only t))

(defparameter +empty-ch-replay-set+ (make-ch-replay-set nil nil +fset-default-hash-set-org+))

;;; Moved here from above because forward-referencing `+empty-ch-replay-set+' ran afoul of
;;; https://bugs.launchpad.net/sbcl/+bug/2129827 .
(declaim (inline empty-replay-set))
(defun empty-replay-set ()
  "Returns an empty replay set of the default implementation."
  ;; Now CHAMP!
  +empty-ch-replay-set+)

(declaim (inline empty-ch-replay-set fset2:empty-ch-replay-set))
(defun empty-ch-replay-set (&optional compare-fn-name)
  (if (null compare-fn-name)
      +empty-ch-replay-set+
    (empty-ch-custom-replay-set compare-fn-name)))
(defun fset2:empty-ch-replay-set (&key compare-fn-name)
  (if (null compare-fn-name)
      +empty-ch-replay-set+
    (empty-ch-custom-replay-set compare-fn-name)))

(deflex +empty-ch-custom-replay-set-cache+ (make-hash-table :test 'eq))

(defun empty-ch-custom-replay-set (compare-fn-name)
  (assert (and compare-fn-name (symbolp compare-fn-name) (symbol-package compare-fn-name)) ()
	  "compare-fn-name must be a nonnull interned symbol")
  (if (eq compare-fn-name 'compare)
      +empty-ch-replay-set+
    (let ((prev-instance (gethash compare-fn-name +empty-ch-custom-replay-set-cache+))
	  (compare-fn (symbol-function compare-fn-name))
	  (hash-fn-name (or (get compare-fn-name 'hash-function)
			    (error "compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				   compare-fn-name)))
	  ((hash-fn (symbol-function hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-org (ch-replay-set-org prev-instance)))
		 (and (eq compare-fn (hash-set-org-compare-fn prev-org))
		      (eq hash-fn (hash-set-org-hash-fn prev-org)))))
	  prev-instance
	(setf (gethash compare-fn-name +empty-ch-custom-replay-set-cache+)
	      (make-ch-replay-set nil nil (make-hash-set-org compare-fn-name compare-fn hash-fn)))))))

(defmethod compare-fn ((s ch-replay-set))
  (hash-set-org-compare-fn (ch-replay-set-org s)))

(defmethod compare-fn-name ((s ch-replay-set))
  (hash-set-org-compare-fn-name (ch-replay-set-org s)))

(defmethod empty? ((s ch-replay-set))
  (null (ch-replay-set-contents s)))

(defmethod size ((s ch-replay-set))
  (ch-set-tree-size (ch-replay-set-contents s)))

(defmethod arb ((s ch-replay-set))
  (let ((tree (ch-replay-set-contents s)))
    (if tree (values (ch-set-tree-arb tree) t)
      (values nil nil))))

(defmethod first ((s ch-replay-set))
  (let ((val? val (wb-seq-tree-subscript (replay-set-ordering s) 0)))
    (values val val?)))

(defmethod last ((s ch-replay-set))
  (let ((tree (replay-set-ordering s))
	((val? val (wb-seq-tree-subscript tree (1- (wb-seq-tree-size tree))))))
    (values val val?)))

(defmethod index ((s ch-replay-set) x)
  "WARNING: linear-time operation!"
  (let ((idx 0)
	(compare-fn (hash-set-org-compare-fn (ch-replay-set-org s))))
    (do-ch-set-tree-members (e (ch-replay-set-contents s))
      (when (equal?-cmp e x compare-fn)
	(return idx)))))

(defmethod at-index ((m ch-replay-set) index)
  (let ((ordering (replay-set-ordering m))
	((size (wb-seq-tree-size ordering))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
	     :format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index m)))
    (let ((ignore val (wb-seq-tree-subscript ordering index)))
      (declare (ignore ignore))
      val)))

(defmethod contains? ((s ch-replay-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'ch-replay-set)
  (let ((hsorg (ch-replay-set-org s)))
    (ch-set-tree-contains? (ch-replay-set-contents s) x
			   (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))))

(define-methods (lookup fset2:lookup) ((s ch-replay-set) value)
  (let ((hsorg (ch-replay-set-org s)))
    (ch-set-tree-contains? (ch-replay-set-contents s) value
			   (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg))))

(defmethod compare ((set1 ch-replay-set) (set2 ch-replay-set))
  (let ((hsorg1 (ch-replay-set-org set1))
	(hsorg2 (ch-replay-set-org set2)))
    (if (or (eq hsorg1 hsorg2)
	    (and (eq (hash-set-org-hash-fn hsorg1) (hash-set-org-hash-fn hsorg2))
		 (eq (hash-set-org-compare-fn hsorg1) (hash-set-org-compare-fn hsorg2))))
	(let ((compare-fn (hash-set-org-compare-fn hsorg1))
	      ((comp (ch-set-tree-compare (ch-replay-set-contents set1) (ch-replay-set-contents set2) compare-fn))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (replay-set-ordering set1) (replay-set-ordering set2) compare-fn)))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(if (or (eq comp ':unequal) (eq ord-comp ':unequal))
		    ':unequal
		  ':equal)))))
      (let ((s1-cfn-name (hash-set-org-compare-fn-name hsorg1))
	    (s2-cfn-name (hash-set-org-compare-fn-name hsorg2))
	    ((name-comp (compare s1-cfn-name s2-cfn-name))))
	(ecase name-comp
	  ((:less :greater)
	    name-comp)
	  (:equal
	    (compare (convert 'ch-set set1 :compare-fn-name s1-cfn-name)
		     (convert 'ch-set set2 :compare-fn-name s1-cfn-name)))
	  (:unequal
	    (error "Can't compare ch-sets with uninterned compare-fn-names with same symbol-name")))))))

(defmethod hash-value ((rs ch-replay-set))
  ;; Doing it this way means we'll have collisions if the contents are equal but the orderings are
  ;; different, but that doesn't seem like a likely case, and this is much faster than hashing the ordering.
  (ch-set-tree-hash-value (ch-replay-set-contents rs)))

(defmethod convert ((to-type (eql 'set)) (s ch-replay-set) &key)
  (make-ch-set (ch-replay-set-contents s) (ch-replay-set-org s)))
(defmethod convert ((to-type (eql 'fset2:set)) (s ch-replay-set) &key)
  (make-ch-set (ch-replay-set-contents s) (ch-replay-set-org s)))

(defmethod convert ((to-type (eql 'ch-set)) (s ch-replay-set) &key compare-fn-name)
  (convert 'ch-set (make-ch-set (ch-replay-set-contents s) (ch-replay-set-org s))
	   :compare-fn-name compare-fn-name))

(defmethod convert ((to-type (eql 'ch-replay-set)) (s replay-set) &key compare-fn-name)
  ;; If this method is called, `s' is not a `ch-replay-set'.
  (let ((cs (convert 'ch-set (convert 'set s) :compare-fn-name compare-fn-name)))
    (make-ch-replay-set (ch-set-contents cs) (replay-set-ordering s) (ch-set-org cs))))

(defmethod convert ((to-type (eql 'ch-replay-set)) (s ch-replay-set) &key compare-fn-name)
  (let ((ts (convert 'set s))
	((cs (convert 'ch-set ts :compare-fn-name compare-fn-name))))
    (if (eq cs ts) s
      (make-ch-replay-set (ch-set-contents cs) (replay-set-ordering s) (ch-set-org cs)))))

(defmethod convert ((to-type (eql 'ch-replay-set)) (l list) &key compare-fn-name)
  (gmap (:result ch-replay-set :compare-fn-name compare-fn-name) nil (:arg list l)))

(defmethod convert ((to-type (eql 'ch-replay-set)) (s seq) &key compare-fn-name)
  (gmap (:result ch-replay-set :compare-fn-name compare-fn-name) nil (:arg seq s)))

(defmethod convert ((to-type (eql 'ch-replay-set)) (s sequence) &key compare-fn-name)
  (gmap (:result ch-replay-set :compare-fn-name compare-fn-name) nil (:arg sequence s)))

(defmethod with ((s ch-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'ch-replay-set)
  (let ((contents (ch-replay-set-contents s))
	(hsorg (ch-replay-set-org s))
	((new-contents (ch-set-tree-with contents value (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)))))
    (if (eq new-contents contents)
	s
      (make-ch-replay-set new-contents (wb-seq-tree-append (replay-set-ordering s) value) hsorg))))

(defmethod union ((s1 ch-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: it adds the members
of `s2' to `s1', so the ordering of the result will be that of `s1' with any
new members appended."
  (if (empty? s2) s1
    (let ((hsorg (ch-replay-set-org s1))
	  ((hash-fn (hash-set-org-hash-fn hsorg))
	   (compare-fn (hash-set-org-compare-fn hsorg)))
	  (contents (ch-replay-set-contents s1))
	  (new-ord-elts nil))
      ;; This is O(n log m), rather than the usual O(m + n).
      (do-set (x s2)
	(let ((tmp (ch-set-tree-with contents x hash-fn compare-fn)))
	  (unless (eq tmp contents)
	    (setq contents tmp)
	    (push x new-ord-elts))))
      (make-ch-replay-set contents
			  (wb-seq-tree-concat (replay-set-ordering s1)
					      (wb-seq-tree-from-list (nreverse new-ord-elts)))
			  hsorg))))

(defmethod intersection ((s1 ch-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: the ordering of the
result is that of `s1', filtered by membership in `s2'."
  (cond ((empty? s2) (empty-ch-replay-set (hash-set-org-compare-fn-name (ch-replay-set-org s1))))
	((empty? s1) s1)
	(t
	 (make-ch-replay-set (ch-set-contents (intersection (convert 'ch-set s1) s2))
			     (wb-seq-contents (filter s2 (convert 'wb-seq s1)))
			     (ch-replay-set-org s1)))))

(defmethod less ((s ch-replay-set) value &optional (arg2 nil arg2?))
  "WARNING: linear-time operation!"
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-replay-set)
  (let ((contents (ch-replay-set-contents s))
	(hsorg (ch-replay-set-org s))
	((hash-fn (hash-set-org-hash-fn hsorg))
	 (compare-fn (hash-set-org-compare-fn hsorg))
	 ((new-contents (ch-set-tree-less contents value hash-fn compare-fn)))))
    (if (eq new-contents contents)
	s
      (make-ch-replay-set new-contents
			  (let ((tree (replay-set-ordering s)))
			    (wb-seq-tree-remove tree
						(or (position value (make-wb-seq tree nil) :test (equal?-fn compare-fn))
						    (error "Bug in `less' on `ch-replay-set'"))))
			  hsorg))))

(defun print-ch-replay-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "##{="
				    :suffix (let ((hsorg (ch-replay-set-org set))
						  ((compare-fn-name (hash-set-org-compare-fn-name hsorg))))
					      (if (eq compare-fn-name 'compare) " }"
						(format nil " }[~S]" compare-fn-name))))
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))


;;; ================================================================================
;;; Replay maps

;;; We have some implementation-generic methods.
;;; `empty-replay-map' is below.

(defmethod convert ((to-type (eql 'replay-map)) (m replay-map) &key)
  m)

(defmethod convert ((to-type (eql 'list)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (if keys-only?
      (convert 'list (make-wb-seq (replay-map-ordering m) nil))
    (let ((result nil))
      (do-wb-seq-tree-members (x (replay-map-ordering m))
	(let ((val val? (lookup m x)))
	  (unless val?
	    (error "Bug in replay-map"))
	  (push (funcall pair-fn x val) result)))
      (nreverse result))))

(defmethod convert ((to-type (eql 'vector)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (convert 'vector (convert 'list m :pair-fn pair-fn :keys-only? keys-only?)))

(defmethod convert ((to-type (eql 'seq)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (convert 'seq (convert 'list m :pair-fn pair-fn :keys-only? keys-only?)))
(defmethod convert ((to-type (eql 'fset2:seq)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (convert 'fset2:seq (convert 'list m :pair-fn pair-fn :keys-only? keys-only?)))

(defmethod convert ((to-type (eql 'wb-seq)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (convert 'wb-seq (convert 'list m :pair-fn pair-fn :keys-only? keys-only?)))
(defmethod convert ((to-type (eql 'fset2:wb-seq)) (m replay-map) &key (pair-fn #'cons) keys-only?)
  (convert 'fset2:wb-seq (convert 'list m :pair-fn pair-fn :keys-only? keys-only?)))

(defmethod convert ((to-type (eql 'replay-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (gmap (:result replay-map :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg list list)))
(defmethod convert ((to-type (eql 'replay-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (gmap (:result replay-map :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg seq s)))
(defmethod convert ((to-type (eql 'replay-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) default)
  (gmap (:result replay-map :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg sequence s)))

(defmethod iterator ((m replay-map) &key)
  (let ((iter (make-wb-seq-tree-iterator-internal (replay-map-ordering m))))
    (lambda (op)
      (ecase op
	(:get (let ((key key? (wb-seq-tree-iterator-get iter)))
		(if (not key?)
		    (values nil nil nil)
		  (let ((val val? (lookup m key)))
		    (assert val? () "Bug in `replay-map' iterator")
		    (values key val t)))))
	(:done? (wb-seq-tree-iterator-done? iter))
	(:more? (not (wb-seq-tree-iterator-done? iter)))))))

(defmethod internal-do-map ((m replay-map) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (replay-map-ordering m) (funcall value-fn))
    (funcall elt-fn x (let ((val val? (lookup m x)))
			(assert val? () "Bug in `replay-map' iteration")
			val))))


;;; ================
;;; WB-replay-maps

(defstruct (wb-replay-map
	     (:include replay-map)
	     (:constructor make-wb-replay-map (contents ordering org default))
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
  (org nil :type tree-map-org :read-only t))

(defparameter +empty-wb-replay-map+ (make-wb-replay-map nil nil +fset-default-tree-map-org+ nil))

(defparameter +empty-wb-replay-map/no-default+ (make-wb-replay-map nil nil +fset-default-tree-map-org+ 'no-default))

(declaim (inline empty-wb-replay-map fset2:empty-wb-replay-map))
(defun empty-wb-replay-map (&optional default key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  +empty-wb-replay-map+
	(make-wb-replay-map nil nil +fset-default-tree-map-org+ default))
    (empty-wb-custom-replay-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))
(defun fset2:empty-wb-replay-map (&key (default nil default?) no-default? key-compare-fn-name val-compare-fn-name)
  "Returns an empty wb-replay-map with the specified default and comparison
functions.  The map's default is `nil' unless a different default is supplied,
or `no-default?' is true."
  (empty-wb-replay-map-internal (fset2-default default? default no-default?) key-compare-fn-name val-compare-fn-name))

(defun empty-wb-replay-map-internal (default key-compare-fn-name val-compare-fn-name)
  (cond ((or key-compare-fn-name val-compare-fn-name)
	 (empty-wb-custom-replay-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare)))
	((null default)
	 +empty-wb-replay-map+)
	((eq default 'no-default)
	 +empty-wb-replay-map/no-default+)
	(t (make-wb-replay-map nil nil +fset-default-tree-map-org+ default))))

(deflex +empty-wb-custom-replay-map-cache+ (make-hash-table :test 'equal))

(defun empty-wb-custom-replay-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) +empty-wb-replay-map+
	(make-wb-replay-map nil nil +fset-default-tree-map-org+ default))
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-wb-custom-replay-map-cache+)))
	  (key-compare-fn (symbol-function key-compare-fn-name))
	  (val-compare-fn (symbol-function val-compare-fn-name)))
      (if (and prev-instance
	       (let ((prev-org (wb-replay-map-org prev-instance)))
		 (and (eq key-compare-fn (tree-map-org-key-compare-fn prev-org))
		      (eq val-compare-fn (tree-map-org-val-compare-fn prev-org))
		      (equal?-cmp default (map-default prev-instance) val-compare-fn))))
	  prev-instance
	(setf (gethash cache-key +empty-wb-custom-replay-map-cache+)
	      (make-wb-replay-map nil nil (make-tree-map-org key-compare-fn-name key-compare-fn
							     val-compare-fn-name val-compare-fn)
				  default))))))

(defmethod with-default ((m wb-replay-map) new-default)
  (make-wb-replay-map (wb-replay-map-contents m) (replay-map-ordering m) (wb-replay-map-org m) new-default))

(defmethod fset2:without-default ((m wb-replay-map))
  (make-wb-replay-map (wb-replay-map-contents m) (replay-map-ordering m) (wb-replay-map-org m) 'no-default))

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
  (at-index m 0))

(defmethod last ((m wb-replay-map))
  (at-index m (1- (size m))))

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

(defmethod index ((m wb-replay-map) key)
  "WARNING: linear-time operation!"
  (let ((idx 0)
	(key-compare-fn (tree-map-org-key-compare-fn (wb-replay-map-org m))))
    (do-wb-map-tree-pairs (k v (wb-replay-map-contents m))
      (declare (ignore v))
      (when (equal?-cmp k key key-compare-fn)
	(return idx)))))

(defmethod at-index ((m wb-replay-map) index)
  (let ((ordering (replay-map-ordering m))
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

(defmethod convert ((to-type (eql 'fset2:replay-map)) (m replay-map) &key)
  m)

(define-convert-methods (wb-replay-map fset2:wb-replay-map)
			((m replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (let ((prototype (empty-wb-map default key-compare-fn-name val-compare-fn-name))
	((proto-tmorg (wb-map-org prototype))
	 ((key-compare-fn (tree-map-org-key-compare-fn proto-tmorg))
	  (val-compare-fn (tree-map-org-val-compare-fn proto-tmorg)))))
    (let ((contents nil))
      (do-map (k v m)
	(setq contents (WB-Map-Tree-With contents k v key-compare-fn val-compare-fn)))
      (make-wb-replay-map contents (replay-map-ordering m) proto-tmorg (if default? default (map-default m))))))

(define-convert-methods (wb-replay-map fset2:wb-replay-map)
			((m wb-replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
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
	(make-wb-replay-map contents (replay-map-ordering m) proto-tmorg (if default? default (map-default m)))))))

(define-convert-methods (map fset2:map) ((m wb-replay-map) &key (default nil default?))
  (make-wb-map (wb-replay-map-contents m) (wb-replay-map-org m) (if default? default (map-default m))))

(define-convert-methods (wb-map fset2:wb-map)
			((m wb-replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  (convert 'wb-map (make-wb-map (wb-replay-map-contents m) (wb-replay-map-org m)
				(if default? default (map-default m)))
	   :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'wb-replay-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (wb-replay-map-from-iterable list key-fn value-fn default key-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'wb-replay-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (wb-replay-map-from-iterable s key-fn value-fn default key-compare-fn-name val-compare-fn-name))

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
(defmethod fset2:lookup ((m wb-replay-map) key)
  (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) key
				      (tree-map-org-key-compare-fn (wb-replay-map-org m)))))
    (values (if val? val
	      (let ((dflt (map-default m)))
		(if (eq dflt 'no-default)
		    (error 'fset2:map-domain-error :map m :key key)
		  dflt)))
	    val?)))

(defmethod domain-contains? ((m wb-replay-map) x)
  (wb-map-tree-lookup (wb-replay-map-contents m) x (tree-map-org-key-compare-fn (wb-replay-map-org m))))

(defmethod compare ((map1 wb-replay-map) (map2 wb-replay-map))
  (let ((tmorg1 (wb-replay-map-org map1))
	(tmorg2 (wb-replay-map-org map2)))
    (if (or (eq tmorg1 tmorg2)
	    (and (eq (tree-map-org-key-compare-fn tmorg1) (tree-map-org-key-compare-fn tmorg2))
		 (eq (tree-map-org-val-compare-fn tmorg1) (tree-map-org-val-compare-fn tmorg2))))
	(let ((key-compare-fn (tree-map-org-key-compare-fn tmorg1))
	      ((comp (wb-map-tree-compare (wb-replay-map-contents map1) (wb-replay-map-contents map2)
					  key-compare-fn (tree-map-org-val-compare-fn tmorg1)))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (replay-map-ordering map1) (replay-map-ordering map2) key-compare-fn)))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(if (or (eq comp ':unequal) (eq ord-comp ':unequal))
		    ':unequal
		  ':equal)))))
      (let ((m1-kcfn-name (tree-map-org-key-compare-fn-name tmorg1))
	    (m1-vcfn-name (tree-map-org-val-compare-fn-name tmorg1))
	    (m2-kcfn-name (tree-map-org-key-compare-fn-name tmorg2))
	    (m2-vcfn-name (tree-map-org-val-compare-fn-name tmorg2))
	    ((name-comp (compare (list m1-kcfn-name m1-vcfn-name) (list m2-kcfn-name m2-vcfn-name)))))
	(ecase name-comp
	  ((:less :greater)
	    name-comp)
	  (:equal
	    (compare (convert 'wb-map map1 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)
		     (convert 'wb-map map2 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)))
	  (:unequal
	    (error "Can't compare wb-maps with uninterned compare-fn-names with same symbol-name")))))))

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
	  (make-wb-replay-map new-contents (replay-map-ordering m) tmorg (map-default m))
	(make-wb-replay-map new-contents (wb-seq-tree-append (replay-map-ordering m) key)
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
			  (let ((tree (replay-map-ordering m)))
			    (wb-seq-tree-remove tree (or (position key (make-wb-seq tree nil))
							 (error "Bug in `less' on `wb-replay-map'"))))
			  tmorg (map-default m)))))

(defmethod domain ((m wb-replay-map))
  "The domain of a replay map is a replay set."
  (let ((tmorg (wb-replay-map-org m))
	((set-prototype (empty-wb-set (tree-map-org-key-compare-fn-name tmorg)))))
    (make-wb-replay-set (wb-map-tree-domain (wb-replay-map-contents m)) (replay-map-ordering m)
			(wb-set-org set-prototype))))

(defun print-wb-replay-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{=|"
				    :suffix (let ((tmorg (wb-replay-map-org map))
						  ((key-cf-name (tree-map-org-key-compare-fn-name tmorg))
						   (val-cf-name (tree-map-org-val-compare-fn-name tmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare))))
						  (dflt (map-default map)))
					      (format nil " |}~:[[~:[~S~;~*~]:~:[~S~;~*~]]~;~4*~]~:[/~S~;~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name
						      (eq dflt 'no-default) dflt)))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))


;;; ================
;;; CH-replay-maps

(defstruct (ch-replay-map
	     (:include replay-map)
	     (:constructor make-ch-replay-map (contents ordering org default))
	     (:predicate ch-replay-map?)
	     (:print-function print-ch-replay-map)
	     (:copier nil))
  "A replay map is like a map, except that its iteration order is the order in which keys
were first added to it.  It does not support all map operations, but you can convert it
to a map.  Note that in the current implementation, `less' on a replay map takes O(n) time.
Also, two replay maps are equal only if they both contain the same pairs and have the same
iteration order; if you just want to compare the contents, convert them to ordinary maps
first.  Replay maps are printed as \"##{=| ... |}\"."
  (contents nil :read-only t)
  (org nil :type hash-map-org :read-only t))

(defparameter +empty-ch-replay-map+ (make-ch-replay-map nil nil +fset-default-hash-map-org+ nil))

(defparameter +empty-ch-replay-map/no-default+ (make-ch-replay-map nil nil +fset-default-hash-map-org+ 'no-default))

;;; Moved here from above because forward-referencing `+empty-ch-replay-map+' ran afoul of
;;; https://bugs.launchpad.net/sbcl/+bug/2129827 .
(declaim (inline empty-replay-map fset2:empty-replay-map))
(defun empty-replay-map (&optional default)
  (if default (make-ch-replay-map nil nil +fset-default-hash-map-org+ default)
    +empty-ch-replay-map+))
(defun fset2:empty-replay-map (&key (default nil default?) no-default?)
  (empty-ch-replay-map-internal (fset2-default default? default no-default?) nil nil))

(declaim (inline empty-ch-replay-map fset2:empty-ch-replay-map))
(defun empty-ch-replay-map (&optional default key-compare-fn-name val-compare-fn-name)
  (if (and (null key-compare-fn-name) (null val-compare-fn-name))
      (if (null default)
	  +empty-ch-replay-map+
	(make-ch-replay-map nil nil +fset-default-hash-map-org+ default))
    (empty-ch-custom-replay-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare))))
(defun fset2:empty-ch-replay-map (&key (default nil default?) no-default? key-compare-fn-name val-compare-fn-name)
  "Returns an empty ch-replay-map with the specified default and comparison
functions.  The map's default is `nil' unless a different default is supplied,
or `no-default?' is true."
  (empty-ch-replay-map-internal (fset2-default default? default no-default?) key-compare-fn-name val-compare-fn-name))

(defun empty-ch-replay-map-internal (default key-compare-fn-name val-compare-fn-name)
  (cond ((or key-compare-fn-name val-compare-fn-name)
	 (empty-ch-custom-replay-map default (or key-compare-fn-name 'compare) (or val-compare-fn-name 'compare)))
	((null default)
	 +empty-ch-replay-map+)
	((eq default 'no-default)
	 +empty-ch-replay-map/no-default+)
	(t (make-ch-replay-map nil nil +fset-default-hash-map-org+ default))))

(deflex +empty-ch-custom-replay-map-cache+ (make-hash-table :test 'equal))

(defun empty-ch-custom-replay-map (default key-compare-fn-name val-compare-fn-name)
  (assert (and key-compare-fn-name (symbolp key-compare-fn-name)
	       (symbol-package key-compare-fn-name))
	  () "key-compare-fn-name must be a nonnull interned symbol")
  (assert (and val-compare-fn-name (symbolp val-compare-fn-name)
	       (symbol-package val-compare-fn-name))
	  () "val-compare-fn-name must be a nonnull interned symbol")
  (if (and (eq key-compare-fn-name 'compare) (eq val-compare-fn-name 'compare))
      (if (null default) +empty-ch-replay-map+
	(make-ch-replay-map nil nil +fset-default-tree-map-org+ default))
    (let ((cache-key (list key-compare-fn-name val-compare-fn-name))
	  ((prev-instance (gethash cache-key +empty-ch-custom-replay-map-cache+)))
	  (key-hash-fn-name (or (get key-compare-fn-name 'hash-function)
				(error "key-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       key-compare-fn-name)))
	  (val-hash-fn-name (or (get val-compare-fn-name 'hash-function)
				(error "val-compare-fn-name `~S' not defined for hashing -- see `define-hash-function'"
				       val-compare-fn-name)))
	  ((key-compare-fn (symbol-function key-compare-fn-name))
	   (key-hash-fn (symbol-function key-hash-fn-name))
	   (val-compare-fn (symbol-function val-compare-fn-name))
	   (val-hash-fn (symbol-function val-hash-fn-name))))
      (if (and prev-instance
	       (let ((prev-org (ch-map-org prev-instance)))
		 (and (eq key-compare-fn (hash-map-org-key-compare-fn prev-org))
		      (eq key-hash-fn (hash-map-org-key-hash-fn prev-org))
		      (eq val-compare-fn (hash-map-org-val-compare-fn prev-org))
		      (eq val-hash-fn (hash-map-org-val-hash-fn prev-org))
		      (equal?-cmp default (map-default prev-instance) val-compare-fn))))
	  prev-instance
	(setf (gethash cache-key +empty-ch-custom-replay-map-cache+)
	      (make-ch-replay-map nil nil (make-hash-map-org key-compare-fn-name key-compare-fn key-hash-fn
							     val-compare-fn-name val-compare-fn val-hash-fn)
				  default))))))

(defmethod with-default ((m ch-replay-map) new-default)
  (make-ch-replay-map (ch-replay-map-contents m) (replay-map-ordering m) (ch-replay-map-org m) new-default))

(defmethod fset2:without-default ((m ch-replay-map))
  (make-ch-replay-map (ch-replay-map-contents m) (replay-map-ordering m) (ch-replay-map-org m) 'no-default))

(defmethod key-compare-fn ((m ch-replay-map))
  (hash-map-org-key-compare-fn (ch-replay-map-org m)))

(defmethod val-compare-fn ((m ch-replay-map))
  (hash-map-org-val-compare-fn (ch-replay-map-org m)))

(defmethod key-compare-fn-name ((m ch-replay-map))
  (hash-map-org-key-compare-fn-name (ch-replay-map-org m)))

(defmethod val-compare-fn-name ((m ch-replay-map))
  (hash-map-org-val-compare-fn-name (ch-replay-map-org m)))

(defmethod empty? ((m ch-replay-map))
  (null (ch-replay-map-contents m)))

(defmethod arb ((m ch-replay-map))
  (let ((tree (ch-replay-map-contents m)))
    (if tree
	(let ((key val (ch-map-tree-arb-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod first ((m ch-replay-map))
  (at-index m 0))

(defmethod last ((m ch-replay-map))
  (at-index m (1- (size m))))

(defmethod index ((m ch-replay-map) key)
  "WARNING: linear-time operation!"
  (let ((idx 0)
	(key-compare-fn (hash-map-org-key-compare-fn (ch-replay-map-org m))))
    (do-ch-map-tree-pairs (k v (ch-replay-map-contents m))
      (declare (ignore v))
      (when (equal?-cmp k key key-compare-fn)
	(return idx)))))

(defmethod at-index ((m ch-replay-map) index)
  (let ((ordering (replay-map-ordering m))
	((size (wb-seq-tree-size ordering))))
    (unless (and (>= index 0) (< index size))
      (error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
	     :format-control "Index ~D out of bounds on ~A"
				:format-arguments (list index m)))
    (let ((ignore key (wb-seq-tree-subscript ordering index))
	  (hmorg (ch-replay-map-org m))
	  ((val? val (ch-map-tree-lookup (ch-replay-map-contents m) key
					 (hash-map-org-key-hash-fn hmorg)
					 (hash-map-org-key-compare-fn hmorg)))))
      (declare (ignore ignore))
      (unless val?
	(error "Bug in ch-replay-map"))
      (values key val))))

(defmethod size ((m ch-replay-map))
  (ch-map-tree-size (ch-replay-map-contents m)))

(define-convert-methods (ch-replay-map fset2:ch-replay-map)
			((m replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  ;; If this method is called, `m' is not a `ch-replay-map'.
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (let ((cm (convert 'ch-map (convert 'map m)
		     :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name)))
    (make-ch-replay-map (ch-map-contents cm) (replay-map-ordering m) (ch-map-org cm)
			(if default? default (map-default m)))))

(define-convert-methods (ch-replay-map fset2:ch-replay-map)
			((m ch-replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  "The result uses `default' if supplied, otherwise has the same default as `m'."
  (let ((tm (convert 'map m))
	((cm (convert 'ch-map tm
		      :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name))))
    (if (eq cm tm) m
      (make-ch-replay-map (ch-map-contents cm) (replay-map-ordering m) (ch-map-org cm)
			  (if default? default (map-default m))))))

(define-convert-methods (map fset2:map) ((m ch-replay-map) &key (default nil default?))
  (make-ch-map (ch-replay-map-contents m) (ch-replay-map-org m) (if default? default (map-default m))))

(define-convert-methods (ch-map fset2:ch-map)
			((m ch-replay-map) &key (default nil default?) key-compare-fn-name val-compare-fn-name)
  (convert 'ch-map (make-ch-map (ch-replay-map-contents m) (ch-replay-map-org m)
				(if default? default (map-default m)))
	   :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name))

(defmethod convert ((to-type (eql 'list)) (m ch-replay-map) &key (pair-fn #'cons))
  (let ((hmorg (ch-replay-map-org m))
	(result nil))
    (do-wb-seq-tree-members (x (replay-map-ordering m))
      (let ((val? val (ch-map-tree-lookup (ch-replay-map-contents m) x
					  (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg))))
	(declare (ignore val?))
	(push (funcall pair-fn x val) result)))
    (nreverse result)))
(defmethod convert ((to-type (eql 'vector)) (m ch-replay-map) &key (pair-fn #'cons))
  (convert 'vector (convert 'list m :pair-fn pair-fn)))
(defmethod convert ((to-type (eql 'seq)) (m ch-replay-map) &key (pair-fn #'cons))
  (convert 'seq (convert 'list m :pair-fn pair-fn)))
(defmethod convert ((to-type (eql 'fset2:seq)) (m ch-replay-map) &key (pair-fn #'cons))
  (convert 'fset2:seq (convert 'list m :pair-fn pair-fn)))

(defmethod convert ((to-type (eql 'ch-replay-map)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (gmap (:result ch-replay-map :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name
	 :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg list list)))

(defmethod convert ((to-type (eql 'ch-replay-map)) (s seq)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (gmap (:result ch-replay-map :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name
	 :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg seq s)))

(defmethod convert ((to-type (eql 'ch-replay-map)) (s sequence)
		    &key (key-fn #'car) (value-fn #'cdr) default key-compare-fn-name val-compare-fn-name)
  (gmap (:result ch-replay-map :key-compare-fn-name key-compare-fn-name :val-compare-fn-name val-compare-fn-name
	 :default default)
	(fn (x) (values (funcall key-fn x) (funcall value-fn x)))
	(:arg sequence s)))

(defmethod lookup ((m ch-replay-map) key)
  (let ((hmorg (ch-replay-map-org m))
	((val? val (ch-map-tree-lookup (ch-replay-map-contents m) key
				       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    (values (if val? val (map-default m)) val?)))
(defmethod fset2:lookup ((m ch-replay-map) key)
  (let ((hmorg (ch-replay-map-org m))
	((val? val (ch-map-tree-lookup (ch-replay-map-contents m) key
				       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
    (values (if val? val
	      (let ((dflt (map-default m)))
		(if (eq dflt 'no-default)
		    (error 'fset2:map-domain-error :map m :key key)
		  dflt)))
	    val?)))

(defmethod domain-contains? ((m ch-replay-map) x)
  (let ((hmorg (ch-replay-map-org m)))
    (ch-map-tree-lookup (ch-replay-map-contents m) x
			(hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg))))

(defmethod compare ((map1 ch-replay-map) (map2 ch-replay-map))
  (let ((hmorg1 (ch-replay-map-org map1))
	(hmorg2 (ch-replay-map-org map2)))
    (if (or (eq hmorg1 hmorg2)
	    (and (eq (hash-map-org-key-hash-fn hmorg1) (hash-map-org-key-hash-fn hmorg2))
		 (eq (hash-map-org-key-compare-fn hmorg1) (hash-map-org-key-compare-fn hmorg2))
		 (eq (hash-map-org-val-hash-fn hmorg1) (hash-map-org-val-hash-fn hmorg2))
		 (eq (hash-map-org-val-compare-fn hmorg1) (hash-map-org-val-compare-fn hmorg2))))
	(let ((key-compare-fn (hash-map-org-key-compare-fn hmorg1))
	      (val-compare-fn (hash-map-org-val-compare-fn hmorg1))
	      ((comp (ch-map-tree-compare (ch-replay-map-contents map1) (ch-replay-map-contents map2)
					  key-compare-fn (hash-map-org-val-hash-fn hmorg1) val-compare-fn))))
	  (if (member comp '(:less :greater))
	      comp
	    (let ((ord-comp (wb-seq-tree-compare (replay-map-ordering map1) (replay-map-ordering map2) key-compare-fn)))
	      (if (member ord-comp '(:less :greater))
		  ord-comp
		(let ((def-comp (funcall val-compare-fn (map-default map1) (map-default map2))))
		  (if (member def-comp '(:less :greater))
		      def-comp
		    (if (or (eq comp ':unequal) (eq ord-comp ':unequal) (eq def-comp ':unequal))
			':unequal
		      ':equal)))))))
      (let ((m1-kcfn-name (hash-map-org-key-compare-fn-name hmorg1))
	    (m1-vcfn-name (hash-map-org-val-compare-fn-name hmorg1))
	    (m2-kcfn-name (hash-map-org-key-compare-fn-name hmorg2))
	    (m2-vcfn-name (hash-map-org-val-compare-fn-name hmorg2))
	    ((name-comp (compare (list m1-kcfn-name m1-vcfn-name) (list m2-kcfn-name m2-vcfn-name)))))
	(ecase name-comp
	  ((:less :greater)
	    name-comp)
	  (:equal
	    (compare (convert 'ch-replay-map map1 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)
		     (convert 'ch-replay-map map2 :key-compare-fn-name m1-kcfn-name :val-compare-fn-name m1-vcfn-name)))
	  (:unequal
	    (error "Can't compare ch-replay-maps with uninterned compare-fn-names with same symbol-name")))))))

(defmethod with ((m ch-replay-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'ch-replay-map)
  (let ((contents (ch-replay-map-contents m))
	(hmorg (ch-replay-map-org m))
	((new-contents (ch-map-tree-with contents key value
					 (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					 (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg)))))
    (if (eq new-contents contents)
	m
      (if (= (ch-map-tree-size new-contents) (ch-map-tree-size contents))
	  ;; New value for existing key.
	  (make-ch-replay-map new-contents (replay-map-ordering m) hmorg (map-default m))
	(make-ch-replay-map new-contents (wb-seq-tree-append (replay-map-ordering m) key)
			    hmorg (map-default m))))))

;;; WARNING: linear-time operation!
(defmethod less ((m ch-replay-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-replay-map)
  (let ((contents (ch-replay-map-contents m))
	(hmorg (ch-replay-map-org m))
	((new-contents (ch-map-tree-less contents key
					 (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
					 (hash-map-org-val-hash-fn hmorg)))))
    (if (eq new-contents contents)
	m
      (make-ch-replay-map new-contents
			  (let ((tree (replay-map-ordering m)))
			    (wb-seq-tree-remove tree (or (position key (make-wb-seq tree nil))
							 (error "Bug in `less' on `ch-replay-map'"))))
			  hmorg (map-default m)))))

(defmethod domain ((m ch-replay-map))
  "The domain of a replay map is a replay set."
  (let ((hmorg (ch-replay-map-org m))
	((set-prototype (empty-ch-set (hash-map-org-key-compare-fn-name hmorg)))))
    (make-ch-replay-set (ch-map-tree-domain (ch-replay-map-contents m) (hash-map-org-key-hash-fn hmorg))
			(replay-map-ordering m) (ch-set-org set-prototype))))

(defun print-ch-replay-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "##{=|"
				    :suffix (let ((hmorg (ch-replay-map-org map))
						  ((key-cf-name (hash-map-org-key-compare-fn-name hmorg))
						   (val-cf-name (hash-map-org-val-compare-fn-name hmorg))
						   ((key-default? (eq key-cf-name 'compare))
						    (val-default? (eq val-cf-name 'compare))))
						  (dflt (map-default map)))
					      (format nil " |}~:[[~:[~S~;~*~]:~:[~S~;~*~]]~;~4*~]~:[/~S~;~]"
						      (and key-default? val-default?)
						      key-default? key-cf-name val-default? val-cf-name
						      (eq dflt 'no-default) dflt)))
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))))


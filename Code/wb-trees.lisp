;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: wb-trees.lisp
;;; Contents: Weight-balanced binary tree implementation for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

#||

Reference: Adams, Stephen.  "Implementing Sets Efficiently In a Functional
Language", CSTR 92-10, Dept. of Electronics and Computer Science, University
of Southampton, 1992.

We introduce two substantial complications into Adams' nice clean code.

First, we store the lowest one to three levels or so of the tree (as
convenient) in small vectors.  Since most of the nodes are near the bottom,
this saves a lot of space -- probably between a factor of 2 and 4 in
practice, depending on how hard we work to keep these vectors from getting
too short.

Second, we have to deal with the fact that our ordering relation is not
total (it is a strict weak ordering); there may be distinct values neither
of which is "less than" the other.  These are called "equivalent" values.
Our intent is not to handle large sets of equivalent values efficiently;
it is simply to ensure that our code doesn't break, as Adams' would, if
we occasionally get a pair of equivalent values in a set or bag or the
domain of a map, and that as long as this does happen only occasionally,
the performance cost will be insignificant.

This means, for instance, that a viable way to implement the ordering
relation for some class is to compute hash codes for the instances and
compare the hash codes.  If the hashing is done well, collisions will be
rare -- rare enough that the performance consequences will be negligible.

Also, we go to considerable effort to minimize the number of calls to the
ordering function `compare'.  In fact, this is why we have a 4-valued
comparator rather than separate `less-than?' and `equal?' -- so we have to
do the comparison only once for each pair of values.

||#

(in-package :fset)


;;; ================================================================================
;;; ================================================================================
;;; Weight-balanced trees

;;; The threshold length above which tree nodes will be built.
(defconstant WB-Tree-Max-Vector-Length 8)

;;; This is longer because characters are smaller (on SBCL, 1 byte for base-chars,
;;; 4 for extended-chars).  We could reasonably go to 32 for 1-byte chars.
;;; It is assumed that this is no more than twice `WB-Tree-Max-Vector-Length'.
(defconstant WB-Tree-Max-String-Length 16)

;;; The factor by which one subtree may outweigh another.  See Adams.  Don't
;;; change this unless you understand his analysis.
(defconstant WB-Tree-Balance-Factor 4)


;;; ================================================================================
;;; ================================================================================
;;; Sets

(declaim (inline Make-Raw-WB-Set-Tree-Node))

(deftype WB-Set-Tree ()
  '(or null WB-Set-Tree-Node simple-vector))

(defstruct (WB-Set-Tree-Node
	    (:constructor Make-Raw-WB-Set-Tree-Node (Size Value Left Right))
	    (:predicate WB-Set-Tree-Node?)
	    (:print-function WB-Set-Tree-Node-Print))
  (Left  nil :type WB-Set-Tree :read-only t)
  (Right nil :type WB-Set-Tree :read-only t)
  (Value nil :read-only t)		; normally the value at the node, but see `Equivalent-Node' below.
  (Size 0 :type fixnum :read-only t))	; the number of members in this subtree

(defun WB-Set-Tree-Node-Print (node stream depth)
  "Print function for `WB-Set-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      ;; Fun with `format'!
      (format stream "~<#set-node<~;~D, ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (WB-Set-Tree-Node-Size node)
		    (WB-Set-Tree-Node-Value node)
		    (let ((sub (WB-Set-Tree-Node-Left node)))
		      (if (simple-vector-p sub)
			  (list t (coerce sub 'list))
			(list nil sub)))
		    (let ((sub (WB-Set-Tree-Node-Right node)))
		      (if (simple-vector-p sub)
			  (list t (coerce sub 'list))
			(list nil sub)))))
    (format stream "#set-node<...>")))

(declaim (inline Make-Equivalent-Node))

;;; When we get two or more equivalent values in a set, we use one of these
;;; as the `Value' of the tree node.  It would be a bit simpler, and would
;;; make the code more uniform, to say that the node value slot would always
;;; contain a list.  But then we would be paying a space cost, albeit a
;;; small one, on every node; I'd rather pay the cost only when it's needed.
;;; -- We used to have three different types, one for each of sets, bags, and
;;; maps.  But now that we're allowing collection-specific comparison functions,
;;; any comparison function that ever returns `:unequal' must arrange to check for
;;; this case.  It has no way to know which collection type the node might have
;;; come from, so it would have to check for all three, which would be wasteful.
(defstruct (Equivalent-Node
	    (:constructor Make-Equivalent-Node (set? list))
	    (:predicate Equivalent-Node?))
  Set?
  ;; If `set?' is true, this is just a list of values; otherwise, it is an alist.
  (List nil :type list :read-only t))

;;; Speeds up `typep' slightly.
(declaim #+sbcl (sb-ext:freeze-type Equivalent-Node))

(declaim (inline Unwrap-Equivalent-Node))

;;; Custom comparison functions that can ever return `:unequal' must call this on each argument.
;;; Note that if one of the arguments is an `Equivalent-Node', the resulting comparison might
;;; return `:equal' or `:unequal' based on which value happened to be at the head of the list.
;;; Functions in this file that call the comparison function on values that could be
;;; `Equivalent-Node's must handle these cases identically; such code has more work to do anyway,
;;; such as calling `Equivalent-Set-Union' etc.
(defun Unwrap-Equivalent-Node (x)
  (if (Equivalent-Node? x)
      (if (Equivalent-Node-Set? x)
	  (car (Equivalent-Node-List x))
	(caar (Equivalent-Node-List x)))
    x))

(declaim (inline Make-Equivalent-Set))

(defun Make-Equivalent-Set (list)
  (Make-Equivalent-Node t list))

(declaim (ftype (function (t) fixnum) Set-Value-Size))

(defun Set-Value-Size (value)
  "The number of members represented by `value', which can be more than 1 if
`value' is an `Equivalent-Node'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Node? value)
      (length (Equivalent-Node-List value))
    1))


(declaim (ftype (function (WB-Set-Tree) fixnum) WB-Set-Tree-Size))
(declaim (inline WB-Set-Tree-Size))

(defun WB-Set-Tree-Size (tree)
  "The number of members contained in this tree."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) 0)
	((simple-vector-p tree) (length tree))
	(t (WB-Set-Tree-Node-Size tree))))


(declaim (inline Make-WB-Set-Tree-Node))

(defun Make-WB-Set-Tree-Node (value left right)
  "The low-level constructor for a set tree node."
  (declare (optimize (speed 3) (safety 0)))
  (Make-Raw-WB-Set-Tree-Node (the fixnum
			       (+ (WB-Set-Tree-Size left) (WB-Set-Tree-Size right)
				  (Set-Value-Size value)))
			     value left right))


(defun WB-Set-Tree-Arb (tree)
  "Selects an arbitrary member of the set.  Assumes `tree' is nonnull."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree)
	 (error "`WB-Set-Tree-Arb' called on empty tree"))
	((simple-vector-p tree)
	 (svref tree 0))
	(t
	 (let ((value (WB-Set-Tree-Node-Value tree)))
	   (if (Equivalent-Node? value)
	       (car (Equivalent-Node-List value))
	     value)))))

(defun WB-Set-Tree-Least (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (let ((val (WB-Set-Tree-Minimum-Value tree)))
    (if (Equivalent-Node? val)
	(car (Equivalent-Node-List val))
      val)))

#|| Don't think I'm going to use this.
(defun WB-Set-Tree-Less-Least (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (and (> (length tree) 1) (Vector-Subseq tree 1)))
	(t
	 (let ((left (WB-Set-Tree-Node-Left tree)))
	   (if left
	       (WB-Set-Tree-Build-Node (WB-Set-Tree-Node-Value tree)
				       (WB-Set-Tree-Less-Least left)
				       (WB-Set-Tree-Node-Right tree))
	     (let ((val (WB-Set-Tree-Node-Value tree)))
	       (if (Equivalent-Node? val)
		   (let ((mems (Equivalent-Node-List val)))
		     (Make-WB-Set-Tree-Node (if (= (length mems) 2)
						(cadr mems)
					      (Make-Equivalent-Node t (cdr mems)))
					    nil
					    (WB-Set-Tree-Node-Right tree)))
		 (WB-Set-Tree-Node-Right tree))))))))
||#

(defun WB-Set-Tree-Greatest (tree)
  "Assumes `tree' is nonempty.  Returns the greatest member, or an arbitrary
greatest member if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (if (simple-vector-p tree)
      (svref tree (1- (length tree)))
    (let ((right (WB-Set-Tree-Node-Right tree)))
      (if right
	  (WB-Set-Tree-Greatest right)
	(let ((val (WB-Set-Tree-Node-Value tree)))
	  (if (Equivalent-Node? val)
	      (car (cl:last (Equivalent-Node-List val)))
	    val))))))

(defun WB-Set-Tree-Contains? (tree value cmp-fn)
  "Returns true iff `value' is a member of `tree'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (eq (Vector-Set-Binary-Search tree value cmp-fn) ':equal))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal t)
	     ((:unequal)
	      (and (Equivalent-Node? node-val)
		   (member value (Equivalent-Node-List node-val) :test (equal?-fn cmp-fn))))
	     ((:less)
	      (WB-Set-Tree-Contains? (WB-Set-Tree-Node-Left tree) value cmp-fn))
	     ((:greater)
	      (WB-Set-Tree-Contains? (WB-Set-Tree-Node-Right tree) value cmp-fn)))))))

(defun WB-Set-Tree-Find-Equivalent (tree value cmp-fn)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true and (second value) either the one value or an `Equivalent-Set'
containing the values; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value cmp-fn)))
	   (and found? (values t (svref tree idx)))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val))
	     ((:less)
	      (WB-Set-Tree-Find-Equivalent (WB-Set-Tree-Node-Left tree) value cmp-fn))
	     ((:greater)
	      (WB-Set-Tree-Find-Equivalent (WB-Set-Tree-Node-Right tree) value cmp-fn)))))))

(defun WB-Set-Tree-Find-Equal (tree value cmp-fn)
  "If `tree' contains a value equal to `value', returns (first value) true and
\(second value) the value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value cmp-fn)))
	   (and (eq found? ':equal)
		(values t (svref tree idx)))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal (values t (if (Equivalent-Node? node-val)
				   (car (Equivalent-Node-List node-val))
				 node-val)))
	     (:unequal
	      (and (Equivalent-Node? node-val)
		   (let ((mem (member value (Equivalent-Node-List node-val) :test (equal?-fn cmp-fn))))
		     (and mem (values t (car mem))))))
	     ((:less)
	      (WB-Set-Tree-Find-Equal (WB-Set-Tree-Node-Left tree) value cmp-fn))
	     ((:greater)
	      (WB-Set-Tree-Find-Equal (WB-Set-Tree-Node-Right tree) value cmp-fn)))))))


;;; ================================================================================
;;; With

(declaim (ftype (function (simple-vector t function) (values t fixnum)) Vector-Set-Binary-Search))

(defun WB-Set-Tree-With (tree value cmp-fn)
  "If `value' is in `tree', returns `tree'; otherwise returns `tree' with
`value' added.  `value' may be an `Equivalent-Node'."
  ;; The case where `value' is an `Equivalent-Node' is used by `WB-Set-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree)
	 (if (not (Equivalent-Node? value))
	     (vector value)
	   (Make-WB-Set-Tree-Node value nil nil)))
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Node', because
	   ;; this routine is called by `WB-Set-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Node? value)))
	       tree
	     (if (and (not found?)
		      (< (length tree) WB-Tree-Max-Vector-Length)
		      (not (Equivalent-Node? value)))
		 (Vector-Insert tree idx value)
	       ;; Originally, I split the vector in half rather than at the point
	       ;; where `value' goes.  But in the not unlikely case where values
	       ;; are being inserted in order, this will give longer vectors.
	       (Make-WB-Set-Tree-Node (if found?
					  (Equivalent-Set-Union (svref tree idx) value cmp-fn)
					value)
				      (and (> idx 0)
					   (Vector-Subseq tree 0 idx))
				      (and (< right-start (length tree))
					   (Vector-Subseq tree right-start)))))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (and (not (Equivalent-Node? node-val)) (not (Equivalent-Node? value))
		       (eq comp ':equal))
		  tree
		(let ((eqvs (Equivalent-Set-Union node-val value cmp-fn)))
		  (if (eq eqvs node-val)
		      tree
		    (Make-WB-Set-Tree-Node eqvs
					   (WB-Set-Tree-Node-Left tree)
					   (WB-Set-Tree-Node-Right tree))))))
	     ((:less)
	      (let ((left (WB-Set-Tree-Node-Left tree))
		    ((new-left (WB-Set-Tree-With left value cmp-fn))))
		(if (eq new-left left)
		    tree
		  (WB-Set-Tree-Build-Node node-val new-left
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Set-Tree-Node-Right tree))
		    ((new-right (WB-Set-Tree-With right value cmp-fn))))
		(if (eq new-right right)
		    tree
		  (WB-Set-Tree-Build-Node node-val (WB-Set-Tree-Node-Left tree)
					  new-right)))))))))

(defun Vector-Insert (vec idx val)
  "Returns a new vector like `vec' but with `val' inserted at `idx'.  Careful --
does no bounds checking on `vec', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec))
	((new-vec (make-array (1+ len)))))
    (declare (type fixnum len))
    (dotimes (i idx)
      (setf (svref new-vec i) (svref vec i)))
    (setf (svref new-vec idx) val)
    (dotimes (i (- len idx))
      (setf (svref new-vec (+ idx i 1))
	    (svref vec (+ idx i))))
    new-vec))

;;; Specialized version should be faster than `cl:subseq'.
(defun Vector-Subseq (vec start &optional (end (length vec)))
  "Returns a subsequence of `vec' between `start' and `end', or `nil' if
the result would be of zero length.  Careful -- does no bounds checking
on `vec', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum start end))
  (and (> end start)
       (let ((len (the fixnum (- end start)))
	     ((new-vec (make-array len))))
	 (declare (fixnum len))
	 (dotimes (i len)
	   (setf (svref new-vec i) (svref vec (+ i start))))
	 new-vec)))


;;; ================================================================================
;;; Less

;;; Currently doesn't handle the case where `value' is an `Equivalent-Set' --
;;; any need to?
(defun WB-Set-Tree-Less (tree value cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value cmp-fn)))
	   (if (eq found? ':equal)
	       (Vector-Remove-At tree idx)
	     tree)))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (Equivalent-Node? node-val))
		  (if (eq comp ':unequal)
		      tree
		    (WB-Set-Tree-Join (WB-Set-Tree-Node-Left tree) (WB-Set-Tree-Node-Right tree) cmp-fn))
		(let ((ignore diff (Equivalent-Set-Difference node-val value cmp-fn)))
		  (declare (ignore ignore))	; difference can't be null
		  (WB-Set-Tree-Build-Node diff
					  (WB-Set-Tree-Node-Left tree)
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:less)
	      (let ((left (WB-Set-Tree-Node-Left tree))
		    ((new-left (WB-Set-Tree-Less left value cmp-fn))))
		(if (eq new-left left)
		    tree
		  (WB-Set-Tree-Build-Node node-val new-left
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Set-Tree-Node-Right tree))
		    ((new-right (WB-Set-Tree-Less right value cmp-fn))))
		(if (eq new-right right)
		    tree
		  (WB-Set-Tree-Build-Node node-val (WB-Set-Tree-Node-Left tree)
					  new-right)))))))))

(defun Vector-Remove-At (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type (unsigned-byte 24) idx))
  (let ((len (length vec)))
    (declare (type fixnum len))
    (and (> len 1)
	 (let ((new-vec (make-array (1- len))))
	   (dotimes (i idx)
	     (setf (svref new-vec i) (svref vec i)))
	   (dotimes (i (- len idx 1))
	     (setf (svref new-vec (+ idx i)) (svref vec (+ idx i 1))))
	   new-vec))))


;;; ================================================================================
;;; Hedge algorithm constants

(defconstant Hedge-Negative-Infinity
  '|&*$ Hedge negative infinity $*&|)

(defconstant Hedge-Positive-Infinity
  '|&*$ Hedge positive infinity $*&|)

(defun WB-Set-Tree-Split-Above (tree value cmp-fn)
  (WB-Set-Tree-Split tree value Hedge-Positive-Infinity cmp-fn))

(defun WB-Set-Tree-Split-Below (tree value cmp-fn)
  (WB-Set-Tree-Split tree Hedge-Negative-Infinity value cmp-fn))


;;; ================================================================================
;;; Union, intersection, and set difference

;;; Adams recommends using four versions of each of these routines, one for each
;;; boundedness case (no bound, a lower bound, an upper bound, or both).  He probably
;;; had to do that given that he was working in ML, but in Lisp it's easy to make
;;; up distinguished "negative infinity" and "positive infinity" values which, for
;;; all practical purposes, will never show up in sets.

(defun WB-Set-Tree-Union (tree1 tree2 cmp-fn)
  "Returns the union of `tree1' and `tree2'.  Runs in time linear in the total
sizes of the two trees."
  (declare (type function cmp-fn))
  (if (eq tree1 tree2)
      tree1
    (WB-Set-Tree-Union-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))

(defun WB-Set-Tree-Union-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the union of `tree1' with `tree2', considering only those members
that are above `lo' and below `hi', and assuming that the root values of `tree1'
and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ;; If the sets are historically related -- one was produced by a sufficiently
	;; small number of `with' and `less' operations on the other, or they are both
	;; related in this way to a third set -- then we might get lucky and find
	;; ourselves with the same subtree on both sides.  This can reduce this
	;; linear-time algorithm to log-time.
	((eq tree1 tree2) (WB-Set-Tree-Split tree1 lo hi cmp-fn))
	((null tree2)
	 (WB-Set-Tree-Split tree1 lo hi cmp-fn))
	((null tree1)
	 (WB-Set-Tree-Split tree2 lo hi cmp-fn))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (WB-Set-Tree-Vector-Union tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2 cmp-fn))))
	   (WB-Set-Tree-Concat (if eqvv1? (Equivalent-Set-Union eqvv1 val2 cmp-fn)
				 val2)
			       (WB-Set-Tree-Union-Rng (WB-Set-Tree-Trim tree1 lo val2 cmp-fn)
						      ;; We have to trim the children too, because their
						      ;; node values might be outside (lo, hi).
						      (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree2)
									lo val2 cmp-fn)
						      lo val2 cmp-fn)
			       (WB-Set-Tree-Union-Rng (WB-Set-Tree-Trim tree1 val2 hi cmp-fn)
						      (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree2)
									val2 hi cmp-fn)
						      val2 hi cmp-fn)
			       cmp-fn)))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn))))
	   (WB-Set-Tree-Concat (if eqvv2? (Equivalent-Set-Union val1 eqvv2 cmp-fn)
				 val1)
			       (WB-Set-Tree-Union-Rng (WB-Set-Tree-Node-Left tree1)
						      (WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
						      lo val1 cmp-fn)
			       (WB-Set-Tree-Union-Rng (WB-Set-Tree-Node-Right tree1)
						      (WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
						      val1 hi cmp-fn)
			       cmp-fn)))))

(defun WB-Set-Tree-Intersect (tree1 tree2 cmp-fn)
  "Returns the intersection of `tree1' and `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (declare (type function cmp-fn))
  (WB-Set-Tree-Intersect-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Set-Tree-Intersect-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2)		; historically-related-set optimization
	 (WB-Set-Tree-Split tree1 lo hi cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Intersect tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((new-left
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Trim tree1 lo val2 cmp-fn)
					     (WB-Set-Tree-Node-Left tree2)
					     lo val2 cmp-fn))
		(new-right
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Trim tree1 val2 hi cmp-fn)
					     (WB-Set-Tree-Node-Right tree2)
					     val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull? isect (and eqvv1? (Equivalent-Set-Intersect eqvv1 val2 cmp-fn))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat isect new-left new-right cmp-fn)
	     (WB-Set-Tree-Join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Node-Left tree1)
					     (WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
					     lo val1 cmp-fn))
		(new-right
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Node-Right tree1)
					     (WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
					     val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((nonnull? isect (and eqvv2? (Equivalent-Set-Intersect val1 eqvv2 cmp-fn))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat isect new-left new-right cmp-fn)
	     (WB-Set-Tree-Join new-left new-right cmp-fn))))))


(defun WB-Set-Tree-Diff (tree1 tree2 cmp-fn)
  "Returns the set difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (declare (type function cmp-fn))
  (WB-Set-Tree-Diff-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Set-Tree-Diff-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the set difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) nil)		; historically-related-set optimization
	((null tree1) nil)
	((null tree2)
	 (WB-Set-Tree-Split tree1 lo hi cmp-fn))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Diff tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((new-left (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Trim tree1 lo val2 cmp-fn)
						(WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree2) lo val2 cmp-fn)
						lo val2 cmp-fn))
		(new-right (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Trim tree1 val2 hi cmp-fn)
						 (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree2) val2 hi cmp-fn)
						 val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull? diff (and eqvv1? (Equivalent-Set-Difference eqvv1 val2 cmp-fn))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat diff new-left new-right cmp-fn)
	     (WB-Set-Tree-Join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Node-Left tree1)
						(WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
						lo val1 cmp-fn))
		(new-right (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Node-Right tree1)
						 (WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
						 val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((nonnull? diff (if eqvv2? (Equivalent-Set-Difference val1 eqvv2 cmp-fn)
				  (values t val1))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat diff new-left new-right cmp-fn)
	     (WB-Set-Tree-Join new-left new-right cmp-fn))))))


(defun WB-Set-Tree-Diff-2 (tree1 tree2 cmp-fn)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1'.  Runs in time linear in the total sizes of the two trees."
  (WB-Set-Tree-Diff-2-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Set-Tree-Diff-2-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1', considering only those members that are above `lo' and
below `hi', and assuming that the root values of `tree1' and `tree2' are in
this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) (values nil nil)) ; historically-related tree optimization
	((or (null tree1) (null tree2))
	 (values (WB-Set-Tree-Split tree1 lo hi cmp-fn)
		 (WB-Set-Tree-Split tree2 lo hi cmp-fn)))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Diff-2 tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((new-left-1 new-left-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Trim tree1 lo val2 cmp-fn)
					  (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree2) lo val2 cmp-fn)
					  lo val2 cmp-fn))
		(new-right-1 new-right-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Trim tree1 val2 hi cmp-fn)
					  (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree2) val2 hi cmp-fn)
					  val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull1? diff1 (and eqvv1? (Equivalent-Set-Difference eqvv1 val2 cmp-fn)))
		 (nonnull2? diff2 (if eqvv1? (Equivalent-Set-Difference val2 eqvv1 cmp-fn)
				    (values t val2))))))
	   (values
	     (if nonnull1?
		 (WB-Set-Tree-Concat diff1 new-left-1 new-right-1 cmp-fn)
	       (WB-Set-Tree-Join new-left-1 new-right-1 cmp-fn))
	     (if nonnull2?
		 (WB-Set-Tree-Concat diff2 new-left-2 new-right-2 cmp-fn)
	       (WB-Set-Tree-Join new-left-2 new-right-2 cmp-fn)))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left-1 new-left-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Node-Left tree1)
					  (WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
					  lo val1 cmp-fn))
		(new-right-1 new-right-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Node-Right tree1)
					  (WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
					  val1 hi cmp-fn))
		((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn))
		 ((nonnull1? diff1 (if eqvv2? (Equivalent-Set-Difference val1 eqvv2 cmp-fn)
				     (values t val1)))
		  (nonnull2? diff2 (and eqvv2? (Equivalent-Set-Difference eqvv2 val1 cmp-fn)))))))
	   (values
	     (if nonnull1?
		 (WB-Set-Tree-Concat diff1 new-left-1 new-right-1 cmp-fn)
	       (WB-Set-Tree-Join new-left-1 new-right-1 cmp-fn))
	     (if nonnull2?
		 (WB-Set-Tree-Concat diff2 new-left-2 new-right-2 cmp-fn)
	       (WB-Set-Tree-Join new-left-2 new-right-2 cmp-fn)))))))


;;; ================================================================================
;;; Comparison

(defun WB-Set-Tree-Compare (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((size1 (WB-Set-Tree-Size tree1))
	  (size2 (WB-Set-Tree-Size tree2)))
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t (WB-Set-Tree-Compare-Rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun WB-Set-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi cmp-fn)
  ;; This is similar to the other hedge algorithms, but there is a key difference:
  ;; it is concerned not with the values of nodes but with their rank, that is,
  ;; the number of values to their left.  The `base' parameters specify, for
  ;; each tree, the number of values to the left of the tree.
  ;; Another subtlety: we can return as soon as we get a comparison result of
  ;; ':less or ':greater, but ':unequal has to wait until the end.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi)
	   (type function cmp-fn))
  (cond ((and (eq tree1 tree2) (= base1 base2)) ; historically-related-set optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (let ((unequal? nil))
	   (or (gmap (:result or) (fn (val1 val2)
				    (let ((comp (funcall cmp-fn val1 val2)))
				      (when (eq comp ':unequal)
					(setq unequal? t))
				      (and (or (eq comp ':less) (eq comp ':greater))
					   comp)))
		     (:arg simple-vector tree1 :start (- lo base1) :stop (- hi base1))
		     (:arg simple-vector tree2 :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((simple-vector-p tree1)
	 (invert-comparison (WB-Set-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (WB-Set-Tree-Node-Left tree1))
	       ((left1-size (WB-Set-Tree-Size left1))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ;; See the comment beginning `Subtlety:' in `WB-Set-Tree-Union-Rng'.
		 ((left1a base1a (WB-Set-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Set-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Set-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (WB-Set-Tree-Node-Value tree1))
		   (val2 (WB-Set-Tree-Rank-Element-Internal
			   tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (Equivalent-Set-Compare val1 val2 cmp-fn))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (Set-Value-Size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (WB-Set-Tree-Rank-Trim (WB-Set-Tree-Node-Right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (WB-Set-Tree-Rank-Trim tree2 base2 new-lo hi))
			 ((right-comp (WB-Set-Tree-Compare-Rng
					right1a base1a tree2a base2a new-lo hi cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal val-comp))))))))))

(defun WB-Set-Tree-Rank-Trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type fixnum base lo hi)
	   #+(or cmu scl)
	   (values WB-Set-Tree fixnum))
  (if (or (null tree) (simple-vector-p tree))
      (values tree base)
    (let ((node-rank (+ base (WB-Set-Tree-Size (WB-Set-Tree-Node-Left tree)))))
      (declare (type fixnum node-rank))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (WB-Set-Tree-Rank-Trim (WB-Set-Tree-Node-Left tree) base lo hi))
	(WB-Set-Tree-Rank-Trim (WB-Set-Tree-Node-Right tree)
			       (+ node-rank
				  (Set-Value-Size (WB-Set-Tree-Node-Value tree)))
			       lo hi)))))

(defun WB-Set-Tree-Rank (tree value cmp-fn)
  "Searches a set tree `tree' for `value'.  Returns two values, a boolean and an
index.  If `value', or a value equivalent to `value', is in `tree', the boolean
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `value' would have if it were to be added.  Note that
if the set contains equivalent-but-unequal elements, the rank of each of several
such elements is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (declare (type function cmp-fn))
  (labels ((rec (tree value base)
	     (cond ((null tree) (values nil base))
		   ((simple-vector-p tree)
		    (let ((found? idx (Vector-Set-Binary-Search tree value cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Set-Tree-Node-Value tree))
			  (left (WB-Set-Tree-Node-Left tree))
			  ((left-size (WB-Set-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall cmp-fn value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Node? node-val)
			     (let ((mems (Equivalent-Node-List node-val))
				   ((pos (cl:position value mems :test (equal?-fn cmp-fn)))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left value base))
			((:greater)
			 (rec (WB-Set-Tree-Node-Right tree) value
			      (+ node-base (Set-Value-Size node-val))))))))))
    (rec tree value 0)))

(defun WB-Set-Tree-Rank-Element (tree rank)
  (let ((elt rem (WB-Set-Tree-Rank-Element-Internal tree rank)))
    (if (Equivalent-Node? elt)
	(nth rem (Equivalent-Node-List elt))
      elt)))

(defun WB-Set-Tree-Rank-Element-Internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in set comparator"))
	((simple-vector-p tree)
	 (values (svref tree rank) 0))
	(t
	 (let ((left (WB-Set-Tree-Node-Left tree))
	       ((left-size (WB-Set-Tree-Size left))))
	   (if (< rank left-size)
	       (WB-Set-Tree-Rank-Element-Internal left rank)
	     (let ((val (WB-Set-Tree-Node-Value tree))
		   ((val-size (Set-Value-Size val))
		    (rank (- rank left-size))))
	       (declare (type fixnum rank))
	       (if (< rank val-size)
		   (values val rank)
		 (WB-Set-Tree-Rank-Element-Internal (WB-Set-Tree-Node-Right tree)
						    (- rank val-size)))))))))


;;; ================================================================================
;;; Subset testing

(defun WB-Set-Tree-Subset? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (let ((size1 (WB-Set-Tree-Size tree1))
	(size2 (WB-Set-Tree-Size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (WB-Set-Tree-Subset?-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))))

(defun WB-Set-Tree-Subset?-Rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) t)		; historically-related-set optimization
	((null tree1) t)
	((and (simple-vector-p tree1) (or (null tree2) (simple-vector-p tree2)))
	 (Vector-Set-Subset? tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2)))
	   (and (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Trim tree1 lo val2 cmp-fn)
					 (WB-Set-Tree-Node-Left tree2)
					 lo val2 cmp-fn)
		(let ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2 cmp-fn)))
		  (and (or (not eqvv1?)
			   (Equivalent-Set-Subset? eqvv1 val2 cmp-fn))
		       (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Trim tree1 val2 hi cmp-fn)
						(WB-Set-Tree-Node-Right tree2)
						val2 hi cmp-fn))))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1)))
	   (and (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Node-Left tree1)
					 (WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
					 lo val1 cmp-fn)
		(let ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn)))
		  (and eqvv2?
		       (Equivalent-Set-Subset? val1 eqvv2 cmp-fn)
		       (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Node-Right tree1)
						(WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
						val1 hi cmp-fn))))))))


;;; ================================================================================
;;; Disjointness testing

(defun WB-Set-Tree-Disjoint? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (WB-Set-Tree-Disjoint?-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Set-Tree-Disjoint?-Rng (tree1 tree2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 t)
	((eq tree1 tree2)
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Disjoint? tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Trim tree2 lo hi cmp-fn)
				    tree1 lo hi cmp-fn))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1 cmp-fn))))
	   (and (or (null eqvv2?) (Equivalent-Set-Disjoint? val1 eqvv2 cmp-fn))
		(WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Node-Left tree1)
					   (WB-Set-Tree-Trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn)
		(WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Node-Right tree1)
					   (WB-Set-Tree-Trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn))))))

;;; ================================================================================
;;; Miscellany

(defun WB-Set-Tree-From-List (lst cmp-fn)
  (let ((tree nil))
    (dolist (x lst)
      (setq tree (WB-Set-Tree-With tree x cmp-fn)))
    tree))

(defun WB-Set-Tree-From-Iterable (it cmp-fn)
  (declare (type function it))
  (let ((tree nil))
    (while (funcall it ':more?)
      (setq tree (WB-Set-Tree-With tree (funcall it ':get) cmp-fn)))
    tree))

;;; Much faster than repeated `with' if the input is sorted.  Still correct if the
;;; input is not actually sorted, but if it isn't even close to sorted, this is slower.
(defun WB-Set-Tree-From-Sorted-Iterable (it len cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function it cmp-fn))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil Hedge-Positive-Infinity Hedge-Negative-Infinity))
		   ((= n 1)
		    (let ((e (funcall it ':get)))
		      (values (vector e) e e)))
		   ;; Reduces consing about 12%, improves speed.
		   ((= n 2)
		    (let ((a (funcall it ':get))
			  (b (funcall it ':get)))
		      (ecase (funcall cmp-fn a b)
			(:equal (values (vector a) a a))
			(:less (values (vector a b) a b))
			(:greater (values (vector b a) b a))
			(:unequal (values (WB-Set-Tree-With (vector a) b cmp-fn) a a)))))
		   (t
		    (let ((n2 (floor (1- n) 2))
			  ((left left-first left-last (recur n2))
			   ((n2-elt (funcall it ':get))
			    ((right right-first right-last (recur (- n n2 1)))))))
		      ;; Here we check whether the tree really is sorted as promised.
		      ;; (We really have to do this for correctness, because even if it is sorted, it
		      ;; could have sequences of equivalent-but-unequal elements.)
		      (if (and (less-than?-cmp left-last n2-elt cmp-fn)
			       (less-than?-cmp n2-elt right-first cmp-fn))
			  (values (WB-Set-Tree-Build-Node n2-elt left right) left-first right-last)
			;; Fall back to general case.
			(values (WB-Set-Tree-With (WB-Set-Tree-Union left right cmp-fn) n2-elt cmp-fn)
				(if (less-than?-cmp left-first right-first cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last cmp-fn) right-last left-last))))))))
    (recur len)))


;;; ================================================================================
;;; Support routines for the above (sets)

(defun Vector-Set-Binary-Search (vec value cmp-fn)
  "Searches a vector set `vec' for `value'.  Returns two values, a symbol and an
index.  If `value', or a value equivalent to `value', is in `vec', the symbol
is `:equal' resp. `:unequal', and the index is the position of the value;
otherwise, the symbol is `nil' and the index is where `value' would go if it
were to be inserted."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type function cmp-fn)
	   #+(or cmu scl)
	   (values t fixnum))
  (do ((lo 0)
       (hi (1- (length vec))))
      ((> lo hi)
       (values nil lo))
    (declare (type fixnum lo hi))
    (let ((mid (ash (the fixnum (+ lo hi)) -1))
	  ((vec-val (svref vec mid))
	   ((comp (funcall cmp-fn value vec-val)))))
      (ecase comp
	((:equal :unequal) (return (values comp mid)))
	(:less             (setq hi (1- mid)))
	(:greater          (setq lo (1+ mid)))))))

(defun Vector-Set-Binary-Search-Lo (vec lo cmp-fn)
  "Returns the index of the left edge of the first member of `vec' that is
above `lo'."
  (declare (type simple-vector vec)
	   (type function cmp-fn)
	   #+(or cmu sbcl scl)
	   (values fixnum))
  (let ((found? idx (Vector-Set-Binary-Search vec lo cmp-fn)))
    (if found? (1+ idx) idx)))

(defun Vector-Set-Binary-Search-Hi (vec hi cmp-fn)
  "Returns the index of the right edge of the last member of `vec' that is
below `hi'."
  (declare (type simple-vector vec)
	   (type function cmp-fn)
	   #+(or cmu sbcl scl)
	   (values fixnum))
  (let ((found? idx (Vector-Set-Binary-Search vec hi cmp-fn)))
    (declare (ignore found?))
    idx))

(declaim (ftype (function (simple-vector t function) fixnum) Vector-Set-Binary-Search-Lo))
(declaim (ftype (function (simple-vector t function) fixnum) Vector-Set-Binary-Search-Hi))

(defun WB-Set-Tree-Split (tree lo hi cmp-fn)
  "Corresponds to Adams' `split_lt' and `split_gt'.  Returns a tree containing
those members of `tree' above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((simple-vector-p tree)
	 (let ((len (length tree))
	       ((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				    0
				  (Vector-Set-Binary-Search-Lo tree lo cmp-fn)))
		(split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi tree hi cmp-fn)))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (Vector-Subseq tree split-point-lo split-point-hi)))))
	((and (not (eq lo Hedge-Negative-Infinity))
	      (not (greater-than?-cmp (WB-Set-Tree-Node-Value tree) lo cmp-fn)))
	 (WB-Set-Tree-Split (WB-Set-Tree-Node-Right tree) lo hi cmp-fn))
	((and (not (eq hi Hedge-Positive-Infinity))
	      (not (less-than?-cmp (WB-Set-Tree-Node-Value tree) hi cmp-fn)))
	 (WB-Set-Tree-Split (WB-Set-Tree-Node-Left tree) lo hi cmp-fn))
	(t
	 (let ((new-left (WB-Set-Tree-Split (WB-Set-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity cmp-fn))
	       (new-right (WB-Set-Tree-Split (WB-Set-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi cmp-fn)))
	   (if (and (eq new-left (WB-Set-Tree-Node-Left tree))
		    (eq new-right (WB-Set-Tree-Node-Right tree)))
	       tree
	     (WB-Set-Tree-Concat (WB-Set-Tree-Node-Value tree) new-left new-right cmp-fn))))))

(defun WB-Set-Tree-Trim (tree lo hi cmp-fn)
  "Corresponds to Adams' `trim' and variants.  Removes any tree nodes whose
values are less than `lo' or greater than `hi'.  Note, this does _not_ guarantee
that the result only contains values between `lo' and `hi'; use `-Split' for
that.  This, however, doesn't cons."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 ;; If the vector is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than?-cmp (svref tree (1- (length tree))) lo cmp-fn))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than?-cmp (svref tree 0) hi cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo Hedge-Negative-Infinity)
					0
				      (Vector-Set-Binary-Search-Lo tree lo cmp-fn)))
		    (split-point-hi (if (eq hi Hedge-Positive-Infinity)
					(length tree)
				      (Vector-Set-Binary-Search-Hi tree hi cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((val (WB-Set-Tree-Node-Value tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than?-cmp val lo cmp-fn))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than?-cmp val hi cmp-fn))
		   tree
		 (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree) lo hi cmp-fn))
	     (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree) lo hi cmp-fn))))))

(defun WB-Set-Tree-Concat (value left right cmp-fn)
  "Corresponds to Adams' `concat3'.  Assumes that (all values in `left') <=
`value' <= (all values in `right'); returns a new tree containing all values.
This does more rebalancing than `WB-Set-Tree-Build-Node', which otherwise
has the same contract.  `value' may be an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree left right))
  (cond ((null left)
	 (WB-Set-Tree-With right value cmp-fn))
	((null right)
	 (WB-Set-Tree-With left value cmp-fn))
	((and (WB-Set-Tree-Node? left)
	      (> (WB-Set-Tree-Size left)
		 (the fixnum
		   (* (WB-Set-Tree-Size right) WB-Tree-Balance-Factor))))
	 (WB-Set-Tree-Build-Node (WB-Set-Tree-Node-Value left)
				 (WB-Set-Tree-Node-Left left)
				 (WB-Set-Tree-Concat value (WB-Set-Tree-Node-Right left) right cmp-fn)))
	((and (WB-Set-Tree-Node? right)
	      (> (WB-Set-Tree-Size right)
		 (the fixnum
		   (* (WB-Set-Tree-Size left) WB-Tree-Balance-Factor))))
	 (WB-Set-Tree-Build-Node (WB-Set-Tree-Node-Value right)
				 (WB-Set-Tree-Concat value left (WB-Set-Tree-Node-Left right) cmp-fn)
				 (WB-Set-Tree-Node-Right right)))
	(t
	 (WB-Set-Tree-Build-Node value left right))))

(defun WB-Set-Tree-Join (left right cmp-fn)
  "Returns the union of `left' and `right' under the assumption that all values
in `left' are less than any value in `right'."
  (if (null left) right
    (if (null right) left
      (let ((val (WB-Set-Tree-Minimum-Value right)))
	(WB-Set-Tree-Concat val left (WB-Set-Tree-Less-Minimum right cmp-fn) cmp-fn)))))

(defun WB-Set-Tree-Minimum-Value (tree)
  "Assumes `tree' is nonempty.  Returns the minimum value.  This may be an
`Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (if (simple-vector-p tree)
      (svref tree 0)
    (let ((left (WB-Set-Tree-Node-Left tree)))
      (if left
	  (WB-Set-Tree-Minimum-Value left)
	(WB-Set-Tree-Node-Value tree)))))

(defun WB-Set-Tree-Less-Minimum (tree cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum value
or `Equivalent-Set' removed."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (if (simple-vector-p tree)
      (and (> (length (the simple-vector tree)) 1) (Vector-Subseq tree 1))
    (let ((left (WB-Set-Tree-Node-Left tree)))
      (if left
	  (WB-Set-Tree-Concat (WB-Set-Tree-Node-Value tree) (WB-Set-Tree-Less-Minimum left cmp-fn)
			      (WB-Set-Tree-Node-Right tree) cmp-fn)
	(WB-Set-Tree-Node-Right tree)))))

(defun WB-Set-Tree-Build-Node (value left right)
  "Constructs a `WB-Set-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree left right))
  (cond ((and (or (null left) (simple-vector-p left))
	      (or (null right) (simple-vector-p right)))
	 (if (and (not (Equivalent-Node? value))
		  (< (+ (length-nv left) (length-nv right))
		     WB-Tree-Max-Vector-Length))
	     (concatenate 'simple-vector left (vector value) right)
	   (Make-WB-Set-Tree-Node value left right)))
	(t
	 (let ((sizl (WB-Set-Tree-Size left))
	       (sizr (WB-Set-Tree-Size right)))
	   ;; This code is subtly different from Adams's in order to create more
	   ;; opportunities to coalesce adjacent short vectors.  &&& We won't really
	   ;; know how well we're doing here, or how we might improve further, until
	   ;; we have some real applications running and can gather some stats.
	   (cond ((and (WB-Set-Tree-Node? left)
		       (> sizl (the fixnum (* sizr WB-Tree-Balance-Factor))))
		  (let ((ll (WB-Set-Tree-Node-Left left))
			(rl (WB-Set-Tree-Node-Right left)))
		    (if (or (null rl) (simple-vector-p rl)
			    (<= (WB-Set-Tree-Size rl) (WB-Set-Tree-Size ll)))
			(Make-WB-Set-Tree-Node (WB-Set-Tree-Node-Value left)
					       ll
					       (WB-Set-Tree-Build-Node value rl right))
		      (Make-WB-Set-Tree-Node (WB-Set-Tree-Node-Value rl)
					     (WB-Set-Tree-Build-Node
					       (WB-Set-Tree-Node-Value left)
					       ll
					       (WB-Set-Tree-Node-Left rl))
					     (WB-Set-Tree-Build-Node
					       value (WB-Set-Tree-Node-Right rl)
					       right)))))
		 ((and (WB-Set-Tree-Node? right)
		       (> sizr (the fixnum (* sizl WB-Tree-Balance-Factor))))
		  (let ((lr (WB-Set-Tree-Node-Left right))
			(rr (WB-Set-Tree-Node-Right right)))
		    (if (or (null lr) (simple-vector-p lr)
			    (<= (WB-Set-Tree-Size lr) (WB-Set-Tree-Size rr)))
			(Make-WB-Set-Tree-Node (WB-Set-Tree-Node-Value right)
					       (WB-Set-Tree-Build-Node value left lr)
					       rr)
		      (Make-WB-Set-Tree-Node (WB-Set-Tree-Node-Value lr)
					     (WB-Set-Tree-Build-Node
					       value left (WB-Set-Tree-Node-Left lr))
					     (WB-Set-Tree-Build-Node
					       (WB-Set-Tree-Node-Value right)
					       (WB-Set-Tree-Node-Right lr)
					       rr)))))
		 (t
		  (Make-WB-Set-Tree-Node value left right)))))))


(defun WB-Set-Tree-Verify (tree cmp-fn)
  (macrolet ((test (form)
	       `(or ,form
		    (progn
		      (cerror "Ignore and proceed."
			      "WB-Set verification check failed: ~S" ',form)
		      t))))
    (rlabels (rec tree Hedge-Negative-Infinity Hedge-Positive-Infinity)
      (rec (tree lo hi)
	(cond ((null tree) t)
	      ((simple-vector-p tree)
	       (let ((len (length tree)))
		 (and (<= len WB-Tree-Max-Vector-Length)
		      (let ((prev lo))
			(and (gmap :and
				   (fn (elt)
				     (prog1 (and (test (not (Equivalent-Node? elt)))
						 (or (eq prev Hedge-Negative-Infinity)
						     (test (less-than?-cmp prev elt cmp-fn))))
				       (setq prev elt)))
				   (:arg simple-vector tree))
			     (or (eq hi Hedge-Positive-Infinity)
				 (test (less-than?-cmp prev hi cmp-fn))))))))
	      (t
	       (let ((sizl (WB-Set-Tree-Size (WB-Set-Tree-Node-Left tree)))
		     (sizr (WB-Set-Tree-Size (WB-Set-Tree-Node-Right tree)))
		     (value (WB-Set-Tree-Node-Value tree)))
		 (and (test (= (WB-Set-Tree-Node-Size tree) (+ sizl sizr (Set-Value-Size value))))
		      (test (or (eq lo Hedge-Negative-Infinity) (less-than?-cmp lo value cmp-fn)))
		      (test (or (eq hi Hedge-Positive-Infinity) (less-than?-cmp value hi cmp-fn)))
		      (test (or (not (Equivalent-Node? value))
				(> (length (Equivalent-Node-List value)) 1)))
		      (test (or (<= sizr 4)
				(<= sizl (* sizr WB-Tree-Balance-Factor))))
		      (test (or (<= sizl 4)
				(<= sizr (* sizl WB-Tree-Balance-Factor))))
		      (rec (WB-Set-Tree-Node-Left tree) lo value)
		      (rec (WB-Set-Tree-Node-Right tree) value hi)))))))))


;;; ================================================================================
;;; Vector set operations

(defun WB-Set-Tree-Vector-Union (vec1 vec2 lo hi cmp-fn)
  "Returns the union of vectors `vec1' and `vec2', restricted to those members
above `lo' and below `hi'.  Creates new set tree nodes if needed, either
because the result exceeds the vector threshold size, or because one or more
pairs of equivalent members were found."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((new-vec any-equivalent? (Vector-Set-Union vec1 vec2 lo hi cmp-fn)))
    (declare (type simple-vector new-vec))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(reduce (fn (st x) (WB-Set-Tree-With st x cmp-fn)) new-vec :initial-value nil)
      (if (> (length new-vec) WB-Tree-Max-Vector-Length)
	  (let ((split-point (floor (length new-vec) 2)))
	    (Make-WB-Set-Tree-Node (svref new-vec split-point)
				   (Vector-Subseq new-vec 0 split-point)
				   (Vector-Subseq new-vec (1+ split-point))))
	new-vec))))

(defun Vector-Set-Union (vec1 vec2 lo hi cmp-fn)
  "Returns, as a vector, the union of vectors `vec1' and `vec2', restricted to
those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      ;; We do these with linear rather than binary search because frequently,
      ;; the ends of the vectors will already be in range (the worst case for
      ;; binary search).
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vec2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vec2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((res nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (Reverse-List-To-Vector res)
		 any-equivalent?))
      (cond ((= i1 len1)
	     (do () ((= i2 len2))
	       (push (svref vec2 i2) res)
	       (incf i2)))
	    ((= i2 len2)
	     (do () ((= i1 len1))
	       (push (svref vec1 i1) res)
	       (incf i1)))
	    (t
	     (let ((v1 (svref vec1 i1))
		   (v2 (svref vec2 i2))
		   ((comp (funcall cmp-fn v1 v2))))
	       (ecase comp
		 ((:equal)
		  (push v1 res) ; prefer `v1'
		  (incf i1)
		  (incf i2))
		 ((:less)
		  (push v1 res)
		  (incf i1))
		 ((:greater)
		  (push v2 res)
		  (incf i2))
		 ((:unequal)
		  (push (Equivalent-Set-Union v1 v2 cmp-fn) res)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun Reverse-List-To-Vector (lst)
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (let ((len (length lst))
	((res (make-array len))))
    (do ((i (1- len) (1- i))
	 (lst lst (cdr lst)))
	((null lst) res)
      (setf (svref res i) (car lst)))))

;;; We don't need a `WB-Set-Tree-Vector-Intersect' because the intersection is
;;; never longer than the operands.

(defun Vector-Set-Intersect (vec1 vec2 lo hi cmp-fn)
  "Returns, as a vector, the intersection of vectors `vec1' and `vec2', restricted
to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (and res (Reverse-List-To-Vector res)))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (push v1 res) ; prefer `v1'
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Set-Diff (vec1 vec2 lo hi cmp-fn)
  "Returns, as a vector, the set difference of vectors `vec1' less `vec2',
restricted to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vec1 i1) res)
	   (incf i1))
	 (and res (Reverse-List-To-Vector res)))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (push v1 res)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (push v1 res)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Set-Diff-2 (vec1 vec2 lo hi cmp-fn)
  "Returns, as two vector values, the set difference of vectors `str1' less `str2'
and that of `str2' less `str1', restricted to those members above `lo' and below
`hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vec2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vec2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((res1 nil)
	 (res2 nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vec1 i1) res1)
	   (incf i1))
	 (do () ((= i2 len2))
	   (push (svref vec2 i2) res2)
	   (incf i2))
	 (values (and res1 (Reverse-List-To-Vector res1))
		 (and res2 (Reverse-List-To-Vector res2))))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (push v1 res1)
	   (incf i1))
	  ((:greater)
	   (push v2 res2)
	   (incf i2))
	  ((:unequal)
	   (push v1 res1)
	   (push v2 res2)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Set-Subset? (vec1 vec2 lo hi cmp-fn)
  "Returns true iff `vec2' contains all members of `vec1', restricted
to those members above `lo' and below `hi'.  `vec2' may be `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type (or null simple-vector) vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length-nv vec1))
	(len2 (length-nv vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 (= i1 len1))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (return nil))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (return nil)))))))

(defun Vector-Set-Disjoint? (vec1 vec2 lo hi cmp-fn)
  "Returns true iff `vec1' does not contain any member of `vec2', restricted
to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 t)
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (return nil))
	  ((:less)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (incf i1)
	   (incf i2)))))))

;;; ================================================================================
;;; Iteration primitives

(defmacro Do-WB-Set-Tree-Members ((var tree-form &optional value-form) &body body)
  ;; You may ask, why do we do this with a macro rather than a mapper when
  ;; we're going to have a function call for every invocation of the body anyway?
  ;; First, this local call is faster, or should be, than a general funcall; and
  ;; second, some compilers may decide to inline `body-fn' if the body is small.
  (let ((body-fn (gensymx #:body-))
	(recur-fn (gensymx #:recur-)))
    `(block nil
       (labels ((,body-fn (,var) . ,body)
		(,recur-fn (tree)
		  (when tree
		    (cond ((simple-vector-p tree)
			   (dotimes (i (length tree))
			     (,body-fn (svref tree i))))
			  (t
			   (,recur-fn (WB-Set-Tree-Node-Left tree))
			   (let ((val (WB-Set-Tree-Node-Value tree)))
			     (if (Equivalent-Node? val)
				 (dolist (val (Equivalent-Node-List val))
				   (,body-fn val))
			       (,body-fn val)))
			   (,recur-fn (WB-Set-Tree-Node-Right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))


;;; ----------------
;;; Stateful iterator

(defun Make-WB-Set-Tree-Iterator (tree)
  (let ((iter (Make-WB-Set-Tree-Iterator-Internal tree)))
    (lambda (op)
      (ecase op
	(:get (WB-Set-Tree-Iterator-Get iter))
	(:done? (WB-Set-Tree-Iterator-Done? iter))
	(:more? (not (WB-Set-Tree-Iterator-Done? iter)))))))

(defun Make-WB-Set-Tree-Iterator-Internal (tree)
  (WB-Set-Tree-Iterator-Canonicalize
    (Make-WB-Tree-Iterator tree (WB-Set-Tree-Size tree) 2 t)))

(defun WB-Set-Tree-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((sp (svref iter 0))
	  ((node (svref iter sp))
	   (idx (svref iter (1+ sp)))))
      (declare (fixnum sp idx))
      (cond ((null node)
	     (if (= sp 1)
		 (return)
	       (progn
		 (decf sp 2)
		 (setf (svref iter 0) sp)
		 (incf (the fixnum (svref iter (1+ sp)))))))
	    ((simple-vector-p node)
	     (cond ((< idx (length node))
		    (return))
		   ((= sp 1)
		    (setf (svref iter 1) nil)
		    (return))
		   (t
		    (decf sp 2)
		    (setf (svref iter 0) sp)
		    (incf (the fixnum (svref iter (1+ sp)))))))
	    ((= idx 0)
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (WB-Set-Tree-Node-Left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx (1+ (Set-Value-Size (WB-Set-Tree-Node-Value node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (WB-Set-Tree-Node-Right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun WB-Set-Tree-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun WB-Set-Tree-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx (svref iter (1+ sp)))))
    (declare (fixnum idx))
    (if (null node)
	(values nil nil)
      (progn
	(setf (svref iter (1+ sp)) (1+ idx))
	(if (simple-vector-p node)
	    (progn
	      (when (= (1+ idx) (length node))
		(WB-Set-Tree-Iterator-Canonicalize iter))
	      (values (svref node idx) t))
	  (let ((val (WB-Set-Tree-Node-Value node)))
	    (if (Equivalent-Node? val)
		(progn
		  (when (= idx (length (Equivalent-Node-List val)))
		    (WB-Set-Tree-Iterator-Canonicalize iter))
		  (values (nth (1- idx) (Equivalent-Node-List val)) t))
	      (progn
		(when (= idx 1)
		  (WB-Set-Tree-Iterator-Canonicalize iter))
		(values val t)))))))))


;;; ----------------
;;; Functional iterators.  Fun!!!

(defun WB-Set-Tree-Fun-Iter (tree &optional (cont (lambda (op)
						    (ecase op
						      (:first (values nil nil))
						      (:empty? t)
						      (:more? nil)))))
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree cont)
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((simple-vector-p node)
	     (let ((len (length node)))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (svref node i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     cont)))))
	    (t
	     (walk (WB-Set-Tree-Node-Left node)
		   (let ((value (WB-Set-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (Equivalent-Node-List value))
			   (iter (mems)
			     (if mems
				 (lambda (op)
				   (ecase op
				     (:first (values (car mems) t))
				     (:rest (iter (cdr mems)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Set-Tree-Node-Right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value t))
			   (:rest (walk (WB-Set-Tree-Node-Right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun WB-Set-Tree-Rev-Fun-Iter (tree &optional (cont (lambda (op)
							(ecase op
							  (:first (values nil nil))
							  (:empty? t)
							  (:more? nil)))))
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree cont)
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((simple-vector-p node)
	     (rlabels (iter (1- (length node)))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (lambda (op)
		       (ecase op
			 (:first (values (svref node i) t))
			 (:rest (iter (1- i)))
			 (:empty? nil)
			 (:more? t)))
		   cont))))
	    (t
	     (walk (WB-Set-Tree-Node-Right node)
		   (let ((value (WB-Set-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (reverse (Equivalent-Node-List value)))
			   (iter (mems)
			     (if mems
				 (lambda (op)
				   (ecase op
				     (:first (values (car mems) t))
				     (:rest (iter (cdr mems)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Set-Tree-Node-Left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value t))
			   (:rest (walk (WB-Set-Tree-Node-Left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ----------------
;;; Utilities used by all tree types in this file

(defun Make-WB-Tree-Iterator (tree size frame-size nodes-have-values?)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum frame-size))
  (let ((depth (the fixnum (WB-Tree-Max-Depth size nodes-have-values?)))
	((stack (make-array (the fixnum (1+ (the fixnum (* frame-size depth))))))))
    (setf (svref stack 0) 1)
    (setf (svref stack 1) tree)
    (dotimes (i (1- frame-size))
      (setf (svref stack (+ i 2)) 0))
    stack))

(defun WB-Tree-True-Max-Depth (size nodes-have-values?)
  (cond ((= size 0) 1)		; not really, but this is convenient
	((= size 1) 1)
	((= size 2) 2)
	(t
	 (let ((size (if nodes-have-values? (1- size) size))
	       ((subtree-max (min (1- size)
				  (floor (* size (/ WB-Tree-Balance-Factor
						    (1+ WB-Tree-Balance-Factor))))))))
	   (1+ (WB-Tree-True-Max-Depth subtree-max nodes-have-values?))))))

(defconstant WB-Tree-Precomputed-Max-Depths 1000)

(deflex +WB-Tree-Max-Depths-Without-Values+
    (gmap (:result vector) (fn (i) (WB-Tree-True-Max-Depth i nil))
	  (:arg index 0 WB-Tree-Precomputed-Max-Depths)))

(deflex +WB-Tree-Max-Depths-With-Values+
    (gmap (:result vector) (fn (i) (WB-Tree-True-Max-Depth i t))
	  (:arg index 0 WB-Tree-Precomputed-Max-Depths)))

(defun WB-Tree-Max-Depth (size nodes-have-values?)
  ;; For purposes of this worst-case analysis I ignore the leaf vectors, though I
  ;; think it would be possible to prove that they are always at least half full.
  ;; There's almost no cost to overestimating this by a few, so this tries to be
  ;; very fast and conservative.
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum size))
  (if (< size WB-Tree-Precomputed-Max-Depths)
      (svref (if nodes-have-values?
		 +WB-Tree-Max-Depths-With-Values+
	       +WB-Tree-Max-Depths-Without-Values+)
	     size)
    (ceiling (* (1- (integer-length size))
		;; constant:
		(/ (log 2) (log (/ (+ 1 WB-Tree-Balance-Factor)
				   WB-Tree-Balance-Factor)))))))


;;; ================================================================================
;;; Equivalent-Set routines

(defun Equivalent-Set-Union (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns their union represented as a single
value if a singleton, else as an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (if (Equivalent-Node? val2)
	  (let ((mems1 (Equivalent-Node-List val1))
		(mems2 (Equivalent-Node-List val2))
		((union mems1)) ; prefer `val1' values
		(any-new? nil))
	    (dolist (m2 mems2)
	      (unless (member m2 mems1 :test (equal?-fn cmp-fn))
		(push m2 union)
		(setq any-new? t)))
	    (if any-new? (Make-Equivalent-Set union)
	      val1))
	(if (member val2 (Equivalent-Node-List val1) :test (equal?-fn cmp-fn))
	    val1
	  (Make-Equivalent-Set (cons val2 (Equivalent-Node-List val1)))))
    (if (Equivalent-Node? val2)
	(let ((mems2 (Equivalent-Node-List val2))
	      ((pos (position val1 mems2 :test (equal?-fn cmp-fn)))))
	  (if pos
	      ;; prefer `val1', even in this case
	      (Make-Equivalent-Set (cons val1 (append (cl:subseq mems2 0 pos) (nthcdr (1+ pos) mems2))))
	    (Make-Equivalent-Set (cons val1 mems2))))
      (if (equal?-cmp val1 val2 cmp-fn) val1
	(Make-Equivalent-Set (list val1 val2))))))

(defun Equivalent-Set-Intersect (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is nonnull, returns
two values: true, and the intersection, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the intersection is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (if (Equivalent-Node? val2)
	  (let ((mems1 (Equivalent-Node-List val1))
		(mems2 (Equivalent-Node-List val2))
		((isect nil)))
	    (dolist (m1 mems1)
	      (when (member m1 mems2 :test (equal?-fn cmp-fn))
		(push m1 isect)))
	    (let ((isect-len (length isect)))
	      (cond ((null isect) nil)
		    ((= isect-len (length mems1)) (values t val1))
		    ((= isect-len 1) (values t (car isect)))
		    (t (values t (Make-Equivalent-Set isect))))))
	;; prefer `val1' value
	(let ((pos (position val2 (Equivalent-Node-List val1) :test (equal?-fn cmp-fn))))
	  (and pos (values t (nth pos (Equivalent-Node-List val1))))))
    (if (Equivalent-Node? val2)
	(and (member val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))
	     (values t val1))
      (and (equal?-cmp val1 val2 cmp-fn) (values t val1)))))

(defun Equivalent-Set-Difference (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their difference is nonnull, returns
two values: true, and the difference, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the difference is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((mems1 (Equivalent-Node-List val1))
	    (mems2 (if (Equivalent-Node? val2) (Equivalent-Node-List val2)
		     (list val2)))
	    ((diff (cl:set-difference mems1 mems2 :test (equal?-fn cmp-fn)))
	     ((diff-len (length diff)))))
	(cond ((null diff) nil)
	      ((= diff-len (length mems1)) (values t val1))
	      ((= diff-len 1) (values t (car diff)))
	      (t (values t (Make-Equivalent-Set diff)))))
    (if (Equivalent-Node? val2)
	(and (not (member val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn)))
	     (values t val1))
      (and (not (equal?-cmp val1 val2 cmp-fn)) (values t val1)))))

(defun Equivalent-Set-Subset? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns true iff `val2' contains all members
of `val1'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (and (Equivalent-Node? val2)
	   (let ((mems2 (Equivalent-Node-List val2)))
	     (dolist (m1 (Equivalent-Node-List val1) t)
	       (unless (member m1 mems2 :test (equal?-fn cmp-fn))
		 (return nil)))))
    (if (Equivalent-Node? val2)
	(member val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))
      (equal?-cmp val1 val2 cmp-fn))))

(defun Equivalent-Set-Disjoint? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is null, returns
true, else false."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (if (Equivalent-Node? val2)
	  (dolist (m1 (Equivalent-Node-List val1) t)
	    (when (member m1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))
	      (return nil)))
	(not (member val2 (Equivalent-Node-List val1) :test (equal?-fn cmp-fn))))
    (if (Equivalent-Node? val2)
	(not (member val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn)))
      (not (equal?-cmp val1 val2 cmp-fn)))))

(defun Equivalent-Set-Compare (val1 val2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (let ((comp (funcall cmp-fn val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Node? val1)
	  (if (Equivalent-Node? val2)
	      (let ((mems1 (Equivalent-Node-List val1))
		    (mems2 (Equivalent-Node-List val2))
		    ((len1 (length mems1))
		     (len2 (length mems2))))
		(cond ((< len1 len2) ':greater)	; counterintuitive, but correct
		      ((> len1 len2) ':less)
		      (t
		       (if (gmap (:result and)
				 (fn (x) (member x mems2 :test (equal?-fn cmp-fn)))
				 (:arg list mems1))
			   ':equal
			 ':unequal))))
	    ':less)
	(if (Equivalent-Node? val2)
	    ':greater
	  comp)))))


;;; When called on a value and an `Equivalent-Node', or on two `Equivalent-Node's,
;;; the result of `compare' is meaningful only for ordering; the distinction between
;;; `:equal' and `:unequal' is not meaningful.  Code that may care about the latter
;;; has more work to do anyway, such as calling `Equivalent-Set-Union' etc.
(defmethod compare (x (eqvn Equivalent-Node))
  "Returns `:less' or `:greater' if `x' is less than resp. greater than the
values in `eqvs'; or EITHER `:equal' or `:unequal' if `x' is equivalent to any
value in `eqvs'."
  (compare x (if (Equivalent-Node-Set? eqvn)
		 (car (Equivalent-Node-List eqvn))
	       (caar (Equivalent-Node-List eqvn)))))

(defmethod compare ((eqvn Equivalent-Node) x)
  "Returns `:less' or `:greater' if the values in `eqvs' are less than resp.
greater than `x'; or EITHER `:equal' or `:unequal' if `x' is equivalent to
any value in `eqvs'."
  (compare (if (Equivalent-Node-Set? eqvn)
	       (car (Equivalent-Node-List eqvn))
	     (caar (Equivalent-Node-List eqvn)))
	   x))

(defmethod compare ((eqvn1 Equivalent-Node) (eqvn2 Equivalent-Node))
  "Returns `:less' or `:greater' if the values in `eqvs1' are less than resp.
greater than those in `eqvs2'; returns EITHER `:equal' or `:unequal' if those
in `eqvs1' are equivalent to those in `eqvs2'."
  (compare (if (Equivalent-Node-Set? eqvn1)
	       (car (Equivalent-Node-List eqvn1))
	     (caar (Equivalent-Node-List eqvn1)))
	   (if (Equivalent-Node-Set? eqvn2)
	       (car (Equivalent-Node-List eqvn2))
	     (caar (Equivalent-Node-List eqvn2)))))


;;; ================================================================================
;;; ================================================================================
;;; Bags

(declaim (inline Make-Raw-WB-Bag-Tree-Node))

;;; A bag tree is either null, a node, or a cons of two simple-vectors.
(deftype WB-Bag-Tree ()
  '(or null WB-Bag-Tree-Node cons))

(defstruct (WB-Bag-Tree-Node
	    (:constructor Make-Raw-WB-Bag-Tree-Node (Size Total-Count Value Count
						     Left Right))
	    (:predicate WB-Bag-Tree-Node?)
	    (:print-function WB-Bag-Tree-Node-Print))
  (Left  nil :type WB-Bag-Tree :read-only t)
  (Right nil :type WB-Bag-Tree :read-only t)
  ;; If we get equivalent values, then the `Value' is an `Equivalent-Bag', and the
  ;; `Count' is unused.
  (Value nil :read-only t)			; the value at the node, or an `Equivalent-Bag'
  (Count 0 :type integer :read-only t)		; the count (multiplicity) for this value
  (Total-Count 0 :type integer :read-only t)	; total count of all values in this subtree
  (Size 0 :type fixnum :read-only t))		; the number of < value, count > pairs


(defun WB-Bag-Tree-Node-Print (node stream depth)
  "Print function for `WB-Bag-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#bag-node<~;~D/~D, ~S -> ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (WB-Bag-Tree-Node-Size node)
		    (WB-Bag-Tree-Node-Total-Count node)
		    (WB-Bag-Tree-Node-Value node)
		    (WB-Bag-Tree-Node-Count node)
		    (let ((sub (WB-Bag-Tree-Node-Left node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))
		    (let ((sub (WB-Bag-Tree-Node-Right node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))))
    (format stream "#bag-node<...>")))

(declaim (inline Make-Equivalent-Bag))

(defun Make-Equivalent-Bag (alist)
  (Make-Equivalent-Node nil alist))

(declaim (ftype (function (t) fixnum) Bag-Value-Size))
(declaim (inline Bag-Value-Size))

(defun Bag-Value-Size (value)
  "The number of values represented by `value', which can be more than 1 if
`key' is an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Node? value)
      (length (Equivalent-Node-List value))
    1))

(declaim (inline WB-Bag-Tree-Size))

(defun WB-Bag-Tree-Size (tree)
  "The number of value/count pairs contained in this tree."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) 0)
	((consp tree) (length (the simple-vector (car tree))))
	(t (WB-Bag-Tree-Node-Size tree))))

(declaim (ftype (function (WB-Bag-Tree) fixnum) WB-Bag-Tree-Size))

(defun WB-Bag-Tree-Total-Count (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   #+(or cmu sbcl scl)
	   (values integer))
  (cond ((null tree) 0)
	((consp tree) (reduce #'+ (cdr tree)))
	(t (WB-Bag-Tree-Node-Total-Count tree))))

(declaim (ftype (function (WB-Bag-Tree) integer) WB-Bag-Tree-Total-Count))


;;; This is just to get rid of compiler optimization notes.
(gmap:def-result-type gen-sum (&key filterp)
  "Returns the sum of the values, optionally filtered by `filterp', using
generic arithmetic."
  `(0 #'(lambda (x y) (gen + x y)) nil ,filterp))

(defun Make-WB-Bag-Tree-Node (value count left right)
  "The low-level constructor for a bag tree node.  `count' is ignored and can be
`nil' if value is an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree left right))
  (Make-Raw-WB-Bag-Tree-Node (gen + (WB-Bag-Tree-Size left) (WB-Bag-Tree-Size right)
				  (Bag-Value-Size value))
			     (gen + (WB-Bag-Tree-Total-Count left)
				  (WB-Bag-Tree-Total-Count right)
				  (if (Equivalent-Node? value)
				      (gmap (:result gen-sum) #'cdr
					    (:arg list (Equivalent-Node-List value)))
				    (or count 0)))
			     value (or count 0) left right))


(defun WB-Bag-Tree-Arb-Pair (tree)
  "Returns an arbitrary member of the bag and its count.  Assumes the bag is
nonempty."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (if (consp tree)
      (values (svref (car tree) 0) (svref (cdr tree) 0))
    (let ((value (WB-Bag-Tree-Node-Value tree)))
      (if (Equivalent-Node? value)
	  (let ((alist (Equivalent-Node-List value)))
	    (values (caar alist) (cdar alist)))
	(values value (WB-Bag-Tree-Node-Count tree))))))

(defun WB-Bag-Tree-Least-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one; the second value is the associated
count."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (let ((val count (WB-Bag-Tree-Minimum-Pair tree)))
    (if (Equivalent-Node? val)
	(let ((pr (car (Equivalent-Node-List val))))
	  (values (car pr) (cdr pr)))
      (values val count))))

#|| Don't think I'm going to use this.
(defun WB-Bag-Tree-Less-Least (tree all?)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (if (or all? (= 1 (the integer (svref (cdr tree) 0))))
	     (and (> (length (the simple-vector (car tree))) 1)
		  (cons (Vector-Subseq (car tree) 1)
			(Vector-Subseq (cdr tree) 1)))
	   (cons (car tree)
		 (Vector-Update (cdr tree) 0 (1- (the integer (svref (cdr tree) 0)))))))
	(t
	 (let ((left (WB-Bag-Tree-Node-Left tree)))
	   (if left
	       (WB-Bag-Tree-Build-Node (WB-Bag-Tree-Node-Value tree)
				       (WB-Bag-Tree-Node-Count tree)
				       (WB-Bag-Tree-Less-Least left all?)
				       (WB-Bag-Tree-Node-Right tree))
	     (let ((val (WB-Bag-Tree-Node-Value tree)))
	       (if (Equivalent-Node? val)
		   (let ((alist (Equivalent-Node-List val)))
		     (if (or all? (= (the integer (cdar alist)) 1))
			 (if (= (length alist) 2)
			     (Make-WB-Bag-Tree-Node (caadr alist) (cdadr alist) nil
						    (WB-Bag-Tree-Node-Right tree))
			   (Make-WB-Bag-Tree-Node (Make-Equivalent-Bag (cdr alist)) 0 nil
						  (WB-Bag-Tree-Node-Right tree)))
		       (Make-WB-Bag-Tree-Node
			 (Make-Equivalent-Bag (cons (cons (caar alist)
							  (1- (the integer (cdar alist))))
						    (cdr alist)))
			 0 nil (WB-Bag-Tree-Node-Right tree))))
		 (if (or all? (= (WB-Bag-Tree-Node-Count tree) 1))
		     (WB-Bag-Tree-Node-Right tree)
		   (Make-WB-Bag-Tree-Node (WB-Bag-Tree-Node-Value tree)
					  (1- (WB-Bag-Tree-Node-Count tree))
					  nil
					  (WB-Bag-Tree-Node-Right tree))))))))))
||#

(defun WB-Bag-Tree-Greatest-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the greatest member, or an arbitrary
greatest member if there are more than one; the second value is the associated
multiplicity."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (if (consp tree)
      (let ((idx (1- (length (the simple-vector (car tree))))))
	(values (svref (car tree) idx)
		(svref (cdr tree) idx)))
    (let ((right (WB-Bag-Tree-Node-Right tree)))
      (if right
	  (WB-Bag-Tree-Greatest-Pair right)
	(let ((val (WB-Bag-Tree-Node-Value tree)))
	  (if (Equivalent-Node? val)
	      (let ((pr (car (cl:last (Equivalent-Node-List val)))))
		(values (car pr) (cdr pr)))
	    (values val (WB-Bag-Tree-Node-Count tree))))))))

(defun WB-Bag-Tree-Multiplicity (tree value cmp-fn)
  "Returns the multiplicity of `value' in `tree', or 0 if `value' does not
appear in `tree'.  As a second value, returns the value found, if any."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) (values 0 nil))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value cmp-fn)))
	   (if (eq found? ':equal)
	       (values (svref (cdr tree) idx) (svref (car tree) idx))
	     (values 0 nil))))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal
	      (if (Equivalent-Node? node-val)
		  (let ((pr (car (Equivalent-Node-List node-val))))
		    (values (cdr pr) (car pr)))
		(values (WB-Bag-Tree-Node-Count tree) node-val)))
	     (:unequal
	      (if (Equivalent-Node? node-val)
		  (let ((pr (assoc value (Equivalent-Node-List node-val)
				   :test (equal?-fn cmp-fn))))
		    (if pr (values (cdr pr) (car pr))
		      (values 0 nil)))
		(values 0 nil)))
	     ((:less)
	      (WB-Bag-Tree-Multiplicity (WB-Bag-Tree-Node-Left tree) value cmp-fn))
	     ((:greater)
	      (WB-Bag-Tree-Multiplicity (WB-Bag-Tree-Node-Right tree) value cmp-fn)))))))

(defun WB-Bag-Tree-Find-Equivalent (tree value cmp-fn)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true, (second value) either the one value or an `Equivalent-Bag'
containing the values, and (third value) if the second value was a single
value, the corresponding count; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value cmp-fn)))
	   (and found?
		(values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val (WB-Bag-Tree-Node-Count tree)))
	     (:less
	       (WB-Bag-Tree-Find-Equivalent (WB-Bag-Tree-Node-Left tree) value cmp-fn))
	     (:greater
	       (WB-Bag-Tree-Find-Equivalent (WB-Bag-Tree-Node-Right tree) value cmp-fn)))))))

;;; ================================================================================
;;; With

(defun WB-Bag-Tree-With (tree value cmp-fn &optional (count 1))
  "Returns `tree' with `value' added with a count of `count' (if it was already
present, its count is incremented by `count').  `value' may be an `Equivalent-Bag'."
  ;; The case where `value' is an `Equivalent-Bag' is used by `WB-Bag-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree)
	 (if (not (Equivalent-Node? value))
	     (cons (vector value) (vector count))
	   (Make-WB-Bag-Tree-Node value count nil nil)))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Bag', because
	   ;; this routine is called by `WB-Bag-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Node? value)))
	       (cons (car tree)
		     (Vector-Update (cdr tree) idx (gen + (svref (cdr tree) idx)
							count)))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 WB-Tree-Max-Vector-Length)
		      (not (Equivalent-Node? value)))
		 (cons (Vector-Insert (car tree) idx value)
		       (Vector-Insert (cdr tree) idx count))
	       (let ((new-val new-count
			(if found? (Equivalent-Bag-Sum (svref (car tree) idx)
						       (svref (cdr tree) idx)
						       value count cmp-fn)
			  (values value count))))
		 (Make-WB-Bag-Tree-Node new-val new-count
					(and (> idx 0)
					     (cons (Vector-Subseq (car tree) 0 idx)
						   (Vector-Subseq (cdr tree) 0 idx)))
					(and (< right-start (length (the simple-vector
								      (car tree))))
					     (cons (Vector-Subseq (car tree) right-start)
						   (Vector-Subseq (cdr tree)
								  right-start)))))))))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       (node-count (WB-Bag-Tree-Node-Count tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((new-val new-count
		       (Equivalent-Bag-Sum node-val node-count value count cmp-fn)))
		(Make-WB-Bag-Tree-Node new-val new-count
				       (WB-Bag-Tree-Node-Left tree)
				       (WB-Bag-Tree-Node-Right tree))))
	     ((:less)
	      (WB-Bag-Tree-Build-Node node-val node-count
				      (WB-Bag-Tree-With (WB-Bag-Tree-Node-Left tree)
							value cmp-fn count)
				      (WB-Bag-Tree-Node-Right tree)))
	     ((:greater)
	      (WB-Bag-Tree-Build-Node node-val node-count
				      (WB-Bag-Tree-Node-Left tree)
				      (WB-Bag-Tree-With (WB-Bag-Tree-Node-Right tree)
							value cmp-fn count))))))))


;;; ================================================================================
;;; Less

(defun WB-Bag-Tree-Less (tree value cmp-fn &optional (count 1))
  "Returns `tree' with the count for `value' decremented; if that count was
1, `value' is removed entirely."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type integer count)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value cmp-fn)))
	   (if (eq found? ':equal)
	       (let ((prev-count (the integer (svref (cdr tree) idx))))
		 (if (gen > prev-count count)
		     (cons (car tree) (Vector-Update (cdr tree) idx
						     (gen - prev-count count)))
		   (and (> (length (the simple-vector (car tree))) 1)
			(cons (Vector-Remove-At (car tree) idx)
			      (Vector-Remove-At (cdr tree) idx)))))
	     tree)))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       (node-count (WB-Bag-Tree-Node-Count tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((nonnull? value count
		      (Equivalent-Bag-Difference node-val node-count value count cmp-fn)))
		(if nonnull?
		    (Make-WB-Bag-Tree-Node value count
					   (WB-Bag-Tree-Node-Left tree)
					   (WB-Bag-Tree-Node-Right tree))
		  (WB-Bag-Tree-Join (WB-Bag-Tree-Node-Left tree) (WB-Bag-Tree-Node-Right tree) cmp-fn))))
	     ((:less)
	      (let ((left (WB-Bag-Tree-Node-Left tree))
		    ((new-left (WB-Bag-Tree-Less left value cmp-fn count))))
		(if (eq new-left left) tree
		  (WB-Bag-Tree-Build-Node node-val node-count new-left
					  (WB-Bag-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Bag-Tree-Node-Right tree))
		    ((new-right (WB-Bag-Tree-Less right value cmp-fn count))))
		(if (eq new-right right) tree
		  (WB-Bag-Tree-Build-Node node-val node-count
					  (WB-Bag-Tree-Node-Left tree)
					  new-right)))))))))


;;; ================================================================================
;;; Union, sum, intersection, and bag difference

(defun WB-Bag-Tree-Union (tree1 tree2 cmp-fn)
  "Returns the union of `tree' and `tree2'."
  (if (eq tree1 tree2)
      tree1
    (WB-Bag-Tree-Union-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))

(defun WB-Bag-Tree-Union-Rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (WB-Bag-Tree-Split tree1 lo hi cmp-fn))
	((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi cmp-fn))
	((null tree1)
	 (WB-Bag-Tree-Split tree2 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (WB-Bag-Tree-Vector-Pair-Union tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((val count (if eqvv1? (Equivalent-Bag-Union eqvv1 eqvc1 val2 count2 cmp-fn)
			      (values val2 count2))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
				    (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree2) lo val2 cmp-fn)
				    lo val2 cmp-fn)
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
				    (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree2) val2 hi cmp-fn)
				    val2 hi cmp-fn)
	     cmp-fn)))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((val count (if eqvv2? (Equivalent-Bag-Union val1 count1 eqvv2 eqvc2 cmp-fn)
			      (values val1 count1))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree1) lo val1 cmp-fn)
				    (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
				    lo val1 cmp-fn)
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree1) val1 hi cmp-fn)
				    (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
				    val1 hi cmp-fn)
	     cmp-fn)))))

(defun WB-Bag-Tree-Sum (tree1 tree2 cmp-fn)
  "Returns the sum of `tree' and `tree2'."
  (WB-Bag-Tree-Sum-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Bag-Tree-Sum-Rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi cmp-fn))
	((null tree1)
	 (WB-Bag-Tree-Split tree2 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (WB-Bag-Tree-Vector-Pair-Sum tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((val count (if eqvv1? (Equivalent-Bag-Sum eqvv1 eqvc1 val2 count2 cmp-fn)
			      (values val2 count2))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
				  (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree2) lo val2 cmp-fn)
				  lo val2 cmp-fn)
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
				  (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree2) val2 hi cmp-fn)
				  val2 hi cmp-fn)
	     cmp-fn)))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((val count (if eqvv2? (Equivalent-Bag-Sum val1 count1 eqvv2 eqvc2 cmp-fn)
			      (values val1 count1))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree1) lo val1 cmp-fn)
				  (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
				  lo val1 cmp-fn)
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree1) val1 hi cmp-fn)
				  (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
				  val1 hi cmp-fn)
	     cmp-fn)))))


(defun WB-Bag-Tree-Intersect (tree1 tree2 cmp-fn)
  (if (eq tree1 tree2)
      tree1
    (WB-Bag-Tree-Intersect-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))

(defun WB-Bag-Tree-Intersect-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (WB-Bag-Tree-Split tree1 lo hi cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Intersect tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((new-left
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
					     (WB-Bag-Tree-Node-Left tree2)
					     lo val2 cmp-fn))
		(new-right
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
					     (WB-Bag-Tree-Node-Right tree2)
					     val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (Equivalent-Bag-Intersect eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Node-Left tree1)
					     (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
					     lo val1 cmp-fn))
		(new-right
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Node-Right tree1)
					     (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
					     val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (and eqvv2? (Equivalent-Bag-Intersect val1 count1 eqvv2 eqvc2 cmp-fn))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))))


(defun WB-Bag-Tree-Product (tree1 tree2 cmp-fn)
  (WB-Bag-Tree-Product-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Bag-Tree-Product-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the Production of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Product tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((new-left
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
					   (WB-Bag-Tree-Node-Left tree2)
					   lo val2 cmp-fn))
		(new-right
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
					   (WB-Bag-Tree-Node-Right tree2)
					   val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (Equivalent-Bag-Product eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Node-Left tree1)
					   (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn))
		(new-right
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Node-Right tree1)
					   (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (and eqvv2? (Equivalent-Bag-Product val1 count1 eqvv2 eqvc2 cmp-fn))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))))


(defun WB-Bag-Tree-Diff (tree1 tree2 cmp-fn)
  "Returns the bag difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (and (not (eq tree1 tree2))
       (WB-Bag-Tree-Diff-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))

(defun WB-Bag-Tree-Diff-Rng (tree1 tree2 lo hi cmp-fn)
  "Returns the bag difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) nil)		; historically-related-bag optimization
	((null tree1) nil)
	((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Diff tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((new-left (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
						(WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree2) lo val2 cmp-fn)
						lo val2 cmp-fn))
		(new-right (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
						 (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree2) val2 hi cmp-fn)
						 val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (Equivalent-Bag-Difference eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Node-Left tree1)
						(WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
						lo val1 cmp-fn))
		(new-right (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Node-Right tree1)
						 (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
						 val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (if eqvv2? (Equivalent-Bag-Difference val1 count1 eqvv2 eqvc2 cmp-fn)
		     (values t val1 count1))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right cmp-fn)
	     (WB-Bag-Tree-Join new-left new-right cmp-fn))))))


;;; ================================================================================
;;; Comparison

(defun WB-Bag-Tree-Compare (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((totct1 (WB-Bag-Tree-Total-Count tree1))
	  (totct2 (WB-Bag-Tree-Total-Count tree2))
	  (size1 (WB-Bag-Tree-Size tree1))
	  (size2 (WB-Bag-Tree-Size tree2)))
      (cond ((< totct1 totct2) ':less)
	    ((> totct1 totct2) ':greater)
	    ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t (WB-Bag-Tree-Compare-Rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun WB-Bag-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi cmp-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (type function cmp-fn))
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related-bag optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (consp tree1) (consp tree2))
	 (let ((unequal? nil))
	   (or (gmap (:result or) (fn (val1 count1 val2 count2)
				    (let ((val-comp (funcall cmp-fn val1 val2)))
				      (when (eq val-comp ':unequal)
					(setq unequal? t))
				      (cond ((or (eq val-comp ':less) (eq val-comp ':greater))
					     val-comp)
					    ((< count1 count2) ':less)
					    ((> count1 count2) ':greater))))
		     (:arg simple-vector (car tree1) :start (- lo base1) :stop (- hi base1))
		     (:arg simple-vector (cdr tree1) :start (- lo base1) :stop (- hi base1))
		     (:arg simple-vector (car tree2) :start (- lo base2) :stop (- hi base2))
		     (:arg simple-vector (cdr tree2) :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((consp tree1)
	 (invert-comparison (WB-Bag-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (WB-Bag-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Bag-Tree-Size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (WB-Bag-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Bag-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Bag-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (WB-Bag-Tree-Node-Value tree1))
		   (count1 (WB-Bag-Tree-Node-Count tree1))
		   (val2 count2
		      (WB-Bag-Tree-Rank-Pair-Internal
			tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (Equivalent-Bag-Compare val1 count1 val2 count2 cmp-fn))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (Bag-Value-Size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (WB-Bag-Tree-Rank-Trim (WB-Bag-Tree-Node-Right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (WB-Bag-Tree-Rank-Trim tree2 base2 new-lo hi))
			 ((right-comp (WB-Bag-Tree-Compare-Rng right1a base1a tree2a
							       base2a new-lo hi cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal val-comp))))))))))

(defun WB-Bag-Tree-Rank-Trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type fixnum base lo hi)
	   #+(or cmu scl)
	   (values WB-Bag-Tree fixnum))
  (if (or (null tree) (consp tree))
      (values tree base)
    (let ((node-rank (+ base (WB-Bag-Tree-Size (WB-Bag-Tree-Node-Left tree)))))
      (declare (type fixnum node-rank))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (WB-Bag-Tree-Rank-Trim (WB-Bag-Tree-Node-Left tree) base lo hi))
	(WB-Bag-Tree-Rank-Trim (WB-Bag-Tree-Node-Right tree)
			       (+ node-rank
				  (Bag-Value-Size (WB-Bag-Tree-Node-Value tree)))
			       lo hi)))))

(defun WB-Bag-Tree-Rank (tree value cmp-fn)
  "Searches a bag tree `tree' for `value'.  Returns two values, a boolean and an
index.  If `value', or a value equivalent to `value', is in `tree', the symbol
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `value' would have if it were to be added.  Note that
if the bag contains equivalent-but-unequal elements, the rank of each of several
such elements is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (declare (type function cmp-fn))
  (labels ((rec (tree value base)
	     (cond ((null tree) (values nil base))
		   ((consp tree)
		    (let ((found? idx (Vector-Set-Binary-Search (car tree) value cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Bag-Tree-Node-Value tree))
			  (left (WB-Bag-Tree-Node-Left tree))
			  ((left-size (WB-Bag-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall cmp-fn value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Node? node-val)
			     (let ((mems (Equivalent-Node-List node-val))
				   ((pos (cl:position value mems :test (equal?-fn cmp-fn) :key #'car))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left value base))
			((:greater)
			 (rec (WB-Bag-Tree-Node-Right tree) value
			      (+ node-base (Bag-Value-Size node-val))))))))))
    (rec tree value 0)))

(defun WB-Bag-Tree-Rank-Pair (tree rank)
  (let ((elt count rem (WB-Bag-Tree-Rank-Pair-Internal tree rank)))
    (if (Equivalent-Node? elt)
	(let ((pr (nth rem (Equivalent-Node-List elt))))
	  (values (car pr) (cdr pr)))
      (values elt count))))

(defun WB-Bag-Tree-Rank-Pair-Internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in bag comparator"))
	((consp tree)
	 (values (svref (car tree) rank) (svref (cdr tree) rank) 0))
	(t
	 (let ((left (WB-Bag-Tree-Node-Left tree))
	       ((left-size (WB-Bag-Tree-Size left))))
	   (if (< rank left-size)
	       (WB-Bag-Tree-Rank-Pair-Internal left rank)
	     (let ((val (WB-Bag-Tree-Node-Value tree))
		   ((val-size (Bag-Value-Size val))
		    (rank (- rank left-size))))
	       (declare (type fixnum rank))
	       (if (< rank val-size)
		   (values val (WB-Bag-Tree-Node-Count tree) rank)
		 (WB-Bag-Tree-Rank-Pair-Internal (WB-Bag-Tree-Node-Right tree)
						 (the fixnum (- rank val-size))))))))))


;;; ================================================================================
;;; Subbag testing

(defun WB-Bag-Tree-Subbag? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (let ((size1 (WB-Bag-Tree-Size tree1))
	(size2 (WB-Bag-Tree-Size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (WB-Bag-Tree-Subbag?-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn)))))

(defun WB-Bag-Tree-Subbag?-Rng (tree1 tree2 lo hi cmp-fn)
  (declare ;(optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((null tree1) t)
	((eq tree1 tree2) t)		; historically-related-tree optimization
	((and (consp tree1) (or (null tree2) (consp tree2)))
	 (Vector-Pair-Bag-Subbag? tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2)))
	   (and (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Trim tree1 lo val2 cmp-fn)
					 (WB-Bag-Tree-Node-Left tree2)
					 lo val2 cmp-fn)
		(let ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2 cmp-fn)))
		  (and (or (not eqvv1?)
			   (Equivalent-Bag-Subbag? eqvv1 eqvc1 val2 count2 cmp-fn))
		       (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Trim tree1 val2 hi cmp-fn)
						(WB-Bag-Tree-Node-Right tree2)
						val2 hi cmp-fn))))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1)))
	   (and (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Node-Left tree1)
					 (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
					 lo val1 cmp-fn)
		(let ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn)))
		  (and eqvv2?
		       (Equivalent-Bag-Subbag? val1 count1 eqvv2 eqvc2 cmp-fn)
		       (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Node-Right tree1)
						(WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
						val1 hi cmp-fn))))))))


;;; ================================================================================
;;; Disjointness testing

(defun WB-Bag-Tree-Disjoint? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (WB-Bag-Tree-Disjoint?-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Bag-Tree-Disjoint?-Rng (tree1 tree2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 t)
	((eq tree1 tree2)
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Set-Disjoint? (car tree1) (car tree2) lo hi cmp-fn))
	((consp tree1)
	 (WB-Bag-Tree-Disjoint?-Rng (WB-Bag-Tree-Trim tree2 lo hi cmp-fn)
				    tree1 lo hi cmp-fn))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       ((eqvv2? eqvv2 (WB-Bag-Tree-Find-Equivalent tree2 val1 cmp-fn))))
	   (and (or (null eqvv2?) (Equivalent-Bag-Disjoint? val1 eqvv2 cmp-fn))
		(WB-Bag-Tree-Disjoint?-Rng (WB-Bag-Tree-Node-Left tree1)
					   (WB-Bag-Tree-Trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn)
		(WB-Bag-Tree-Disjoint?-Rng (WB-Bag-Tree-Node-Right tree1)
					   (WB-Bag-Tree-Trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn))))))


;;; ================================================================================
;;; Miscellany

(defun WB-Bag-Tree-From-List (lst pairs? cmp-fn)
  (let ((tree nil))
    (if pairs?
	(dolist (x lst)
	  (when (and pairs? (not (and (integerp (cdr x)) (< 0 (cdr x)))))
	    (error 'simple-type-error :datum (cdr x) :expected-type '(integer 0 *)
				      :format-control "Supplied count is not a positive integer: ~S"
				      :format-arguments (list (cdr x))))
	  (setq tree (WB-Bag-Tree-With tree (car x) cmp-fn (cdr x))))
      (dolist (x lst)
	(setq tree (WB-Bag-Tree-With tree x cmp-fn))))
    tree))

(defun WB-Bag-Tree-From-Iterable (it pairs? cmp-fn)
  (declare (type function it))
  (let ((tree nil))
    (if pairs?
	(while (funcall it ':more?)
	  (let ((x (funcall it ':get)))
	    (when (and pairs? (not (and (integerp (cdr x)) (< 0 (cdr x)))))
	      (error 'simple-type-error :datum (cdr x) :expected-type '(integer 0 *)
					:format-control "Supplied count is not a positive integer: ~S"
					:format-arguments (list (cdr x))))
	    (setq tree (WB-Bag-Tree-With tree (car x) cmp-fn (cdr x)))))
      (while (funcall it ':more?)
	(setq tree (WB-Bag-Tree-With tree (funcall it ':get) cmp-fn))))
    tree))

;;; See `WB-Set-Tree-From-Sorted-Iterable'.
(defun WB-Bag-Tree-From-Sorted-Iterable (it size pairs? cmp-fn)
  (declare (type function it cmp-fn))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil Hedge-Positive-Infinity Hedge-Negative-Infinity))
		   ((= n 1)
		    (let ((e (funcall it ':get)))
		      (if pairs?
			  (let ((e (car e))
				(ne (cdr e)))
			    (check-count ne)
			    (values (cons (vector e) (vector ne)) e e))
			(values (cons (vector e) (vector 1)) e e))))
		   ;; Reduces consing about 12%, improves speed.
		   ((= n 2)
		    (let ((a (funcall it ':get))
			  (b (funcall it ':get))
			  ((a na (if pairs? (values (car a) (cdr a)) (values a 1)))
			   (b nb (if pairs? (values (car b) (cdr b)) (values b 1)))))
		      (when pairs?
			(check-count na)
			(check-count nb))
		      (ecase (funcall cmp-fn a b)
			(:equal (values (cons (vector a) (vector (+ na nb))) a a))
			(:less (values (cons (vector a b) (vector na nb)) a b))
			(:greater (values (cons (vector b a) (vector nb na)) b a))
			(:unequal (values (WB-Bag-Tree-With (cons (vector a) (vector na)) b cmp-fn nb) a a)))))
		   (t
		    (let ((n2 (floor (1- n) 2))
			  ((left left-first left-last (recur n2))
			   ((n2-elt (funcall it ':get))
			    ((right right-first right-last (recur (- n n2 1)))
			     (e ne (if pairs? (values (car n2-elt) (cdr n2-elt))
				     (values n2-elt 1)))))))
		      (when pairs?
			(check-count ne))
		      ;; Here we check whether the tree really is sorted as promised.
		      ;; (We really have to do this for correctness, because even if it is sorted, it
		      ;; could have sequences of equivalent-but-unequal elements.)
		      (if (and (less-than?-cmp left-last e cmp-fn)
			       (less-than?-cmp e right-first cmp-fn))
			  (values (WB-Bag-Tree-Build-Node e ne left right) left-first right-last)
			;; Fall back to general case.
			(values (WB-Bag-Tree-With (WB-Bag-Tree-Sum left right cmp-fn) e cmp-fn ne)
				(if (less-than?-cmp left-first right-first cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last cmp-fn) right-last left-last)))))))
	   (check-count (n)
	     (unless (and (integerp n) (< 0 n))
	       (error 'simple-type-error :datum n :expected-type '(integer 0 *)
					 :format-control "Supplied count is not a positive integer: ~S"
					 :format-arguments (list n)))))
    (recur size)))


(defun WB-Set-Tree-To-Bag-Tree (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (cons tree (make-array (length tree) :initial-element 1)))
	(t
	 (let ((value (WB-Set-Tree-Node-Value tree))
	       (size (WB-Set-Tree-Node-Size tree))
	       (new-left (WB-Set-Tree-To-Bag-Tree (WB-Set-Tree-Node-Left tree)))
	       (new-right (WB-Set-Tree-To-Bag-Tree (WB-Set-Tree-Node-Right tree))))
	   (if (Equivalent-Node? value)
	       (Make-Raw-WB-Bag-Tree-Node
		 size size
		 (Make-Equivalent-Bag (mapcar #'(lambda (x) (cons x 1))
					      (Equivalent-Node-List value)))
		 0 new-left new-right)
	     (Make-Raw-WB-Bag-Tree-Node size size value 1 new-left new-right))))))

(defun WB-Bag-Tree-To-Set-Tree (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (car tree))
	(t
	 (let ((value (WB-Bag-Tree-Node-Value tree))
	       (size (WB-Bag-Tree-Node-Size tree))
	       (new-left (WB-Bag-Tree-To-Set-Tree (WB-Bag-Tree-Node-Left tree)))
	       (new-right (WB-Bag-Tree-To-Set-Tree (WB-Bag-Tree-Node-Right tree))))
	   (if (Equivalent-Node? value)
	       (Make-Raw-WB-Set-Tree-Node
		 size (Make-Equivalent-Set (mapcar #'car (Equivalent-Node-List value)))
		 new-left new-right)
	     (Make-Raw-WB-Set-Tree-Node size value new-left new-right))))))

(defun WB-Bag-Tree-Split-Above (tree value cmp-fn)
  (WB-Bag-Tree-Split tree value Hedge-Positive-Infinity cmp-fn))

(defun WB-Bag-Tree-Split-Below (tree value cmp-fn)
  (WB-Bag-Tree-Split tree Hedge-Negative-Infinity value cmp-fn))


;;; ================================================================================
;;; Support routines for the above (bags)

(defun WB-Bag-Tree-Split (tree lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((consp tree)
	 (let ((vals (the simple-vector (car tree)))
	       (counts (the simple-vector (cdr tree)))
	       ((len (length vals))
		((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				     0
				   (Vector-Set-Binary-Search-Lo vals lo cmp-fn)))
		 (split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi vals hi cmp-fn))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (Vector-Subseq vals split-point-lo split-point-hi)
			(Vector-Subseq counts split-point-lo split-point-hi))))))
	((not (or (eq lo Hedge-Negative-Infinity)
		  (greater-than?-cmp (WB-Bag-Tree-Node-Value tree) lo cmp-fn)))
	 (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Right tree) lo hi cmp-fn))
	((not (or (eq hi Hedge-Positive-Infinity)
		  (less-than?-cmp (WB-Bag-Tree-Node-Value tree) hi cmp-fn)))
	 (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Left tree) lo hi cmp-fn))
	(t
	 (let ((new-left (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity cmp-fn))
	       (new-right (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi cmp-fn)))
	   (if (and (eq new-left (WB-Bag-Tree-Node-Left tree))
		    (eq new-right (WB-Bag-Tree-Node-Right tree)))
	       tree
	     (WB-Bag-Tree-Concat (WB-Bag-Tree-Node-Value tree)
				 (WB-Bag-Tree-Node-Count tree)
				 new-left new-right cmp-fn))))))

(defun WB-Bag-Tree-Trim (tree lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than?-cmp (svref (car tree) (1- (length (the simple-vector (car tree)))))
				     lo cmp-fn))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than?-cmp (svref (car tree) 0) hi cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo Hedge-Negative-Infinity)
					0
				      (Vector-Set-Binary-Search-Lo (car tree) lo cmp-fn)))
		    (split-point-hi (if (eq hi Hedge-Positive-Infinity)
					(length (the simple-vector (car tree)))
				      (Vector-Set-Binary-Search-Hi (car tree) hi cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((val (WB-Bag-Tree-Node-Value tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than?-cmp val lo cmp-fn))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than?-cmp val hi cmp-fn))
		   tree
		 (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree) lo hi cmp-fn))
	     (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree) lo hi cmp-fn))))))

(defun WB-Bag-Tree-Concat (value count left right cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree left right))
  (cond ((null left)
	 (WB-Bag-Tree-With right value cmp-fn count))
	((null right)
	 (WB-Bag-Tree-With left value cmp-fn count))
	((and (WB-Bag-Tree-Node? left)
	      (> (WB-Bag-Tree-Size left)
		 (the fixnum (* (WB-Bag-Tree-Size right) WB-Tree-Balance-Factor))))
	 (WB-Bag-Tree-Build-Node (WB-Bag-Tree-Node-Value left)
				 (WB-Bag-Tree-Node-Count left)
				 (WB-Bag-Tree-Node-Left left)
				 (WB-Bag-Tree-Concat value count
						     (WB-Bag-Tree-Node-Right left)
						     right cmp-fn)))
	((and (WB-Bag-Tree-Node? right)
	      (> (WB-Bag-Tree-Size right)
		 (the fixnum (* (WB-Bag-Tree-Size left) WB-Tree-Balance-Factor))))
	 (WB-Bag-Tree-Build-Node (WB-Bag-Tree-Node-Value right)
				 (WB-Bag-Tree-Node-Count right)
				 (WB-Bag-Tree-Concat value count left (WB-Bag-Tree-Node-Left right) cmp-fn)
				 (WB-Bag-Tree-Node-Right right)))
	(t
	 (WB-Bag-Tree-Build-Node value count left right))))

(defun WB-Bag-Tree-Join (left right cmp-fn)
  (if (null left) right
    (if (null right) left
      (let ((min-val min-count (WB-Bag-Tree-Minimum-Pair right)))
	(WB-Bag-Tree-Concat min-val min-count
			    left (WB-Bag-Tree-Less-Minimum right cmp-fn) cmp-fn)))))

(defun WB-Bag-Tree-Minimum-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the minimum value and count as two
values.  The value may be an `Equivalent-Bag', in which case, as usual, the
count is not meaningful."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (if (consp tree)
      (values (svref (car tree) 0)
	      (svref (cdr tree) 0))
    (let ((left (WB-Bag-Tree-Node-Left tree)))
      (if left
	  (WB-Bag-Tree-Minimum-Pair left)
	(values (WB-Bag-Tree-Node-Value tree)
		(WB-Bag-Tree-Node-Count tree))))))

(defun WB-Bag-Tree-Less-Minimum (tree cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum value
removed."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (if (consp tree)
      (and (> (length (the simple-vector (car tree))) 1)
	   (cons (Vector-Subseq (car tree) 1)
		 (Vector-Subseq (cdr tree) 1)))
    (let ((left (WB-Bag-Tree-Node-Left tree)))
      (if left
	  (WB-Bag-Tree-Concat (WB-Bag-Tree-Node-Value tree) (WB-Bag-Tree-Node-Count tree)
			      (WB-Bag-Tree-Less-Minimum left cmp-fn) (WB-Bag-Tree-Node-Right tree) cmp-fn)
	(WB-Bag-Tree-Node-Right tree)))))

(defun WB-Bag-Tree-Build-Node (value count left right)
  "Constructs a `WB-Bag-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (Equivalent-Node? value))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  WB-Tree-Max-Vector-Length))
	  (cons (concatenate 'simple-vector (car left) (vector value) (car right))
		(concatenate 'simple-vector (cdr left) (vector count) (cdr right)))
	(Make-WB-Bag-Tree-Node value count left right))
    (let ((sizl (WB-Bag-Tree-Size left))
	  (sizr (WB-Bag-Tree-Size right)))
      (cond ((and (WB-Bag-Tree-Node? left)
		  (> sizl (the fixnum (* sizr WB-Tree-Balance-Factor))))
	     (let ((ll (WB-Bag-Tree-Node-Left left))
		   (rl (WB-Bag-Tree-Node-Right left)))
	       (if (or (null rl) (consp rl)
		       (<= (WB-Bag-Tree-Size rl) (WB-Bag-Tree-Size ll)))
		   (Make-WB-Bag-Tree-Node (WB-Bag-Tree-Node-Value left)
					  (WB-Bag-Tree-Node-Count left)
					  ll
					  (WB-Bag-Tree-Build-Node value count rl right))
		 (Make-WB-Bag-Tree-Node (WB-Bag-Tree-Node-Value rl)
					(WB-Bag-Tree-Node-Count rl)
					(WB-Bag-Tree-Build-Node
					  (WB-Bag-Tree-Node-Value left)
					  (WB-Bag-Tree-Node-Count left)
					  ll
					  (WB-Bag-Tree-Node-Left rl))
					(WB-Bag-Tree-Build-Node
					  value count (WB-Bag-Tree-Node-Right rl)
					  right)))))
	    ((and (WB-Bag-Tree-Node? right)
		  (> sizr (the fixnum (* sizl WB-Tree-Balance-Factor))))
	     (let ((lr (WB-Bag-Tree-Node-Left right))
		   (rr (WB-Bag-Tree-Node-Right right)))
	       (if (or (null lr) (consp lr)
		       (<= (WB-Bag-Tree-Size lr) (WB-Bag-Tree-Size rr)))
		   (Make-WB-Bag-Tree-Node (WB-Bag-Tree-Node-Value right)
					  (WB-Bag-Tree-Node-Count right)
					  (WB-Bag-Tree-Build-Node value count left lr)
					  rr)
		 (Make-WB-Bag-Tree-Node (WB-Bag-Tree-Node-Value lr)
					(WB-Bag-Tree-Node-Count lr)
					(WB-Bag-Tree-Build-Node
					  value count left (WB-Bag-Tree-Node-Left lr))
					(WB-Bag-Tree-Build-Node
					  (WB-Bag-Tree-Node-Value right)
					  (WB-Bag-Tree-Node-Count right)
					  (WB-Bag-Tree-Node-Right lr)
					  rr)))))
	    (t
	     (Make-WB-Bag-Tree-Node value count left right))))))


(defun WB-Bag-Tree-Verify (tree cmp-fn)
  (WB-Bag-Tree-Verify-Rng tree Hedge-Negative-Infinity Hedge-Positive-Infinity cmp-fn))

(defun WB-Bag-Tree-Verify-Rng (tree lo hi cmp-fn)
  (declare (type function cmp-fn))
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len WB-Tree-Max-Vector-Length)
		(do ((i 0 (1+ i))
		     (prev lo))
		    ((= i len)
		     (or (eq hi Hedge-Positive-Infinity)
			 (less-than?-cmp prev hi cmp-fn)))
		  (let ((elt (svref (car tree) i)))
		    (unless (and (not (Equivalent-Node? elt))
				 (or (eq prev Hedge-Negative-Infinity)
				     (less-than?-cmp prev elt cmp-fn)))
		      (return nil))
		    (setq prev elt))))))
	(t
	 (let ((left (WB-Bag-Tree-Node-Left tree))
	       (right (WB-Bag-Tree-Node-Right tree))
	       ((sizl (WB-Bag-Tree-Size left))
		(sizr (WB-Bag-Tree-Size right)))
	       (value (WB-Bag-Tree-Node-Value tree)))
	   (and (WB-Bag-Tree-Node-Count tree)
		(= (WB-Bag-Tree-Node-Size tree) (+ sizl sizr (Bag-Value-Size value)))
		(= (WB-Bag-Tree-Node-Total-Count tree)
		   (+ (WB-Bag-Tree-Total-Count left)
		      (WB-Bag-Tree-Total-Count right)
		      (if (Equivalent-Node? value)
			  (gmap (:result sum) #'cdr (:arg list (Equivalent-Node-List value)))
			(WB-Bag-Tree-Node-Count tree))))
		(or (not (Equivalent-Node? value))
		    (> (length (Equivalent-Node-List value)) 1))
		(or (<= sizr 4)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 4)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Bag-Tree-Verify-Rng (WB-Bag-Tree-Node-Left tree) lo value cmp-fn)
		(WB-Bag-Tree-Verify-Rng (WB-Bag-Tree-Node-Right tree) value hi cmp-fn))))))


;;; ================================================================================
;;; Vector pair bag operations

(defun WB-Bag-Tree-Vector-Pair-Union (pr1 pr2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (let ((new-pr any-equivalent? (Vector-Pair-Bag-Union pr1 pr2 lo hi cmp-fn)))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i (length (car new-pr)))
	    (setq result (WB-Bag-Tree-With result (svref (car new-pr) i)
					   cmp-fn (svref (cdr new-pr) i))))
	  result)
      (if (> (length (car new-pr)) WB-Tree-Max-Vector-Length)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (Make-WB-Bag-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Bag-Union (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (Reverse-List-To-Vector vals)
		       (Reverse-List-To-Vector counts))
		 any-equivalent?))
      (cond ((= i1 len1)
	     (do () ((= i2 len2))
	       (push (svref vals2 i2) vals)
	       (push (svref counts2 i2) counts)
	       (incf i2)))
	    ((= i2 len2)
	     (do () ((= i1 len1))
	       (push (svref vals1 i1) vals)
	       (push (svref counts1 i1) counts)
	       (incf i1)))
	    (t
	     (let ((val1 (svref vals1 i1))
		   (val2 (svref vals2 i2))
		   ((comp (funcall cmp-fn val1 val2))))
	       (ecase comp
		 (:equal
		  (push val1 vals) ; prefer `val1'
		  (push (gen max (svref counts1 i1) (svref counts2 i2))
			counts)
		  (incf i1)
		  (incf i2))
		 (:less
		  (push val1 vals)
		  (push (svref counts1 i1) counts)
		  (incf i1))
		 (:greater
		  (push val2 vals)
		  (push (svref counts2 i2) counts)
		  (incf i2))
		 (:unequal
		  (push (Equivalent-Bag-Union val1 (svref counts1 i1)
					      val2 (svref counts2 i2) cmp-fn)
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun WB-Bag-Tree-Vector-Pair-Sum (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((new-pr any-equivalent? (Vector-Pair-Bag-Sum pr1 pr2 lo hi cmp-fn))
	((len (length (the simple-vector (car new-pr))))))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i len)
	    (setq result (WB-Bag-Tree-With result (svref (car new-pr) i)
					   cmp-fn (svref (cdr new-pr) i))))
	  result)
      (if (> len WB-Tree-Max-Vector-Length)
	  (let ((split-point (floor len 2)))
	    (Make-WB-Bag-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Bag-Sum (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (Reverse-List-To-Vector vals)
		       (Reverse-List-To-Vector counts))
		 any-equivalent?))
      (cond ((= i1 len1)
	     (do () ((= i2 len2))
	       (push (svref vals2 i2) vals)
	       (push (svref counts2 i2) counts)
	       (incf i2)))
	    ((= i2 len2)
	     (do () ((= i1 len1))
	       (push (svref vals1 i1) vals)
	       (push (svref counts1 i1) counts)
	       (incf i1)))
	    (t
	     (let ((val1 (svref vals1 i1))
		   (val2 (svref vals2 i2))
		   ((comp (funcall cmp-fn val1 val2))))
	       (ecase comp
		 (:equal
		  (push val1 vals)
		  (push (gen + (svref counts1 i1) (svref counts2 i2))
			counts)
		  (incf i1)
		  (incf i2))
		 (:less
		  (push val1 vals)
		  (push (svref counts1 i1) counts)
		  (incf i1))
		 (:greater
		  (push val2 vals)
		  (push (svref counts2 i2) counts)
		  (incf i2))
		 (:unequal
		  (push (Equivalent-Bag-Union val1 (svref counts1 i1)
					      val2 (svref counts2 i2) cmp-fn)
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun Vector-Pair-Bag-Intersect (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (Reverse-List-To-Vector vals)
			 (Reverse-List-To-Vector counts))))
      (let ((val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (funcall cmp-fn val1 val2))))
	(ecase comp
	  (:equal
	   (push val1 vals)
	   (push (gen min (svref counts1 i1) (svref counts2 i2))
		 counts)
	   (incf i1)
	   (incf i2))
	  (:less
	   (incf i1))
	  (:greater
	   (incf i2))
	  (:unequal
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Bag-Product (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (Reverse-List-To-Vector vals)
			 (Reverse-List-To-Vector counts))))
      (let ((val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (funcall cmp-fn val1 val2))))
	(ecase comp
	  (:equal
	   (push val1 vals)
	   (push (gen * (svref counts1 i1) (svref counts2 i2))
		 counts)
	   (incf i1)
	   (incf i2))
	  (:less
	   (incf i1))
	  (:greater
	   (incf i2))
	  (:unequal
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Bag-Diff (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vals1 i1) vals)
	   (push (svref counts1 i1) counts)
	   (incf i1))
	 (and vals (cons (Reverse-List-To-Vector vals)
			 (Reverse-List-To-Vector counts))))
      (let ((v1 (svref vals1 i1))
	    (v2 (svref vals2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (let ((c1 (the integer (svref counts1 i1)))
		 ((c (gen - c1 (svref counts2 i2)))))
	     (when (gen > c 0)
	       (push v1 vals)
	       (push c counts)))
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (push v1 vals)
	   (push (svref counts1 i1) counts)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (push v1 vals)
	   (push (svref counts1 i1) counts)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Bag-Subbag? (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((vals1 (the simple-vector (car pr1)))
	(vals2 (the simple-vector (car pr2)))
	(counts1 (cdr pr1))
	(counts2 (cdr pr2))
	(i1 0)
	(i2 0)
	((len1 (length vals1))
	 (len2 (length vals2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 (= i1 len1))
      (let ((v1 (svref vals1 i1))
	    (v2 (svref vals2 i2))
	    ((comp (funcall cmp-fn v1 v2))))
	(ecase comp
	  ((:equal)
	   (when (gen > (svref counts1 i1) (svref counts2 i2))
	     (return nil))
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (return nil))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (return nil)))))))


;;; ================================================================================
;;; Iteration primitives

(defmacro Do-WB-Bag-Tree-Pairs ((val-var count-var tree-form &optional value-form)
				&body body)
  "Iterates over the pairs of the bag, for each one binding `val-var' to the
value and `count-var' to its member count."
  (let ((body-fn (gensymx #:body-))
	(recur-fn (gensymx #:recur-)))
    `(block nil
       (labels ((,body-fn (,val-var ,count-var)
		   (declare (type integer ,count-var))
		   . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(dotimes (i (length (the simple-vector (car tree))))
			  (,body-fn (svref (car tree) i) (svref (cdr tree) i)))
		      (progn
			(,recur-fn (WB-Bag-Tree-Node-Left tree))
			(let ((value (WB-Bag-Tree-Node-Value tree)))
			  (if (Equivalent-Node? value)
			      (dolist (pr (Equivalent-Node-List value))
				(,body-fn (car pr) (cdr pr)))
			    (,body-fn value (WB-Bag-Tree-Node-Count tree))))
			(,recur-fn (WB-Bag-Tree-Node-Right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))


;;; ----------------
;;; Stateful iterator

(defun Make-WB-Bag-Tree-Iterator (tree)
  (let ((iter (Make-WB-Bag-Tree-Iterator-Internal tree)))
    (lambda (op)
      (ecase op
	(:get (WB-Bag-Tree-Iterator-Get iter))
	(:done? (WB-Bag-Tree-Iterator-Done? iter))
	(:more? (not (WB-Bag-Tree-Iterator-Done? iter)))))))

(defun Make-WB-Bag-Tree-Iterator-Internal (tree)
  (WB-Bag-Tree-Iterator-Canonicalize
    (Make-WB-Tree-Iterator tree (WB-Bag-Tree-Size tree) 3 t)))

(defun WB-Bag-Tree-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((sp (svref iter 0))
	  ((node (svref iter sp))
	   (idx1 (svref iter (+ sp 1)))
	   (idx2 (svref iter (+ sp 2)))))
      (declare (fixnum sp idx1 idx2))
      (cond ((null node)
	     (if (= sp 1)
		 (return)
	       (progn
		 (decf sp 3)
		 (setf (svref iter 0) sp)
		 (incf (the fixnum (svref iter (+ sp 1)))))))
	    ((consp node)
	     (cond ((< idx1 (length (the simple-vector (cdr node))))
		    (if (< idx2 (the fixnum (svref (cdr node) idx1)))
			(return)
		      (progn
			(incf (the fixnum (svref iter (+ sp 1))))
			(setf (svref iter (+ sp 2)) 0))))
		   ((= sp 1)
		    (setf (svref iter 1) nil)
		    (return))
		   (t
		    (decf sp 3)
		    (setf (svref iter 0) sp)
		    (incf (the fixnum (svref iter (+ sp 1)))))))
	    ((= idx1 0)
	     (unless (< (+ sp 5) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 3)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (WB-Bag-Tree-Node-Left node))
	     (setf (svref iter (+ sp 1)) 0)
	     (setf (svref iter (+ sp 2)) 0))
	    (t
	     (let ((val (WB-Bag-Tree-Node-Value node)))
	       (if (Equivalent-Node? val)
		   (let ((alist (Equivalent-Node-List val)))
		     (if (< (1- idx1) (length alist))
			 (if (< idx2 (the fixnum (cdr (nth (1- idx1) alist))))
			     (return)
			   (progn
			     (incf (the fixnum (svref iter (+ sp 1))))
			     (setf (svref iter (+ sp 2)) 0)))
		       (progn
			 ;; Tail recursion
			 (setf (svref iter sp) (WB-Bag-Tree-Node-Right node))
			 (setf (svref iter (+ sp 1)) 0)
			 (setf (svref iter (+ sp 2)) 0))))
		 (if (= idx1 1)
		     (if (< idx2 (the fixnum (WB-Bag-Tree-Node-Count node)))
			 (return)
		       (incf (the fixnum (svref iter (+ sp 1)))))
		   (progn
		     ;; Tail recursion
		     (setf (svref iter sp) (WB-Bag-Tree-Node-Right node))
		     (setf (svref iter (+ sp 1)) 0)
		     (setf (svref iter (+ sp 2)) 0)))))))))
  iter)

(defun WB-Bag-Tree-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun WB-Bag-Tree-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx1 (svref iter (+ sp 1)))))
    (declare (fixnum sp idx1))
    (cond ((null node)
	   (values nil nil))
	  ((consp node)
	   (progn
	     (incf (the fixnum (svref iter (+ sp 2))))
	     (WB-Bag-Tree-Iterator-Canonicalize iter)
	     (values (svref (car node) idx1) t)))
	  (t
	   (let ((val (WB-Bag-Tree-Node-Value node)))
	     (if (Equivalent-Node? val)
		 (let ((alist (Equivalent-Node-List val)))
		   (incf (the fixnum (svref iter (+ sp 2))))
		   (WB-Bag-Tree-Iterator-Canonicalize iter)
		   (values (car (nth (1- idx1) alist)) t))
	       (progn
		 (incf (the fixnum (svref iter (+ sp 2))))
		 (WB-Bag-Tree-Iterator-Canonicalize iter)
		 (values val t))))))))


;;; Map-style bag iterator

(defun Make-WB-Bag-Tree-Pair-Iterator (tree)
  (let ((iter (Make-WB-Bag-Tree-Pair-Iterator-Internal tree)))
    (lambda (op)
      (ecase op
	(:get (WB-Bag-Tree-Pair-Iterator-Get iter))
	(:done? (WB-Bag-Tree-Pair-Iterator-Done? iter))
	(:more? (not (WB-Bag-Tree-Pair-Iterator-Done? iter)))))))

(defun Make-WB-Bag-Tree-Pair-Iterator-Internal (tree)
  (WB-Bag-Tree-Pair-Iterator-Canonicalize
    (Make-WB-Tree-Iterator tree (WB-Bag-Tree-Size tree) 2 t)))

(defun WB-Bag-Tree-Pair-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((sp (svref iter 0))
	  ((node (svref iter sp))
	   (idx (svref iter (1+ sp)))))
      (declare (fixnum sp idx))
      (cond ((null node)
	     (if (= sp 1)
		 (return)
	       (progn
		 (decf sp 2)
		 (setf (svref iter 0) sp)
		 (incf (the fixnum (svref iter (1+ sp)))))))
	    ((consp node)
	     (cond ((< idx (length (the simple-vector (car node))))
		    (return))
		   ((= sp 1)
		    (setf (svref iter 1) nil)
		    (return))
		   (t
		    (decf sp 2)
		    (setf (svref iter 0) sp)
		    (incf (the fixnum (svref iter (1+ sp)))))))
	    ((= idx 0)
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (WB-Bag-Tree-Node-Left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx (1+ (Bag-Value-Size (WB-Bag-Tree-Node-Value node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (WB-Bag-Tree-Node-Right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun WB-Bag-Tree-Pair-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun WB-Bag-Tree-Pair-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx (svref iter (1+ sp)))))
    (declare (fixnum idx))
    (if (null node)
	(values nil nil nil)
      (progn
	(setf (svref iter (1+ sp)) (1+ idx))
	(if (consp node)
	    (progn
	      (when (= (1+ idx) (length (the simple-vector (car node))))
		(WB-Bag-Tree-Pair-Iterator-Canonicalize iter))
	      (values (svref (car node) idx) (svref (cdr node) idx) t))
	  (let ((val (WB-Bag-Tree-Node-Value node)))
	    (when (= idx (Bag-Value-Size val))
	      (WB-Bag-Tree-Pair-Iterator-Canonicalize iter))
	    (if (Equivalent-Node? val)
		(let ((pr (nth (1- idx) (Equivalent-Node-List val))))
		  (values (car pr) (cdr pr) t))
	      (values val (WB-Bag-Tree-Node-Count node) t))))))))

;;; ----------------
;;; Functional iterators.  Fun!!!

(defun WB-Bag-Tree-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree (lambda (op)
			(ecase op
			  (:first (values nil nil))
			  (:empty? t)
			  (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (let ((len (length (the simple-vector (car node)))))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (copies i 0)
		     cont))
		 (copies (i j)
		   (declare (fixnum i j))
		   (if (< j (the fixnum (svref (cdr node) i)))
		       (lambda (op)
			 (ecase op
			   (:first (values (svref (car node) i) t))
			   (:rest (copies i (1+ j)))
			   (:empty? nil)
			   (:more? t)))
		     (iter (1+ i)))))))
	    (t
	     (walk (WB-Bag-Tree-Node-Left node)
		   (let ((value (WB-Bag-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (Equivalent-Node-List value))
			   (iter (prs)
			     (if prs
				 (copies prs 0)
			       (walk (WB-Bag-Tree-Node-Right node) cont)))
			   (copies (prs j)
			     (declare (fixnum j))
			     (if (< j (the fixnum (cdar prs)))
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) t))
				     (:rest (copies prs (1+ j)))
				     (:empty? nil)
				     (:more? t)))
			       (iter (cdr prs)))))
		       (rlabels (copies 0)
			 (copies (j)
			   (declare (fixnum j))
			   (if (gen < j (WB-Bag-Tree-Node-Count node))
			       (lambda (op)
				 (ecase op
				   (:first (values value t))
				   (:rest (copies (1+ j)))
				   (:empty? nil)
				   (:more? t)))
			     (walk (WB-Bag-Tree-Node-Right node) cont))))))))))))

(defun WB-Bag-Tree-Rev-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree (lambda (op)
			(ecase op
			  (:first (values nil nil))
			  (:empty? t)
			  (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (rlabels (iter (1- (length (the simple-vector (car node)))))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (copies i 0)
		   cont))
	       (copies (i j)
		 (declare (fixnum i j))
		 (if (gen < j (svref (cdr node) i))
		     (lambda (op)
		       (ecase op
			 (:first (values (svref (car node) i) t))
			 (:rest (copies i (1+ j)))
			 (:empty? nil)
			 (:more? t)))
		   (iter (1- i))))))
	    (t
	     (walk (WB-Bag-Tree-Node-Right node)
		   (let ((value (WB-Bag-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (reverse (Equivalent-Node-List value)))
			   (iter (prs)
			     (if prs
				 (copies prs 0)
			       (walk (WB-Bag-Tree-Node-Left node) cont)))
			   (copies (prs j)
			     (declare (fixnum j))
			     (if (< j (the fixnum (cdar prs)))
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) t))
				     (:rest (copies prs (1+ j)))
				     (:empty? nil)
				     (:more? t)))
			       (iter (cdr prs)))))
		       (rlabels (copies 0)
			 (copies (j)
			   (declare (fixnum j))
			   (if (gen < j (WB-Bag-Tree-Node-Count node))
			       (lambda (op)
				 (ecase op
				   (:first (values value t))
				   (:rest (copies (1+ j)))
				   (:empty? nil)
				   (:more? t)))
			     (walk (WB-Bag-Tree-Node-Left node) cont))))))))))))

(defun WB-Bag-Tree-Pair-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree (lambda (op)
			(ecase op
			  (:first (values nil nil))
			  (:empty? t)
			  (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (let ((len (length (the simple-vector (car node)))))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (svref (car node) i) (svref (cdr node) i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     cont)))))
	    (t
	     (walk (WB-Bag-Tree-Node-Left node)
		   (let ((value (WB-Bag-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (Equivalent-Node-List value))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Bag-Tree-Node-Right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value (WB-Bag-Tree-Node-Count node) t))
			   (:rest (walk (WB-Bag-Tree-Node-Right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun WB-Bag-Tree-Pair-Rev-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree (lambda (op)
			(ecase op
			  (:first (values nil nil))
			  (:empty? t)
			  (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (rlabels (iter (1- (length (the simple-vector (car node)))))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (lambda (op)
		       (ecase op
			 (:first (values (svref (car node) i) (svref (cdr node) i) t))
			 (:rest (iter (1- i)))
			 (:empty? nil)
			 (:more? t)))
		   cont))))
	    (t
	     (walk (WB-Bag-Tree-Node-Right node)
		   (let ((value (WB-Bag-Tree-Node-Value node)))
		     (if (Equivalent-Node? value)
			 (rlabels (iter (reverse (Equivalent-Node-List value)))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Bag-Tree-Node-Left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value (WB-Bag-Tree-Node-Count node) t))
			   (:rest (walk (WB-Bag-Tree-Node-Left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ================================================================================
;;; Equivalent-Bag routines

(defun Equivalent-Bag-Sum (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((alist1 (Equivalent-Node-List val1)))
	(if (Equivalent-Node? val2)
	    (let ((alist2 (copy-list (Equivalent-Node-List val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (if pr2
		      (progn (push (cons (car pr1) (gen + (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (Make-Equivalent-Bag result))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (if pr1
		(Make-Equivalent-Bag (cons (cons (car pr1) (gen + (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (Make-Equivalent-Bag (cons (cons val2 count2) alist1))))))
    (if (Equivalent-Node? val2)
	(let ((alist2 (Equivalent-Node-List val2))
	      ((pr2 (assoc val1 alist2 :test (equal?-fn cmp-fn)))))
	  (if pr2
	      (Make-Equivalent-Bag (cons (cons val1 (gen + count1 (cdr pr2)))
					 (cl:remove pr2 alist2)))
	    (Make-Equivalent-Bag (cons (cons val1 count1) alist2))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (values val1 (gen + count1 count2))
	(Make-Equivalent-Bag (list (cons val1 count1) (cons val2 count2)))))))

(defun Equivalent-Bag-Union (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((alist1 (Equivalent-Node-List val1)))
	(if (Equivalent-Node? val2)
	    (let ((alist2 (copy-list (Equivalent-Node-List val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (if pr2
		      (progn (push (cons (car pr1) (gen max (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (Make-Equivalent-Bag result))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (if pr1
		(Make-Equivalent-Bag (cons (cons (car pr1) (gen max (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (Make-Equivalent-Bag (cons (cons val2 count2) alist1))))))
    (if (Equivalent-Node? val2)
	(let ((alist2 (Equivalent-Node-List val2))
	      ((pr2 (assoc val1 alist2 :test (equal?-fn cmp-fn)))))
	  (if pr2
	      (Make-Equivalent-Bag (cons (cons val1 (gen max count1 (cdr pr2)))
					 (cl:remove pr2 alist2)))
	    (Make-Equivalent-Bag (cons (cons val1 count1) alist2))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (values val1 (gen max count1 count2))
	(Make-Equivalent-Bag (list (cons val1 count1) (cons val2 count2)))))))

(defun Equivalent-Bag-Intersect (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((alist1 (Equivalent-Node-List val1)))
	(if (Equivalent-Node? val2)
	    (let ((alist2 (Equivalent-Node-List val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (when pr2
		    (push (cons (car pr1) (gen min (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (Make-Equivalent-Bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (and pr1
		 (values t (car pr1) (gen min (cdr pr1) count2))))))
    (if (Equivalent-Node? val2)
	(let ((pr2 (assoc val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (values t val1 (gen min count1 (cdr pr2)))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (values t val1 (gen min count1 count2))))))

(defun Equivalent-Bag-Product (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((alist1 (Equivalent-Node-List val1)))
	(if (Equivalent-Node? val2)
	    (let ((alist2 (Equivalent-Node-List val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (when pr2
		    (push (cons (car pr1) (gen * (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (Make-Equivalent-Bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (and pr1
		 (values t (car pr1) (gen * (cdr pr1) count2))))))
    (if (Equivalent-Node? val2)
	(let ((pr2 (assoc val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (values t val1 (gen * count1 (cdr pr2)))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (values t val1 (gen * count1 count2))))))

(defun Equivalent-Bag-Difference (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (let ((alist1 (Equivalent-Node-List val1))
	    (alist2 (if (Equivalent-Node? val2) (Equivalent-Node-List val2)
		      (list (cons val2 count2))))
	    (result nil))
	(dolist (pr1 alist1)
	  (let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
	    (cond ((null pr2)
		   (push pr1 result))
		  ((gen > (cdr pr1) (cdr pr2))
		   (push (cons (car pr1)
			       (gen - (cdr pr1) (cdr pr2)))
			 result)))))
	(cond ((null result) nil)
	      ((null (cdr result)) (values t (caar result) (cdar result)))
	      (t (values t (Make-Equivalent-Bag result)))))
    (if (Equivalent-Node? val2)
	(let ((pr2 (assoc val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))))
	  (cond ((null pr2)
		 (values t val1 count1))
		((gen > count1 (cdr pr2))
		 (values t val1 (gen - count1 (cdr pr2))))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (and (gen > count1 count2) (values t val1 (gen - count1 count2)))
	(values t val1 count1)))))

(defun Equivalent-Bag-Subbag? (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (and (Equivalent-Node? val2)
	   (let ((alist2 (Equivalent-Node-List val2)))
	     (dolist (pr1 (Equivalent-Node-List val1) t)
	       (let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		 (unless (and pr2 (gen <= (cdr pr1) (cdr pr2)))
		   (return nil))))))
    (if (Equivalent-Node? val2)
	(let ((pr2 (assoc val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (gen <= count1 (cdr pr2))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (gen <= count1 count2)))))

(defun Equivalent-Bag-Disjoint? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values or `Equivalent-Node's of values.
If their intersection is null, returns true, else false."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (Equivalent-Node? val1)
      (if (Equivalent-Node? val2)
	  (dolist (m1 (Equivalent-Node-List val1) t)
	    (when (assoc m1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn))
	      (return nil)))
	(not (assoc val2 (Equivalent-Node-List val1) :test (equal?-fn cmp-fn))))
    (if (Equivalent-Node? val2)
	(not (assoc val1 (Equivalent-Node-List val2) :test (equal?-fn cmp-fn)))
      (not (equal?-cmp val1 val2 cmp-fn)))))

(defun Equivalent-Bag-Compare (val1 count1 val2 count2 cmp-fn)
  "Compares two pairs where the key of either or both may be an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (let ((comp (funcall cmp-fn val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Node? val1)
	  (if (Equivalent-Node? val2)
	      (let ((mems1 (Equivalent-Node-List val1))
		    (mems2 (Equivalent-Node-List val2))
		    ((len1 (length mems2))
		     (len2 (length mems2))))
		(declare (type integer len1 len2))
		;; The reason these are "backward": in order for us to have gotten here, everything to
		;; our left must have been equal, and we know that the total sizes of the two bags are
		;; equal, so if, say, bag 1 has a smaller total here, then it must have some element
		;; greater than these following this node.
		(cond ((gen < len1 len2) ':greater)
		      ((gen > len1 len2) ':less)
		      ((cl:every #'(lambda (pr1)
				     (let ((pr2 (assoc (car pr1) mems2 :test (equal?-fn cmp-fn))))
				       (and pr2 (equal?-cmp (cdr pr1) (cdr pr2) cmp-fn))))
				 mems1)
		       ':equal)
		      (t
		       ;; Odd-looking, but it's order-independent and implements a strict weak ordering
		       ;; (because `WB-Set-Tree-Compare' does).  We don't pass down `cmp-fn' because we're
		       ;; setifying the counts, not the elements.
		       (let ((set1 (cl:reduce (fn (s x) (WB-Set-Tree-With s x #'compare)) (mapcar #'cdr mems1)
					      :initial-value nil))
			     (set2 (cl:reduce (fn (s x) (WB-Set-Tree-With s x #'compare)) (mapcar #'cdr mems2)
					      :initial-value nil))
			     ((comp (WB-Set-Tree-Compare set1 set2 #'compare))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(cond ((Equivalent-Node? val2)
	       ':greater)
	      ((gen < count1 count2) ':less)
	      ((gen > count1 count2) ':greater)
	      (t comp))))))


;;; ================================================================================
;;; ================================================================================
;;; Maps

(declaim (inline Make-Raw-WB-Map-Tree-Node))

;;; A map tree is either null, a node, or a cons of two simple-vectors.
(deftype WB-Map-Tree ()
  '(or null WB-Map-Tree-Node cons))

(defstruct (WB-Map-Tree-Node
	    (:constructor Make-Raw-WB-Map-Tree-Node (Size Key Value
						     Left Right))
	    (:predicate WB-Map-Tree-Node?)
	    (:print-function WB-Map-Tree-Node-Print))
  (Left  nil :type WB-Map-Tree :read-only t)
  (Right nil :type WB-Map-Tree :read-only t)
  ;; If we get equivalent keys, then the `Key' is an `Equivalent-Node', and the
  ;; `Value' is unused.
  (Key nil :read-only t)		; the domain value
  (Value nil :read-only t)		; the range value
  (Size 0 :type fixnum :read-only t))	; the number of < key, value > pairs


(defun WB-Map-Tree-Node-Print (node stream depth)
  "Print function for `WB-Map-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#map-node<~;~D, ~S -> ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^, ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^, ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (WB-Map-Tree-Node-Size node)
		    (WB-Map-Tree-Node-Key node)
		    (WB-Map-Tree-Node-Value node)
		    (let ((sub (WB-Map-Tree-Node-Left node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))
		    (let ((sub (WB-Map-Tree-Node-Right node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))))
    (format stream "#map-node<...>")))

(declaim (inline Make-Equivalent-Map))

(defun Make-Equivalent-Map (alist)
  (Make-Equivalent-Node nil alist))

(declaim (ftype (function (t) fixnum) Map-Key-Size))
(declaim (inline Map-Key-Size))

(defun Map-Key-Size (key)
  "The number of domain values represented by `key', which can be more than 1 if
`key' is an `Equivalent-Map'."
  (if (Equivalent-Node? key)
      (length (Equivalent-Node-List key))
    1))

(declaim (ftype (function (WB-Map-Tree) fixnum) WB-Map-Tree-Size))
(declaim (inline WB-Map-Tree-Size))

(defun WB-Map-Tree-Size (tree)
  "The number of key/value pairs contained in this tree."
  (cond ((null tree) 0)
	((consp tree) (length (the simple-vector (car tree))))
	(t (WB-Map-Tree-Node-Size tree))))

(declaim (inline Make-WB-Map-Tree-Node))

(defun Make-WB-Map-Tree-Node (key value left right)
  "The low-level constructor for a map tree node."
  (declare (optimize (speed 3) (safety 0)))
  (Make-Raw-WB-Map-Tree-Node (the fixnum
			       (+ (WB-Map-Tree-Size left) (WB-Map-Tree-Size right)
				  (Map-Key-Size key)))
			     key value left right))


(defun WB-Map-Tree-Arb-Pair (tree)
  "Selects an arbitrary pair of the map.  Assumes it is nonnull."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree)
	 (error "`WB-Map-Tree-Arb-Pair' called on empty tree"))
	((consp tree)
	 (values (svref (car tree) 0) (svref (cdr tree) 0)))
	(t
	 (let ((key (WB-Map-Tree-Node-Key tree)))
	   (if (Equivalent-Node? key)
	       (let ((pr (car (Equivalent-Node-List key))))
		 (values (car pr) (cdr pr)))
	     (values key (WB-Map-Tree-Node-Value tree)))))))

(defun WB-Map-Tree-Least-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the least key and its value, or an
arbitrary least key and its value if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (let ((key val (WB-Map-Tree-Minimum-Pair tree)))
    (if (Equivalent-Node? key)
	(let ((pr (car (Equivalent-Node-List key))))
	  (values (car pr) (cdr pr)))
      (values key val))))

#|| Don't think I'm going to use this.
;;; See note at `WB-Set-Tree-Less-Least'.
(defun WB-Map-Tree-Less-Least-Pair (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (and (> (length (the simple-vector (car tree))) 1)
	      (cons (Vector-Subseq (car tree) 1)
		    (Vector-Subseq (cdr tree) 1))))
	(t
	 (let ((left (WB-Map-Tree-Node-Left tree)))
	   (if left
	       (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key tree)
				       (WB-Map-Tree-Node-Value tree)
				       (WB-Map-Tree-Less-Least-Pair left)
				       (WB-Map-Tree-Node-Right tree))
	     (let ((key (WB-Map-Tree-Node-Key tree)))
	       (if (Equivalent-Node? key)
		   (let ((alist (Equivalent-Node-List key)))
		     (if (= (length alist) 2)
			 (Make-WB-Map-Tree-Node (caadr alist) (cdadr alist) nil
						(WB-Map-Tree-Node-Right tree))
		       (Make-WB-Map-Tree-Node (Make-Equivalent-Map (cdr alist)) 0 nil
					      (WB-Map-Tree-Node-Right tree))))
		 (WB-Map-Tree-Node-Right tree))))))))
||#

(defun WB-Map-Tree-Greatest-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the greatest key and its value, or an
arbitrary greatest key and its value if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (if (consp tree)
      (let ((idx (1- (length (the simple-vector (car tree))))))
	(values (svref (car tree) idx)
		(svref (cdr tree) idx)))
    (let ((right (WB-Map-Tree-Node-Right tree)))
      (if right
	  (WB-Map-Tree-Greatest-Pair right)
	(let ((key (WB-Map-Tree-Node-Key tree)))
	  (if (Equivalent-Node? key)
	      (let ((pr (car (cl:last (Equivalent-Node-List key)))))
		(values (car pr) (cdr pr)))
	    (values key (WB-Map-Tree-Node-Value tree))))))))


;;; ================================================================================
;;; Lookup

(defun WB-Map-Tree-Lookup (tree key key-cmp-fn)
  "If `tree' contains a pair whose key is equal to `key', returns three values:
true, the associated value, and the key found; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn)))
	   (and (eq found? ':equal)
		(values t (svref (cdr tree) idx) (svref (car tree) idx)))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     (:equal (if (Equivalent-Node? node-key)
			 (let ((alist (Equivalent-Node-List node-key)))
			   (values t (cdar alist) (caar alist)))
		       (values t (WB-Map-Tree-Node-Value tree) node-key)))
	     (:unequal
	       (and (Equivalent-Node? node-key)
		    (let ((pr (assoc key (Equivalent-Node-List node-key) :test (equal?-fn key-cmp-fn))))
		      (and pr (values t (cdr pr) (car pr))))))
	     (:less
	       (WB-Map-Tree-Lookup (WB-Map-Tree-Node-Left tree) key key-cmp-fn))
	     (:greater
	       (WB-Map-Tree-Lookup (WB-Map-Tree-Node-Right tree) key key-cmp-fn)))))))

(defun WB-Map-Tree-Find-Equivalent (tree key key-cmp-fn)
  "If `tree' contains one or more keys equivalent to `value', returns (first
value) true, (second value) either the one key or an `Equivalent-Map'
containing the values, and (third value) if the second value was a single
key, the corresponding value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn)))
	   (and found? (values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     ((:equal :unequal) (values t node-key (WB-Map-Tree-Node-Value tree)))
	     (:less
	       (WB-Map-Tree-Find-Equivalent (WB-Map-Tree-Node-Left tree) key key-cmp-fn))
	     (:greater
	       (WB-Map-Tree-Find-Equivalent (WB-Map-Tree-Node-Right tree) key key-cmp-fn)))))))


;;; ================================================================================
;;; Map-with

;;; This could almost just call `WB-Map-Tree-Update', except that it has to handle the case
;;; where `key' is an `Equivalent-Node', which can be generated internally by `WB-Map-Tree-Split'
;;; via `WB-Map-Tree-Concat'.
(defun WB-Map-Tree-With (tree key value key-cmp-fn val-cmp-fn)
  "Returns a new tree like `tree' but with the pair < `key', `value' > added,
shadowing any previous pair with the same key."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn val-cmp-fn))
  (cond ((null tree)
	 (if (not (Equivalent-Node? key))
	     (cons (vector key) (vector value))
	   (Make-WB-Map-Tree-Node key nil nil nil)))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `key' is an `Equivalent-Node' because
	   ;; this routine is called by `WB-Map-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Node? key)))
	       (if (equal?-cmp (svref (cdr tree) idx) value val-cmp-fn)
		   tree
		 (cons (car tree) (Vector-Update (cdr tree) idx value)))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 WB-Tree-Max-Vector-Length)
		      (not (Equivalent-Node? key)))
		 (cons (Vector-Insert (car tree) idx key)
		       (Vector-Insert (cdr tree) idx value))
	       (Make-WB-Map-Tree-Node (if found?
					  (Equivalent-Map-With (svref (car tree) idx) (svref (cdr tree) idx)
							       key value key-cmp-fn val-cmp-fn)
					key)
				      value
				      (and (> idx 0)
					   (cons (Vector-Subseq (car tree) 0 idx)
						 (Vector-Subseq (cdr tree) 0 idx)))
				      (and (< right-start (length (the simple-vector (car tree))))
					   (cons (Vector-Subseq (car tree) right-start)
						 (Vector-Subseq (cdr tree) right-start))))))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       (node-val (WB-Map-Tree-Node-Value tree))
	       (node-left (WB-Map-Tree-Node-Left tree))
	       (node-right (WB-Map-Tree-Node-Right tree))
	       ((key-comp (funcall key-cmp-fn key node-key))))
	   (ecase key-comp
	     ((:equal :unequal)
	       (if (and (eq key-comp ':equal) (not (Equivalent-Node? key)) (not (Equivalent-Node? node-key))
			(equal?-cmp value node-val val-cmp-fn))
		  tree
		(Make-WB-Map-Tree-Node (Equivalent-Map-With node-key node-val key value key-cmp-fn val-cmp-fn)
				       value node-left node-right)))
	     ((:less)
	      (let ((new-left (WB-Map-Tree-With node-left key value key-cmp-fn val-cmp-fn)))
		(if (eq new-left node-left)
		    tree
		  (WB-Map-Tree-Build-Node node-key node-val new-left node-right))))
	     ((:greater)
	       (let ((new-right (WB-Map-Tree-With node-right key value key-cmp-fn val-cmp-fn)))
		 (if (eq new-right node-right)
		     tree
		   (WB-Map-Tree-Build-Node node-key node-val node-left new-right)))))))))

(defun Vector-Update (vec idx val)
  "Returns a new vector like `vec' but with `val' at `idx'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec))
	((new-vec (make-array len))))
    (declare (fixnum len))
    (dotimes (i len)
      (setf (svref new-vec i) (svref vec i)))
    (setf (svref new-vec idx) val)
    new-vec))


;;; Untested and currently unused, but not a bad idea.
(defun WB-Map-Tree-Update (tree key value-fn default
   			   &optional (key-cmp-fn #'compare) (val-cmp-fn #'compare))
  "Returns a new tree like `tree', except that the value associated with `key'
is the result of calling `value-fn' on either the existing such value, if any,
or else `default'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function value-fn key-cmp-fn val-cmp-fn)
	   (dynamic-extent value-fn))
  (cond ((null tree)
	 (cons (vector key) (vector (funcall value-fn default))))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   (if (eq found? ':equal)
	       (let ((value (funcall value-fn (svref (cdr tree) idx))))
		 (if (equal?-cmp (svref (cdr tree) idx) value val-cmp-fn)
		     tree
		   (cons (car tree) (Vector-Update (cdr tree) idx value))))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 WB-Tree-Max-Vector-Length))
		 (cons (Vector-Insert (car tree) idx key)
		       (Vector-Insert (cdr tree) idx (funcall value-fn default)))
	       (let ((new-key new-val (Equivalent-Map-Update (svref (car tree) idx) (svref (cdr tree) idx)
							     key value-fn default key-cmp-fn val-cmp-fn)))
		 (Make-WB-Map-Tree-Node new-key new-val
					(and (> idx 0)
					     (cons (Vector-Subseq (car tree) 0 idx)
						   (Vector-Subseq (cdr tree) 0 idx)))
					(and (< right-start (length (the simple-vector (car tree))))
					     (cons (Vector-Subseq (car tree) right-start)
						   (Vector-Subseq (cdr tree) right-start)))))))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       (node-val (WB-Map-Tree-Node-Value tree))
	       (node-left (WB-Map-Tree-Node-Left tree))
	       (node-right (WB-Map-Tree-Node-Right tree))
	       ((key-comp (funcall key-cmp-fn key node-key))))
	   (ecase key-comp
	     ((:equal :unequal)
	      (if (and (eq key-comp ':equal) (not (Equivalent-Node? node-val)))
		  (let ((new-val (funcall value-fn node-val)))
		    (if (equal?-cmp new-val node-val val-cmp-fn)
			tree
		      (Make-WB-Map-Tree-Node node-key new-val node-left node-right)))
		(let ((new-key new-val
			(Equivalent-Map-Update node-key node-val key value-fn default key-cmp-fn val-cmp-fn)))
		  (Make-WB-Map-Tree-Node new-key new-val node-left node-right))))
	     ((:less)
	      (WB-Map-Tree-Build-Node node-key node-val
				      (WB-Map-Tree-Update node-left key value-fn default key-cmp-fn val-cmp-fn)
				      node-right))
	     ((:greater)
	      (WB-Map-Tree-Build-Node node-key node-val node-left
				      (WB-Map-Tree-Update node-right key value-fn default key-cmp-fn val-cmp-fn))))))))


;;; ================================================================================
;;; Map-less

(defun WB-Map-Tree-Less (tree key key-cmp-fn)
  "Returns a new tree like `tree', but with any entry for `key' removed.
If such an entry was found, returns the associated value as a second value."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn)))
	   (if (eq found? ':equal)
	       (values (and (> (length (the simple-vector (car tree))) 1)
			    (cons (Vector-Remove-At (car tree) idx)
				  (Vector-Remove-At (cdr tree) idx)))
		       (svref (cdr tree) idx))
	     tree)))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (Equivalent-Node? node-key))
		  (if (eq comp ':unequal)
		      tree
		    (values (WB-Map-Tree-Join (WB-Map-Tree-Node-Left tree)
					      (WB-Map-Tree-Node-Right tree) key-cmp-fn)
			    (WB-Map-Tree-Node-Value tree)))
		(let ((less-key less-val range-val (Equivalent-Map-Less node-key key key-cmp-fn)))
		  (if (eq less-key node-key)
		      tree
		    (values (WB-Map-Tree-Build-Node less-key less-val (WB-Map-Tree-Node-Left tree)
						    (WB-Map-Tree-Node-Right tree))
			    range-val)))))
	     ((:less)
	      (let ((left (WB-Map-Tree-Node-Left tree))
		    ((new-left range-val (WB-Map-Tree-Less left key key-cmp-fn))))
		(if (eq new-left left)
		    tree
		  (values (WB-Map-Tree-Build-Node node-key (WB-Map-Tree-Node-Value tree)
						  new-left (WB-Map-Tree-Node-Right tree))
			  range-val))))
	     ((:greater)
	      (let ((right (WB-Map-Tree-Node-Right tree))
		    ((new-right range-val (WB-Map-Tree-Less right key key-cmp-fn))))
		(if (eq new-right right)
		    tree
		  (values (WB-Map-Tree-Build-Node node-key (WB-Map-Tree-Node-Value tree)
						  (WB-Map-Tree-Node-Left tree) new-right)
			  range-val)))))))))


(defun WB-Map-Tree-Minimum-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the minimum key and value as two
values.  The key may be an `Equivalent-Map', in which case, as usual, the
value is not meaningful."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (if (consp tree)
      (values (svref (car tree) 0)
	      (svref (cdr tree) 0))
    (let ((left (WB-Map-Tree-Node-Left tree)))
      (if left
	  (WB-Map-Tree-Minimum-Pair left)
	(values (WB-Map-Tree-Node-Key tree)
		(WB-Map-Tree-Node-Value tree))))))

(defun WB-Map-Tree-Less-Minimum (tree key-cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum key/value
pair removed."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (if (consp tree)
      (and (> (length (the simple-vector (car tree))) 1)
	   (cons (Vector-Subseq (car tree) 1)
		 (Vector-Subseq (cdr tree) 1)))
    (let ((left (WB-Map-Tree-Node-Left tree)))
      (if left
	  (WB-Map-Tree-Concat (WB-Map-Tree-Node-Key tree) (WB-Map-Tree-Node-Value tree)
			      (WB-Map-Tree-Less-Minimum left key-cmp-fn) (WB-Map-Tree-Node-Right tree)
			      key-cmp-fn)
	(WB-Map-Tree-Node-Right tree)))))


;;; ================================================================================
;;; Domain

;;; We've chosen a representation that makes this pretty fast (though still linear
;;; time).
(defun WB-Map-Tree-Domain (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree) (car tree))
	(t
	 (let ((key (WB-Map-Tree-Node-Key tree))
	       ((elt (if (Equivalent-Node? key)
			 (Make-Equivalent-Set (mapcar #'car (Equivalent-Node-List key)))
		       key))))
	   (Make-WB-Set-Tree-Node elt
				  (WB-Map-Tree-Domain (WB-Map-Tree-Node-Left tree))
				  (WB-Map-Tree-Domain (WB-Map-Tree-Node-Right tree)))))))


;;; ================================================================================
;;; Union, intersection, and map difference

(defun WB-Map-Tree-Union (tree1 tree2 val-fn key-cmp-fn val-cmp-fn)
  (WB-Map-Tree-Union-Rng tree1 tree2 val-fn Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn val-cmp-fn))

(defun WB-Map-Tree-Union-Rng (tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn key-cmp-fn)
	   (type WB-Map-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (WB-Map-Tree-Split tree1 lo hi key-cmp-fn))
	((null tree2)
	 (WB-Map-Tree-Split tree1 lo hi key-cmp-fn))
	((null tree1)
	 (WB-Map-Tree-Split tree2 lo hi key-cmp-fn))
	((and (consp tree1) (consp tree2))
	 (WB-Map-Tree-Vector-Pair-Union tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2 key-cmp-fn))
		((nonnull? key val (if eqvk1? (Equivalent-Map-Union eqvk1 eqvv1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
				     (values t key2 val2))))))
	   (WB-Map-Tree-Concat-Maybe
	     nonnull? key val
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Trim tree1 lo key2 key-cmp-fn)
				    (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2) lo key2 key-cmp-fn)
				    val-fn lo key2 key-cmp-fn val-cmp-fn)
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Trim tree1 key2 hi key-cmp-fn)
				    (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2) key2 hi key-cmp-fn)
				    val-fn key2 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1 key-cmp-fn))
		((nonnull? key val (if eqvk2? (Equivalent-Map-Union key1 val1 eqvk2 eqvv2 val-fn key-cmp-fn val-cmp-fn)
				     (values t key1 val1))))))
	   (WB-Map-Tree-Concat-Maybe
	     nonnull? key val
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Node-Left tree1)
				    (WB-Map-Tree-Trim tree2 lo key1 key-cmp-fn)
				    val-fn lo key1 key-cmp-fn val-cmp-fn)
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Node-Right tree1)
				    (WB-Map-Tree-Trim tree2 key1 hi key-cmp-fn)
				    val-fn key1 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))))

(defun WB-Map-Tree-Intersect (tree1 tree2 val-fn key-cmp-fn val-cmp-fn)
  (WB-Map-Tree-Intersect-Rng tree1 tree2 val-fn Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn val-cmp-fn))

(defun WB-Map-Tree-Intersect-Rng (tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn key-cmp-fn val-cmp-fn)
	   (type WB-Map-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (WB-Map-Tree-Split tree1 lo hi key-cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Intersect tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2 key-cmp-fn))
		((nonnull? key val
		   (and eqvk1? (Equivalent-Map-Intersect eqvk1 eqvv1 key2 val2 val-fn key-cmp-fn val-cmp-fn))))))
	   (WB-Map-Tree-Concat-Maybe
	     nonnull? key val
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Trim tree1 lo key2 key-cmp-fn)
					(WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2) lo key2 key-cmp-fn)
					val-fn lo key2 key-cmp-fn val-cmp-fn)
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Trim tree1 key2 hi key-cmp-fn)
					(WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2) key2 hi key-cmp-fn)
					val-fn key2 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1 key-cmp-fn))
		((nonnull? key val
		   (and eqvk2? (Equivalent-Map-Intersect key1 val1 eqvk2 eqvv2 val-fn key-cmp-fn val-cmp-fn))))))
	   (WB-Map-Tree-Concat-Maybe nonnull? key val
				     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Node-Left tree1)
								(WB-Map-Tree-Trim tree2 lo key1 key-cmp-fn)
								val-fn lo key1 key-cmp-fn val-cmp-fn)
				     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Node-Right tree1)
								(WB-Map-Tree-Trim tree2 key1 hi key-cmp-fn)
								val-fn key1 hi key-cmp-fn val-cmp-fn)
				     key-cmp-fn)))))


(defun WB-Map-Tree-Diff-2 (tree1 tree2 key-cmp-fn val-cmp-fn)
  "Returns two values: one containing the pairs that are in `tree1' but not
`tree2', and the other containing the pairs that are in `tree2' but not
`tree1'."
  (WB-Map-Tree-Diff-2-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn val-cmp-fn))

(defun WB-Map-Tree-Diff-2-Rng (tree1 tree2 lo hi key-cmp-fn val-cmp-fn)
  (cond ((eq tree1 tree2)		; historically-related tree optimization
	 (values nil nil))
	((or (null tree1) (null tree2))
	 (values (WB-Map-Tree-Split tree1 lo hi key-cmp-fn)
		 (WB-Map-Tree-Split tree2 lo hi key-cmp-fn)))
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Diff-2 tree1 tree2 lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((new-left-1 new-left-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim tree1 lo key2 key-cmp-fn)
					  (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2) lo key2 key-cmp-fn)
					  lo key2 key-cmp-fn val-cmp-fn))
		(new-right-1 new-right-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim tree1 key2 hi key-cmp-fn)
					  (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2) key2 hi key-cmp-fn)
					  key2 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (and eqvk1? (Equivalent-Map-Difference eqvk1 eqvv1 key2 val2 key-cmp-fn val-cmp-fn)))
		 (nonnull2? diffk2 diffv2
		   (if eqvk1? (Equivalent-Map-Difference key2 val2 eqvk1 eqvv1 key-cmp-fn val-cmp-fn)
		     (values t key2 val2))))))
	   (values (if nonnull1? (WB-Map-Tree-Concat diffk1 diffv1 new-left-1 new-right-1 key-cmp-fn)
		     (WB-Map-Tree-Join new-left-1 new-right-1 key-cmp-fn))
		   (if nonnull2? (WB-Map-Tree-Concat diffk2 diffv2 new-left-2 new-right-2 key-cmp-fn)
		     (WB-Map-Tree-Join new-left-2 new-right-2 key-cmp-fn)))))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((new-left-1 new-left-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree1) lo key1 key-cmp-fn)
					  (WB-Map-Tree-Trim tree2 lo key1 key-cmp-fn)
					  lo key1 key-cmp-fn val-cmp-fn))
		(new-right-1 new-right-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree1) key1 hi key-cmp-fn)
					  (WB-Map-Tree-Trim tree2 key1 hi key-cmp-fn)
					  key1 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (if eqvk2? (Equivalent-Map-Difference key1 val1 eqvk2 eqvv2 key-cmp-fn val-cmp-fn)
		     (values t key1 val1)))
		 (nonnull2? diffk2 diffv2
		   (and eqvk2? (Equivalent-Map-Difference eqvk2 eqvv2 key1 val1 key-cmp-fn val-cmp-fn))))))
	   (values (if nonnull1? (WB-Map-Tree-Concat diffk1 diffv1 new-left-1 new-right-1 key-cmp-fn)
		     (WB-Map-Tree-Join new-left-1 new-right-1 key-cmp-fn))
		   (if nonnull2? (WB-Map-Tree-Concat diffk2 diffv2 new-left-2 new-right-2 key-cmp-fn)
		     (WB-Map-Tree-Join new-left-2 new-right-2 key-cmp-fn)))))))


;;; ================================================================================
;;; Restrict and restrict-not

(defun WB-Map-Tree-Restrict (map-tree set-tree key-cmp-fn)
  (WB-Map-Tree-Restrict-Rng map-tree set-tree Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn))

(defun WB-Map-Tree-Restrict-Rng (map-tree set-tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree map-tree)
	   (type WB-Set-Tree set-tree))
  (cond ((or (null map-tree) (null set-tree))
	 nil)
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (Vector-Pair-Restrict map-tree set-tree lo hi key-cmp-fn)
	   (let ((raw-elt (WB-Set-Tree-Node-Value set-tree))
		 ((set-elt (if (Equivalent-Node? raw-elt)
			       (car (Equivalent-Node-List raw-elt))
			     raw-elt))
		  ((new-left (WB-Map-Tree-Restrict-Rng
			       (WB-Map-Tree-Trim map-tree lo set-elt key-cmp-fn)
			       (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left set-tree) lo set-elt key-cmp-fn)
			       lo set-elt key-cmp-fn))
		   (new-right (WB-Map-Tree-Restrict-Rng
				(WB-Map-Tree-Trim map-tree set-elt hi key-cmp-fn)
				(WB-Set-Tree-Trim (WB-Set-Tree-Node-Right set-tree) set-elt hi key-cmp-fn)
				set-elt hi key-cmp-fn))
		   (eqvk? eqvk eqvv (WB-Map-Tree-Find-Equivalent map-tree set-elt key-cmp-fn)))))
	     (if (not eqvk?)
		 (WB-Map-Tree-Join new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (Equivalent-Map-Restrict eqvk eqvv raw-elt key-cmp-fn)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right key-cmp-fn)
		   (WB-Map-Tree-Join new-left new-right key-cmp-fn)))))))
	(t
	 (let ((raw-key (WB-Map-Tree-Node-Key map-tree))
	       ((map-key (if (Equivalent-Node? raw-key) ; for benefit of `compare'
			     (caar (Equivalent-Node-List raw-key))
			   raw-key))
		((new-left (WB-Map-Tree-Restrict-Rng (WB-Map-Tree-Node-Left map-tree)
						     (WB-Set-Tree-Trim set-tree lo map-key key-cmp-fn)
						     lo map-key key-cmp-fn))
		 (new-right (WB-Map-Tree-Restrict-Rng (WB-Map-Tree-Node-Right map-tree)
						      (WB-Set-Tree-Trim set-tree map-key hi key-cmp-fn)
						      map-key hi key-cmp-fn))
		 (eqvv? eqvv (WB-Set-Tree-Find-Equivalent set-tree map-key key-cmp-fn)))))
	   (if (not eqvv?)
	       (WB-Map-Tree-Join new-left new-right key-cmp-fn)
	     (let ((map-val (WB-Map-Tree-Node-Value map-tree))
		   ((rpr? rkey rval (Equivalent-Map-Restrict raw-key map-val eqvv key-cmp-fn))))
	       (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right key-cmp-fn)
		 (WB-Map-Tree-Join new-left new-right key-cmp-fn))))))))

(defun WB-Map-Tree-Restrict-Not (map-tree set-tree key-cmp-fn)
  (WB-Map-Tree-Restrict-Not-Rng map-tree set-tree Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn))

(defun WB-Map-Tree-Restrict-Not-Rng (map-tree set-tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree map-tree)
	   (type WB-Set-Tree set-tree))
  (cond ((null map-tree)
	 nil)
	((null set-tree)
	 (WB-Map-Tree-Split map-tree lo hi key-cmp-fn))
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (Vector-Pair-Restrict-Not map-tree set-tree lo hi key-cmp-fn)
	   (let ((raw-elt (WB-Set-Tree-Node-Value set-tree))
		 ((set-elt (if (Equivalent-Node? raw-elt)
			       (car (Equivalent-Node-List raw-elt))
			     raw-elt))
		  ((new-left (WB-Map-Tree-Restrict-Not-Rng
			       (WB-Map-Tree-Trim map-tree lo set-elt key-cmp-fn)
			       (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left set-tree) lo set-elt key-cmp-fn)
			       lo set-elt key-cmp-fn))
		   (new-right (WB-Map-Tree-Restrict-Not-Rng
				(WB-Map-Tree-Trim map-tree set-elt hi key-cmp-fn)
				(WB-Set-Tree-Trim (WB-Set-Tree-Node-Right set-tree) set-elt hi key-cmp-fn)
				set-elt hi key-cmp-fn))
		   (eqvk? eqvk eqvv (WB-Map-Tree-Find-Equivalent map-tree set-elt key-cmp-fn)))))
	     (if (not eqvk?)
		 (WB-Map-Tree-Join new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (Equivalent-Map-Restrict-Not eqvk eqvv raw-elt key-cmp-fn)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right key-cmp-fn)
		   (WB-Map-Tree-Join new-left new-right key-cmp-fn)))))))
	(t
	 (let ((raw-key (WB-Map-Tree-Node-Key map-tree))
	       ((map-key (if (Equivalent-Node? raw-key)
			     (caar (Equivalent-Node-List raw-key))
			   raw-key))
		((new-left (WB-Map-Tree-Restrict-Not-Rng (WB-Map-Tree-Node-Left map-tree)
							 (WB-Set-Tree-Trim set-tree lo map-key key-cmp-fn)
							 lo map-key key-cmp-fn))
		 (new-right (WB-Map-Tree-Restrict-Not-Rng (WB-Map-Tree-Node-Right map-tree)
							  (WB-Set-Tree-Trim set-tree map-key hi key-cmp-fn)
							  map-key hi key-cmp-fn))
		 (eqvv? eqvv (WB-Set-Tree-Find-Equivalent set-tree map-key key-cmp-fn)))))
	   (let ((map-val (WB-Map-Tree-Node-Value map-tree)))
	     (if (not eqvv?)
		 (WB-Map-Tree-Concat raw-key map-val new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (Equivalent-Map-Restrict-Not raw-key map-val eqvv key-cmp-fn)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right key-cmp-fn)
		   (WB-Map-Tree-Join new-left new-right key-cmp-fn)))))))))

;;; ================================================================================
;;; Compare

(defun WB-Map-Tree-Compare (tree1 tree2 key-cmp-fn val-cmp-fn)
  (if (eq tree1 tree2) ':equal
    (let ((size1 (WB-Map-Tree-Size tree1))
	  (size2 (WB-Map-Tree-Size tree2)))
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t (WB-Map-Tree-Compare-Rng tree1 0 tree2 0 0 size1 key-cmp-fn val-cmp-fn))))))

(defun WB-Map-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi key-cmp-fn val-cmp-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi)
	   (type function key-cmp-fn val-cmp-fn))
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related-map optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (consp tree1) (consp tree2))
	 (let ((unequal? nil))
	   (or (gmap (:result or) (fn (key1 val1 key2 val2)
				    (let ((key-comp (funcall key-cmp-fn key1 key2)))
				      (when (eq key-comp ':unequal)
					(setq unequal? t))
				      (if (or (eq key-comp ':less) (eq key-comp ':greater))
					  key-comp
					(let ((val-comp (funcall val-cmp-fn val1 val2)))
					  (when (eq val-comp ':unequal)
					    (setq unequal? t))
					  (and (or (eq val-comp ':less) (eq val-comp ':greater))
					       val-comp)))))
		     (:arg simple-vector (car tree1) :start (- lo base1) :stop (- hi base1))
		     (:arg simple-vector (cdr tree1) :start (- lo base1) :stop (- hi base1))
		     (:arg simple-vector (car tree2) :start (- lo base2) :stop (- hi base2))
		     (:arg simple-vector (cdr tree2) :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((consp tree1)
	 (invert-comparison (WB-Map-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi key-cmp-fn val-cmp-fn)))
	(t
	 (let ((left1 (WB-Map-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Map-Tree-Size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (WB-Map-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Map-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Map-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi key-cmp-fn val-cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((key1 (WB-Map-Tree-Node-Key tree1))
		   (val1 (WB-Map-Tree-Node-Value tree1))
		   (key2 val2
		      (WB-Map-Tree-Rank-Pair-Internal
			tree2 (the fixnum (- new-hi base2))))
		   ((comp (Equivalent-Map-Compare key1 val1 key2 val2 key-cmp-fn val-cmp-fn))))
	       (if (or (eq comp ':less) (eq comp ':greater))
		   comp
		 (let ((key1-size (Map-Key-Size key1))
		       ((new-lo (the fixnum (+ base1 left1-size key1-size)))
			((right1a base1a
			   (WB-Map-Tree-Rank-Trim (WB-Map-Tree-Node-Right tree1)
						  (the fixnum
						    (+ base1 left1-size key1-size))
						  new-lo hi))
			 (tree2a base2a (WB-Map-Tree-Rank-Trim tree2 base2 new-lo hi))
			 ((right-comp
			    (WB-Map-Tree-Compare-Rng right1a base1a tree2a base2a
						     new-lo hi key-cmp-fn val-cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal comp))))))))))

(defun WB-Map-Tree-Rank-Trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type fixnum base lo hi))
  (if (or (null tree) (consp tree))
      (values tree base)
    (let ((node-rank (the fixnum
		       (+ base (WB-Map-Tree-Size (WB-Map-Tree-Node-Left tree))))))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (WB-Map-Tree-Rank-Trim (WB-Map-Tree-Node-Left tree) base lo hi))
	(WB-Map-Tree-Rank-Trim (WB-Map-Tree-Node-Right tree)
			       (+ node-rank (Map-Key-Size (WB-Map-Tree-Node-Key tree)))
			       lo hi)))))

(defun WB-Map-Tree-Rank (tree key key-cmp-fn)
  "Searches a map tree `tree' for `key'.  Returns two values, a boolean and an
index.  If `key', or a value equivalent to `key', is in `tree', the boolean
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `key' would have if it were to be added.  Note that
if the map contains equivalent-but-unequal keys, the rank of each of several
such keys is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (declare (type function key-cmp-fn))
  (labels ((rec (tree key base)
	     (cond ((null tree) (values nil base))
		   ((consp tree)
		    (let ((found? idx (Vector-Set-Binary-Search (car tree) key key-cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Map-Tree-Node-Key tree))
			  (left (WB-Map-Tree-Node-Left tree))
			  ((left-size (WB-Map-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall key-cmp-fn key node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Node? node-val)
			     (let ((prs (Equivalent-Node-List node-val))
				   ((pos (cl:position key prs :test (equal?-fn key-cmp-fn)
						      :key #'car))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left key base))
			((:greater)
			 (rec (WB-Map-Tree-Node-Right tree) key
			      (+ node-base (Map-Key-Size node-val))))))))))
    (rec tree key 0)))

(defun WB-Map-Tree-Rank-Pair (tree rank)
  (let ((key value rem (WB-Map-Tree-Rank-Pair-Internal tree rank)))
    (if (Equivalent-Node? key)
	(let ((pr (nth rem (Equivalent-Node-List key))))
	  (values (car pr) (cdr pr)))
      (values key value))))

(defun WB-Map-Tree-Rank-Pair-Internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in map comparator"))
	((consp tree)
	 (values (svref (car tree) rank) (svref (cdr tree) rank) 0))
	(t
	 (let ((left (WB-Map-Tree-Node-Left tree))
	       ((left-size (WB-Map-Tree-Size left))))
	   (if (< rank left-size)
	       (WB-Map-Tree-Rank-Pair-Internal left rank)
	     (let ((key (WB-Map-Tree-Node-Key tree))
		   ((key-size (Map-Key-Size key)))
		   (rank (- rank left-size)))
	       (declare (type fixnum rank key-size))
	       (if (< rank key-size)
		   (values key (WB-Map-Tree-Node-Value tree) rank)
		 (WB-Map-Tree-Rank-Pair-Internal (WB-Map-Tree-Node-Right tree)
						 (the fixnum (- rank key-size))))))))))


;;; ================================================================================
;;; Miscellany

(defun WB-Map-Tree-From-List (lst key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (type function key-fn value-fn))
  (let ((tree nil))
    (dolist (pr lst)
      (setq tree (WB-Map-Tree-With tree (funcall key-fn pr) (funcall value-fn pr) key-cmp-fn val-cmp-fn)))
    tree))

(defun WB-Map-Tree-From-Iterable (it key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (type function it key-fn value-fn))
  (let ((tree nil))
    (while (funcall it ':more?)
      (let ((pr (funcall it ':get)))
	(setq tree (WB-Map-Tree-With tree (funcall key-fn pr) (funcall value-fn pr) key-cmp-fn val-cmp-fn))))
    tree))

;;; See `WB-Set-Tree-From-Sorted-Iterable'.
(defun WB-Map-Tree-From-Sorted-Iterable (it len key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function it key-fn value-fn key-cmp-fn))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil Hedge-Positive-Infinity Hedge-Negative-Infinity))
		   ((= n 1)
		    (let ((e (funcall it ':get))
			  ((k (funcall key-fn e))))
		      (values (cons (vector k) (vector (funcall value-fn e))) k k)))
		   ;; Reduces consing about 12%, improves speed.
		   ((= n 2)
		    (let ((a (funcall it ':get))
			  (b (funcall it ':get))
			  ((ka (funcall key-fn a))
			   (kb (funcall key-fn b))
			   (va (funcall value-fn a))
			   (vb (funcall value-fn b))))
		      (ecase (funcall key-cmp-fn ka kb)
			(:equal ; Shouldn't really happen ... I guess the `b' pair should win.
			  (values (cons (vector kb) (vector vb)) kb kb))
			(:less (values (cons (vector ka kb) (vector va vb)) ka kb))
			(:greater (values (cons (vector kb ka) (vector vb va)) kb ka))
			(:unequal (values (WB-Map-Tree-With (cons (vector ka) (vector va)) kb vb key-cmp-fn val-cmp-fn)
					  ka ka)))))
		   (t
		    (let ((n2 (floor (1- n) 2))
			  ((left left-first left-last (recur n2))
			   ((n2-elt (funcall it ':get))
			    ((right right-first right-last (recur (- n n2 1)))
			     (k (funcall key-fn n2-elt))
			     (v (funcall value-fn n2-elt))))))
		      ;; Here we check whether the tree really is sorted as promised.
		      ;; (We really have to do this for correctness, because even if it is sorted, it
		      ;; could have sequences of equivalent-but-unequal keys.)
		      (if (and (less-than?-cmp left-last k key-cmp-fn)
			       (less-than?-cmp k right-first key-cmp-fn))
			  (values (WB-Map-Tree-Build-Node k v left right) left-first right-last)
			;; Fall back to the general case, being careful to keep the rightmost value for
			;; a duplicated key.
			(values (WB-Map-Tree-Union (WB-Map-Tree-With left k v key-cmp-fn val-cmp-fn)
						   right (fn (_a b) b) key-cmp-fn val-cmp-fn)
				(if (less-than?-cmp left-first right-first key-cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last key-cmp-fn) right-last left-last))))))))
    (recur len)))

(defun WB-Map-Tree-Split-Above (tree value cmp-fn)
  (WB-Map-Tree-Split tree value Hedge-Positive-Infinity cmp-fn))

(defun WB-Map-Tree-Split-Below (tree value cmp-fn)
  (WB-Map-Tree-Split tree Hedge-Negative-Infinity value cmp-fn))


;;; ================================================================================
;;; Support routines for the above (maps)

(defun WB-Map-Tree-Split (tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((consp tree)
	 (let ((keys (car tree))
	       (vals (cdr tree))
	       ((len (length (the simple-vector keys)))
		((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				     0
				   (Vector-Set-Binary-Search-Lo keys lo key-cmp-fn)))
		 (split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi keys hi key-cmp-fn))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (Vector-Subseq keys split-point-lo split-point-hi)
			(Vector-Subseq vals split-point-lo split-point-hi))))))
	((not (or (eq lo Hedge-Negative-Infinity)
		  (greater-than?-cmp (WB-Map-Tree-Node-Key tree) lo key-cmp-fn)))
	 (WB-Map-Tree-Split (WB-Map-Tree-Node-Right tree) lo hi key-cmp-fn))
	((not (or (eq hi Hedge-Positive-Infinity)
		  (less-than?-cmp (WB-Map-Tree-Node-Key tree) hi key-cmp-fn)))
	 (WB-Map-Tree-Split (WB-Map-Tree-Node-Left tree) lo hi key-cmp-fn))
	(t
	 (let ((new-left (WB-Map-Tree-Split (WB-Map-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity key-cmp-fn))
	       (new-right (WB-Map-Tree-Split (WB-Map-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi key-cmp-fn)))
	   (if (and (eq new-left (WB-Map-Tree-Node-Left tree))
		    (eq new-right (WB-Map-Tree-Node-Right tree)))
	       tree
	     (WB-Map-Tree-Concat (WB-Map-Tree-Node-Key tree)
				 (WB-Map-Tree-Node-Value tree)
				 new-left new-right key-cmp-fn))))))

(defun WB-Map-Tree-Trim (tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than?-cmp (svref (car tree) (1- (length (the simple-vector (car tree)))))
				     lo key-cmp-fn))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than?-cmp (svref (car tree) 0) hi key-cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo Hedge-Negative-Infinity)
					0
				      (Vector-Set-Binary-Search-Lo (car tree) lo key-cmp-fn)))
		    (split-point-hi (if (eq hi Hedge-Positive-Infinity)
					(length (the simple-vector (car tree)))
				      (Vector-Set-Binary-Search-Hi (car tree) hi key-cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((key (WB-Map-Tree-Node-Key tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than?-cmp key lo key-cmp-fn))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than?-cmp key hi key-cmp-fn))
		   tree
		 (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree) lo hi key-cmp-fn))
	     (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree) lo hi key-cmp-fn))))))

(defun WB-Map-Tree-Concat-Maybe (pair? key value left right key-cmp-fn)
  (declare (optimize (speed 3) (safety 0)))
  (if pair? (WB-Map-Tree-Concat key value left right key-cmp-fn)
    (WB-Map-Tree-Join left right key-cmp-fn)))

(defun WB-Map-Tree-Concat (key value left right key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree left right)
	   (type function key-cmp-fn))
  (cond ((null left)
	 ;; To avoid needing `val-cmp-fn' absolutely everywhere, and because the only reason it's
	 ;; needed here is to optimize out a redundant `with', and we know this `with' can't be
	 ;; redundant, we just pass a `val-cmp-fn' that disables the optimization.
	 (WB-Map-Tree-With right key value key-cmp-fn (fn (_x _y) ':unequal)))
	((null right)
	 (WB-Map-Tree-With left key value key-cmp-fn (fn (_x _y) ':unequal)))
	((and (WB-Map-Tree-Node? left)
	      (> (WB-Map-Tree-Node-Size left)
		 (the fixnum (* (WB-Map-Tree-Size right) WB-Tree-Balance-Factor))))
	 (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key left)
				 (WB-Map-Tree-Node-Value left)
				 (WB-Map-Tree-Node-Left left)
				 (WB-Map-Tree-Concat key value
						     (WB-Map-Tree-Node-Right left)
						     right key-cmp-fn)))
	((and (WB-Map-Tree-Node? right)
	      (> (WB-Map-Tree-Node-Size right)
		 (the fixnum (* (WB-Map-Tree-Size left) WB-Tree-Balance-Factor))))
	 (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key right)
				 (WB-Map-Tree-Node-Value right)
				 (WB-Map-Tree-Concat key value left
						     (WB-Map-Tree-Node-Left right) key-cmp-fn)
				 (WB-Map-Tree-Node-Right right)))
	(t
	 (WB-Map-Tree-Build-Node key value left right))))

(defun WB-Map-Tree-Join (left right key-cmp-fn)
  (if (null left) right
    (if (null right) left
      (let ((min-key min-val (WB-Map-Tree-Minimum-Pair right)))
	(WB-Map-Tree-Concat min-key min-val
			    left (WB-Map-Tree-Less-Minimum right key-cmp-fn) key-cmp-fn)))))

(defun WB-Map-Tree-Build-Node (key value left right)
  "Constructs a `WB-Map-Tree', performing one rebalancing step if required.
`key' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (Equivalent-Node? key))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  WB-Tree-Max-Vector-Length))
	  (cons (concatenate 'simple-vector (car left) (vector key) (car right))
		(concatenate 'simple-vector (cdr left) (vector value) (cdr right)))
	(Make-WB-Map-Tree-Node key value left right))
    (let ((sizl (WB-Map-Tree-Size left))
	  (sizr (WB-Map-Tree-Size right)))
      (cond ((and (WB-Map-Tree-Node? left)
		  (> sizl (the fixnum (* sizr WB-Tree-Balance-Factor))))
	     (let ((ll (WB-Map-Tree-Node-Left left))
		   (rl (WB-Map-Tree-Node-Right left)))
	       (if (or (null rl) (consp rl)
		       (<= (WB-Map-Tree-Size rl) (WB-Map-Tree-Size ll)))
		   (Make-WB-Map-Tree-Node (WB-Map-Tree-Node-Key left)
					  (WB-Map-Tree-Node-Value left)
					  ll
					  (WB-Map-Tree-Build-Node key value rl right))
		 (Make-WB-Map-Tree-Node (WB-Map-Tree-Node-Key rl)
					(WB-Map-Tree-Node-Value rl)
					(WB-Map-Tree-Build-Node
					  (WB-Map-Tree-Node-Key left)
					  (WB-Map-Tree-Node-Value left)
					  ll
					  (WB-Map-Tree-Node-Left rl))
					(WB-Map-Tree-Build-Node
					  key value (WB-Map-Tree-Node-Right rl) right)))))
	    ((and (WB-Map-Tree-Node? right)
		  (> sizr (the fixnum (* sizl WB-Tree-Balance-Factor))))
	     (let ((lr (WB-Map-Tree-Node-Left right))
		   (rr (WB-Map-Tree-Node-Right right)))
	       (if (or (null lr) (consp lr)
		       (<= (WB-Map-Tree-Size lr) (WB-Map-Tree-Size rr)))
		   (Make-WB-Map-Tree-Node (WB-Map-Tree-Node-Key right)
					  (WB-Map-Tree-Node-Value right)
					  (WB-Map-Tree-Build-Node key value left lr)
					  rr)
		 (Make-WB-Map-Tree-Node (WB-Map-Tree-Node-Key lr)
					(WB-Map-Tree-Node-Value lr)
					(WB-Map-Tree-Build-Node
					  key value left (WB-Map-Tree-Node-Left lr))
					(WB-Map-Tree-Build-Node
					  (WB-Map-Tree-Node-Key right)
					  (WB-Map-Tree-Node-Value right)
					  (WB-Map-Tree-Node-Right lr)
					  rr)))))
	    (t
	     (Make-WB-Map-Tree-Node key value left right))))))


(defun WB-Map-Tree-Verify (tree key-cmp-fn)
  (WB-Map-Tree-Verify-Rng tree Hedge-Negative-Infinity Hedge-Positive-Infinity key-cmp-fn))

(defun WB-Map-Tree-Verify-Rng (tree lo hi key-cmp-fn)
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len WB-Tree-Max-Vector-Length)
		(do ((i 0 (1+ i))
		     (prev lo))
		    ((= i len)
		     (or (eq hi Hedge-Positive-Infinity)
			 (less-than?-cmp prev hi key-cmp-fn)))
		  (let ((key (svref (car tree) i)))
		    (unless (and (not (Equivalent-Node? key))
				 (or (eq prev Hedge-Negative-Infinity)
				     (less-than?-cmp prev key key-cmp-fn)))
		      (return nil))
		    (setq prev key))))))
	(t
	 (let ((sizl (WB-Map-Tree-Size (WB-Map-Tree-Node-Left tree)))
	       (sizr (WB-Map-Tree-Size (WB-Map-Tree-Node-Right tree)))
	       (key (WB-Map-Tree-Node-Key tree)))
	   (and (= (WB-Map-Tree-Node-Size tree) (+ sizl sizr (Map-Key-Size key)))
		(or (not (Equivalent-Node? key))
		    (> (length (Equivalent-Node-List key)) 1))
		(or (<= sizr 4)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 4)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Map-Tree-Verify-Rng (WB-Map-Tree-Node-Left tree) lo key key-cmp-fn)
		(WB-Map-Tree-Verify-Rng (WB-Map-Tree-Node-Right tree) key hi key-cmp-fn))))))


(defun WB-Map-Tree-Vector-Pair-Union (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (let ((new-pr any-equivalent? (Vector-Pair-Union pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)))
    (if any-equivalent?
	(let ((tree nil))
	  ;; Let's just do it the stupid way -- it's not supposed to happen often.
	  (dotimes (i (length (car new-pr)))
	    ;; As above -- the funny `val-cmp-fn' value just blocks an optimization that can't fire anyway.
	    (setq tree (WB-Map-Tree-With tree (svref (car new-pr) i)
					 (svref (cdr new-pr) i) key-cmp-fn (fn (_x _y) ':unequal))))
	  tree)
      (if (> (length (car new-pr)) WB-Tree-Max-Vector-Length)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (Make-WB-Map-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Union (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function val-fn key-cmp-fn val-cmp-fn))
  (let ((keys1 (the simple-vector (car pr1)))
	(keys2 (the simple-vector (car pr2)))
	(vals1 (the simple-vector (cdr pr1)))
	(vals2 (the simple-vector (cdr pr2)))
	(i1 0)
	(i2 0)
	((len1 (length keys1))
	 (len2 (length keys2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref keys2 i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref keys2 (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 ;; The use of `:no-value' could produce an empty result.
	 (and keys (values (cons (Reverse-List-To-Vector keys)
				 (Reverse-List-To-Vector vals))
			   any-equivalent?)))
      (cond ((= i1 len1)
	     (do () ((= i2 len2))
	       (push (svref keys2 i2) keys)
	       (push (svref vals2 i2) vals)
	       (incf i2)))
	    ((= i2 len2)
	     (do () ((= i1 len1))
	       (push (svref keys1 i1) keys)
	       (push (svref vals1 i1) vals)
	       (incf i1)))
	    (t
	     (let ((key1 (svref keys1 i1))
		   (key2 (svref keys2 i2))
		   ((comp (funcall key-cmp-fn key1 key2))))
	       (ecase comp
		 ((:equal)
		   (let ((val1 (svref vals1 i1))
			 (val2 (svref vals2 i2))
			 ((new-val second-val (if (equal?-cmp val1 val2 val-cmp-fn) val1
						(funcall val-fn val1 val2)))))
		    (unless (eq second-val ':no-value)
		      (push key1 keys)
		      (push new-val vals)))
		   (incf i1)
		   (incf i2))
		 ((:less)
		  (push key1 keys)
		  (push (svref vals1 i1) vals)
		  (incf i1))
		 ((:greater)
		  (push key2 keys)
		  (push (svref vals2 i2) vals)
		  (incf i2))
		 ((:unequal)
		  (let ((nonnull? key val
			  (Equivalent-Map-Union key1 (svref vals1 i1)
			   key2 (svref vals2 i2) val-fn key-cmp-fn val-cmp-fn)))
		    (when nonnull?
		      (push key keys)
		      (push val vals)
		      (when (Equivalent-Node? key)
			(setq any-equivalent? t))))
		   (incf i1)
		   (incf i2)))))))))

(defun Vector-Pair-Intersect (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function val-fn key-cmp-fn val-cmp-fn))
  (let ((keys1 (the simple-vector (car pr1)))
	(vals1 (the simple-vector (cdr pr1)))
	(keys2 (the simple-vector (car pr2)))
	(vals2 (the simple-vector (cdr pr2)))
	(i1 0)
	(i2 0)
	((len1 (length keys1))
	 (len2 (length keys2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (Reverse-List-To-Vector keys)
			 (Reverse-List-To-Vector vals))))
      (let ((key1 (svref keys1 i1))
	    (key2 (svref keys2 i2))
	    ((comp (funcall key-cmp-fn key1 key2))))
	(ecase comp
	  ((:equal)
	    (let ((val1 (svref vals1 i1))
		  (val2 (svref vals2 i2))
		  ((new-val second-val (if (equal?-cmp val1 val2 val-cmp-fn) val1
					 (funcall val-fn val1 val2)))))
	      (unless (eq second-val ':no-value)
		(push key1 keys)
		(push new-val vals)))
	    (incf i1)
	    (incf i2))
	  ((:less)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Diff-2 (pr1 pr2 lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function key-cmp-fn val-cmp-fn))
  (let ((keys1 (the simple-vector (car pr1)))
	(vals1 (the simple-vector (cdr pr1)))
	(keys2 (the simple-vector (car pr2)))
	(vals2 (the simple-vector (cdr pr2)))
	(i1 0)
	(i2 0)
	((len1 (length keys1))
	 (len2 (length keys2))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref keys2 i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref keys2 (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((diff-1-keys nil)
	 (diff-1-vals nil)
	 (diff-2-keys nil)
	 (diff-2-vals nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref keys1 i1) diff-1-keys)
	   (push (svref vals1 i1) diff-1-vals)
	   (incf i1))
	 (do () ((= i2 len2))
	   (push (svref keys2 i2) diff-2-keys)
	   (push (svref vals2 i2) diff-2-vals)
	   (incf i2))
	 (values (and diff-1-keys (cons (Reverse-List-To-Vector diff-1-keys)
					(Reverse-List-To-Vector diff-1-vals)))
		 (and diff-2-keys (cons (Reverse-List-To-Vector diff-2-keys)
					(Reverse-List-To-Vector diff-2-vals)))))
      (let ((key1 (svref keys1 i1))
	    (key2 (svref keys2 i2))
	    (val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (funcall key-cmp-fn key1 key2))))
	(ecase comp
	  ((:equal)
	   (unless (equal?-cmp val1 val2 val-cmp-fn)
	     (push key1 diff-1-keys)
	     (push val1 diff-1-vals)
	     (push key2 diff-2-keys)
	     (push val2 diff-2-vals))
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (push key1 diff-1-keys)
	   (push val1 diff-1-vals)
	   (incf i1))
	  ((:greater)
	   (push key2 diff-2-keys)
	   (push val2 diff-2-vals)
	   (incf i2))
	  ((:unequal)
	   (push key1 diff-1-keys)
	   (push val1 diff-1-vals)
	   (push key2 diff-2-keys)
	   (push val2 diff-2-vals)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Restrict (map-pr set-vec lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons map-pr)
	   (type simple-vector set-vec)
	   (type function key-cmp-fn))
  (let ((map-keys (the simple-vector (car map-pr)))
	(map-vals (the simple-vector (cdr map-pr)))
	(i1 0)
	(i2 0)
	((len1 (length map-keys))
	 (len2 (length set-vec))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref map-keys i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref set-vec i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref map-keys (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref set-vec (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (Reverse-List-To-Vector keys)
			 (Reverse-List-To-Vector vals))))
      (let ((k (svref map-keys i1))
	    (e (svref set-vec i2))
	    ((comp (funcall key-cmp-fn k e))))
	(ecase comp
	  (:equal
	   (push k keys)
	   (push (svref map-vals i1) vals)
	   (incf i1)
	   (incf i2))
	  (:less
	   (incf i1))
	  (:greater
	   (incf i2))
	  (:unequal
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Restrict-Not (map-pr set-vec lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons map-pr)
	   (type simple-vector set-vec)
	   (type function key-cmp-fn))
  (let ((map-keys (the simple-vector (car map-pr)))
	(map-vals (the simple-vector (cdr map-pr)))
	(i1 0)
	(i2 0)
	((len1 (length map-keys))
	 (len2 (length set-vec))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref map-keys i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref set-vec i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref map-keys (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref set-vec (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref map-keys i1) keys)
	   (push (svref map-vals i1) vals)
	   (incf i1))
	 (and keys (cons (Reverse-List-To-Vector keys)
			 (Reverse-List-To-Vector vals))))
      (let ((k (svref map-keys i1))
	    (e (svref set-vec i2))
	    ((comp (funcall key-cmp-fn k e))))
	(ecase comp
	  (:equal
	   (incf i1)
	   (incf i2))
	  (:less
	   (push k keys)
	   (push (svref map-vals i1) vals)
	   (incf i1))
	  (:greater
	   (incf i2))
	  (:unequal
	   (push k keys)
	   (push (svref map-vals i1) vals)
	   (incf i1)
	   (incf i2)))))))


;;; ================================================================================
;;; Iteration primitives

(defmacro Do-WB-Map-Tree-Pairs ((key-var value-var tree-form &optional value-form)
				&body body)
  ;; See comment at `Do-WB-Set-Tree-Members'.
  (let ((body-fn (gensymx #:body-))
	(recur-fn (gensymx #:recur-)))
    `(block nil
       (labels ((,body-fn (,key-var ,value-var)
		  . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(let ((keys (car tree))
			      (vals (cdr tree)))
			  (dotimes (i (length (the simple-vector (car tree))))
			    (,body-fn (svref keys i) (svref vals i))))
		      (progn
			(,recur-fn (WB-Map-Tree-Node-Left tree))
			(let ((key (WB-Map-Tree-Node-Key tree)))
			  (if (Equivalent-Node? key)
			      (dolist (pr (Equivalent-Node-List key))
				(,body-fn (car pr) (cdr pr)))
			    (,body-fn key (WB-Map-Tree-Node-Value tree))))
			(,recur-fn (WB-Map-Tree-Node-Right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))

(defun WB-Map-Tree-Compose (tree fn)
  (and tree
       (if (consp tree)
	   (cons (car tree)
		 (gmap (:result vector :length (length (cdr tree)))
		       fn (:arg simple-vector (cdr tree))))
	 (let ((key (WB-Map-Tree-Node-Key tree))
	       (val (WB-Map-Tree-Node-Value tree))
	       (new-left (WB-Map-Tree-Compose (WB-Map-Tree-Node-Left tree) fn))
	       (new-right (WB-Map-Tree-Compose (WB-Map-Tree-Node-Right tree) fn)))
	   (if (Equivalent-Node? key)
	       (Make-WB-Map-Tree-Node
		 (Make-Equivalent-Map (mapcar (lambda (pr)
						(cons (car pr) (funcall fn (cdr pr))))
					      (Equivalent-Node-List key)))
		 val new-left new-right)
	     (Make-WB-Map-Tree-Node key (funcall fn val) new-left new-right))))))


;;; ----------------
;;; Stateful iterator

(defun Make-WB-Map-Tree-Iterator (tree)
  (let ((iter (Make-WB-Map-Tree-Iterator-Internal tree)))
    (lambda (op)
      (ecase op
	(:get (WB-Map-Tree-Iterator-Get iter))
	(:done? (WB-Map-Tree-Iterator-Done? iter))
	(:more? (not (WB-Map-Tree-Iterator-Done? iter)))))))

(defun Make-WB-Map-Tree-Iterator-Internal (tree)
  (WB-Map-Tree-Iterator-Canonicalize
    (Make-WB-Tree-Iterator tree (WB-Map-Tree-Size tree) 2 t)))

(defun WB-Map-Tree-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((sp (svref iter 0))
	  ((node (svref iter sp))
	   (idx (svref iter (1+ sp)))))
      (declare (fixnum sp idx))
      (cond ((null node)
	     (if (= sp 1)
		 (return)
	       (progn
		 (decf sp 2)
		 (setf (svref iter 0) sp)
		 (incf (the fixnum (svref iter (1+ sp)))))))
	    ((consp node)
	     (cond ((< idx (length (the simple-vector (car node))))
		    (return))
		   ((= sp 1)
		    (setf (svref iter 1) nil)
		    (return))
		   (t
		    (decf sp 2)
		    (setf (svref iter 0) sp)
		    (incf (the fixnum (svref iter (1+ sp)))))))
	    ((= idx 0)
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (WB-Map-Tree-Node-Left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx (1+ (Map-Key-Size (WB-Map-Tree-Node-Key node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (WB-Map-Tree-Node-Right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun WB-Map-Tree-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun WB-Map-Tree-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx (svref iter (1+ sp)))))
    (declare (fixnum idx))
    (if (null node)
	(values nil nil nil)
      (progn
	(setf (svref iter (1+ sp)) (1+ idx))
	(if (consp node)
	    (progn
	      (when (= (1+ idx) (length (the simple-vector (car node))))
		(WB-Map-Tree-Iterator-Canonicalize iter))
	      (values (svref (car node) idx) (svref (cdr node) idx) t))
	  (let ((key (WB-Map-Tree-Node-Key node)))
	    (if (Equivalent-Node? key)
		(let ((pr (nth (1- idx) (Equivalent-Node-List key))))
		  (when (= idx (length (Equivalent-Node-List key)))
		    (WB-Map-Tree-Iterator-Canonicalize iter))
		  (values (car pr) (cdr pr) t))
	      (progn
		(when (= idx 1)
		  (WB-Map-Tree-Iterator-Canonicalize iter))
		(values key (WB-Map-Tree-Node-Value node) t)))))))))


;;; ----------------
;;; Functional iterators.  Fun!!!

(defun WB-Map-Tree-Fun-Iter (tree &optional (cont (lambda (op)
						    (ecase op
						      (:first (values nil nil nil))
						      (:empty? t)
						      (:more? nil)))))
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree cont)
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (let ((len (length (the simple-vector (car node)))))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (svref (car node) i) (svref (cdr node) i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     cont)))))
	    (t
	     (walk (WB-Map-Tree-Node-Left node)
		   (let ((key (WB-Map-Tree-Node-Key node)))
		     (if (Equivalent-Node? key)
			 (rlabels (iter (Equivalent-Node-List key))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Map-Tree-Node-Right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values key (WB-Map-Tree-Node-Value node) t))
			   (:rest (walk (WB-Map-Tree-Node-Right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun WB-Map-Tree-Rev-Fun-Iter (tree &optional (cont (lambda (op)
							(ecase op
							  (:first (values nil nil nil))
							  (:empty? t)
							  (:more? nil)))))
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk tree cont)
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((consp node)
	     (rlabels (iter (1- (length (the simple-vector (car node)))))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (lambda (op)
		       (ecase op
			 (:first (values (svref (car node) i) (svref (cdr node) i) t))
			 (:rest (iter (1- i)))
			 (:empty? nil)
			 (:more? t)))
		   cont))))
	    (t
	     (walk (WB-Map-Tree-Node-Right node)
		   (let ((key (WB-Map-Tree-Node-Key node)))
		     (if (Equivalent-Node? key)
			 (rlabels (iter (reverse (Equivalent-Node-List key)))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (WB-Map-Tree-Node-Left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values key (WB-Map-Tree-Node-Value node) t))
			   (:rest (walk (WB-Map-Tree-Node-Left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ================================================================================
;;; Equivalent-Map routines

(defun Equivalent-Map-With (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  (let ((nonnull? key val (Equivalent-Map-Union key1 val1 key2 val2 (fn (_a b) b) key-cmp-fn val-cmp-fn)))
    (declare (ignore nonnull?))
    ;; `nonnull?' must be true given `val-fn'.
    (values key val)))

(defun Equivalent-Map-Union (key1 val1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
Returns one or more new key/value pairs in which the \"2\" pairs override
the \"1\" pairs.  If the result is a single pair, it's returned as two values;
otherwise one value is returned, which is an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn val-cmp-fn val-fn)
	   (dynamic-extent val-fn))
  (flet ((compute-new-val (val1 val2)
	   (if (equal?-cmp val1 val2 val-cmp-fn)
	       (values val1 nil)
	     (n-values 2 (funcall val-fn val1 val2)))))
    (if (Equivalent-Node? key1)
	(if (Equivalent-Node? key2)
	    (let ((alist1 (Equivalent-Node-List key1))
		  (alist2 (copy-list (Equivalent-Node-List key2)))
		  ((result nil)))
	      (declare (type list alist1 alist2))
	      (dolist (pr1 alist1)
		(let ((pr2 (find (car pr1) alist2 :test (equal?-fn key-cmp-fn) :key #'car)))
		  (if pr2
		      (let ((new-val second-val (compute-new-val (cdr pr1) (cdr pr2))))
			(unless (eq second-val ':no-value)
			  (push (cons (car pr1) new-val) result))
			(setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc result alist2))
	      (cond ((null result) nil)
		    ((null (cdr result))
		     (values t (caar result) (cdar result)))
		    (t
		     (values t (Make-Equivalent-Map result)))))
	  (let ((alist1 (Equivalent-Node-List key1))
		((pr1 (find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist1))
	    (if pr1
		(progn
		  (setq alist1 (remove pr1 alist1))
		  (let ((new-val second-val (compute-new-val (cdr pr1) val2)))
		    (if (eq second-val ':no-value)
			(if (null (cdr alist1))
			    (values t (caar alist1) (cdar alist1))
			  (values t (Make-Equivalent-Map alist1)))
		      (values t (Make-Equivalent-Map (cons (cons (car pr1) new-val) alist1))))))
	      (values t (Make-Equivalent-Map (cons (cons key2 val2) alist1))))))
      (if (Equivalent-Node? key2)
	  (let ((alist2 (Equivalent-Node-List key2))
		((pr2 (find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist2))
	    (if pr2
		(progn
		  (setq alist2 (remove pr2 alist2))
		  (let ((new-val second-val (compute-new-val val1 (cdr pr2))))
		    (if (eq second-val ':no-value)
			(if (null (cdr alist2))
			    (values t (caar alist2) (cdar alist2))
			  (values t (Make-Equivalent-Map alist2)))
		      (values t (Make-Equivalent-Map (cons (cons key1 new-val) alist2))))))
	      (values t (Make-Equivalent-Map (cons (cons key1 val1) alist2)))))
	(if (equal?-cmp key1 key2 key-cmp-fn)
	    (let ((new-val second-val (compute-new-val val1 val2)))
	      (and (not (eq second-val ':no-value))
		   (values t key1 new-val)))
	  (values t (Make-Equivalent-Map (list (cons key1 val1) (cons key2 val2)))))))))

(defun Equivalent-Map-Update (key1 val1 key2 value-fn default second-arg key-cmp-fn)
  "`key1' may be either a single value (representing a single key/value pair)
or an `Equivalent-Map' of key/value pairs.  That is, if `key1' is an
`Equivalent-Map', `val1' is ignored.  Returns one or more new key/value pairs
in which the value associated with `key2' is the result of calling `value-fn'
on (a) either the previous such value, if any, or else `default', and (b)
`second-arg'.  If the result is a single pair, it's returned as two values;
otherwise one value is returned, which is an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function value-fn key-cmp-fn))
  (if (Equivalent-Node? key1)
      (let ((alist1 (Equivalent-Node-List key1))
	    ((pr1 (find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	(declare (type list alist1))
	(if pr1
	    (Make-Equivalent-Map (cons (cons key1 (funcall value-fn (cdr pr1) second-arg))
				       (remove pr1 alist1)))
	  (Make-Equivalent-Map (cons (cons key2 (funcall value-fn default second-arg)) alist1))))
    (if (equal?-cmp key1 key2 key-cmp-fn)
	(values key1 (funcall value-fn val1 second-arg))
      (Make-Equivalent-Map (list (cons key1 val1)
				 (cons key2 (funcall value-fn default second-arg)))))))

(defun Equivalent-Map-Intersect (key1 val1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
If the intersection is nonnull, returns two or three values: if it is a
single pair, returns true, the key, and the value; if it is more than one
pair, returns true and an `Equivalent-Map' of the pairs.  If the intersection
is null, returns false."
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn key-cmp-fn val-cmp-fn))
  (flet ((compute-new-val (val1 val2)
	   (if (equal?-cmp val1 val2 val-cmp-fn)
	       (values val1 nil)
	     (n-values 2 (funcall val-fn val1 val2)))))
    (if (Equivalent-Node? key1)
	(if (Equivalent-Node? key2)
	    (let ((alist1 (Equivalent-Node-List key1))
		  (alist2 (Equivalent-Node-List key2))
		  (result nil))
	      (declare (type list alist1 alist2))
	      (dolist (pr1 alist1)
		(let ((pr2 (cl:find (car pr1) alist2 :test (equal?-fn key-cmp-fn) :key #'car)))
		  (when pr2
		    (let ((new-val second-val (compute-new-val (cdr pr1) (cdr pr2))))
		      (unless (eq second-val ':no-value)
			(push (cons (car pr1) new-val) result))))))
	      (and result
		   (if (cdr result)
		       (values t (Make-Equivalent-Map result))
		     (values t (caar result) (cdar result)))))
	  (let ((alist1 (Equivalent-Node-List key1))
		((pr1 (cl:find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist1))
	    (and pr1
		 (let ((new-val second-val (compute-new-val (cdr pr1) val2)))
		   (and (not (eq second-val ':no-value)) (values t (car pr1) new-val))))))
      (if (Equivalent-Node? key2)
	  (let ((alist2 (Equivalent-Node-List key2))
		((pr2 (cl:find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist2))
	    (and pr2
		 (let ((new-val second-val (compute-new-val val1 (cdr pr2))))
		   (and (not (eq second-val ':no-value)) (values t key1 new-val)))))
	(and (equal?-cmp key1 key2 key-cmp-fn)
	     (let ((new-val second-val (compute-new-val val1 val2)))
	       (and (not (eq second-val ':no-value)) (values t key1 new-val))))))))

(defun Equivalent-Map-Difference (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
If the difference is nonnull, returns two or three values: if it is a single
pair, returns true, the key, and the value; if it is more than one pair,
returns true and an `Equivalent-Map' of the pairs.  If the difference is
empty, returns false."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn val-cmp-fn))
  (if (Equivalent-Node? key1)
      (let ((alist1 (Equivalent-Node-List key1)))
	(declare (type list alist1))
	(let ((alist2 (if (Equivalent-Node? key2) (Equivalent-Node-List key2)
			(list (cons key2 val2))))
	      (result nil))
	  (declare (type list alist2))
	  (dolist (pr1 alist1)
	    (let ((pr2 (cl:find (car pr1) alist2 :test (equal?-fn key-cmp-fn) :key #'car)))
	      (when (or (null pr2) (not (equal?-cmp (cdr pr1) (cdr pr2) val-cmp-fn)))
		(push pr1 result))))
	  (and result
	       (if (cdr result)
		   (values t (Make-Equivalent-Map result))
		 (values t (caar result) (cdar result))))))
    (if (Equivalent-Node? key2)
	(let ((alist2 (Equivalent-Node-List key2))
	      ((pr2 (cl:find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	  (declare (type list alist2))
	  (and (or (null pr2) (not (equal?-cmp val1 (cdr pr2) val-cmp-fn)))
	       (values t key1 val1)))
      (and (or (not (equal?-cmp key1 key2 key-cmp-fn)) (not (equal?-cmp val1 val2 val-cmp-fn)))
	   (values t key1 val1)))))

(defun Equivalent-Map-Less (eqvm key key-cmp-fn)
  "Removes the pair associated with `key' from `eqvm', an `Equivalent-Map'.  If
the result is a single pair, it's returned as two values; otherwise one value
is returned, which is an `Equivalent-Map'.  If a pair is removed, its value is
returned as the third value."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (let ((alist (Equivalent-Node-List eqvm))
	((pr (assoc key alist :test (equal?-fn key-cmp-fn)))))
    (if pr
	(let ((result (cl:remove pr alist)))
	  (declare (type list result))
	  (if (= (length result) 1)
	      (values (caar result) (cdar result) (cdr pr))
	    (values (Make-Equivalent-Map result) nil (cdr pr))))
      eqvm)))

(defun Equivalent-Map-Restrict (key val set-elt key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (if (Equivalent-Node? key)
      (let ((alist1 (Equivalent-Node-List key))
	    (mems2 (if (Equivalent-Node? set-elt) (Equivalent-Node-List set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if-not #'(lambda (pr)
					    (member (car pr) mems2 :test (equal?-fn key-cmp-fn)))
					alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (Make-Equivalent-Map result) nil)))))
    (if (Equivalent-Node? set-elt)
	(and (member key (Equivalent-Node-List set-elt) :test (equal?-fn key-cmp-fn))
	     (values t key val))
      (and (equal?-cmp key set-elt key-cmp-fn)
	   (values t key val)))))

(defun Equivalent-Map-Restrict-Not (key val set-elt key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (if (Equivalent-Node? key)
      (let ((alist1 (Equivalent-Node-List key))
	    (mems2 (if (Equivalent-Node? set-elt) (Equivalent-Node-List set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if #'(lambda (pr)
					(member (car pr) mems2 :test (equal?-fn key-cmp-fn)))
				    alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (Make-Equivalent-Map result) nil)))))
    (if (Equivalent-Node? set-elt)
	(and (not (member key (Equivalent-Node-List set-elt) :test (equal?-fn key-cmp-fn)))
	     (values t key val))
      (and (not (equal?-cmp key set-elt key-cmp-fn))
	   (values t key val)))))

(defun Equivalent-Map-Compare (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  "Compares two pairs where the key of either or both may be an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn val-cmp-fn))
  (let ((comp (funcall key-cmp-fn key1 key2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Node? key1)
	  (if (Equivalent-Node? key2)
	      (let ((mems1 (Equivalent-Node-List key1))
		    (mems2 (Equivalent-Node-List key2))
		    ((len1 (length mems1))
		     (len2 (length mems2))))
		(cond ((< len1 len2) ':greater)
		      ((> len1 len2) ':less)
		      ((cl:every #'(lambda (pr1)
				     (let ((pr2 (assoc (car pr1) mems2 :test (equal?-fn key-cmp-fn))))
				       (and pr2 (equal?-cmp (cdr pr1) (cdr pr2) val-cmp-fn))))
				 mems1)
		       ':equal)
		      (t
		       (let ((set1 (reduce (fn (s x) (WB-Set-Tree-With s (cdr x) val-cmp-fn))
					   mems1 :initial-value nil))
			     (set2 (reduce (fn (s x) (WB-Set-Tree-With s (cdr x) val-cmp-fn))
					   mems2 :initial-value nil))
			     ((comp (WB-Set-Tree-Compare set1 set2 val-cmp-fn))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(if (Equivalent-Node? key2)
	    ':greater
	  (let ((val-comp (funcall val-cmp-fn val1 val2)))
	    (if (not (eq val-comp ':equal)) val-comp comp)))))))


;;; ================================================================================
;;; ================================================================================
;;; Sequences

(defconstant WB-Seq-Tree-HT-Threshold 32
  "The size threshold above which we convert the root node to HT form.")
(defconstant WB-Seq-Tree-Non-HT-Threshold 28
  "The size threshold below which we convert an HT root to non-HT form.")

(declaim (inline Make-Raw-WB-Seq-Tree-Node Make-Raw-WB-HT-Seq-Tree))

(deftype WB-Seq-Tree ()
  '(or null WB-Seq-Tree-Node
       simple-string simple-vector))

(deftype symmetric-fixnum ()
  "Excludes `most-negative-fixnum', so negating it doesn't need to check for overflow."
  `(integer ,(- most-positive-fixnum) ,most-positive-fixnum))

;;; Seq tree nodes have no associated value.
(defstruct (WB-Seq-Tree-Node
	     (:constructor Make-Raw-WB-Seq-Tree-Node (raw-size left right))
	     (:predicate WB-Seq-Tree-Node?)
	     (:print-function WB-Seq-Tree-Node-Print))
  ;; If negative, the seq contains only characters; size is the absolute value.
  (Raw-Size 0 :type symmetric-fixnum :read-only t)
  ;; These should never actually be null, since that would make the node itself redundant.
  (Left  nil :type WB-Seq-Tree :read-only t)
  (Right nil :type WB-Seq-Tree :read-only t))

(declaim (inline WB-Seq-Tree-Node-Size))
(defun WB-Seq-Tree-Node-Size (node)
  (abs (WB-Seq-Tree-Node-Raw-Size node)))

;;; "HT" for "Head/Tail".
(defstruct (WB-HT-Seq-Tree
	     (:constructor Make-Raw-WB-HT-Seq-Tree (raw-size head body tail))
	     (:predicate WB-HT-Seq-Tree?)
	     (:print-function WB-HT-Seq-Tree-Print))
  ;; If negative, the seq contains only characters; size is the absolute value.
  (Raw-Size 0 :type symmetric-fixnum :read-only t)
  (Head nil :type (or null simple-vector simple-string))
  (Body nil :type WB-Seq-Tree)
  (Tail nil :type (or null simple-vector simple-string)))

;;; Speeds up `typep' slightly.
(declaim #+sbcl (sb-ext:freeze-type WB-HT-Seq-Tree))

(deflex +Empty-Simple-Vector+ #())

(declaim (ftype (function (WB-Seq-Tree) (values fixnum boolean)) WB-Seq-Tree-Size))
(declaim (inline WB-Seq-Tree-Size))
(defun WB-Seq-Tree-Size (tree)
  "As a second value, returns true iff `tree' contains only characters."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree))
  (cond ((null tree) (values 0 nil))
	((stringp tree) (values (length tree) t))
	((simple-vector-p tree) (values (length tree) nil))
	(t (let ((raw-size (WB-Seq-Tree-Node-Raw-Size tree)))
	     (if (minusp raw-size)
		 (values (- raw-size) t)
	       (values raw-size nil))))))

(declaim (inline WB-HT-Seq-Tree-Size))
(defun WB-HT-Seq-Tree-Size (tree)
  "As a second value, returns true iff `tree' contains only characters."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree))
  (let ((raw-size (WB-HT-Seq-Tree-Raw-Size tree)))
    (if (minusp raw-size)
	(values (- raw-size) t)
      (values raw-size nil))))

(defun Make-WB-Seq-Tree-Node (left right)
  "The low-level constructor for a sequence tree node."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree left right))
  (let ((left-size left-chars? (WB-Seq-Tree-Size left))
	(right-size right-chars? (WB-Seq-Tree-Size right))
	((total-size (+ left-size right-size))))
    (Make-Raw-WB-Seq-Tree-Node (if (and left-chars? right-chars?) (- total-size) total-size)
			       left right)))

(defun Make-WB-HT-Seq-Tree (head body tail)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree head body tail))
  (flet ((split-for-head (tree)
	   (let ((leaf (WB-Seq-Tree-First-Leaf tree)))
	     (values leaf (WB-Seq-Tree-Subseq tree (WB-Seq-Tree-Size leaf) (WB-Seq-Tree-Size tree)))))
	 (split-for-tail (tree)
	   (let ((leaf (WB-Seq-Tree-Last-Leaf tree)))
	     (values leaf (WB-Seq-Tree-Subseq tree 0 (- (WB-Seq-Tree-Size tree) (WB-Seq-Tree-Size leaf)))))))
    (when (null head)
      (setf (values head body) (split-for-head body)))
    (when (null tail)
      (setf (values tail body) (split-for-tail body)))
    (when (WB-Seq-Tree-Node? head)
      (let ((new-head body-left (split-for-head head)))
	(setq head new-head)
	(setq body (WB-Seq-Tree-Concat body-left body))))
    (when (WB-Seq-Tree-Node? tail)
      (let ((new-tail body-right (split-for-tail tail)))
	(setq tail new-tail)
	(setq body (WB-Seq-Tree-Concat body body-right)))))
  (let ((head-size left-chars? (WB-Seq-Tree-Size head))
	(body-size body-chars? (WB-Seq-Tree-Size body))
	(tail-size tail-chars? (WB-Seq-Tree-Size tail))
	((total-size (the fixnum (+ head-size body-size tail-size)))))
    (Make-Raw-WB-HT-Seq-Tree (if (and left-chars? body-chars? tail-chars?) (- total-size) total-size)
			     head body tail)))

(defun WB-Seq-Tree-Node-Print (node stream depth)
  "Print function for `WB-Seq-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#seq-node<~;~D, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (WB-Seq-Tree-Node-Size node)
		    (let ((sub (WB-Seq-Tree-Node-Left node)))
		      (if (and (simple-vector-p sub) (not (stringp sub)))
			  (list t (coerce sub 'list))
			(list nil sub)))
		    (let ((sub (WB-Seq-Tree-Node-Right node)))
		      (if (and (simple-vector-p sub) (not (stringp sub)))
			  (list t (coerce sub 'list))
			(list nil sub)))))
    (format stream "#seq-node<...>")))

(defun WB-HT-Seq-Tree-Print (node stream depth)
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "#seq-ht-tree<~D, ~S, ~S, ~S>"
	      (WB-HT-Seq-Tree-Size node)
	      (WB-HT-Seq-Tree-Head node)
	      (WB-HT-Seq-Tree-Body node)
	      (WB-HT-Seq-Tree-Tail node))
    (format stream "#seq-ht-node<...>")))

(declaim (inline character-type))
(defun character-type (ch)
  (declare (ignorable ch))
  #-FSet-Ext-Strings 'base-char
  #+FSet-Ext-Strings (if (base-char-p ch) 'base-char 'character))

(declaim (inline object-or-char-type))
(defun object-or-char-type (x)
  (cond ((base-char-p x) 'base-char)
	#+FSet-Ext-Strings
	((characterp x) 'character)
	(t 't)))

(declaim (inline string-plus-char-type))
(defun string-plus-char-type (str ch)
  "The element-type of a new string with `ch' being added to the contents of `str'."
  (declare (ignorable str ch))
  #-FSet-Ext-Strings 'base-char
  #+FSet-Ext-Strings (if (and (or (null str) (typep str 'base-string))
			      (typep ch 'base-char))
			 'base-char
		       'character))

(declaim (inline string-plus-string-type))
(defun string-plus-string-type (str1 str2)
  (declare (ignorable str1 str2))
  #-FSet-Ext-Strings 'base-string
  #+FSet-Ext-Strings (if (and (typep str1 'base-string) (typep str2 'base-string))
			 'base-string
		       'string))

(declaim (inline string-subrange-element-type))
(defun string-subrange-element-type (str start end &optional (prev-type 'base-char))
  (declare (optimize (speed 3) (safety 0))
	   (simple-string str)
	   (fixnum start end)
	   (ignorable str start end prev-type))
  #-FSet-Ext-Strings 'base-char
  #+FSet-Ext-Strings
  (if (eq prev-type 'character) prev-type
    (if (typep str 'simple-base-string)
	'base-char
      (dotimes (i (- end start) 'base-char)
	(unless (typep (schar str (+ i start)) 'base-char)
	  (return-from string-subrange-element-type 'character))))))

(declaim (inline vector-subrange-element-type))
(defun vector-subrange-element-type (vec start end &optional (prev-type 'base-char))
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum start end))
  (if (eq prev-type 't) prev-type
    (let ((result prev-type))
      (dotimes (i (- end start))
	(let ((e (svref vec (+ i start))))
	  (cond ((not (characterp e))
		 (return-from vector-subrange-element-type 't))
		#+FSet-Ext-Strings
		((and (eq result 'base-char) (not (typep e 'base-char)))
		 (setq result 'character)))))
      result)))


(defun WB-Seq-Tree-Subscript (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree) nil)
	;; &&& Might be faster to check for a node first.
	((stringp tree)
	 (and (>= idx 0) (< idx (length tree))
	      (values t (schar tree idx))))
	((simple-vector-p tree)
	 (and (>= idx 0) (< idx (length tree))
	      (values t (svref tree idx))))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left))))
	   (if (< idx left-size)
	       (WB-Seq-Tree-Subscript left idx)
	     (WB-Seq-Tree-Subscript (WB-Seq-Tree-Node-Right tree)
				    (- idx left-size)))))))

(defun WB-HT-Seq-Tree-Subscript (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree)
	   (type fixnum idx))
  (let ((whole-size (WB-HT-Seq-Tree-Size tree)))
    (and (>= idx 0) (< idx whole-size)
	 (let ((head (WB-HT-Seq-Tree-Head tree))
	       ((head-size (length-nv head))))
	   (if (< idx head-size)
	       (values t (if (simple-vector-p head) (svref head idx) (schar head idx)))
	     (let ((tail (WB-HT-Seq-Tree-Tail tree))
		   ((tail-start (- whole-size (length-nv tail)))))
	       (if (>= idx tail-start)
		   (values t (if (simple-vector-p tail) (svref tail (- idx tail-start))
			       (schar tail (- idx tail-start))))
		 (WB-Seq-Tree-Subscript (WB-HT-Seq-Tree-Body tree) (- idx head-size)))))))))


;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-Insert (tree idx value &optional not-at-root?)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree)
	 (if (characterp value)
	     (make-string 1 :element-type (character-type value) :initial-element value)
	   (vector value)))
	((stringp tree)
	 (let ((len (length tree)))
	   (if (characterp value)
	       (if (< len WB-Tree-Max-String-Length)
		   (String-Insert tree idx value)
		 (let ((midpt (floor len 2)))
		   (if (<= idx midpt)
		       (Make-WB-Seq-Tree-Node (String-Subseq-Insert tree 0 midpt idx value)
					      (String-Subseq tree midpt))
		     (Make-WB-Seq-Tree-Node (String-Subseq tree 0 midpt)
					    (String-Subseq-Insert tree midpt len (- idx midpt) value)))))
	     (if (< len WB-Tree-Max-Vector-Length)
		 (Vector-Insert-From-String tree idx value)
	       (let ((midpt (floor len 2)))
		 ;; The recursive calls are necessary in the case where the original string is
		 ;; of length 15 or 16 and so the substring we're inserting into is of length 8
		 ;; (the max vector length).
		 (if (<= idx midpt)
		     (Make-WB-Seq-Tree-Node (WB-Seq-Tree-Insert (String-Subseq tree 0 midpt) idx value)
					    (String-Subseq tree midpt))
		   (Make-WB-Seq-Tree-Node (String-Subseq tree 0 midpt)
					  (WB-Seq-Tree-Insert (String-Subseq tree midpt)
							      (- idx midpt) value))))))))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (if (< len WB-Tree-Max-Vector-Length)
	       (Vector-Insert tree idx value)
	     (if (< (* idx 2) len)
		 (Make-WB-Seq-Tree-Node (Vector-Subseq-Maybe-String-Insert tree 0 idx idx value)
					(Vector-Subseq-Maybe-String tree idx len))
	       (Make-WB-Seq-Tree-Node (Vector-Subseq-Maybe-String tree 0 idx)
				      (Vector-Subseq-Maybe-String-Insert tree idx len 0 value))))))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       (right (WB-Seq-Tree-Node-Right tree))
	       ((left-size (WB-Seq-Tree-Size left))
		((new-tree
		   (if (< idx left-size)
		       (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Insert left idx value t) right)
		     (WB-Seq-Tree-Build-Node left (WB-Seq-Tree-Insert right (- idx left-size) value t)))))))
	   (if not-at-root?
	       new-tree
	     (WB-Seq-Tree-Canonicalize-Up new-tree))))))

(defun WB-HT-Seq-Tree-Insert (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree)
	   (type fixnum idx))
  (let ((head (WB-HT-Seq-Tree-Head tree))
	((head-size (length-nv head)))
	(body (WB-HT-Seq-Tree-Body tree))
	(tail (WB-HT-Seq-Tree-Tail tree)))
    (if (<= idx head-size)
	(if (and (characterp value) (or (null head) (stringp head)) (< head-size WB-Tree-Max-String-Length))
	    (Make-WB-HT-Seq-Tree (String-Insert head idx value) body tail)
	  (Make-WB-HT-Seq-Tree (WB-Seq-Tree-Insert head idx value t) body tail))
      (let ((size (WB-HT-Seq-Tree-Size tree))
	    (tail-size (length-nv tail))
	    ((tail-start (- size tail-size))
	     ((idx1 (- idx tail-start)))))
	(declare (fixnum tail-size tail-start idx1))
	(if (>= idx1 0)
	    (if (and (characterp value) (or (null tail) (stringp tail)) (< tail-size WB-Tree-Max-String-Length))
		(Make-WB-HT-Seq-Tree head body (String-Insert tail idx1 value))
	      (Make-WB-HT-Seq-Tree head body (WB-Seq-Tree-Insert tail idx1 value t)))
	  (Make-WB-HT-Seq-Tree head (WB-Seq-Tree-Insert body (- idx head-size) value t) tail))))))

(defun WB-Seq-Tree-Append (tree value)
  (WB-Seq-Tree-Insert tree (WB-Seq-Tree-Size tree) value))

(defun WB-HT-Seq-Tree-Append (tree value)
  (WB-HT-Seq-Tree-Insert tree (WB-HT-Seq-Tree-Size tree) value))

;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-With (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree)
	 (error "This case shouldn't happen"))
	((stringp tree)
	 (if (characterp value)
	     (String-Update tree idx value)
	   (let ((len (length tree)))
	     (if (<= len WB-Tree-Max-Vector-Length)
		 (Vector-Update-From-String tree idx value)
	       (let ((split-point (ash len -1)))
		 (if (< idx split-point)
		     (Make-WB-Seq-Tree-Node (WB-Seq-Tree-With (String-Subseq tree 0 split-point) idx value)
					    (String-Subseq tree split-point))
		   (Make-WB-Seq-Tree-Node (String-Subseq tree 0 split-point)
					  (WB-Seq-Tree-With (String-Subseq tree split-point)
							    (- idx split-point) value))))))))
	((simple-vector-p tree)
	 (Vector-Update-Maybe-To-String tree idx value))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree)))
	   (if (< idx left-size)
	       (Make-WB-Seq-Tree-Node (WB-Seq-Tree-With left idx value) right)
	     (Make-WB-Seq-Tree-Node left (WB-Seq-Tree-With right (- idx left-size) value)))))))

(defun WB-HT-Seq-Tree-With (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree)
	   (type fixnum idx))
  (let ((head (WB-HT-Seq-Tree-Head tree))
	((head-size (length-nv head))))
    (if (< idx head-size)
	(Make-WB-HT-Seq-Tree (WB-Seq-Tree-With head idx value) (WB-HT-Seq-Tree-Body tree)
			     (WB-HT-Seq-Tree-Tail tree))
      (let ((whole-size (WB-HT-Seq-Tree-Size tree))
	    (tail (WB-HT-Seq-Tree-Tail tree))
	    ((tail-size (length-nv tail))
	     ((tail-start (- whole-size tail-size)))))
	(if (>= idx tail-start)
	    (Make-WB-HT-Seq-Tree head (WB-HT-Seq-Tree-Body tree)
				 (WB-Seq-Tree-With tail (the fixnum (- idx tail-start)) value))
	  (Make-WB-HT-Seq-Tree head (WB-Seq-Tree-With (WB-HT-Seq-Tree-Body tree) (- idx head-size) value)
			       tail))))))


(defun Vector-Subseq-Maybe-String (vec start end)
  "Returns a subsequence of `vec' between `start' and `end', or `nil' if
the result would be of zero length.  Careful -- does no bounds checking
on `vec', which it assumes is simple.  Returns a string or base-string if
possible."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum start end))
  (and (> end start)
       (let ((len (- end start))
	     (elt-type (vector-subrange-element-type vec start end)))
	 (declare (fixnum len))
	 (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character t)
	   (let ((result (make-array len :element-type elt-type)))
	     (dotimes (i len)
	       ;; Have to use `aref' here to cover all three cases, but SBCL, at least,
	       ;; still optimizes away the type tests.
	       (setf (aref result i) (svref vec (+ i start))))
	     result)))))

(defun String-Insert (str idx ch)
  "Returns a new string like `str' but with `ch' inserted at `idx'.  Careful --
does no bounds checking on `str', which it assumes is simple.  As a convenience,
accepts `nil' for an empty `str'."
  (declare (optimize (speed 3) (safety 0))
	   (type (or null simple-string) str)
	   (type fixnum idx))
  (let ((len (length-nv str))
	(elt-type (string-plus-char-type str ch)))
    (declare (ignorable elt-type))
    (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string (1+ len) :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str i)))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx))
	    (setf (schar new-str (+ idx i 1))
		  (schar str (+ idx i))))
	  new-str)))))

(defun Vector-Insert-From-String (str idx val)
  "Returns a new vector like `str' (a string) but with `val' inserted at `idx'.
Careful -- does no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-vec (make-array (1+ len)))))
    (declare (fixnum len))
    (split-cases ((typep str 'simple-base-string))
      (dotimes (i idx)
		 (setf (svref new-vec i) (schar str i)))
	       (setf (svref new-vec idx) val)
	       (dotimes (i (- len idx))
		 (setf (svref new-vec (+ idx i 1))
		       (schar str (+ idx i))))
	       new-vec)))

;;; Specialized version should be faster than `cl:subseq'.
(defun String-Subseq (str start &optional (end (length str)))
  "Returns a subsequence of `str' between `start' and `end'.  Careful -- does
no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end))
  (let ((len (- end start))
	(elt-type (string-subrange-element-type str start end)))
    (declare (ignorable elt-type)
	     (fixnum len))
    (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string len :element-type elt-type)))
	  (dotimes (i len)
	    (setf (schar new-str i) (schar str (+ i start))))
	  new-str)))))

(defun String-Subseq-Insert (str start end idx ch)
  "Takes the subsequence of `str' from `start' to `end', then at `idx' within
the result, inserts `ch', returning the new string."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end idx))
  (let ((len (- end start))
	(elt-type (string-subrange-element-type str start end (character-type ch))))
    (declare (ignorable elt-type)
	     (type fixnum len))
    (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string (1+ len) :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str (+ i start))))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx))
	    (setf (schar new-str (+ idx i 1))
		  (schar str (+ idx i start))))
	  new-str)))))

(defun Vector-Subseq-Maybe-String-Insert (vec start end idx val)
  "Returns a subsequence of `vec' between `start' and `end', or `nil' if
the result would be of zero length.  Careful -- does no bounds checking
on `vec', which it assumes is simple.  Returns a string or base-string if
possible."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum start end))
  (let ((len (- end start))
	(elt-type (if (not (characterp val)) 't
		    (vector-subrange-element-type vec start end (character-type val)))))
    (declare (fixnum len))
    (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character t)
      (let ((result (make-array (1+ len) :element-type elt-type)))
	(dotimes (i idx)
	  (setf (aref result i) (svref vec (+ start i))))
	(setf (aref result idx) val)
	(dotimes (i (- len idx))
	  (setf (aref result (+ idx i 1)) (svref vec (+ start idx i))))
	result))))


;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-Remove (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree) nil)
	((stringp tree)
	 (String-Remove-At tree idx))
	((simple-vector-p tree)
	 (Vector-Remove-To-Maybe-String tree idx))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree)))
	   (if (< idx left-size)
	       (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Remove left idx) right)
	     (WB-Seq-Tree-Build-Node left
				     (WB-Seq-Tree-Remove right (- idx left-size))))))))

(defun WB-HT-Seq-Tree-Remove (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree)
	   (type fixnum idx))
  (WB-Seq-Tree-Canonicalize-Down
    (let ((head (WB-HT-Seq-Tree-Head tree))
	  ((head-size (length-nv head))))
      (if (< idx head-size)
	  (Make-WB-HT-Seq-Tree (WB-Seq-Tree-Remove head idx) (WB-HT-Seq-Tree-Body tree)
			       (WB-HT-Seq-Tree-Tail tree))
	(let ((whole-size (WB-HT-Seq-Tree-Size tree))
	      (tail (WB-HT-Seq-Tree-Tail tree))
	      ((tail-start (- whole-size (length-nv tail)))))
	  (if (>= idx tail-start)
	      (Make-WB-HT-Seq-Tree head (WB-HT-Seq-Tree-Body tree)
				   (WB-Seq-Tree-Remove tail (the fixnum (- idx tail-start))))
	    (Make-WB-HT-Seq-Tree head (WB-Seq-Tree-Remove (WB-HT-Seq-Tree-Body tree) (- idx head-size))
				 tail)))))))

(defun WB-Seq-Tree-Canonicalize-Down (tree)
  (declare (type WB-HT-Seq-Tree tree))
  (if (>= (WB-HT-Seq-Tree-Size tree) WB-Seq-Tree-Non-HT-Threshold)
      tree
    (WB-Seq-Tree-Remove-HT tree)))

(defun WB-Seq-Tree-Remove-HT (tree)
  (declare (type WB-HT-Seq-Tree tree))
  (let ((head (WB-HT-Seq-Tree-Head tree))
	(body (WB-HT-Seq-Tree-Body tree))
	(tail (WB-HT-Seq-Tree-Tail tree))
	((hb (if (null head) body
	       (WB-Seq-Tree-Concat head body)))))
    (if (null tail) hb
      (WB-Seq-Tree-Concat hb tail))))

;;; For some use cases, we don't need a balanced normal tree.
(defun WB-Seq-Tree-Canonicalize-Down-Unbalanced (tree)
  (declare (type (or WB-Seq-Tree WB-HT-Seq-Tree) tree))
  (if (WB-HT-Seq-Tree? tree)
      (let ((head (WB-HT-Seq-Tree-Head tree))
	    (body (WB-HT-Seq-Tree-Body tree))
	    (tail (WB-HT-Seq-Tree-Tail tree))
	    ((head+body (if head (Make-WB-Seq-Tree-Node head body)
			  body))))
	(if tail (Make-WB-Seq-Tree-Node head+body tail)
	  head+body))
    tree))

(defun WB-Seq-Tree-Canonicalize-Up (tree)
  "Canonicalizes after insertion by possibly converting to an HT node."
  (declare (type WB-Seq-Tree tree))
  (let ((size chars? (WB-Seq-Tree-Size tree)))
    (if (< size (if chars? (* WB-Seq-Tree-HT-Threshold 2) WB-Seq-Tree-HT-Threshold))
	tree
      (Make-WB-HT-Seq-Tree nil tree nil))))

(defun WB-Seq-Tree-First-Leaf (tree)
  (if (or (stringp tree) (simple-vector-p tree))
      tree
    (WB-Seq-Tree-First-Leaf (WB-Seq-Tree-Node-Left tree))))

(defun WB-Seq-Tree-Last-Leaf (tree)
  (if (or (stringp tree) (simple-vector-p tree))
      tree
    (WB-Seq-Tree-Last-Leaf (WB-Seq-Tree-Node-Right tree))))

(defun String-Remove-At (str idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type (unsigned-byte 24) idx))
  (let ((len (length str))
	((elt-type (string-subrange-element-type str 0 idx (string-subrange-element-type str (1+ idx) len)))))
    (declare (ignorable elt-type))
    (and (> len 1)
	 (split-cases ((typep str 'simple-base-string))
	   (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character)
	     (let ((new-str (make-string (1- len) :element-type elt-type)))
	       (declare (fixnum len))
	       (dotimes (i idx)
		 (setf (schar new-str i) (schar str i)))
	       (dotimes (i (- len idx 1))
		 (setf (schar new-str (+ idx i)) (schar str (+ idx i 1))))
	       new-str))))))

(defun Vector-Remove-To-Maybe-String (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type (unsigned-byte 24) idx))
  (let ((len (length vec))
	((elt-type (vector-subrange-element-type vec 0 idx (vector-subrange-element-type vec (1+ idx) len)))))
    (declare (type fixnum len))
    (and (> len 1)
	 (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character t)
	   (let ((new-vec (make-array (1- len) :element-type elt-type)))
	     (dotimes (i idx)
	       (setf (aref new-vec i) (svref vec i)))
	     (dotimes (i (- len idx 1))
	       (setf (aref new-vec (+ idx i)) (svref vec (+ idx i 1))))
	     new-vec)))))

(defun String-Update (str idx ch)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((elt-type-0 (string-subrange-element-type str 0 idx (character-type ch)))
	 ((elt-type (string-subrange-element-type str (1+ idx) len elt-type-0)))))
    (declare (ignorable elt-type)
	     (fixnum len))
    (split-cases ((typep str 'simple-base-string))
      (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character)
	(let ((new-str (make-string len :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str i)))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx 1))
	    (let ((j (+ i idx 1)))
	      (setf (schar new-str j) (schar str j))))
	  new-str)))))

(defun Vector-Update-From-String (str idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-vec (make-array len))))
    (declare (fixnum len))
    (split-cases ((typep str 'simple-base-string))
      (dotimes (i len)
	(setf (svref new-vec i) (schar str i)))
      (setf (svref new-vec idx) value)
      new-vec)))

(defun Vector-Update-Maybe-To-String (vec idx val)
  "Returns a new vector like `vec' but with `val' at `idx'.  If the elements
of the result are all characters, returns a string instead."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec))
	((elt-type (if (not (typep val 'character))
		       't
		     (let ((vec-elt-type (if (typep val 'base-char) 'base-char 'character)))
		       (dotimes (i len)
			 (unless (= i idx)
			   (let ((e (svref vec i)))
			     (unless (characterp e)
			       (setq vec-elt-type 't)
			       (return))
			     (when (and (eq vec-elt-type 'base-char) (not (typep e 'base-char)))
			       (setq vec-elt-type 'character)))))
		       vec-elt-type)))))
    (declare (fixnum len))
    (split-cases-on-var (elt-type base-char #+FSet-Ext-Strings character t)
      (let ((new-vec (make-array len :element-type elt-type)))
	(dotimes (i idx)
	  (setf (aref new-vec i) (svref vec i)))
	(setf (aref new-vec idx) val)
	(dotimes (i (- len idx 1))
	  (let ((j (+ i idx 1)))
	    (setf (aref new-vec j) (svref vec j))))
	new-vec))))


;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-Subseq (tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum start end))
  (cond ((or (null tree) (>= start end)) nil)
	((and (= start 0) (= end (WB-Seq-Tree-Size tree))) tree)
	((simple-vector-p tree)
	 (Vector-Subseq-Maybe-String tree start end))
	((stringp tree)
	 (String-Subseq tree start end))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       (right (WB-Seq-Tree-Node-Right tree))
	       ((left-size (WB-Seq-Tree-Size left))
		((new-left (WB-Seq-Tree-Subseq left start (min end left-size)))
		 (new-right (WB-Seq-Tree-Subseq right (max 0 (the fixnum (- start left-size)))
						(- end left-size))))))
	   (if (and (eq new-left left) (eq new-right right))
	       tree
	     (WB-Seq-Tree-Concat new-left new-right))))))

(defun WB-HT-Seq-Tree-Subseq (tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-HT-Seq-Tree tree)
	   (type fixnum start end))
  (let ((head (WB-HT-Seq-Tree-Head tree))
	((head-size (length-nv head)))
	(body (WB-HT-Seq-Tree-Body tree))
	(tail (WB-HT-Seq-Tree-Tail tree))
	((body-size (WB-Seq-Tree-Size body))
	 ((tail-start (+ head-size body-size))
	  ((new-head (and (< start head-size) (subseq head start (min head-size end))))
	   (new-body (and (> end head-size) (< start tail-start)
			  (WB-Seq-Tree-Subseq body (the fixnum (max 0 (- start head-size)))
					      (the fixnum (min body-size (- end head-size))))))
	   (new-tail (and (> end tail-start) (subseq tail (max 0 (- start tail-start)) (- end tail-start))))))))
    (if (> (- end start) WB-Seq-Tree-HT-Threshold)
	(Make-WB-HT-Seq-Tree new-head new-body new-tail)
      (WB-Seq-Tree-Concat (WB-Seq-Tree-Concat new-head new-body) new-tail))))


(defun WB-Seq-Tree-Reverse (tree)
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (cl:reverse tree))
	((stringp tree)
	 (cl:reverse tree))
	(t
	 (Make-WB-Seq-Tree-Node (WB-Seq-Tree-Reverse (WB-Seq-Tree-Node-Right tree))
				(WB-Seq-Tree-Reverse (WB-Seq-Tree-Node-Left tree))))))

(defun WB-HT-Seq-Tree-Reverse (tree)
  (declare (type WB-HT-Seq-Tree tree))
  (Make-WB-HT-Seq-Tree (cl:reverse (WB-HT-Seq-Tree-Tail tree))
		       (WB-Seq-Tree-Reverse (WB-HT-Seq-Tree-Body tree))
		       (cl:reverse (WB-HT-Seq-Tree-Head tree))))


;;; ================================================================================
;;; Conversion to/from vectors

;;; An alternative would be for this not to do anything but copy the vector.  Breaking it into
;;; pieces could then be done lazily on update, and skipped altogether if there aren't any updates.
;;; OTOH, this takes only about twice as long as a vector copy.
(defun WB-Seq-Tree-From-Vector (vec)
  (declare (optimize (speed 3) (safety 0))
	   (type vector vec))
  (and (> (length vec) 0)
       ;; We walk the vector left-to-right, breaking it up into nearly-equal-sized
       ;; subsequences, and combining those into a tree.
       (let ((len (length vec))
	     ((ht? (>= len WB-Seq-Tree-HT-Threshold))
	      (npieces (ceiling len (if (stringp vec)
					WB-Tree-Max-String-Length
				      WB-Tree-Max-Vector-Length)))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (base 0)
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((tail (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((right (pop stack))
			(left (pop stack)))
		    (push (Make-WB-Seq-Tree-Node left right) stack)))
		(if ht? (Make-WB-HT-Seq-Tree (cadr stack) (car stack) tail)
		  (car stack))))
	   (declare (type fixnum ipiece base))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap (:result and) #'base-char-p
				      (:arg vector vec :start base :stop (+ base piece-len)))
				(let ((str (make-string piece-len :element-type 'base-char)))
				  (typecase vec
				    (simple-base-string
				      (dotimes (i piece-len)
					(setf (schar str i) (schar vec (+ base i)))))
				    (string
				      (dotimes (i piece-len)
					(setf (schar str i) (schar vec (+ base i)))))
				    (simple-vector
				      (dotimes (i piece-len)
					(setf (schar str i) (svref vec (+ base i)))))
				    (otherwise
				      (dotimes (i piece-len)
					(setf (schar str i) (gen aref vec (+ base i))))))
				  str))
			       #+FSet-Ext-Strings
			       ((gmap (:result and) (fn (x) (typep x 'character))
				      (:arg vector vec :start base :stop (+ base piece-len)))
				(let ((str (make-string piece-len :element-type 'character)))
				  (typecase vec
				    (string
				      (dotimes (i piece-len)
					(setf (schar str i) (schar vec (+ base i)))))
				    (simple-vector
				      (dotimes (i piece-len)
					(setf (schar str i) (svref vec (+ base i)))))
				    (otherwise
				      (dotimes (i piece-len)
					(setf (schar str i) (gen aref vec (+ base i))))))
				  str))
			       ((simple-vector-p vec)
				(Vector-Subseq-Maybe-String vec base (the fixnum (+ base piece-len))))
			       (t
				(cl:subseq vec base (the fixnum (+ base piece-len))))))))
	     (push piece stack)
	     (incf base piece-len)
	     (unless (and ht? (or (zerop ipiece) (= ipiece (1- npieces))))
	       ;; This is the clever part -- by looking at the number of low-order 1s in
	       ;; `ipiece', we know how many nodes to make.
	       (do ((i (if ht? (1- ipiece) ipiece)
		       (ash i -1)))
		   ((evenp i))
		 (declare (type fixnum i))
		 (let ((right (pop stack))
		       (left (pop stack)))
		   (push (Make-WB-Seq-Tree-Node left right) stack)))))))))

(defun WB-Seq-Tree-To-Vector (tree)
  "Accepts HT trees."
  (declare (optimize (speed 3) (safety 0)))
  (if (null tree) #()
    (let ((idx 0))
      (declare (fixnum idx))
      (labels ((fillr (tree result)
		 (cond ((null tree))
		       ((stringp tree)
			(dotimes (i (length (the simple-string tree)))
			  (setf (svref result idx) (schar tree i))
			  (incf idx)))
		       ((simple-vector-p tree)
			(dotimes (i (length tree))
			  (setf (svref result idx) (svref tree i))
			  (incf idx)))
		       (t
			(fillr (WB-Seq-Tree-Node-Left tree) result)
			(fillr (WB-Seq-Tree-Node-Right tree) result)))))
	(if (WB-HT-Seq-Tree? tree)
	    (let ((head (WB-HT-Seq-Tree-Head tree))
		  (body (WB-HT-Seq-Tree-Body tree))
		  (tail (WB-HT-Seq-Tree-Tail tree))
		  (result (make-array (WB-HT-Seq-Tree-Size tree))))
	      (fillr head result)
	      (fillr body result)
	      (fillr tail result)
	      result)
	  (let ((result (make-array (WB-Seq-Tree-Size tree))))
	    (fillr tree result)
	    result))))))

(defun WB-Seq-Tree-To-String (tree &optional (element-type 'character))
  (declare (optimize (speed 3) (safety 0)))
  (unless (member element-type '(character base-char))
    (error "element-type must be `character' or `base-char', not ~S" element-type))
  (if (null tree) ""
    (let ((idx 0))
      (declare (fixnum idx))
      (labels ((fillr (tree result)
		 (cond ((null tree))
		       ((stringp tree)
			(if (eq element-type 'character)
			    (let ((result result))
			      (declare (type (simple-array character (*)) result))
			      (dotimes (i (length (the simple-string tree)))
				(setf (schar result idx) (schar tree i))
				(incf idx)))
			  (let ((result result))
			    (declare (type simple-base-string result))
			    (dotimes (i (length (the simple-string tree)))
			      (let ((c (schar tree i)))
				;; The usual check is suppressed at safety 0.
				(unless (typep c 'base-char)
				  (error 'type-error :datum c :expected-type 'base-char))
				(setf (schar result idx) c))
			      (incf idx)))))
		       (t
			(fillr (WB-Seq-Tree-Node-Left tree) result)
			(fillr (WB-Seq-Tree-Node-Right tree) result)))))
	(if (WB-HT-Seq-Tree? tree)
	    (let ((head (WB-HT-Seq-Tree-Head tree))
		  (body (WB-HT-Seq-Tree-Body tree))
		  (tail (WB-HT-Seq-Tree-Tail tree))
		  (result (make-string (WB-HT-Seq-Tree-Size tree) :element-type element-type)))
	      (fillr head result)
	      (fillr body result)
	      (fillr tail result)
	      result)
	  (let ((result (make-string (WB-Seq-Tree-Size tree) :element-type element-type)))
	    (fillr tree result)
	    result))))))


;;; ================================================================================
;;; Conversion to/from lists

(defun WB-Seq-Tree-From-List (lst)
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (and lst
       (let ((len (length lst))
	     ((ht? (>= len WB-Seq-Tree-HT-Threshold))
	      (npieces (ceiling len WB-Tree-Max-Vector-Length))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((tail (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((right (pop stack))
			(left (pop stack)))
		    (push (Make-WB-Seq-Tree-Node left right) stack)))
		(if ht? (Make-WB-HT-Seq-Tree (cadr stack) (car stack) tail)
		  (car stack))))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap (:result and) (fn (x _y) (typep x 'base-char))
				      (:arg list lst)
				      (:arg index 0 piece-len))
				(let ((str (make-string piece-len :element-type 'base-char)))
				  (dotimes (i piece-len)
				    (setf (schar str i) (pop lst)))
				  str))
			       #+FSet-Ext-Strings
			       ((gmap (:result and) (fn (x _y) (typep x 'character))
				      (:arg list lst)
				      (:arg index 0 piece-len))
				(let ((str (make-string piece-len :element-type 'character)))
				  (dotimes (i piece-len)
				    (setf (char str i) (pop lst)))
				  str))
			       (t
				(let ((vec (make-array piece-len)))
				  (dotimes (i piece-len)
				    (setf (svref vec i) (pop lst)))
				  vec))))))
	     (push piece stack)
	     (unless (and ht? (or (= ipiece 0) (= ipiece (1- npieces))))
	       (do ((i (if ht? (1- ipiece) ipiece) (ash i -1)))
		   ((evenp i))
		 (declare (type fixnum i))
		 (let ((right (pop stack))
		       (left (pop stack)))
		   (push (Make-WB-Seq-Tree-Node left right) stack)))))))))

(defun WB-Seq-Tree-From-List-Reverse (lst)
  "Always returns a `WB-Seq-Tree', not an HT tree."
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (and lst
       (let ((len (length lst))
	     ((ht? (>= len WB-Seq-Tree-HT-Threshold))
	      (npieces (ceiling len WB-Tree-Max-Vector-Length))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((head (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((left (pop stack))
			(right (pop stack)))
		    (push (Make-WB-Seq-Tree-Node left right) stack)))
		(if ht? (Make-WB-HT-Seq-Tree head (car stack) (cadr stack))
		  (car stack))))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap (:result and) (fn (x _y) (typep x 'base-char))
				      (:arg list lst)
				      (:arg index 0 piece-len))
				(let ((str (make-string piece-len :element-type 'base-char)))
				  (dotimes (i piece-len)
				    (setf (schar str (- piece-len i 1)) (pop lst)))
				  str))
			       #+FSet-Ext-Strings
			       ((gmap (:result and) (fn (x _y) (typep x 'character))
				      (:arg list lst)
				      (:arg index 0 piece-len))
				(let ((str (make-string piece-len :element-type 'character)))
				  (dotimes (i piece-len)
				    (setf (char str (- piece-len i 1)) (pop lst)))
				  str))
			       (t
				(let ((vec (make-array piece-len)))
				  (dotimes (i piece-len)
				    (setf (svref vec (- piece-len i 1)) (pop lst)))
				  vec))))))
	     (push piece stack)
	     (unless (and ht? (or (= ipiece 0) (= ipiece (1- npieces))))
	       (do ((i (if ht? (1- ipiece) ipiece) (ash i -1)))
		   ((evenp i))
		 (declare (type fixnum i))
		 (let ((left (pop stack))
		       (right (pop stack)))
		   (push (Make-WB-Seq-Tree-Node left right) stack)))))))))

(defun WB-Seq-Tree-To-List (tree)
  "Accepts HT trees."
  (declare (optimize (speed 3) (safety 0))
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (rlabels (if (WB-HT-Seq-Tree? tree)
	       (build (WB-HT-Seq-Tree-Head tree)
		      (build (WB-HT-Seq-Tree-Body tree)
			     (build (WB-HT-Seq-Tree-Tail tree) nil)))
	     (build tree nil))
    (build (tree result)
      (cond ((null tree) result)
	    ;; SBCL wants the type breakdown to optimize the `coerce'.
	    ((simple-vector-p tree)
	     (nconc (coerce tree 'list) result))
	    #+FSet-Ext-Strings
	    ((typep tree 'base-string)
	     (nconc (coerce tree 'list) result))
	    ((typep tree 'string)
	     (nconc (coerce tree 'list) result))
	    (t
	     ;; We build the list from the right so we don't need an `nreverse'.
	     (build (WB-Seq-Tree-Node-Left tree)
		    (build (WB-Seq-Tree-Node-Right tree) result)))))))

(defun WB-Seq-Tree-Fill (len value)
  (WB-Seq-Tree-From-Iterable (fn (op) (ecase op (:get value))) len))

(defun WB-Seq-Tree-From-Iterable (it len)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum len)
	   (type function it))
  (and (> len 0)
       (let ((ht? (>= len WB-Seq-Tree-HT-Threshold))
	     (npieces (ceiling len WB-Tree-Max-Vector-Length))
	     ((piece-len remainder (floor len npieces))
	      ((tmp-piece (make-array (1+ piece-len))))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((tail (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((right (pop stack))
			(left (pop stack)))
		    (push (Make-WB-Seq-Tree-Node left right) stack)))
		(if ht? (Make-WB-HT-Seq-Tree (cadr stack) (car stack) tail)
		  (car stack))))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len)))
	     (dotimes (i piece-len)
	       (setf (svref tmp-piece i) (funcall it ':get)))
	     (let (((piece (cond ((gmap (:result and) (fn (x _y) (typep x 'base-char))
					(:arg simple-vector tmp-piece)
					(:arg index 0 piece-len))
				  (let ((str (make-string piece-len :element-type 'base-char)))
				    (dotimes (i piece-len)
				      (setf (schar str i) (svref tmp-piece i)))
				    str))
				 #+FSet-Ext-Strings
				 ((gmap (:result and) (fn (x _y) (typep x 'character))
					(:arg simple-vector tmp-piece)
					(:arg index 0 piece-len))
				  (let ((str (make-string piece-len :element-type 'character)))
				    (dotimes (i piece-len)
				      (setf (schar str i) (svref tmp-piece i)))
				    str))
				 (t
				  (subseq tmp-piece 0 piece-len))))))
	       (push piece stack)
	       (unless (and ht? (or (= ipiece 0) (= ipiece (1- npieces))))
		 (do ((i (if ht? (1- ipiece) ipiece) (ash i -1)))
		     ((evenp i))
		   (declare (type fixnum i))
		   (let ((right (pop stack))
			 (left (pop stack)))
		     (push (Make-WB-Seq-Tree-Node left right) stack))))))))))


;;; ================================================================================
;;; Compare

(defun WB-Seq-Tree-Compare (tree1 tree2 cmp-fn &key lexicographic?)
  "Accepts HT trees."
  (declare (optimize (speed 3) (safety 0))
	   (type (or WB-Seq-Tree WB-HT-Seq-Tree) tree1 tree2)
	   (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((size1 (if (WB-HT-Seq-Tree? tree1) (WB-HT-Seq-Tree-Size tree1) (WB-Seq-Tree-Size tree1)))
	  (size2 (if (WB-HT-Seq-Tree? tree2) (WB-HT-Seq-Tree-Size tree2) (WB-Seq-Tree-Size tree2))))
      (cond ((and (not lexicographic?) (< size1 size2)) ':less)
	    ((and (not lexicographic?) (> size1 size2)) ':greater)
	    ((WB-HT-Seq-Tree? tree1)
	     (let ((head1 (WB-HT-Seq-Tree-Head tree1))
		   (head2 body2 tail2
		     (if (WB-HT-Seq-Tree? tree2)
			 (values (WB-HT-Seq-Tree-Head tree2) (WB-HT-Seq-Tree-Body tree2)
				 (WB-HT-Seq-Tree-Tail tree2))
		       (values nil tree2 nil)))
		   ((head1-size (length-nv head1))
		    (head2-size (length-nv head2))
		    ((head-comp (WB-Seq-Tree-Compare-Rng head1 0 head2 0 0 (min head1-size head2-size) cmp-fn)))))
	       ;; Yikes!  Five comparisons: head-head, head-body, body-body, body-tail, tail-tail.
	       ;; (We don't have to handle head-tail because HT nodes aren't used below a certain minimum size.)
	       (if (or (eq head-comp ':less) (eq head-comp ':greater))
		   head-comp
		 (let ((body1 (WB-HT-Seq-Tree-Body tree1))
		       ((head-body-comp
			  (if (< head1-size head2-size)
			      (WB-Seq-Tree-Compare-Rng body1 head1-size head2 0 head1-size head2-size cmp-fn)
			    (WB-Seq-Tree-Compare-Rng head1 0 body2 head2-size head2-size head1-size cmp-fn)))))
		   (if (or (eq head-body-comp ':less) (eq head-body-comp ':greater))
		       head-body-comp
		     (let ((body1-end (the fixnum (+ head1-size (WB-Seq-Tree-Size body1))))
			   (body2-end (the fixnum (+ head2-size (WB-Seq-Tree-Size body2))))
			   ((body-comp
			      (WB-Seq-Tree-Compare-Rng body1 head1-size body2 head2-size
						       (max head1-size head2-size) (min body1-end body2-end) cmp-fn))))
		       (if (or (eq body-comp ':less) (eq body-comp ':greater))
			   body-comp
			 (let ((tail1 (WB-HT-Seq-Tree-Tail tree1))
			       ((body-tail-comp
				  (if (< body1-end body2-end)
				      (WB-Seq-Tree-Compare-Rng tail1 body1-end body2 head2-size
							       body1-end body2-end cmp-fn)
				    (WB-Seq-Tree-Compare-Rng body1 head1-size tail2 body2-end
							     body2-end body1-end cmp-fn)))))
			   (if (or (eq body-tail-comp ':less) (eq body-tail-comp ':greater))
			       body-tail-comp
			     (let ((tail-comp (WB-Seq-Tree-Compare-Rng tail1 body1-end tail2 body2-end
								       (max body1-end body2-end) (min size1 size2)
								       cmp-fn)))
			       (if (or (eq tail-comp ':less) (eq tail-comp ':greater))
				   tail-comp
				 (cond ((< size1 size2) ':less)    ; possible in lexicographic mode
				       ((> size1 size2) ':greater)
				       (t
					(if (or (eq head-comp ':unequal) (eq head-body-comp ':unequal)
						(eq body-comp ':unequal) (eq body-tail-comp ':unequal)
						(eq tail-comp ':unequal))
					    ':unequal
					  ':equal))))))))))))))
	    ((WB-HT-Seq-Tree? tree2)
	     (invert-comparison (WB-Seq-Tree-Compare tree2 tree1 cmp-fn :lexicographic? lexicographic?)))
	    (lexicographic?
	     (let ((comp (WB-Seq-Tree-Compare-Rng tree1 0 tree2 0 0 (min size1 size2) cmp-fn)))
	       (cond ((or (eq comp ':less) (eq comp ':greater))
		      comp)
		     ((< size1 size2) ':less)
		     ((> size1 size2) ':greater)
		     (t comp))))
	    (t (WB-Seq-Tree-Compare-Rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun WB-Seq-Tree-Compare-Lexicographically (tree1 tree2 cmp-fn)
  (WB-Seq-Tree-Compare tree1 tree2 cmp-fn :lexicographic? t))

(defun WB-Seq-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi cmp-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi)
	   (type function cmp-fn))
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related seq optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (vectorp tree1) (vectorp tree2))
	 (let ((result ':equal)
	       (start1 (- lo base1))
	       (start2 (- lo base2)))
	   (declare (fixnum start1 start2))
	   (if (and (eq cmp-fn #'compare) (simple-string-p tree1) (simple-string-p tree2))
	       (split-string-cases (tree1)
		 (split-string-cases (tree2)
		   (dotimes (i (the fixnum (- hi lo)) ':equal)
		     (let ((c1 (schar tree1 (+ i start1)))
			   (c2 (schar tree2 (+ i start2))))
		       (cond ((char< c1 c2) (return ':less))
			     ((char> c1 c2) (return ':greater)))))))
	     (e-split-cases ((simple-vector-p tree1) (typep tree1 'simple-base-string)
			     #+FSet-Ext-Strings (typep tree1 '(simple-array character (*))))
	       (e-split-cases ((simple-vector-p tree2) (typep tree2 'simple-base-string)
			       #+FSet-Ext-Strings (typep tree2 '(simple-array character (*))))
		 (dotimes (i (the fixnum (- hi lo)) result)
		   (let ((comp (funcall cmp-fn (aref tree1 (+ i start1)) (aref tree2 (+ i start2)))))
		     (cond ((or (eq comp ':less) (eq comp ':greater))
			    (return comp))
			   ((eq comp ':unequal)
			    (setq result comp))))))))))
	((or (stringp tree1) (simple-vector-p tree1))
	 (invert-comparison (WB-Seq-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (WB-Seq-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Seq-Tree-Size left1)))
		((node-rank (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (WB-Seq-Tree-Trim left1 base1 lo node-rank))
		  (tree2a base2a (WB-Seq-Tree-Trim tree2 base2 lo node-rank))
		  ((left-comp (if (not (less-than?-cmp lo node-rank cmp-fn))
				  ':equal
				(WB-Seq-Tree-Compare-Rng left1a base1a tree2a base2a
							 lo (min hi node-rank) cmp-fn))))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((right1a base1a (WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Right tree1)
						     node-rank node-rank hi))
		   (tree2a base2a (WB-Seq-Tree-Trim tree2 base2 node-rank hi))
		   ((right-comp (if (not (less-than?-cmp node-rank hi cmp-fn))
				    ':equal
				  (WB-Seq-Tree-Compare-Rng right1a base1a tree2a base2a
							   (max lo node-rank) hi cmp-fn)))))
	       (if (not (eq right-comp ':equal))
		   right-comp
		 left-comp)))))))


;;; ================================================================================
;;; Support routines for the above (sequences)

(defun WB-Seq-Tree-Concat (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree left right))
  (cond ((null left) right)
	((null right) left)
	((and (WB-Seq-Tree-Node? left)
	      (> (WB-Seq-Tree-Size left)
		 (the fixnum (* (WB-Seq-Tree-Size right) WB-Tree-Balance-Factor))))
	 (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Node-Left left)
				 (WB-Seq-Tree-Concat (WB-Seq-Tree-Node-Right left)
						     right)))
	((and (WB-Seq-Tree-Node? right)
	      (> (WB-Seq-Tree-Size right)
		 (the fixnum (* (WB-Seq-Tree-Size left) WB-Tree-Balance-Factor))))
	 (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Concat left (WB-Seq-Tree-Node-Left right))
				 (WB-Seq-Tree-Node-Right right)))
	(t
	 (WB-Seq-Tree-Build-Node left right))))

(defun WB-HT-Seq-Tree-Concat (left right)
  "The arguments can be HT trees, but do not have to be.  The result also
may or may not be an HT tree."
  (declare (optimize (speed 3) (safety 0)))
  (if (WB-HT-Seq-Tree? left)
      (if (WB-HT-Seq-Tree? right)
	  (Make-WB-HT-Seq-Tree (WB-HT-Seq-Tree-Head left)
			       (WB-Seq-Tree-Concat (WB-Seq-Tree-Concat (WB-HT-Seq-Tree-Body left)
								       (WB-HT-Seq-Tree-Tail left))
						   (WB-Seq-Tree-Concat (WB-HT-Seq-Tree-Head right)
								       (WB-HT-Seq-Tree-Body right)))
			       (WB-HT-Seq-Tree-Tail right))
	(Make-WB-HT-Seq-Tree (WB-HT-Seq-Tree-Head left)
			     (WB-Seq-Tree-Concat (WB-Seq-Tree-Concat (WB-HT-Seq-Tree-Body left)
								     (WB-HT-Seq-Tree-Tail left))
						 right)
			     nil))
    (if (WB-HT-Seq-Tree? right)
	(Make-WB-HT-Seq-Tree nil (WB-Seq-Tree-Concat left (WB-Seq-Tree-Concat (WB-HT-Seq-Tree-Head right)
									      (WB-HT-Seq-Tree-Body right)))
			     (WB-HT-Seq-Tree-Tail right))
      (WB-Seq-Tree-Canonicalize-Up (WB-Seq-Tree-Concat left right)))))


(defun WB-Seq-Tree-Build-Node (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree left right))
  (cond ((null left) right)
	((null right) left)
	((and (stringp left) (stringp right)
	      (<= (+ (length-nv left) (length-nv right)) WB-Tree-Max-String-Length))
	 (concatenate (string-plus-string-type left right) left right))
	((and (or (stringp left) (simple-vector-p left))
	      (or (stringp right) (simple-vector-p right)))
	 (if (<= (+ (length-nv left) (length-nv right)) WB-Tree-Max-Vector-Length)
	     (concatenate 'simple-vector left right)
	   (Make-WB-Seq-Tree-Node left right)))
	(t
	 (let ((sizl (WB-Seq-Tree-Size left))
	       (sizr (WB-Seq-Tree-Size right)))
	   (cond ((and (WB-Seq-Tree-Node? left)
		       (> sizl (the fixnum (* sizr WB-Tree-Balance-Factor))))
		  (let ((ll (WB-Seq-Tree-Node-Left left))
			(rl (WB-Seq-Tree-Node-Right left)))
		    (if (or (null rl) (simple-string-p rl) (simple-vector-p rl)
			    (<= (WB-Seq-Tree-Size rl) (WB-Seq-Tree-Size ll)))
			(Make-WB-Seq-Tree-Node ll (WB-Seq-Tree-Build-Node rl right))
		      (Make-WB-Seq-Tree-Node (WB-Seq-Tree-Build-Node
					       ll (WB-Seq-Tree-Node-Left rl))
					     (WB-Seq-Tree-Build-Node
					       (WB-Seq-Tree-Node-Right rl) right)))))
		 ((and (WB-Seq-Tree-Node? right)
		       (> sizr (the fixnum (* sizl WB-Tree-Balance-Factor))))
		  (let ((lr (WB-Seq-Tree-Node-Left right))
			(rr (WB-Seq-Tree-Node-Right right)))
		    (if (or (null lr) (simple-string-p lr) (simple-vector-p lr)
			    (<= (WB-Seq-Tree-Size lr) (WB-Seq-Tree-Size rr)))
			(Make-WB-Seq-Tree-Node (WB-Seq-Tree-Build-Node left lr)
					       rr)
		      (Make-WB-Seq-Tree-Node (WB-Seq-Tree-Build-Node
					       left (WB-Seq-Tree-Node-Left lr))
					     (WB-Seq-Tree-Build-Node
					       (WB-Seq-Tree-Node-Right lr) rr)))))
		 (t
		  (Make-WB-Seq-Tree-Node left right)))))))

(defun WB-Seq-Tree-Trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum base lo hi))
  (assert (not (WB-HT-Seq-Tree? tree)))
  (if (or (null tree) (stringp tree) (simple-vector-p tree))
      (values tree base)
    (let ((node-rank (the fixnum (+ base (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Left tree))))))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Left tree) base lo hi))
	(WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Right tree) node-rank lo hi)))))


;;; ================================================================================
;;; Iteration primitives

(defmacro Do-WB-Seq-Tree-Members ((var tree-form &optional value-form) &body body)
  (let ((body-fn (gensymx #:body-))
	(walk-fn (gensymx #:walk-)))
    `(block nil
       (labels ((,body-fn (,var) . ,body)
		(,walk-fn (tree)
		  (cond ((null tree))
			((stringp tree)
			 (dotimes (i (length (the simple-string tree)))
			   (,body-fn (schar tree i))))
			((simple-vector-p tree)
			 (dotimes (i (length tree))
			   (,body-fn (svref tree i))))
			(t
			 (,walk-fn (WB-Seq-Tree-Node-Left tree))
			 (,walk-fn (WB-Seq-Tree-Node-Right tree))))))
	 (let ((tree ,tree-form))
	   (if (WB-HT-Seq-Tree? tree)
	       (progn
		 (,walk-fn (WB-HT-Seq-Tree-Head tree))
		 (,walk-fn (WB-HT-Seq-Tree-Body tree))
		 (,walk-fn (WB-HT-Seq-Tree-Tail tree)))
	     (,walk-fn tree))))
       ,value-form)))

(defmacro Do-WB-Seq-Tree-Members-Gen ((var tree-form start-form end-form from-end-form
				       &optional value-form)
				      &body body)
  (let ((body-fn (gensymx #:body-))
	(walk-fn (gensymx #:walk-))
	(start-var (gensymx #:start-))
	(end-var (gensymx #:end-))
	(from-end-var (gensymx #:from-end-)))
    `(block nil		; for `return' inside `body'
       (let ((,start-var ,start-form)
	     (,end-var ,end-form)
	     (,from-end-var ,from-end-form))
	 (declare (type fixnum ,start-var ,end-var))
	 (labels ((,body-fn (,var) . ,body)
		  (,walk-fn (tree start end)
		     (declare (type fixnum start end))
		     (cond ((null tree))
			   ((stringp tree)
			    (if (not ,from-end-var)
				(do ((i start (1+ i)))
				    ((>= i end))
				  (,body-fn (schar tree i)))
			      (do ((i (1- end) (1- i)))
				  ((< i start))
				(,body-fn (schar tree i)))))
			   ((simple-vector-p tree)
			    (if (not ,from-end-var)
				(do ((i start (1+ i)))
				    ((>= i end))
				  (,body-fn (svref tree i)))
			      (do ((i (1- end) (1- i)))
				  ((< i start))
				(,body-fn (svref tree i)))))
			   (t
			    (let ((left (WB-Seq-Tree-Node-Left tree))
				  ((left-size (WB-Seq-Tree-Size left)))
				  (right (WB-Seq-Tree-Node-Right tree)))
			      (if (< start left-size)
				  (if (> end left-size)
				      (if (not ,from-end-var)
					  (progn
					    (,walk-fn left start left-size)
					    (,walk-fn right 0 (- end left-size)))
					(progn
					  (,walk-fn right 0 (- end left-size))
					  (,walk-fn left start left-size)))
				    (,walk-fn left start end))
				(,walk-fn right (- start left-size)
					  (- end left-size))))))))
	   (let ((tree ,tree-form)
		 (,start-var (max 0 ,start-var)))
	     (if (WB-HT-Seq-Tree? tree)
		 (let ((,end-var (min ,end-var (WB-HT-Seq-Tree-Size tree))))
		   (when (< ,start-var ,end-var)
		     (let ((head (WB-HT-Seq-Tree-Head tree))
			   (body (WB-HT-Seq-Tree-Body tree))
			   (tail (WB-HT-Seq-Tree-Tail tree))
			   ((head-size (length-nv head))
			    ((body-end (+ head-size (WB-Seq-Tree-Size body))))))
		       (flet ((walk-head ()
				(when (< ,start-var head-size)
				  (,walk-fn head ,start-var (min head-size ,end-var))))
			      (walk-body ()
				(when (and (< ,start-var body-end) (> ,end-var head-size))
				  (,walk-fn body (max 0 (- ,start-var head-size))
					    (- (min body-end ,end-var) head-size))))
			      (walk-tail ()
				(when (> ,end-var body-end)
				  (,walk-fn tail (max 0 (- ,start-var body-end)) (- ,end-var body-end)))))
			 (if ,from-end-var
			     (progn (walk-tail) (walk-body) (walk-head))
			   (progn (walk-head) (walk-body) (walk-tail)))))))
	       (,walk-fn tree ,start-var (min ,end-var (WB-Seq-Tree-Size tree)))))))
       ,value-form)))

(defmacro Do-WB-Seq-Tree-Leaves ((var tree-form &optional value-form) &body body)
  "Iterates over the leaves of the seq tree.  Each leaf is either a
`simple-vector' or a `simple-string'."
  (let ((body-fn (gensymx #:body-))
	(walk-fn (gensymx #:walk-)))
    `(block nil
       (labels ((,body-fn (,var) . ,body)
		(,walk-fn (tree)
		  (cond ((null tree))
			((or (simple-string-p tree) (simple-vector-p tree))
			 (,body-fn tree))
			(t
			 (,walk-fn (WB-Seq-Tree-Node-Left tree))
			 (,walk-fn (WB-Seq-Tree-Node-Right tree))))))
	 (let ((tree ,tree-form))
	   (if (WB-HT-Seq-Tree? tree)
	       (progn
		 (,body-fn (WB-HT-Seq-Tree-Head tree))
		 (,walk-fn (WB-HT-Seq-Tree-Body tree))
		 (,body-fn (WB-HT-Seq-Tree-Tail tree)))
	     (,walk-fn tree))))
       ,value-form)))


(defun WB-Seq-Tree-Image (tree fn)
  (declare (optimize (speed 3) (safety 1))
	   (type function fn))
  (cond ((null tree) nil)
	((stringp tree)
	 (let ((len (length (the simple-string tree)))
	       ((result (make-array len))))
	   (split-cases ((typep tree 'base-string))
	     (dotimes (i len)
	       (setf (svref result i) (funcall fn (schar tree i)))))
	   (cond ((gmap :and #'characterp (:arg simple-vector result))
		  (Vector-Subseq-Maybe-String result 0 len))
		 ((> len WB-Tree-Max-Vector-Length)
		  (WB-Seq-Tree-From-Vector result))
		 (t result))))
	((simple-vector-p tree)
	 (let ((len (length tree))
	       ((result (make-array len))))
	   (dotimes (i len)
	     (setf (svref result i) (funcall fn (svref tree i))))
	   (if (gmap :and #'characterp (:arg simple-vector result))
	       (Vector-Subseq-Maybe-String result 0 len)
	     result)))
	(t
	 (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Image (WB-Seq-Tree-Node-Left tree) fn)
				 (WB-Seq-Tree-Image (WB-Seq-Tree-Node-Right tree) fn)))))

(defun WB-HT-Seq-Tree-Image (tree fn)
  (if (WB-HT-Seq-Tree? tree)
      (Make-WB-HT-Seq-Tree (WB-Seq-Tree-Image (WB-HT-Seq-Tree-Head tree) fn)
			   (WB-Seq-Tree-Image (WB-HT-Seq-Tree-Body tree) fn)
			   (WB-Seq-Tree-Image (WB-HT-Seq-Tree-Tail tree) fn))
    (WB-Seq-Tree-Image tree fn)))


;;; ----------------
;;; Stateful iterator

(defun Make-WB-Seq-Tree-Iterator (tree &optional start end)
  (let ((tree (WB-Seq-Tree-Canonicalize-Down-Unbalanced tree))
	((iter (Make-WB-Seq-Tree-Iterator-Internal tree start end))))
    (lambda (op)
      (ecase op
	(:get (WB-Seq-Tree-Iterator-Get iter))
	(:done? (WB-Seq-Tree-Iterator-Done? iter))
	(:more? (not (WB-Seq-Tree-Iterator-Done? iter)))
	(:reset (WB-Seq-Tree-Iterator-Initialize iter tree start end))))))

(defun Make-WB-Seq-Tree-Iterator-Internal (tree &optional start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (WB-Seq-Tree-Size tree))
	((stack (Make-WB-Tree-Iterator tree tree-size 2 nil))
	 ((iter (cons stack nil)))))
    (WB-Seq-Tree-Iterator-Initialize iter tree start end)))

(defun WB-Seq-Tree-Iterator-Initialize (iter tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (WB-Seq-Tree-Size tree))
	(stack (car iter)))
    (setq start (if start (max 0 start) 0))
    (setq end (max start (if end (min tree-size end) tree-size)))
    (unless (= start end)
      (let ((node tree)
	    (tmp-start start)
	    (sp -1))
	(declare (fixnum tmp-start sp))
	(loop
	  (if (or (stringp node) (simple-vector-p node))
	      (progn
		(incf sp 2)
		(setf (svref stack sp) node)
		(setf (svref stack (1+ sp)) tmp-start)
		(setf (svref stack 0) sp)
		(return))
	    (let ((left (WB-Seq-Tree-Node-Left node))
		  ((left-size (WB-Seq-Tree-Size left))))
	      (if (< tmp-start left-size)
		  (progn
		    (incf sp 2)
		    (setf (svref stack sp) node)
		    (setf (svref stack (1+ sp)) 0)
		    (setq node left))
		(progn
		  (decf tmp-start left-size)
		  (setq node (WB-Seq-Tree-Node-Right node)))))))))
    (setf (cdr iter) (- end start))
    iter))

(defun WB-Seq-Tree-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (unless (zerop (the fixnum (cdr iter)))
    (let ((stack (car iter)))
      (loop
	(let ((sp (svref stack 0))
	      ((node (svref stack sp))
	       (idx (svref stack (1+ sp)))))
	  (declare (fixnum sp idx))
	  (cond ((or (simple-string-p node) (simple-vector-p node))
		 (if (or (< idx (length node)) (= sp 1))
		     (return)
		   (progn
		     (decf sp 2)
		     (setf (svref stack 0) sp)
		     (incf (the fixnum (svref stack (1+ sp)))))))
		((= idx 0)
		 (unless (< (+ sp 3) (length stack))
		   (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
		 (incf sp 2)
		 (setf (svref stack 0) sp)
		 (setf (svref stack sp) (WB-Seq-Tree-Node-Left node))
		 (setf (svref stack (1+ sp)) 0))
		(t
		 ;; Tail recursion
		 (setf (svref stack sp) (WB-Seq-Tree-Node-Right node))
		 (setf (svref stack (1+ sp)) 0))))))))

(defun WB-Seq-Tree-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (zerop (the fixnum (cdr iter))))

(defun WB-Seq-Tree-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (if (zerop (the fixnum (cdr iter)))
      (values nil nil)
    (let ((stack (car iter))
	  ((sp (svref stack 0))
	   ((node (svref stack sp))
	    (idx (svref stack (1+ sp))))))
      (declare (fixnum idx)
	       (type (simple-array * (*)) node))
      (decf (the fixnum (cdr iter)))
      (when (= (incf (the fixnum (svref stack (1+ sp)))) (length node))
	(WB-Seq-Tree-Iterator-Canonicalize iter))
      (values (if (simple-string-p node) (schar node idx) (svref node idx)) t))))


(defun Make-WB-Seq-Tree-Rev-Iterator (tree &optional start end)
  (let ((tree (WB-Seq-Tree-Canonicalize-Down-Unbalanced tree))
	((iter (Make-WB-Seq-Tree-Rev-Iterator-Internal tree start end))))
    (lambda (op)
      (ecase op
	(:get (WB-Seq-Tree-Rev-Iterator-Get iter))
	(:done? (WB-Seq-Tree-Rev-Iterator-Done? iter))
	(:more? (not (WB-Seq-Tree-Rev-Iterator-Done? iter)))
	(:reset (WB-Seq-Tree-Rev-Iterator-Initialize iter tree start end))))))

(defun Make-WB-Seq-Tree-Rev-Iterator-Internal (tree &optional start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (WB-Seq-Tree-Size tree))
	((stack (Make-WB-Tree-Iterator tree tree-size 2 nil))
	 ((iter (cons stack nil)))))
    (WB-Seq-Tree-Rev-Iterator-Initialize iter tree start end)))

(defun WB-Seq-Tree-Rev-Iterator-Initialize (iter tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (WB-Seq-Tree-Size tree))
	(stack (car iter)))
    ;; We flip the coordinate system so `start' and `end' are nonnegative distances from the right end.
    (setq end (if end (max 0 (- tree-size end)) 0))
    (setq start (max end (if start (- tree-size (max 0 start)) tree-size)))
    (unless (= start end)
      (let ((node tree)
	    (tmp-end end)
	    (sp -1))
	(declare (fixnum tmp-end sp))
	(loop
	  (when (or (stringp node) (simple-vector-p node))
	    (incf sp 2)
	    (setf (svref stack sp) node)
	    (setf (svref stack (1+ sp)) (- (length node) tmp-end))
	    (setf (svref stack 0) sp)
	    (return))
	  (let ((right (WB-Seq-Tree-Node-Right node))
		((right-size (WB-Seq-Tree-Size right))))
	    (if (< tmp-end right-size)
		(progn
		  (incf sp 2)
		  (setf (svref stack sp) node)
		  (setf (svref stack (1+ sp)) 1)
		  (setq node right))
	      (progn
		(decf tmp-end right-size)
		(setq node (WB-Seq-Tree-Node-Left node))))))))
    (setf (cdr iter) (- start end))
    iter))

(defun WB-Seq-Tree-Rev-Iterator-Canonicalize (iter)
  (declare (optimize (speed 3) (safety 0)))
  (unless (zerop (the fixnum (cdr iter)))
    (let ((stack (car iter)))
      (loop
	(let ((sp (svref stack 0))
	      ((node (svref stack sp))
	       (idx (svref stack (1+ sp)))))
	  (declare (fixnum sp)
		   (type (or null fixnum) idx))
	  (cond ((or (simple-string-p node) (simple-vector-p node))
		 (when (null idx)
		   (setf (svref stack (1+ sp)) (length node))
		   (return))
		 (when (or (> idx 0) (= sp 1))
		   (return))
		 (decf sp 2)
		 (setf (svref stack 0) sp)
		 (decf (the fixnum (svref stack (1+ sp)))))
		((null idx)
		 (setf (svref stack (1+ sp)) 1)
		 (unless (< (+ sp 3) (length stack))
		   (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
		 (incf sp 2)
		 (setf (svref stack 0) sp)
		 (setf (svref stack sp) (WB-Seq-Tree-Node-Right node))
		 (setf (svref stack (1+ sp)) nil))
		(t
		 ;; Tail recursion
		 (setf (svref stack sp) (WB-Seq-Tree-Node-Left node))
		 (setf (svref stack (1+ sp)) nil))))))))

(defun WB-Seq-Tree-Rev-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (zerop (the fixnum (cdr iter))))

(defun WB-Seq-Tree-Rev-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (if (zerop (the fixnum (cdr iter)))
      (values nil nil)
    (let ((stack (car iter))
	  ((sp (svref stack 0))
	   ((node (svref stack sp))
	    (idx (svref stack (1+ sp))))))
      (declare (fixnum idx))
      (decf (the fixnum (cdr iter)))
      (decf idx)
      (setf (svref stack (1+ sp)) idx)
      (when (zerop idx)
	(WB-Seq-Tree-Rev-Iterator-Canonicalize iter))
      (values (if (simple-string-p node) (schar node idx) (svref node idx)) t))))

;;; ----------------
;;; Functional iterators.  Fun!!!

(defun WB-Seq-Tree-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk (WB-Seq-Tree-Canonicalize-Down-Unbalanced tree)
		 (lambda (op)
		   (ecase op
		     (:first (values nil nil))
		     (:empty? t)
		     (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((simple-string-p node)
	     (let ((len (length node)))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (schar node i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     cont)))))
	    ((simple-vector-p node)
	     (let ((len (length node)))
	       (rlabels (iter 0)
		 (iter (i)
		   (declare (fixnum i))
		   (if (< i len)
		       (lambda (op)
			 (ecase op
			   (:first (values (svref node i) t))
			   (:rest (iter (1+ i)))
			   (:empty? nil)
			   (:more? t)))
		     cont)))))
	    (t
	     (walk (WB-Seq-Tree-Node-Left node)
		   (walk (WB-Seq-Tree-Node-Right node) cont)))))))

(defun WB-Seq-Tree-Rev-Fun-Iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk (WB-Seq-Tree-Canonicalize-Down-Unbalanced tree)
		 (lambda (op)
		   (ecase op
		     (:first (values nil nil))
		     (:empty? t)
		     (:more? nil))))
    (walk (node cont)
      (cond ((null node)
	     cont)
	    ((simple-string-p node)
	     (rlabels (iter (1- (length node)))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (lambda (op)
		       (ecase op
			 (:first (values (schar node i) t))
			 (:rest (iter (1- i)))
			 (:empty? nil)
			 (:more? t)))
		   cont))))
	    ((simple-vector-p node)
	     (rlabels (iter (1- (length node)))
	       (iter (i)
		 (declare (fixnum i))
		 (if (>= i 0)
		     (lambda (op)
		       (ecase op
			 (:first (values (svref node i) t))
			 (:rest (iter (1- i)))
			 (:empty? nil)
			 (:more? t)))
		   cont))))
	    (t
	     (walk (WB-Seq-Tree-Node-Right node)
		   (walk (WB-Seq-Tree-Node-Left node) cont)))))))


;;; ================================================================================
;;; Verifier

(defun WB-Seq-Tree-Verify (tree)
  (declare (optimize (debug 3)))
  (rlabels (cond ((null tree) t)
		 ((WB-HT-Seq-Tree? tree)
		  (and (walk-leaf (WB-HT-Seq-Tree-Head tree))
		       (walk (WB-HT-Seq-Tree-Body tree))
		       (walk-leaf (WB-HT-Seq-Tree-Tail tree))))
		 (t (walk tree)))
    (walk (tree)
      (cond ((null tree) nil) ; nil should appear only if the tree is empty
	    ((vectorp tree) (walk-leaf tree))
	    (t
	     (let ((sizl lchars? (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Left tree)))
		   (sizr rchars? (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Right tree))))
	       (and (= (WB-Seq-Tree-Node-Size tree) (+ sizl sizr))
		    (eqv (and lchars? rchars?) (minusp (WB-Seq-Tree-Node-Raw-Size tree)))
		    ;; We suppress the balance test if one side is smaller than 8
		    ;; here, instead of 4, because of `WB-Tree-Max-String-Length',
		    ;; which makes the trees appear less balanced.
		    (or (<= sizr 8)
			(<= sizl (* sizr WB-Tree-Balance-Factor)))
		    (or (<= sizl 8)
			(<= sizr (* sizl WB-Tree-Balance-Factor)))
		    (walk (WB-Seq-Tree-Node-Left tree))
		    (walk (WB-Seq-Tree-Node-Right tree)))))))
    (walk-leaf (leaf)
      (let ((correct-type (reduce (fn (str-type c)
				    (cond ((and (eq str-type 'base-string) (typep c 'base-char))
					   'base-string)
					  ((and (member str-type '(base-string string))
						(typep c 'character))
					   'string)
					  (t 'vector)))
				  leaf :initial-value 'base-string)))
	(cond ((stringp leaf)
	       (or (and (<= (length leaf) WB-Tree-Max-String-Length)
			(or (eq correct-type 'string)
			    (and (eq correct-type 'base-string) (typep leaf 'base-string))))
		   (error "String leaf failed check")))
	      ((simple-vector-p leaf)
	       (or (and (<= (length leaf) WB-Tree-Max-Vector-Length)
			(eq correct-type 'vector))
		   (error "Vector leaf failed check"))))))))

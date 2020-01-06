;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: wb-trees.lisp
;;; Contents: Weight-balanced binary tree implementation for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2012 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
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


;;; ================================================================================
;;; ================================================================================
;;; Weight-balanced trees

;;; The threshold length above which tree nodes will be built.
(declaim (type fixnum *WB-Tree-Max-Vector-Length*))
(defparameter *WB-Tree-Max-Vector-Length* 8)

;;; This is longer because characters are 1 or 2 bytes.  32???
;;; &&& Really, should know, for each implementation and string type, how big
;;; the characters are.
(declaim (type fixnum *WB-Tree-Max-String-Length*))
(defparameter *WB-Tree-Max-String-Length* 16)

;;; The factor by which one subtree may outweigh another.  See Adams.  Don't
;;; change this unless you understand his analysis.
(defconstant WB-Tree-Balance-Factor 4)


;;; ================================================================================
;;; ================================================================================
;;; Sets

(defstruct (WB-Set-Tree-Node
	    (:constructor Make-Raw-WB-Set-Tree-Node (Size Value Left Right))
	    (:predicate WB-Set-Tree-Node?)
	    (:print-function WB-Set-Tree-Node-Print))
  (Left  nil :type (or null WB-Set-Tree-Node simple-vector))
  (Right nil :type (or null WB-Set-Tree-Node simple-vector))
  Value		; normally the value at the node, but see `Equivalent-Set' below.
  (Size 0 :type fixnum))		; the number of members in this subtree

(deftype WB-Set-Tree ()
  '(or null WB-Set-Tree-Node simple-vector))

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

;;; When we get two or more equivalent values in a set, we use one of these
;;; as the `Value' of the tree node.  It would be a bit simpler, and would
;;; make the code more uniform, to say that the node value slot would always
;;; contain a list.  But then we would be paying a space cost, albeit a
;;; small one, on every node; I'd rather pay the cost only when it's needed.
(defstruct (Equivalent-Set
	    (:constructor Make-Equivalent-Set (Members))
	    (:predicate Equivalent-Set?))
  (Members nil :type list))	; list of equivalent values

(declaim (ftype (function (t) fixnum) Set-Value-Size))

(defun Set-Value-Size (value)
  "The number of members represented by `value', which can be more than 1 if
`value' is an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? value)
      (length (Equivalent-Set-Members value))
    1))


(declaim (ftype (function (WB-Set-Tree) fixnum) WB-Set-Tree-Size))

(defun WB-Set-Tree-Size (tree)
  "The number of members contained in this tree."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) 0)
	((simple-vector-p tree) (length tree))
	(t (WB-Set-Tree-Node-Size tree))))


(defun Make-WB-Set-Tree-Node (value left right)
  "The low-level constructor for a set tree node."
  (declare (optimize (speed 3) (safety 0)))
  (Make-Raw-WB-Set-Tree-Node (the fixnum
			       (+ (WB-Set-Tree-Size left) (WB-Set-Tree-Size right)
				  (Set-Value-Size value)))
			     value left right))


(defun WB-Set-Tree-Arb (tree)
  "Selects an arbitrary member of the set.  Assumes it is nonnull."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree)
	 (error "`WB-Set-Tree-Arb' called on empty tree"))
	((simple-vector-p tree)
	 (svref tree 0))
	(t
	 (let ((value (WB-Set-Tree-Node-Value tree)))
	   (if (Equivalent-Set? value)
	       (car (Equivalent-Set-Members value))
	     value)))))

(defun WB-Set-Tree-Least (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (let ((val (WB-Set-Tree-Minimum-Value tree)))
    (if (Equivalent-Set? val)
	(car (Equivalent-Set-Members val))
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
	       (if (Equivalent-Set? val)
		   (let ((mems (Equivalent-Set-Members val)))
		     (Make-WB-Set-Tree-Node (if (= (length mems) 2)
						(cadr mems)
					      (Make-Equivalent-Set (cdr mems)))
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
	  (if (Equivalent-Set? val)
	      (car (lastcons (Equivalent-Set-Members val)))
	    val))))))

(defun WB-Set-Tree-Member? (tree value)
  "Returns true iff `value' is a member of `tree'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (eq (Vector-Set-Binary-Search tree value) ':equal))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     (:equal t)
	     ((:unequal)
	      (and (Equivalent-Set? node-val)
		   (member value (Equivalent-Set-Members node-val) :test #'equal?)))
	     ((:less)
	      (WB-Set-Tree-Member? (WB-Set-Tree-Node-Left tree) value))
	     ((:greater)
	      (WB-Set-Tree-Member? (WB-Set-Tree-Node-Right tree) value)))))))

(defun WB-Set-Tree-Member?-Cfn (tree value cfn)
  "Returns true iff `value' is a member of `tree'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree)
	   (type function cfn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (eq (Vector-Set-Binary-Search-Cfn tree value cfn) ':equal))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (funcall cfn value node-val))))
	   (ecase comp
	     (:equal t)
	     ((:unequal)
	      (and (Equivalent-Set? node-val)
		   (member value (Equivalent-Set-Members node-val) :test #'equal?)))
	     ((:less)
	      (WB-Set-Tree-Member? (WB-Set-Tree-Node-Left tree) value))
	     ((:greater)
	      (WB-Set-Tree-Member? (WB-Set-Tree-Node-Right tree) value)))))))

(defun WB-Set-Tree-Find-Equivalent (tree value)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true and (second value) either the one value or an `Equivalent-Set'
containing the values; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value)))
	   (and found? (values t (svref tree idx)))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val))
	     ((:less)
	      (WB-Set-Tree-Find-Equivalent (WB-Set-Tree-Node-Left tree) value))
	     ((:greater)
	      (WB-Set-Tree-Find-Equivalent (WB-Set-Tree-Node-Right tree) value)))))))

;;; Not used internally, but clients can use this to let a set be its own identity
;;; map (for canonicalization, e.g.).
(defun WB-Set-Tree-Find-Equal (tree value)
  "If `tree' contains a value equal to `value', returns (first value) true and
\(second value) the value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value)))
	   (and found?
		(let ((v (svref tree idx)))
		  (and (equal? v value)
		       (values t (svref tree idx)))))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (Equivalent-Set? node-val)
		  (let ((v (cl:find value (Equivalent-Set-Members node-val)
				    :test #'equal?)))
		    (and v (values t v)))
		(values t node-val)))
	     ((:less)
	      (WB-Set-Tree-Find-Equal (WB-Set-Tree-Node-Left tree) value))
	     ((:greater)
	      (WB-Set-Tree-Find-Equal (WB-Set-Tree-Node-Right tree) value)))))))

(defun WB-Set-Tree-Find-Rank (tree value)
  "Returns the rank at which `value' appears in `tree', if it does, else the rank
it would occupy if it were present.  The second value is true iff the value was
found.  Note that if the set contains equivalent-but-unequal elements, they all
appear at the same rank."
  (cond ((null tree) 0)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value)))
	   (values idx found?)))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val)))
	       (left (WB-Set-Tree-Node-Left tree)))
	   (ecase comp
	     ((:equal :unequal)
	      (WB-Set-Tree-Size left))
	     ((:less)
	      (WB-Set-Tree-Find-Rank left value))
	     ((:greater)
	      (let ((right-rank found?
		      (WB-Set-Tree-Find-Rank (WB-Set-Tree-Node-Right tree) value)))
		(values (+ (WB-Set-Tree-Size left) right-rank)
			found?))))))))


;;; ================================================================================
;;; With

(declaim (ftype (function (simple-vector t) (values t fixnum)) Vector-Set-Binary-Search))

(defun WB-Set-Tree-With (tree value)
  "If `value' is in `tree', returns `tree'; otherwise returns `tree' with
`value' added.  `value' may be an `Equivalent-Set'."
  ;; The case where `value' is an `Equivalent-Set' is used by `WB-Set-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree)
	 (if (not (Equivalent-Set? value))
	     (vector value)
	   (Make-WB-Set-Tree-Node value nil nil)))
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Set', because
	   ;; this routine is called by `WB-Set-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Set? value)))
	       tree
	     (if (and (not found?)
		      (< (length tree) *WB-Tree-Max-Vector-Length*)
		      (not (Equivalent-Set? value)))
		 (Vector-Insert tree idx value)
	       ;; Originally, I split the vector in half rather than at the point
	       ;; where `value' goes.  But in the not unlikely case where values
	       ;; are being inserted in order, this will give longer vectors.
	       (Make-WB-Set-Tree-Node (if found?
					  (Equivalent-Set-Union (svref tree idx) value)
					value)
				      (and (> idx 0)
					   (Vector-Subseq tree 0 idx))
				      (and (< right-start (length tree))
					   (Vector-Subseq tree right-start)))))))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (and (not (Equivalent-Set? node-val)) (not (Equivalent-Set? value))
		       (eq comp ':equal))
		  tree
		(Make-WB-Set-Tree-Node (Equivalent-Set-Union node-val value)
				       (WB-Set-Tree-Node-Left tree)
				       (WB-Set-Tree-Node-Right tree))))
	     ((:less)
	      (let ((left (WB-Set-Tree-Node-Left tree))
		    ((new-left (WB-Set-Tree-With left value))))
		(if (eq new-left left)
		    tree
		  (WB-Set-Tree-Build-Node node-val new-left
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Set-Tree-Node-Right tree))
		    ((new-right (WB-Set-Tree-With right value))))
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
       (let ((len (- end start))
	     ((new-vec (make-array len))))
	 (dotimes (i len)
	   (setf (svref new-vec i) (svref vec (+ i start))))
	 new-vec)))


;;; ================================================================================
;;; Less

;;; Currently doesn't handle the case where `value' is an `Equivalent-Set' --
;;; any need to?
(defun WB-Set-Tree-Less (tree value)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (Vector-Set-Binary-Search tree value)))
	   (if (eq found? ':equal)
	       (and (> (length tree) 1) (Vector-Remove-At tree idx))
	     tree)))
	(t
	 (let ((node-val (WB-Set-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (Equivalent-Set? node-val))
		  (if (eq comp ':unequal)
		      tree
		    (WB-Set-Tree-Join (WB-Set-Tree-Node-Left tree)
				      (WB-Set-Tree-Node-Right tree)))
		(let ((ignore diff (Equivalent-Set-Difference node-val value)))
		  (declare (ignore ignore))	; difference can't be null
		  (WB-Set-Tree-Build-Node diff
					  (WB-Set-Tree-Node-Left tree)
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:less)
	      (let ((left (WB-Set-Tree-Node-Left tree))
		    ((new-left (WB-Set-Tree-Less left value))))
		(if (eq new-left left)
		    tree
		  (WB-Set-Tree-Build-Node node-val new-left
					  (WB-Set-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Set-Tree-Node-Right tree))
		    ((new-right (WB-Set-Tree-Less right value))))
		(if (eq new-right right)
		    tree
		  (WB-Set-Tree-Build-Node node-val (WB-Set-Tree-Node-Left tree)
					  new-right)))))))))

(defun Vector-Remove-At (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec)))
    (and (> len 0)
	 (let ((new-vec (make-array (1- len))))
	   (dotimes (i idx)
	     (setf (svref new-vec i) (svref vec i)))
	   (dotimes (i (- len idx 1))
	     (setf (svref new-vec (+ idx i)) (svref vec (+ idx i 1))))
	   new-vec))))


;;; ================================================================================
;;; Split-Above/Below

(defconstant Hedge-Negative-Infinity
  '|&*$ Hedge negative infinity $*&|)

(defconstant Hedge-Positive-Infinity
  '|&*$ Hedge positive infinity $*&|)

(defun WB-Set-Tree-Split-Above (tree value)
  (WB-Set-Tree-Split tree value Hedge-Positive-Infinity))

(defun WB-Set-Tree-Split-Below (tree value)
  (WB-Set-Tree-Split tree Hedge-Negative-Infinity value))


;;; ================================================================================
;;; Union, intersection, and set difference

;;; Adams recommends using four versions of each of these routines, one for each
;;; boundedness case (no bound, a lower bound, an upper bound, or both).  He probably
;;; had to do that given that he was working in ML, but in Lisp it's easy to make
;;; up distinguished "negative infinity" and "positive infinity" values which, for
;;; all practical purposes, will never show up in sets.

(defun WB-Set-Tree-Union (tree1 tree2)
  "Returns the union of `tree1' and `tree2'.  Runs in time linear in the total
sizes of the two trees."
  (if (eq tree1 tree2)
      tree1
    (WB-Set-Tree-Union-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity)))

(defun WB-Set-Tree-Union-Rng (tree1 tree2 lo hi)
  "Returns the union of `tree1' with `tree2', considering only those members
that are above `lo' and below `hi', and assuming that the root values of `tree1'
and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2))
  (cond ;; If the sets are historically related -- one was produced by a sufficiently
	;; small number of `with' and `less' operations on the other, or they are both
	;; related in this way to a third set -- then we might get lucky and find
	;; ourselves with the same subtree on both sides.  This can reduce this
	;; linear-time algorithm to log-time.
	((eq tree1 tree2) (WB-Set-Tree-Split tree1 lo hi))
	((null tree2)
	 (WB-Set-Tree-Split tree1 lo hi))
	((null tree1)
	 (WB-Set-Tree-Split tree2 lo hi))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (WB-Set-Tree-Vector-Union tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (WB-Set-Tree-Union-Rng tree2 tree1 lo hi))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1))))
	   (WB-Set-Tree-Concat
	     (if eqvv2? (Equivalent-Set-Union val1 eqvv2)
	       val1)
	     ;; Subtlety: we have to trim the children of `tree1' because of the
	     ;; previous `cond' clause, which swaps the two trees, thus destroying
	     ;; the invariant that all members of `tree1' are between `lo' and `hi'.
	     (WB-Set-Tree-Union-Rng (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree1)
						      lo val1)
				    (WB-Set-Tree-Trim tree2 lo val1)
				    lo val1)
	     (WB-Set-Tree-Union-Rng (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree1)
						      val1 hi)
				    (WB-Set-Tree-Trim tree2 val1 hi)
				    val1 hi))))))


(defun WB-Set-Tree-Intersect (tree1 tree2)
  "Returns the intersection of `tree1' and `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (WB-Set-Tree-Intersect-Rng tree1 tree2
			     Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Set-Tree-Intersect-Rng (tree1 tree2 lo hi)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-set optimization
	 (WB-Set-Tree-Split tree1 lo hi))
	((or (null tree1) (null tree2))
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Intersect tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Trim tree2 lo hi)
				    tree1 lo hi))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Node-Left tree1)
					     (WB-Set-Tree-Trim tree2 lo val1)
					     lo val1))
		(new-right
		  (WB-Set-Tree-Intersect-Rng (WB-Set-Tree-Node-Right tree1)
					     (WB-Set-Tree-Trim tree2 val1 hi)
					     val1 hi)))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1))
		((nonnull? isect (and eqvv2? (Equivalent-Set-Intersect val1 eqvv2))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat isect new-left new-right)
	     (WB-Set-Tree-Join new-left new-right))))))


(defun WB-Set-Tree-Diff (tree1 tree2)
  "Returns the set difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (WB-Set-Tree-Diff-Rng tree1 tree2
			Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Set-Tree-Diff-Rng (tree1 tree2 lo hi)
  "Returns the set difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2))
  (cond ((eq tree1 tree2) nil)		; historically-related-set optimization
	((null tree1) nil)
	((null tree2)
	 (WB-Set-Tree-Split tree1 lo hi))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Diff tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((new-left (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Trim tree1 lo val2)
						;; We trim here because `lo' didn't come
						;; from `tree2'.
						(WB-Set-Tree-Trim
						  (WB-Set-Tree-Node-Left tree2)
						  lo val2)
						lo val2))
		(new-right (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Trim tree1 val2 hi)
						 (WB-Set-Tree-Trim
						   (WB-Set-Tree-Node-Right tree2)
						   val2 hi)
						 val2 hi)))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2))
		((nonnull? diff (and eqvv1? (Equivalent-Set-Difference eqvv1 val2))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat diff new-left new-right)
	     (WB-Set-Tree-Join new-left new-right))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Node-Left tree1)
						(WB-Set-Tree-Trim tree2 lo val1)
						lo val1))
		(new-right (WB-Set-Tree-Diff-Rng (WB-Set-Tree-Node-Right tree1)
						 (WB-Set-Tree-Trim tree2 val1 hi)
						 val1 hi)))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1))
		((nonnull? diff (if eqvv2? (Equivalent-Set-Difference val1 eqvv2)
				  (values t val1))))))
	   (if nonnull?
	       (WB-Set-Tree-Concat diff new-left new-right)
	     (WB-Set-Tree-Join new-left new-right))))))


(defun WB-Set-Tree-Diff-2 (tree1 tree2)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1'.  Runs in time linear in the total sizes of the two trees."
  (WB-Set-Tree-Diff-2-Rng tree1 tree2
			  Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Set-Tree-Diff-2-Rng (tree1 tree2 lo hi)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1', considering only those members that are above `lo' and
below `hi', and assuming that the root values of `tree1' and `tree2' are in
this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2))
  (cond ((eq tree1 tree2) (values nil nil)) ; historically-related tree optimization
	((or (null tree1) (null tree2))
	 (values (WB-Set-Tree-Split tree1 lo hi)
		 (WB-Set-Tree-Split tree2 lo hi)))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Diff-2 tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2))
	       ((new-left-1 new-left-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Trim tree1 lo val2)
					  (WB-Set-Tree-Trim
					    (WB-Set-Tree-Node-Left tree2)
					    lo val2)
					  lo val2))
		(new-right-1 new-right-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Trim tree1 val2 hi)
					  (WB-Set-Tree-Trim
					    (WB-Set-Tree-Node-Right tree2)
					    val2 hi)
					  val2 hi)))
	       ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2))
		((nonnull1? diff1 (and eqvv1? (Equivalent-Set-Difference eqvv1 val2)))
		 (nonnull2? diff2 (if eqvv1? (Equivalent-Set-Difference val2 eqvv1)
				    (values t val2))))))
	   (values
	     (if nonnull1?
		 (WB-Set-Tree-Concat diff1 new-left-1 new-right-1)
	       (WB-Set-Tree-Join new-left-1 new-right-1))
	     (if nonnull2?
		 (WB-Set-Tree-Concat diff2 new-left-2 new-right-2)
	       (WB-Set-Tree-Join new-left-2 new-right-2)))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((new-left-1 new-left-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Node-Left tree1)
					  (WB-Set-Tree-Trim tree2 lo val1)
					  lo val1))
		(new-right-1 new-right-2
		  (WB-Set-Tree-Diff-2-Rng (WB-Set-Tree-Node-Right tree1)
					  (WB-Set-Tree-Trim tree2 val1 hi)
					  val1 hi))
		((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1))
		 ((nonnull1? diff1 (if eqvv2? (Equivalent-Set-Difference val1 eqvv2)
				     (values t val1)))
		  (nonnull2? diff2 (and eqvv2?
					(Equivalent-Set-Difference eqvv2 val1)))))))
	   (values
	     (if nonnull1?
		 (WB-Set-Tree-Concat diff1 new-left-1 new-right-1)
	       (WB-Set-Tree-Join new-left-1 new-right-1))
	     (if nonnull2?
		 (WB-Set-Tree-Concat diff2 new-left-2 new-right-2)
	       (WB-Set-Tree-Join new-left-2 new-right-2)))))))


;;; ================================================================================
;;; Comparison

(defun WB-Set-Tree-Compare (tree1 tree2)
  (let ((size1 (WB-Set-Tree-Size tree1))
	(size2 (WB-Set-Tree-Size tree2)))
    (cond ((eq tree1 tree2) ':equal)
	  ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  (t (WB-Set-Tree-Compare-Rng tree1 0 tree2 0 0 size1)))))

(defun WB-Set-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi)
  ;; This is similar to the other hedge algorithms, but there is a key difference:
  ;; it is concerned not with the values of nodes but with their rank, that is,
  ;; the number of values to their left.  The `base' parameters specify, for
  ;; each tree, the number of values to the left of the tree.
  ;; Another subtlety: we can return as soon as we get a comparison result of
  ;; ':less or ':greater, but ':unequal has to wait until the end.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi))
  (cond ((and (eq tree1 tree2) (= base1 base2)) ; historically-related-set optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (let ((unequal? nil))
	   (or (gmap :or #'(lambda (val1 val2)
			     (let ((comp (compare val1 val2)))
			       (when (eq comp ':unequal)
				 (setq unequal? t))
			       (and (or (eq comp ':less) (eq comp ':greater))
				    comp)))
		     (:simple-vector tree1 :start (- lo base1) :stop (- hi base1))
		     (:simple-vector tree2 :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((simple-vector-p tree1)
	 (let ((rev-comp (WB-Set-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi)))
	   (ecase rev-comp
	     (:less ':greater)
	     (:greater ':less)
	     ((:equal :unequal) rev-comp))))
	(t
	 (let ((left1 (WB-Set-Tree-Node-Left tree1))
	       ((left1-size (WB-Set-Tree-Size left1))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ;; See the comment beginning `Subtlety:' in `WB-Set-Tree-Union-Rng'.
		 ((left1a base1a (WB-Set-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Set-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Set-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (WB-Set-Tree-Node-Value tree1))
		   (val2 (WB-Set-Tree-Rank-Element-Internal
			   tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (Equivalent-Set-Compare val1 val2))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (Set-Value-Size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (WB-Set-Tree-Rank-Trim (WB-Set-Tree-Node-Right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (WB-Set-Tree-Rank-Trim tree2 base2 new-lo hi))
			 ((right-comp (WB-Set-Tree-Compare-Rng
					right1a base1a tree2a base2a new-lo hi))))))
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

(defun WB-Set-Tree-Rank (tree value)
  "Searches a set tree `tree' for `value'.  Returns two values, a boolean and an
index.  If `value', or a value equivalent to `value', is in `tree', the boolean
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `value' would have if it were to be added.  Note that
if the set contains equivalent-but-unequal elements, the rank of each of several
such elements is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (labels ((rec (tree value base)
	     (cond ((null tree) (values nil base))
		   ((simple-vector-p tree)
		    (let ((found? idx (Vector-Set-Binary-Search tree value)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Set-Tree-Node-Value tree))
			  (left (WB-Set-Tree-Node-Left tree))
			  ((left-size (WB-Set-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (compare value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Set? node-val)
			     (let ((mems (Equivalent-Set-Members node-val))
				   ((pos (cl:position value mems :test #'equal?))))
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
    (if (Equivalent-Set? elt)
	(nth rem (Equivalent-Set-Members elt))
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

(defun WB-Set-Tree-Subset? (tree1 tree2)
  (let ((size1 (WB-Set-Tree-Size tree1))
	(size2 (WB-Set-Tree-Size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (WB-Set-Tree-Subset?-Rng tree1 tree2
				      Hedge-Negative-Infinity Hedge-Positive-Infinity)))))

(defun WB-Set-Tree-Subset?-Rng (tree1 tree2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree1 tree2))
  (cond ((eq tree1 tree2) t)		; historically-related-set optimization
	((null tree1) t)
	((and (simple-vector-p tree1) (or (null tree2) (simple-vector-p tree2)))
	 (Vector-Set-Subset? tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (let ((val2 (WB-Set-Tree-Node-Value tree2)))
	   (and (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Trim tree1 lo val2)
					 (WB-Set-Tree-Node-Left tree2)
					 lo val2)
		(let ((eqvv1? eqvv1 (WB-Set-Tree-Find-Equivalent tree1 val2)))
		  (and (or (not eqvv1?)
			   (Equivalent-Set-Subset? eqvv1 val2))
		       (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Trim tree1 val2 hi)
						(WB-Set-Tree-Node-Right tree2)
						val2 hi))))))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1)))
	   (and (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Node-Left tree1)
					 (WB-Set-Tree-Trim tree2 lo val1)
					 lo val1)
		(let ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1)))
		  (and eqvv2?
		       (Equivalent-Set-Subset? val1 eqvv2)
		       (WB-Set-Tree-Subset?-Rng (WB-Set-Tree-Node-Right tree1)
						(WB-Set-Tree-Trim tree2 val1 hi)
						val1 hi))))))))


;;; ================================================================================
;;; Disjointness testing

(defun WB-Set-Tree-Disjoint? (tree1 tree2)
  (WB-Set-Tree-Disjoint?-Rng tree1 tree2
			     Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Set-Tree-Disjoint?-Rng (tree1 tree2 lo hi)
  (cond ((or (null tree1) (null tree2))
	 t)
	((eq tree1 tree2)
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (Vector-Set-Disjoint? tree1 tree2 lo hi))
	((simple-vector-p tree1)
	 (WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Trim tree2 lo hi)
				    tree1 lo hi))
	(t
	 (let ((val1 (WB-Set-Tree-Node-Value tree1))
	       ((eqvv2? eqvv2 (WB-Set-Tree-Find-Equivalent tree2 val1))))
	   (and (or (null eqvv2?) (Equivalent-Set-Disjoint? val1 eqvv2))
		(WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Node-Left tree1)
					   (WB-Set-Tree-Trim tree2 lo val1)
					   lo val1)
		(WB-Set-Tree-Disjoint?-Rng (WB-Set-Tree-Node-Right tree1)
					   (WB-Set-Tree-Trim tree2 val1 hi)
					   val1 hi))))))

;;; ================================================================================
;;; Miscellany

;;; &&& Even with the pair special case, this is actually still 70% slower than
;;; repeated `with', though it conses slightly less.
;;; The right way is to sort the list, then do something like WB-Seq-Tree-From-List.
(defun WB-Set-Tree-From-List (lst)
  (labels ((recur (lst n)
	     (cond ((= n 0) nil)
		   ((= n 1) (vector (car lst)))
		   ;; Reduces consing about 12%, improves speed.
		   ((= n 2)
		    (ecase (Compare (car lst) (cadr lst))
		      (:equal (vector (car lst)))
		      (:less (let ((v (make-array 2)))
			       (setf (svref v 0) (car lst)
				     (svref v 1) (cadr lst))
			       v))
		      (:greater (let ((v (make-array 2)))
				  (setf (svref v 0) (cadr lst)
					(svref v 1) (car lst))
				  v))
		      (:unequal (WB-Set-Tree-With (vector (car lst)) (cadr lst)))))
		   (t
		    (let ((n2 (floor n 2)))
		      (WB-Set-Tree-Union (recur lst n2)
					 (recur (nthcdr n2 lst)
						(- n n2))))))))
    (recur lst (length lst))))

(defun WB-Set-Tree-From-CL-Sequence (seq)
  (labels ((recur (n m)
	     (cond ((= n m) nil)
		   ((= n (1- m)) (vector (elt seq n)))
		   (t
		    (let ((n2 (floor (+ n m) 2)))
		      (WB-Set-Tree-Union (recur n n2) (recur n2 m)))))))
    (recur 0 (length seq))))


;;; ================================================================================
;;; Support routines for the above (sets)

(defun Vector-Set-Binary-Search (vec value)
  "Searches a vector set `vec' for `value'.  Returns two values, a symbol and an
index.  If `value', or a value equivalent to `value', is in `vec', the symbol
is `:equal' resp. `:unequal', and the index is the position of the value;
otherwise, the symbol is `nil' and the index is where `value' would go if it
were to be inserted."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   #+(or cmu scl)
	   (values t fixnum))
  (do ((lo 0)
       (hi (1- (length vec))))
      ((> lo hi)
       (values nil lo))
    (declare (type fixnum lo hi))
    (let ((mid (ash (the fixnum (+ lo hi)) -1))
	  ((vec-val (svref vec mid))
	   ((comp (compare value vec-val)))))
      (ecase comp
	((:equal :unequal) (return (values comp mid)))
	(:less             (setq hi (1- mid)))
	(:greater          (setq lo (1+ mid)))))))

(defun Vector-Set-Binary-Search-Cfn (vec value cfn)
  "Searches a vector set `vec' for `value'.  Returns two values, a symbol and an
index.  If `value', or a value equivalent to `value', is in `vec', the symbol
is `:equal' resp. `:unequal', and the index is the position of the value;
otherwise, the symbol is `nil' and the index is where `value' would go if it
were to be inserted."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   #+(or cmu scl)
	   (values t fixnum)
	   (type function cfn))
  (do ((lo 0)
       (hi (1- (length vec))))
      ((> lo hi)
       (values nil lo))
    (declare (type fixnum lo hi))
    (let ((mid (ash (the fixnum (+ lo hi)) -1))
	  ((vec-val (svref vec mid))
	   ((comp (funcall cfn value vec-val)))))
      (ecase comp
	((:equal :unequal) (return (values comp mid)))
	(:less             (setq hi (1- mid)))
	(:greater          (setq lo (1+ mid)))))))

(defun Vector-Set-Binary-Search-Lo (vec lo)
  "Returns the index of the left edge of the first member of `vec' that is
above `lo'."
  (declare (type simple-vector vec)
	   #+(or cmu scl)
	   (values fixnum))
  (let ((found? idx (Vector-Set-Binary-Search vec lo)))
    (if found? (1+ idx) idx)))

(defun Vector-Set-Binary-Search-Hi (vec hi)
  "Returns the index of the right edge of the last member of `vec' that is
below `hi'."
  (declare (type simple-vector vec)
	   #+(or cmu scl)
	   (values fixnum))
  (let ((found? idx (Vector-Set-Binary-Search vec hi)))
    (declare (ignore found?))
    idx))

(declaim (ftype (function (simple-vector t) fixnum) Vector-Set-Binary-Search-Lo))
(declaim (ftype (function (simple-vector t) fixnum) Vector-Set-Binary-Search-Hi))

(defun WB-Set-Tree-Split (tree lo hi)
  "Corresponds to Adams' `split_lt' and `split_gt'.  Returns a tree containing
those members of `tree' above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((simple-vector-p tree)
	 (let ((len (length tree))
	       ((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				    0
				  (Vector-Set-Binary-Search-Lo tree lo)))
		(split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi tree hi)))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (Vector-Subseq tree split-point-lo split-point-hi)))))
	((and (not (eq lo Hedge-Negative-Infinity))
	      (not (greater-than? (WB-Set-Tree-Node-Value tree) lo)))
	 (WB-Set-Tree-Split (WB-Set-Tree-Node-Right tree) lo hi))
	((and (not (eq hi Hedge-Positive-Infinity))
	      (not (less-than? (WB-Set-Tree-Node-Value tree) hi)))
	 (WB-Set-Tree-Split (WB-Set-Tree-Node-Left tree) lo hi))
	(t
	 (let ((new-left (WB-Set-Tree-Split (WB-Set-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity))
	       (new-right (WB-Set-Tree-Split (WB-Set-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi)))
	   (if (and (eq new-left (WB-Set-Tree-Node-Left tree))
		    (eq new-right (WB-Set-Tree-Node-Right tree)))
	       tree
	     (WB-Set-Tree-Concat (WB-Set-Tree-Node-Value tree)
				 new-left new-right))))))

(defun WB-Set-Tree-Trim (tree lo hi)
  "Corresponds to Adams' `trim' and variants.  Removes any tree nodes whose
values are less than `lo' or greater than `hi'.  Note, this does _not_ guarantee
that the result only contains values between `lo' and `hi'; use `-Split' for
that.  This, however, doesn't cons."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 ;; If the vector is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than? (svref tree (1- (length tree))) lo))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than? (svref tree 0) hi))
	      tree))
	(t
	 (let ((val (WB-Set-Tree-Node-Value tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than? val lo))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than? val hi))
		   tree
		 (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left tree) lo hi))
	     (WB-Set-Tree-Trim (WB-Set-Tree-Node-Right tree) lo hi))))))

(defun WB-Set-Tree-Concat (value left right)
  "Corresponds to Adams' `concat3'.  Assumes that (all values in `left') <=
`value' <= (all values in `right'); returns a new tree containing all values.
This does more rebalancing than `WB-Set-Tree-Build-Node', which otherwise
has the same contract.  `value' may be an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree left right))
  (cond ((null left)
	 (WB-Set-Tree-With right value))
	((null right)
	 (WB-Set-Tree-With left value))
	((and (WB-Set-Tree-Node? left)
	      (> (WB-Set-Tree-Size left)
		 (the fixnum
		   (* (WB-Set-Tree-Size right) WB-Tree-Balance-Factor))))
	 (WB-Set-Tree-Build-Node (WB-Set-Tree-Node-Value left)
				 (WB-Set-Tree-Node-Left left)
				 (WB-Set-Tree-Concat value (WB-Set-Tree-Node-Right left)
						     right)))
	((and (WB-Set-Tree-Node? right)
	      (> (WB-Set-Tree-Size right)
		 (the fixnum
		   (* (WB-Set-Tree-Size left) WB-Tree-Balance-Factor))))
	 (WB-Set-Tree-Build-Node (WB-Set-Tree-Node-Value right)
				 (WB-Set-Tree-Concat value left
						     (WB-Set-Tree-Node-Left right))
				 (WB-Set-Tree-Node-Right right)))
	(t
	 (WB-Set-Tree-Build-Node value left right))))

(defun WB-Set-Tree-Join (left right)
  "Returns the union of `left' and `right' under the assumption that all values
in `left' are less than any value in `right'."
  (if (null left) right
    (if (null right) left
      (let ((val (WB-Set-Tree-Minimum-Value right)))
	(WB-Set-Tree-Concat val left (WB-Set-Tree-Less-Minimum right))))))

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

(defun WB-Set-Tree-Less-Minimum (tree)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum value
or `Equivalent-Set' removed."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree tree))
  (if (simple-vector-p tree)
      (and (> (length tree) 1) (Vector-Subseq tree 1))
    (let ((left (WB-Set-Tree-Node-Left tree)))
      (if left
	  (WB-Set-Tree-Concat (WB-Set-Tree-Node-Value tree)
			      (WB-Set-Tree-Less-Minimum left)
			      (WB-Set-Tree-Node-Right tree))
	(WB-Set-Tree-Node-Right tree)))))

(defun WB-Set-Tree-Build-Node (value left right)
  "Constructs a `WB-Set-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Set-Tree left right))
  (cond ((and (or (null left) (simple-vector-p left))
	      (or (null right) (simple-vector-p right)))
	 (if (and (not (Equivalent-Set? value))
		  (< (+ (length-nv left) (length-nv right))
		     *WB-Tree-Max-Vector-Length*))
	     (concatenate 'simple-vector left (vector value) right)
	   (Make-WB-Set-Tree-Node value left right)))
	(t
	 (let ((sizl (WB-Set-Tree-Size left))
	       (sizr (WB-Set-Tree-Size right)))
	   ;; This code is subtly different from Adams' in order to create more
	   ;; opportunities to coalesce adjacent short vectors.  &&& We won't really
	   ;; know how well we're doing here, or how we might improve further, until
	   ;; we have some real applications running and can gather some stats.
	   (cond ((and (WB-Set-Tree-Node? left)
		       (> sizl (the (signed-byte 32)
				 (* sizr WB-Tree-Balance-Factor))))
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
		       (> sizr (the (signed-byte 32)
				 (* sizl WB-Tree-Balance-Factor))))
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


(defun WB-Set-Tree-Verify (tree)
  (WB-Set-Tree-Verify-Rng tree Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Set-Tree-Verify-Rng (tree lo hi)
  (cond ((null tree) t)
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (and (<= len *WB-Tree-Max-Vector-Length*)
		(do ((i 0 (1+ i))
		     (prev Hedge-Negative-Infinity))
		    ((= i len)
		     (or (eq hi Hedge-Positive-Infinity)
			 (less-than? prev hi)))
		  (let ((elt (svref tree i)))
		    (unless (or (eq prev Hedge-Negative-Infinity)
				(less-than? prev elt))
		      (return nil))
		    (setq prev elt))))))
	(t
	 (let ((sizl (WB-Set-Tree-Size (WB-Set-Tree-Node-Left tree)))
	       (sizr (WB-Set-Tree-Size (WB-Set-Tree-Node-Right tree)))
	       (value (WB-Set-Tree-Node-Value tree)))
	   (and (= (WB-Set-Tree-Node-Size tree) (+ sizl sizr (Set-Value-Size value)))
		(or (not (Equivalent-Set? value))
		    (> (length (Equivalent-Set-Members value)) 1))
		(or (<= sizr 4)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 4)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Set-Tree-Verify-Rng (WB-Set-Tree-Node-Left tree) lo value)
		(WB-Set-Tree-Verify-Rng (WB-Set-Tree-Node-Right tree) value hi))))))


;;; ================================================================================
;;; Vector set operations

(defun WB-Set-Tree-Vector-Union (vec1 vec2 lo hi)
  "Returns the union of vectors `vec1' and `vec2', restricted to those members
above `lo' and below `hi'.  Creates new set tree nodes if needed, either
because the result exceeds the vector threshold size, or because one or more
pairs of equivalent members were found."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((new-vec any-equivalent? (Vector-Set-Union vec1 vec2 lo hi)))
    (declare (type simple-vector new-vec))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(reduce #'WB-Set-Tree-With new-vec :initial-value nil)
      (if (> (length new-vec) *WB-Tree-Max-Vector-Length*)
	  (let ((split-point (floor (length new-vec) 2)))
	    (Make-WB-Set-Tree-Node (svref new-vec split-point)
				   (Vector-Subseq new-vec 0 split-point)
				   (Vector-Subseq new-vec (1+ split-point))))
	new-vec))))

(defun Vector-Set-Union (vec1 vec2 lo hi)
  "Returns, as a vector, the union of vectors `vec1' and `vec2', restricted to
those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      ;; We do these with linear rather than binary search because frequently,
      ;; the ends of the vectors will already be in range (the worst case for
      ;; binary search).
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vec2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vec2 (1- len2)) hi)))
	(decf len2)))
    (do ((res nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (coerce (nreverse res) 'simple-vector)
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
		   ((comp (compare v1 v2))))
	       (ecase comp
		 ((:equal)
		  (push v1 res)
		  (incf i1)
		  (incf i2))
		 ((:less)
		  (push v1 res)
		  (incf i1))
		 ((:greater)
		  (push v2 res)
		  (incf i2))
		 ((:unequal)
		  (push (Equivalent-Set-Union v1 v2) res)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

;;; We don't need a `WB-Set-Tree-Vector-Intersect' because the intersection is
;;; never longer than the operands.

(defun Vector-Set-Intersect (vec1 vec2 lo hi)
  "Returns, as a vector, the intersection of vectors `vec1' and `vec2', restricted
to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (and res (coerce (nreverse res) 'simple-vector)))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (compare v1 v2))))
	(ecase comp
	  ((:equal)
	   (push v1 res)
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Set-Diff (vec1 vec2 lo hi)
  "Returns, as a vector, the set difference of vectors `vec1' less `vec2',
restricted to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vec1 i1) res)
	   (incf i1))
	 (and res (coerce (nreverse res) 'simple-vector)))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (compare v1 v2))))
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

(defun Vector-Set-Diff-2 (vec1 vec2 lo hi)
  "Returns, as two vector values, the set difference of vectors `str1' less `str2'
and that of `str2' less `str1', restricted to those members above `lo' and below
`hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vec2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vec2 (1- len2)) hi)))
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
	 (values (and res1 (coerce (nreverse res1) 'simple-vector))
		 (and res2 (coerce (nreverse res2) 'simple-vector))))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (compare v1 v2))))
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

(defun Vector-Set-Subset? (vec1 vec2 lo hi)
  "Returns true iff `vec1' contains all members of `vec2', restricted
to those members above `lo' and below `hi'.  `vec2' may be `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type (or null simple-vector) vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length-nv vec1))
	(len2 (length-nv vec2)))
    (declare (type fixnum len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 (= i1 len1))
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (compare v1 v2))))
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

(defun Vector-Set-Disjoint? (vec1 vec2 lo hi)
  "Returns true iff `vec1' does not contain any member of `vec2', restricted
to those members above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2))
  (let ((i1 0)
	(i2 0)
	(len1 (length vec1))
	(len2 (length vec2)))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref vec1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vec1 (1- len1)) hi)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 t)
      (let ((v1 (svref vec1 i1))
	    (v2 (svref vec2 i2))
	    ((comp (compare v1 v2))))
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
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
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
			     (if (Equivalent-Set? val)
				 (dolist (val (Equivalent-Set-Members val))
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
	(incf (the fixnum (svref iter (1+ sp))))
	(WB-Set-Tree-Iterator-Canonicalize iter)
	(values (if (simple-vector-p node) (svref node idx)
		  (let ((val (WB-Set-Tree-Node-Value node)))
		    (if (Equivalent-Set? val)
			(nth (1- idx) (Equivalent-Set-Members val))
		      val)))
		t)))))


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
  (gmap :vector (lambda (i) (WB-Tree-True-Max-Depth i nil))
	(:index 0 WB-Tree-Precomputed-Max-Depths)))

(deflex +WB-Tree-Max-Depths-With-Values+
  (gmap :vector (lambda (i) (WB-Tree-True-Max-Depth i t))
	(:index 0 WB-Tree-Precomputed-Max-Depths)))

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
    (values (ceiling (* (1- (integer-length size))
			;; constant:
			(/ (log 2) (log (/ (+ 1 WB-Tree-Balance-Factor)
					   WB-Tree-Balance-Factor))))))))


;;; ================================================================================
;;; Equivalent-Set routines

(defun Equivalent-Set-Union (val1 val2)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns their union represented as a single
value if a singleton, else as an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? val1)
      (if (Equivalent-Set? val2)
	  (let ((mems1 (Equivalent-Set-Members val1))
		(mems2 (Equivalent-Set-Members val2))
		((union (cl:union mems1 mems2 :test #'equal?))
		 ((union-len (length union)))))
	    (cond ((= union-len (length mems1)) val1)
		  ((= union-len (length mems2)) val2)
		  (t (Make-Equivalent-Set union))))
	(if (member val2 (Equivalent-Set-Members val1) :test #'equal?)
	    val1
	  (Make-Equivalent-Set (cons val2 (Equivalent-Set-Members val1)))))
    (if (Equivalent-Set? val2)
	(if (member val1 (Equivalent-Set-Members val2) :test #'equal?)
	    val2
	  (Make-Equivalent-Set (cons val1 (Equivalent-Set-Members val2))))
      (if (equal? val1 val2) val1
	(Make-Equivalent-Set (list val1 val2))))))

(defun Equivalent-Set-Intersect (val1 val2)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is nonnull, returns
two values: true, and the intersection, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the intersection is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? val1)
      (if (Equivalent-Set? val2)
	  (let ((mems1 (Equivalent-Set-Members val1))
		(mems2 (Equivalent-Set-Members val2))
		((isect (cl:intersection mems1 mems2 :test #'equal?))
		 ((isect-len (length isect)))))
	    (cond ((null isect) nil)
		  ((= isect-len (length mems1)) (values t val1))
		  ((= isect-len (length mems2)) (values t val2))
		  ((= isect-len 1) (values t (car isect)))
		  (t (values t (Make-Equivalent-Set isect)))))
	(and (member val2 (Equivalent-Set-Members val1) :test #'equal?)
	     (values t val2)))
    (if (Equivalent-Set? val2)
	(and (member val1 (Equivalent-Set-Members val2) :test #'equal?)
	     (values t val1))
      (and (equal? val1 val2) (values t val1)))))

(defun Equivalent-Set-Difference (val1 val2)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their difference is nonnull, returns
two values: true, and the difference, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the difference is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? val1)
      (let ((mems1 (Equivalent-Set-Members val1))
	    (mems2 (if (Equivalent-Set? val2) (Equivalent-Set-Members val2)
		     (list val2)))
	    ((diff (cl:set-difference mems1 mems2 :test #'equal?))
	     ((diff-len (length diff)))))
	(cond ((null diff) nil)
	      ((= diff-len (length mems1)) (values t val1))
	      ((= diff-len 1) (values t (car diff)))
	      (t (values t (Make-Equivalent-Set diff)))))
    (if (Equivalent-Set? val2)
	(and (not (member val1 (Equivalent-Set-Members val2) :test #'equal?))
	     (values t val1))
      (and (not (equal? val1 val2)) (values t val1)))))

(defun Equivalent-Set-Subset? (val1 val2)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns true iff `val2' contains all members
of `val1'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? val1)
      (and (Equivalent-Set? val2)
	   (let ((mems2 (Equivalent-Set-Members val2)))
	     (dolist (m1 (Equivalent-Set-Members val1) t)
	       (unless (member m1 mems2 :test #'equal?)
		 (return nil)))))
    (if (Equivalent-Set? val2)
	(member val1 (Equivalent-Set-Members val2) :test #'equal?)
      (equal? val1 val2))))

(defun Equivalent-Set-Disjoint? (val1 val2)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is null, returns
true, else false."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Set? val1)
      (if (Equivalent-Set? val2)
	  (dolist (m1 (Equivalent-Set-Members val1) t)
	    (when (member m1 (Equivalent-Set-Members val2) :test #'equal?)
	      (return nil)))
	(not (member val2 (Equivalent-Set-Members val1) :test #'equal?)))
    (if (Equivalent-Set? val2)
	(not (member val1 (Equivalent-Set-Members val2) :test #'equal?))
      (not (equal? val1 val2)))))

(defun Equivalent-Set-Compare (val1 val2)
  (declare (optimize (speed 3) (safety 0)))
  (let ((comp (compare val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Set? val1)
	  (if (Equivalent-Set? val2)
	      (let ((mems1 (Equivalent-Set-Members val1))
		    (mems2 (Equivalent-Set-Members val2))
		    ((len1 (length mems1))
		     (len2 (length mems2))))
		(cond ((< len1 len2) ':greater)	; counterintuitive, but correct
		      ((> len1 len2) ':less)
		      (t
		       (if (gmap :and
				 #'(lambda (x)
				     (member x mems2 :test #'equal?))
				 (:list mems1))
			   ':equal
			 ':unequal))))
	    ':less)
	(if (Equivalent-Set? val2)
	    ':greater
	  comp)))))


;;; When called on a value and an `Equivalent-Set', or on two `Equivalent-Set's,
;;; the result of `compare' is meaningful only for ordering; the distinction between
;;; `:equal' and `:unequal' is not meaningful.  Code that may care about the latter
;;; has more work to do anyway, such as calling `Equivalent-Set-Union' etc.
(defmethod compare (x (eqvs Equivalent-Set))
  "Returns `:less' or `:greater' if `x' is less than resp. greater than the
values in `eqvs'; or EITHER `:equal' or `:unequal' if `x' is equivalent to any
value in `eqvs'."
  (compare x (car (Equivalent-Set-Members eqvs))))

(defmethod compare ((eqvs Equivalent-Set) x)
  "Returns `:less' or `:greater' if the values in `eqvs' are less than resp.
greater than `x'; or EITHER `:equal' or `:unequal' if `x' is equivalent to
any value in `eqvs'."
  (compare (car (Equivalent-Set-Members eqvs)) x))

(defmethod compare ((eqvs1 Equivalent-Set) (eqvs2 Equivalent-Set))
  "Returns `:less' or `:greater' if the values in `eqvs1' are less than resp.
greater than those in `eqvs2'; returns EITHER `:equal' or `:unequal' if those
in `eqvs1' are equivalent to those in `eqvs2'."
  (compare (car (Equivalent-Set-Members eqvs1)) (car (Equivalent-Set-Members eqvs2))))


;;; ================================================================================
;;; ================================================================================
;;; Bags

(defstruct (WB-Bag-Tree-Node
	    (:constructor Make-Raw-WB-Bag-Tree-Node (Size Total-Count Value Count
						     Left Right))
	    (:predicate WB-Bag-Tree-Node?)
	    (:print-function WB-Bag-Tree-Node-Print))
  (Left  nil :type (or null WB-Bag-Tree-Node cons))
  (Right nil :type (or null WB-Bag-Tree-Node cons))
  ;; If we get equivalent values, then the `Value' is an `Equivalent-Bag', and the
  ;; `Count' is unused.
  Value				; the value at the node, or an `Equivalent-Bag'
  (Count 0 :type integer)	; the count (multiplicity) for this value
  (Total-Count 0 :type integer)	; total count of all values in this subtree
  (Size 0 :type fixnum))	; the number of < value, count > pairs

;;; A bag tree is either null, a node, or a cons of two simple-vectors.
(deftype WB-Bag-Tree ()
  '(or null WB-Bag-Tree-Node cons))


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

;;; That is, a bag whose domain members are equivalent.
(defstruct (Equivalent-Bag
	    (:constructor Make-Equivalent-Bag (Alist))
	    (:predicate Equivalent-Bag?))
  (Alist nil :type list))	; mapping equivalent values to their counts

(declaim (ftype (function (t) fixnum) Bag-Value-Size))

(defun Bag-Value-Size (value)
  "The number of values represented by `value', which can be more than 1 if
`key' is an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Bag? value)
      (length (Equivalent-Bag-Alist value))
    1))


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
	   (type WB-Bag-Tree tree))
  (cond ((null tree) 0)
	((consp tree) (reduce #'+ (cdr tree)))
	(t (WB-Bag-Tree-Node-Total-Count tree))))

(declaim (ftype (function (WB-Bag-Tree) integer) WB-Bag-Tree-Total-Count))


;;; This is just to get rid of compiler optimization notes.
(def-gmap-res-type :gen-sum (&key filterp)
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
				  (if (Equivalent-Bag? value)
				      (gmap :gen-sum #'cdr
					    (:list (Equivalent-Bag-Alist value)))
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
      (if (Equivalent-Bag? value)
	  (let ((alist (Equivalent-Bag-Alist value)))
	    (values (caar alist) (cdar alist)))
	(values value (WB-Bag-Tree-Node-Count tree))))))

(defun WB-Bag-Tree-Least-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one; the second value is the associated
count."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (let ((val count (WB-Bag-Tree-Minimum-Pair tree)))
    (if (Equivalent-Bag? val)
	(let ((pr (car (Equivalent-Bag-Alist val))))
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
	       (if (Equivalent-Bag? val)
		   (let ((alist (Equivalent-Bag-Alist val)))
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
	  (if (Equivalent-Bag? val)
	      (let ((pr (car (lastcons (Equivalent-Bag-Alist val)))))
		(values (car pr) (cdr pr)))
	    (values val (WB-Bag-Tree-Node-Count tree))))))))

(defun WB-Bag-Tree-Multiplicity (tree value)
  "Returns the multiplicity of `value' in `tree', or 0 if `value' does not
appear in `tree'.  As a second value, returns the value found, if any."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) (values 0 nil))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value)))
	   (if (eq found? ':equal)
	       (values (svref (cdr tree) idx) (svref (car tree) idx))
	     (values 0 nil))))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (Equivalent-Bag? node-val)
		  (let ((pr (assoc value (Equivalent-Bag-Alist node-val)
				   :test #'equal?)))
		    (if pr (values (cdr pr) (car pr))
		      (values 0 nil)))
		(if (eq comp ':equal)
		    (values (WB-Bag-Tree-Node-Count tree) node-val)
		  (values 0 nil))))
	     ((:less)
	      (WB-Bag-Tree-Multiplicity (WB-Bag-Tree-Node-Left tree) value))
	     ((:greater)
	      (WB-Bag-Tree-Multiplicity (WB-Bag-Tree-Node-Right tree) value)))))))

(defun WB-Bag-Tree-Find-Equivalent (tree value)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true, (second value) either the one value or an `Equivalent-Bag'
containing the values, and (third value) if the second value was a single
value, the corresponding count; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value)))
	   (and found?
		(values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-val (WB-Bag-Tree-Node-Value tree))
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val (WB-Bag-Tree-Node-Count tree)))
	     (:less
	       (WB-Bag-Tree-Find-Equivalent (WB-Bag-Tree-Node-Left tree) value))
	     (:greater
	       (WB-Bag-Tree-Find-Equivalent (WB-Bag-Tree-Node-Right tree) value)))))))

;;; ================================================================================
;;; With

(defun WB-Bag-Tree-With (tree value &optional (count 1))
  "Returns `tree' with `value' added with a count of `count' (if it was already
present, its count is incremented by `count').  `value' may be an `Equivalent-Bag'."
  ;; The case where `value' is an `Equivalent-Bag' is used by `WB-Bag-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree)
	 (if (not (Equivalent-Bag? value))
	     (cons (vector value) (vector count))
	   (Make-WB-Bag-Tree-Node value count nil nil)))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Bag', because
	   ;; this routine is called by `WB-Bag-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Bag? value)))
	       (cons (car tree)
		     (Vector-Update (cdr tree) idx (gen + (svref (cdr tree) idx)
							count)))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 *WB-Tree-Max-Vector-Length*)
		      (not (Equivalent-Bag? value)))
		 (cons (Vector-Insert (car tree) idx value)
		       (Vector-Insert (cdr tree) idx count))
	       (let ((new-val new-count
			(if found? (Equivalent-Bag-Sum (svref (car tree) idx)
						       (svref (cdr tree) idx)
						       value count)
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
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((new-val new-count
		       (Equivalent-Bag-Sum node-val node-count value count)))
		(Make-WB-Bag-Tree-Node new-val new-count
				       (WB-Bag-Tree-Node-Left tree)
				       (WB-Bag-Tree-Node-Right tree))))
	     ((:less)
	      (WB-Bag-Tree-Build-Node node-val node-count
				      (WB-Bag-Tree-With (WB-Bag-Tree-Node-Left tree)
							value count)
				      (WB-Bag-Tree-Node-Right tree)))
	     ((:greater)
	      (WB-Bag-Tree-Build-Node node-val node-count
				      (WB-Bag-Tree-Node-Left tree)
				      (WB-Bag-Tree-With (WB-Bag-Tree-Node-Right tree)
							value count))))))))


;;; ================================================================================
;;; Less

(defun WB-Bag-Tree-Less (tree value &optional (count 1))
  "Returns `tree' with the count for `value' decremented; if that count was
1, `value' is removed entirely."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree)
	   (type integer count))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) value)))
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
	       ((comp (compare value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((nonnull? value count
		      (Equivalent-Bag-Difference node-val node-count value count)))
		(if nonnull?
		    (Make-WB-Bag-Tree-Node value count
					   (WB-Bag-Tree-Node-Left tree)
					   (WB-Bag-Tree-Node-Right tree))
		  (WB-Bag-Tree-Join (WB-Bag-Tree-Node-Left tree)
				    (WB-Bag-Tree-Node-Right tree)))))
	     ((:less)
	      (let ((left (WB-Bag-Tree-Node-Left tree))
		    ((new-left (WB-Bag-Tree-Less left value))))
		(WB-Bag-Tree-Build-Node node-val node-count new-left
					(WB-Bag-Tree-Node-Right tree))))
	     ((:greater)
	      (let ((right (WB-Bag-Tree-Node-Right tree))
		    ((new-right (WB-Bag-Tree-Less right value))))
		(WB-Bag-Tree-Build-Node node-val node-count
					(WB-Bag-Tree-Node-Left tree)
					new-right))))))))


;;; ================================================================================
;;; Union, sum, intersection, and bag difference

(defun WB-Bag-Tree-Union (tree1 tree2)
  "Returns the union of `tree' and `tree2'."
  (if (eq tree1 tree2)
      tree1
    (WB-Bag-Tree-Union-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity)))

(defun WB-Bag-Tree-Union-Rng (tree1 tree2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (WB-Bag-Tree-Split tree1 lo hi))
	((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi))
	((null tree1)
	 (WB-Bag-Tree-Split tree2 lo hi))
	((and (consp tree1) (consp tree2))
	 (WB-Bag-Tree-Vector-Pair-Union tree1 tree2 lo hi))
	((consp tree1)
	 (WB-Bag-Tree-Union-Rng tree2 tree1 lo hi))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1))
		((val count (if eqvv2? (Equivalent-Bag-Union val1 count1 eqvv2 eqvc2)
			      (values val1 count1))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree1)
						      lo val1)
				    (WB-Bag-Tree-Trim tree2 lo val1)
				    lo val1)
	     (WB-Bag-Tree-Union-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree1)
						      val1 hi)
				    (WB-Bag-Tree-Trim tree2 val1 hi)
				    val1 hi))))))

(defun WB-Bag-Tree-Sum (tree1 tree2)
  "Returns the sum of `tree' and `tree2'."
  (WB-Bag-Tree-Sum-Rng tree1 tree2 Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Bag-Tree-Sum-Rng (tree1 tree2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi))
	((null tree1)
	 (WB-Bag-Tree-Split tree2 lo hi))
	((and (consp tree1) (consp tree2))
	 (WB-Bag-Tree-Vector-Pair-Sum tree1 tree2 lo hi))
	((consp tree1)
	 (WB-Bag-Tree-Sum-Rng tree2 tree1 lo hi))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1))
		((val count (if eqvv2? (Equivalent-Bag-Sum val1 count1 eqvv2 eqvc2)
			      (values val1 count1))))))
	   (WB-Bag-Tree-Concat
	     val count
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree1)
						    lo val1)
				  (WB-Bag-Tree-Trim tree2 lo val1)
				  lo val1)
	     (WB-Bag-Tree-Sum-Rng (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree1)
						    val1 hi)
				  (WB-Bag-Tree-Trim tree2 val1 hi)
				  val1 hi))))))


(defun WB-Bag-Tree-Intersect (tree1 tree2)
  (if (eq tree1 tree2)
      tree1
    (WB-Bag-Tree-Intersect-Rng tree1 tree2
			       Hedge-Negative-Infinity Hedge-Positive-Infinity)))

(defun WB-Bag-Tree-Intersect-Rng (tree1 tree2 lo hi)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (WB-Bag-Tree-Split tree1 lo hi))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Intersect tree1 tree2 lo hi))
	((consp tree1)
	 (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Trim tree2 lo hi)
				    tree1 lo hi))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Node-Left tree1)
					     (WB-Bag-Tree-Trim tree2 lo val1)
					     lo val1))
		(new-right
		  (WB-Bag-Tree-Intersect-Rng (WB-Bag-Tree-Node-Right tree1)
					     (WB-Bag-Tree-Trim tree2 val1 hi)
					     val1 hi)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1))
		((nonnull? value count
		   (and eqvv2? (Equivalent-Bag-Intersect val1 count1 eqvv2 eqvc2))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right)
	     (WB-Bag-Tree-Join new-left new-right))))))


(defun WB-Bag-Tree-Product (tree1 tree2)
  (WB-Bag-Tree-Product-Rng tree1 tree2
			   Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Bag-Tree-Product-Rng (tree1 tree2 lo hi)
  "Returns the Production of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Product tree1 tree2 lo hi))
	((consp tree1)
	 (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Trim tree2 lo hi)
				  tree1 lo hi))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Node-Left tree1)
					   (WB-Bag-Tree-Trim tree2 lo val1)
					   lo val1))
		(new-right
		  (WB-Bag-Tree-Product-Rng (WB-Bag-Tree-Node-Right tree1)
					   (WB-Bag-Tree-Trim tree2 val1 hi)
					   val1 hi)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1))
		((nonnull? value count
		   (and eqvv2? (Equivalent-Bag-Product val1 count1 eqvv2 eqvc2))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right)
	     (WB-Bag-Tree-Join new-left new-right))))))


(defun WB-Bag-Tree-Diff (tree1 tree2)
  "Returns the set difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (and (not (eq tree1 tree2))
       (WB-Bag-Tree-Diff-Rng tree1 tree2
			     Hedge-Negative-Infinity Hedge-Positive-Infinity)))

(defun WB-Bag-Tree-Diff-Rng (tree1 tree2 lo hi)
  "Returns the set difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((eq tree1 tree2) nil)		; historically-related-bag optimization
	((null tree1) nil)
	((null tree2)
	 (WB-Bag-Tree-Split tree1 lo hi))
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Bag-Diff tree1 tree2 lo hi))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2))
	       ((new-left (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Trim tree1 lo val2)
						(WB-Bag-Tree-Trim
						  (WB-Bag-Tree-Node-Left tree2)
						  lo val2)
						lo val2))
		(new-right (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Trim tree1 val2 hi)
						 (WB-Bag-Tree-Trim
						   (WB-Bag-Tree-Node-Right tree2)
						   val2 hi)
						 val2 hi)))
	       ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2))
		((nonnull? value count
		   (and eqvv1? (Equivalent-Bag-Difference eqvv1 eqvc1 val2 count2))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right)
	     (WB-Bag-Tree-Join new-left new-right))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1))
	       ((new-left (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Node-Left tree1)
						(WB-Bag-Tree-Trim tree2 lo val1)
						lo val1))
		(new-right (WB-Bag-Tree-Diff-Rng (WB-Bag-Tree-Node-Right tree1)
						 (WB-Bag-Tree-Trim tree2 val1 hi)
						 val1 hi)))
	       ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1))
		((nonnull? value count
		   (if eqvv2? (Equivalent-Bag-Difference val1 count1 eqvv2 eqvc2)
				  (values t val1 count1))))))
	   (if nonnull?
	       (WB-Bag-Tree-Concat value count new-left new-right)
	     (WB-Bag-Tree-Join new-left new-right))))))


;;; ================================================================================
;;; Comparison

(defun WB-Bag-Tree-Compare (tree1 tree2)
  (let ((size1 (WB-Bag-Tree-Size tree1))
	(size2 (WB-Bag-Tree-Size tree2)))
    (cond ((eq tree1 tree2) ':equal)
	  ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  (t (WB-Bag-Tree-Compare-Rng tree1 0 tree2 0 0 size1)))))

(defun WB-Bag-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related-bag optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (consp tree1) (consp tree2))
	 (let ((unequal? nil))
	   (or (gmap :or #'(lambda (val1 count1 val2 count2)
			     (let ((val-comp (compare val1 val2)))
			       (when (eq val-comp ':unequal)
				 (setq unequal? t))
			       (cond ((or (eq val-comp ':less) (eq val-comp ':greater))
				      val-comp)
				     ((< count1 count2) ':less)
				     ((> count1 count2) ':greater))))
		     (:simple-vector (car tree1) :start (- lo base1) :stop (- hi base1))
		     (:simple-vector (cdr tree1) :start (- lo base1) :stop (- hi base1))
		     (:simple-vector (car tree2) :start (- lo base2) :stop (- hi base2))
		     (:simple-vector (cdr tree2) :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((consp tree1)
	 (let ((rev-comp (WB-Bag-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi)))
	   (ecase rev-comp
	     (:less ':greater)
	     (:greater ':less)
	     ((:equal :unequal) rev-comp))))
	(t
	 (let ((left1 (WB-Bag-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Bag-Tree-Size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (WB-Bag-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Bag-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Bag-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (WB-Bag-Tree-Node-Value tree1))
		   (count1 (WB-Bag-Tree-Node-Count tree1))
		   (val2 count2
		      (WB-Bag-Tree-Rank-Pair-Internal
			tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (Equivalent-Bag-Compare val1 count1 val2 count2))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (Bag-Value-Size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (WB-Bag-Tree-Rank-Trim (WB-Bag-Tree-Node-Right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (WB-Bag-Tree-Rank-Trim tree2 base2 new-lo hi))
			 ((right-comp (WB-Bag-Tree-Compare-Rng right1a base1a tree2a
							       base2a new-lo hi))))))
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

(defun WB-Bag-Tree-Rank (tree value)
  "Searches a bag tree `tree' for `value'.  Returns two values, a boolean and an
index.  If `value', or a value equivalent to `value', is in `tree', the symbol
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `value' would have if it were to be added.  Note that
if the bag contains equivalent-but-unequal elements, the rank of each of several
such elements is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (labels ((rec (tree value base)
	     (cond ((null tree) (values nil base))
		   ((consp tree)
		    (let ((found? idx (Vector-Set-Binary-Search (car tree) value)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Bag-Tree-Node-Value tree))
			  (left (WB-Bag-Tree-Node-Left tree))
			  ((left-size (WB-Bag-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (compare value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Bag? node-val)
			     (let ((mems (Equivalent-Bag-Alist node-val))
				   ((pos (cl:position value mems :test #'equal?
						      :key #'car))))
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
    (if (Equivalent-Bag? elt)
	(let ((pr (nth rem (Equivalent-Bag-Alist elt))))
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

(defun WB-Bag-Tree-Subbag? (tree1 tree2)
  (let ((size1 (WB-Bag-Tree-Size tree1))
	(size2 (WB-Bag-Tree-Size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (WB-Bag-Tree-Subbag?-Rng tree1 tree2
				      Hedge-Negative-Infinity Hedge-Positive-Infinity)))))

(defun WB-Bag-Tree-Subbag?-Rng (tree1 tree2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree1 tree2))
  (cond ((null tree1) t)
	((eq tree1 tree2) t)		; historically-related-tree optimization
	((and (consp tree1) (or (null tree2) (consp tree2)))
	 (Vector-Pair-Bag-Subbag? tree1 tree2 lo hi))
	((consp tree1)
	 (let ((val2 (WB-Bag-Tree-Node-Value tree2))
	       (count2 (WB-Bag-Tree-Node-Count tree2)))
	   (and (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Trim tree1 lo val2)
					 (WB-Bag-Tree-Node-Left tree2)
					 lo val2)
		(let ((eqvv1? eqvv1 eqvc1 (WB-Bag-Tree-Find-Equivalent tree1 val2)))
		  (and (or (not eqvv1?)
			   (Equivalent-Bag-Subbag? eqvv1 eqvc1 val2 count2))
		       (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Trim tree1 val2 hi)
						(WB-Bag-Tree-Node-Right tree2)
						val2 hi))))))
	(t
	 (let ((val1 (WB-Bag-Tree-Node-Value tree1))
	       (count1 (WB-Bag-Tree-Node-Count tree1)))
	   (and (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Node-Left tree1)
					 (WB-Bag-Tree-Trim tree2 lo val1)
					 lo val1)
		(let ((eqvv2? eqvv2 eqvc2 (WB-Bag-Tree-Find-Equivalent tree2 val1)))
		  (and eqvv2?
		       (Equivalent-Bag-Subbag? val1 count1 eqvv2 eqvc2)
		       (WB-Bag-Tree-Subbag?-Rng (WB-Bag-Tree-Node-Right tree1)
						(WB-Bag-Tree-Trim tree2 val1 hi)
						val1 hi))))))))


;;; ================================================================================
;;; Miscellany

(defun WB-Bag-Tree-From-List (lst)
  (labels ((recur (lst n)
	     (cond ((= n 0) nil)
		   ((= n 1) (cons (vector (car lst)) (vector 1)))
		   (t
		    (let ((n2 (floor n 2)))
		      (WB-Bag-Tree-Sum (recur lst n2)
					 (recur (nthcdr n2 lst)
						(- n n2))))))))
    (recur lst (length lst))))


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
	   (if (Equivalent-Set? value)
	       (Make-Raw-WB-Bag-Tree-Node
		 size size
		 (Make-Equivalent-Bag (mapcar #'(lambda (x) (cons x 1))
					      (Equivalent-Set-Members value)))
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
	   (if (Equivalent-Bag? value)
	       (Make-Raw-WB-Set-Tree-Node
		 size (Make-Equivalent-Set (mapcar #'car (Equivalent-Bag-Alist value)))
		 new-left new-right)
	     (Make-Raw-WB-Set-Tree-Node size value new-left new-right))))))


;;; ================================================================================
;;; Support routines for the above (bags)

(defun WB-Bag-Tree-Split (tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((consp tree)
	 (let ((vals (the simple-vector (car tree)))
	       (counts (the simple-vector (cdr tree)))
	       ((len (length vals))
		((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				     0
				   (Vector-Set-Binary-Search-Lo vals lo)))
		 (split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi vals hi))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (Vector-Subseq vals split-point-lo split-point-hi)
			(Vector-Subseq counts split-point-lo split-point-hi))))))
	((not (or (eq lo Hedge-Negative-Infinity)
		  (greater-than? (WB-Bag-Tree-Node-Value tree) lo)))
	 (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Right tree) lo hi))
	((not (or (eq hi Hedge-Positive-Infinity)
		  (less-than? (WB-Bag-Tree-Node-Value tree) hi)))
	 (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Left tree) lo hi))
	(t
	 (let ((new-left (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity))
	       (new-right (WB-Bag-Tree-Split (WB-Bag-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi)))
	   (if (and (eq new-left (WB-Bag-Tree-Node-Left tree))
		    (eq new-right (WB-Bag-Tree-Node-Right tree)))
	       tree
	     (WB-Bag-Tree-Concat (WB-Bag-Tree-Node-Value tree)
				 (WB-Bag-Tree-Node-Count tree)
				 new-left new-right))))))

(defun WB-Bag-Tree-Trim (tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than? (svref (car tree)
					(1- (length (the simple-vector (car tree)))))
				 lo))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than? (svref (car tree) 0) hi))
	      tree))
	(t
	 (let ((val (WB-Bag-Tree-Node-Value tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than? val lo))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than? val hi))
		   tree
		 (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Left tree) lo hi))
	     (WB-Bag-Tree-Trim (WB-Bag-Tree-Node-Right tree) lo hi))))))

(defun WB-Bag-Tree-Concat (value count left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree left right))
  (cond ((null left)
	 (WB-Bag-Tree-With right value count))
	((null right)
	 (WB-Bag-Tree-With left value count))
	((and (WB-Bag-Tree-Node? left)
	      (> (WB-Bag-Tree-Size left)
		 (* (WB-Bag-Tree-Size right) WB-Tree-Balance-Factor)))
	 (WB-Bag-Tree-Build-Node (WB-Bag-Tree-Node-Value left)
				 (WB-Bag-Tree-Node-Count left)
				 (WB-Bag-Tree-Node-Left left)
				 (WB-Bag-Tree-Concat value count
						     (WB-Bag-Tree-Node-Right left)
						     right)))
	((and (WB-Bag-Tree-Node? right)
	      (> (WB-Bag-Tree-Size right)
		 (* (WB-Bag-Tree-Size left) WB-Tree-Balance-Factor)))
	 (WB-Bag-Tree-Build-Node (WB-Bag-Tree-Node-Value right)
				 (WB-Bag-Tree-Node-Count right)
				 (WB-Bag-Tree-Concat value count left
						     (WB-Bag-Tree-Node-Left right))
				 (WB-Bag-Tree-Node-Right right)))
	(t
	 (WB-Bag-Tree-Build-Node value count left right))))

(defun WB-Bag-Tree-Join (left right)
  (if (null left) right
    (if (null right) left
      (let ((min-val min-count (WB-Bag-Tree-Minimum-Pair right)))
	(WB-Bag-Tree-Concat min-val min-count
			    left (WB-Bag-Tree-Less-Minimum right))))))

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

(defun WB-Bag-Tree-Less-Minimum (tree)
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
	  (WB-Bag-Tree-Concat (WB-Bag-Tree-Node-Value tree)
			      (WB-Bag-Tree-Node-Count tree)
			      (WB-Bag-Tree-Less-Minimum left)
			      (WB-Bag-Tree-Node-Right tree))
	(WB-Bag-Tree-Node-Right tree)))))

(defun WB-Bag-Tree-Build-Node (value count left right)
  "Constructs a `WB-Bag-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Bag-Tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (Equivalent-Bag? value))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  *WB-Tree-Max-Vector-Length*))
	  (cons (concatenate 'simple-vector (car left) (vector value) (car right))
		(concatenate 'simple-vector (cdr left) (vector count) (cdr right)))
	(Make-WB-Bag-Tree-Node value count left right))
    (let ((sizl (WB-Bag-Tree-Size left))
	  (sizr (WB-Bag-Tree-Size right)))
      (cond ((and (WB-Bag-Tree-Node? left)
		  (> sizl (* sizr WB-Tree-Balance-Factor)))
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
		  (> sizr (* sizl WB-Tree-Balance-Factor)))
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


(defun WB-Bag-Tree-Verify (tree)
  (WB-Bag-Tree-Verify-Rng tree Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Bag-Tree-Verify-Rng (tree lo hi)
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len *WB-Tree-Max-Vector-Length*)
		(do ((i 0 (1+ i))
		     (prev Hedge-Negative-Infinity))
		    ((= i len)
		     (or (eq hi Hedge-Positive-Infinity)
			 (less-than? prev hi)))
		  (let ((elt (svref (car tree) i)))
		    (unless (and (not (Equivalent-Bag? elt))
				 (or (eq prev Hedge-Negative-Infinity)
				     (less-than? prev elt)))
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
		      (if (Equivalent-Bag? value)
			  (gmap :sum #'cdr (:list (Equivalent-Bag-Alist value)))
			(WB-Bag-Tree-Node-Count tree))))
		(or (not (Equivalent-Bag? value))
		    (> (length (Equivalent-Bag-Alist value)) 1))
		(or (<= sizr 4)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 4)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Bag-Tree-Verify-Rng (WB-Bag-Tree-Node-Left tree) lo value)
		(WB-Bag-Tree-Verify-Rng (WB-Bag-Tree-Node-Right tree) value hi))))))


;;; ================================================================================
;;; Vector pair bag operations

(defun WB-Bag-Tree-Vector-Pair-Union (pr1 pr2 lo hi)
  (let ((new-pr any-equivalent? (Vector-Pair-Bag-Union pr1 pr2 lo hi)))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i (length (car new-pr)))
	    (setq result (WB-Bag-Tree-With result (svref (car new-pr) i)
					   (svref (cdr new-pr) i))))
	  result)
      (if (> (length (car new-pr)) *WB-Tree-Max-Vector-Length*)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (Make-WB-Bag-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Bag-Union (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vals2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vals2 (1- len2)) hi)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (coerce (nreverse vals) 'simple-vector)
		       (coerce (nreverse counts) 'simple-vector))
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
		   ((comp (compare val1 val2))))
	       (ecase comp
		 (:equal
		  (push val1 vals)
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
					      val2 (svref counts2 i2))
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun WB-Bag-Tree-Vector-Pair-Sum (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
  (let ((new-pr any-equivalent? (Vector-Pair-Bag-Sum pr1 pr2 lo hi))
	((len (length (the simple-vector (car new-pr))))))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i len)
	    (setq result (WB-Bag-Tree-With result (svref (car new-pr) i)
					   (svref (cdr new-pr) i))))
	  result)
      (if (> len *WB-Tree-Max-Vector-Length*)
	  (let ((split-point (floor len 2)))
	    (Make-WB-Bag-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Bag-Sum (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vals2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vals2 (1- len2)) hi)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (coerce (nreverse vals) 'simple-vector)
		       (coerce (nreverse counts) 'simple-vector))
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
		   ((comp (compare val1 val2))))
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
					      val2 (svref counts2 i2))
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun Vector-Pair-Bag-Intersect (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vals2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vals2 (1- len2)) hi)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (coerce (nreverse vals) 'simple-vector)
			 (coerce (nreverse counts) 'simple-vector))))
      (let ((val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (compare val1 val2))))
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

(defun Vector-Pair-Bag-Product (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref vals2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref vals2 (1- len2)) hi)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (coerce (nreverse vals) 'simple-vector)
			 (coerce (nreverse counts) 'simple-vector))))
      (let ((val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (compare val1 val2))))
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

(defun Vector-Pair-Bag-Diff (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vals1 i1) vals)
	   (push (svref counts1 i1) counts)
	   (incf i1))
	 (and vals (cons (coerce (nreverse vals) 'simple-vector)
			 (coerce (nreverse counts) 'simple-vector))))
      (let ((v1 (svref vals1 i1))
	    (v2 (svref vals2 i2))
	    ((comp (compare v1 v2))))
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

(defun Vector-Pair-Bag-Subbag? (pr1 pr2 lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2))
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
      (do () ((or (= i1 len1) (less-than? lo (svref vals1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref vals1 (1- len1)) hi)))
	(decf len1)))
    (do ()
	((or (= i1 len1) (= i2 len2))
	 (= i1 len1))
      (let ((v1 (svref vals1 i1))
	    (v2 (svref vals2 i2))
	    ((comp (compare v1 v2))))
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
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
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
			  (if (Equivalent-Bag? value)
			      (dolist (pr (Equivalent-Bag-Alist value))
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
	       (if (Equivalent-Bag? val)
		   (let ((alist (Equivalent-Bag-Alist val)))
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
	     (if (Equivalent-Bag? val)
		 (let ((alist (Equivalent-Bag-Alist val)))
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
	     (cond ((< idx (length (the simple-array (car node))))
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
	(incf (the fixnum (svref iter (1+ sp))))
	(WB-Bag-Tree-Pair-Iterator-Canonicalize iter)
	(if (consp node)
	    (values (svref (car node) idx) (svref (cdr node) idx) t)
	  (let ((val (WB-Bag-Tree-Node-Value node)))
	    (if (Equivalent-Bag? val)
		(let ((pr (nth (1- idx) (Equivalent-Bag-Alist val))))
		  (values (car pr) (cdr pr) t))
	      (values val (WB-Bag-Tree-Node-Count node) t))))))))

;;; ================================================================================
;;; Equivalent-Bag routines

(defun Equivalent-Bag-Sum (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (let ((alist1 (Equivalent-Bag-Alist val1)))
	(if (Equivalent-Bag? val2)
	    (let ((alist2 (copy-list (Equivalent-Bag-Alist val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
		  (if pr2
		      (progn (push (cons (car pr1) (gen + (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (Make-Equivalent-Bag result))
	  (let ((pr1 (assoc val2 alist1 :test #'equal?)))
	    (if pr1
		(Make-Equivalent-Bag (cons (cons val2 (gen + (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (Make-Equivalent-Bag (cons (cons val2 count2) alist1))))))
    (if (Equivalent-Bag? val2)
	(Equivalent-Bag-Sum val2 count2 val1 count1)
      (if (equal? val1 val2)
	  (values val1 (gen + count1 count2))
	(Make-Equivalent-Bag (list (cons val1 count1) (cons val2 count2)))))))

(defun Equivalent-Bag-Union (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (let ((alist1 (Equivalent-Bag-Alist val1)))
	(if (Equivalent-Bag? val2)
	    (let ((alist2 (copy-list (Equivalent-Bag-Alist val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
		  (if pr2
		      (progn (push (cons (car pr1) (gen max (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (Make-Equivalent-Bag result))
	  (let ((pr1 (assoc val2 alist1 :test #'equal?)))
	    (if pr1
		(Make-Equivalent-Bag (cons (cons val2 (gen max (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (Make-Equivalent-Bag (cons (cons val2 count2) alist1))))))
    (if (Equivalent-Bag? val2)
	(Equivalent-Bag-Union val2 count2 val1 count1)
      (if (equal? val1 val2)
	  (values val1 (gen max count1 count2))
	(Make-Equivalent-Bag (list (cons val1 count1) (cons val2 count2)))))))

(defun Equivalent-Bag-Intersect (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (let ((alist1 (Equivalent-Bag-Alist val1)))
	(if (Equivalent-Bag? val2)
	    (let ((alist2 (Equivalent-Bag-Alist val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
		  (when pr2
		    (push (cons (car pr1) (gen min (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (Make-Equivalent-Bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test #'equal?)))
	    (and pr1
		 (values t val2 (gen min (cdr pr1) count2))))))
    (if (Equivalent-Bag? val2)
	(let ((pr2 (assoc val1 (Equivalent-Bag-Alist val2) :test #'equal?)))
	  (and pr2 (values t val1 (gen min count1 (cdr pr2)))))
      (and (equal? val1 val2)
	   (values t val1 (gen min count1 count2))))))

(defun Equivalent-Bag-Product (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (let ((alist1 (Equivalent-Bag-Alist val1)))
	(if (Equivalent-Bag? val2)
	    (let ((alist2 (Equivalent-Bag-Alist val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
		  (when pr2
		    (push (cons (car pr1) (gen * (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (Make-Equivalent-Bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test #'equal?)))
	    (and pr1
		 (values t val2 (gen * (cdr pr1) count2))))))
    (if (Equivalent-Bag? val2)
	(let ((pr2 (assoc val1 (Equivalent-Bag-Alist val2) :test #'equal?)))
	  (and pr2 (values t val1 (gen * count1 (cdr pr2)))))
      (and (equal? val1 val2)
	   (values t val1 (gen * count1 count2))))))

(defun Equivalent-Bag-Difference (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (let ((alist1 (Equivalent-Bag-Alist val1))
	    (alist2 (if (Equivalent-Bag? val2) (Equivalent-Bag-Alist val2)
		      (list (cons val2 count2))))
	    (result nil))
	(dolist (pr1 alist1)
	  (let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
	    (cond ((null pr2)
		   (push pr1 result))
		  ((gen > (cdr pr1) (cdr pr2))
		   (push (cons (car pr1)
			       (gen - (cdr pr1) (cdr pr2)))
			 result)))))
	(cond ((null result) nil)
	      ((null (cdr result)) (values t (caar result) (cdar result)))
	      (t (values t (Make-Equivalent-Bag result)))))
    (if (Equivalent-Bag? val2)
	(let ((pr2 (assoc val1 (Equivalent-Bag-Alist val2) :test #'equal?)))
	  (cond ((null pr2)
		 (values t val1 count1))
		((gen > count1 (cdr pr2))
		 (values t val1 (gen - count1 (cdr pr2))))))
      (if (equal? val1 val2)
	  (and (gen > count1 count2) (values t val1 (gen - count1 count2)))
	(values t val1 count1)))))

(defun Equivalent-Bag-Subbag? (val1 count1 val2 count2)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (if (Equivalent-Bag? val1)
      (and (Equivalent-Bag? val2)
	   (let ((alist2 (Equivalent-Bag-Alist val2)))
	     (dolist (pr1 (Equivalent-Bag-Alist val1) t)
	       (let ((pr2 (assoc (car pr1) alist2 :test #'equal?)))
		 (unless (and pr2 (gen <= (cdr pr1) (cdr pr2)))
		   (return nil))))))
    (if (Equivalent-Bag? val2)
	(let ((pr2 (assoc val1 (Equivalent-Bag-Alist val2) :test #'equal?)))
	  (and pr2 (gen <= count1 (cdr pr2))))
      (and (equal? val1 val2)
	   (gen <= count1 count2)))))

(defun Equivalent-Bag-Compare (val1 count1 val2 count2)
  "Compares two pairs where the key of either or both may be an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2))
  (let ((comp (compare val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Bag? val1)
	  (if (Equivalent-Bag? val2)
	      (let ((mems1 (Equivalent-Bag-Alist val1))
		    (mems2 (Equivalent-Bag-Alist val2))
		    ((len1 (length mems1))
		     (len2 (length mems2))))
		(cond ((< len1 len2) ':greater)
		      ((> len1 len2) ':less)
		      ((cl:every #'(lambda (pr1)
				     (let ((pr2 (assoc (car pr1) mems2 :test #'equal?)))
				       (and pr2 (equal? (cdr pr1) (cdr pr2)))))
				 mems1)
		       ':equal)
		      (t
		       (let ((set1 (reduce #'WB-Set-Tree-With (mapcar #'cdr mems1)
					   :initial-value nil))
			     (set2 (reduce #'WB-Set-Tree-With (mapcar #'cdr mems2)
					   :initial-value nil))
			     ((comp (WB-Set-Tree-Compare set1 set2))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(cond ((Equivalent-Bag? val2)
	       ':greater)
	      ((gen < count1 count2) ':less)
	      ((gen > count1 count2) ':greater)
	      (t comp))))))

(defmethod compare (x (eqvs Equivalent-Bag))
  "Returns `:less' or `:greater' if `x' is less than resp. greater than the
values in `eqvs'; or EITHER `:equal' or `:unequal' if `x' is equivalent to any
value in `eqvs'."
  (compare x (caar (Equivalent-Bag-Alist eqvs))))

(defmethod compare ((eqvs Equivalent-Bag) x)
  "Returns `:less' or `:greater' if the values in `eqvs' are less than resp.
greater than `x'; or EITHER `:equal' or `:unequal' if `x' is equivalent to
any value in `eqvs'."
  (compare (caar (Equivalent-Bag-Alist eqvs)) x))

(defmethod compare ((eqvs1 Equivalent-Bag) (eqvs2 Equivalent-Bag))
  "Returns `:less' or `:greater' if the values in `eqvs1' are less than resp.
greater than those in `eqvs2'; returns EITHER `:equal' or `:unequal' if those
in `eqvs1' are equivalent to those in `eqvs2'."
  (compare (caar (Equivalent-Bag-Alist eqvs1)) (caar (Equivalent-Bag-Alist eqvs2))))

;;; ================================================================================
;;; ================================================================================
;;; Maps

(defstruct (WB-Map-Tree-Node
	    (:constructor Make-Raw-WB-Map-Tree-Node (Size Key Value
						     Left Right))
	    (:predicate WB-Map-Tree-Node?)
	    (:print-function WB-Map-Tree-Node-Print))
  (Left  nil :type (or null WB-Map-Tree-Node cons))
  (Right nil :type (or null WB-Map-Tree-Node cons))
  ;; If we get equivalent keys, then the `Key' is an `Equivalent-Map', and the
  ;; `Value' is unused.
  Key		; the domain value
  Value		; the range value
  (Size 0 :type fixnum))	; the number of < key, value > pairs

;;; A map tree is either null, a node, or a cons of two simple-vectors.
(deftype WB-Map-Tree ()
  '(or null WB-Map-Tree-Node cons))


(defun WB-Map-Tree-Node-Print (node stream depth)
  "Print function for `WB-Map-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#map-node<~;~D, ~S -> ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}~;>~:>"
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

;;; That is, a map whose domain members are equivalent.
(defstruct (Equivalent-Map
	    (:constructor Make-Equivalent-Map (Alist))
	    (:predicate Equivalent-Map?))
  (Alist nil :type list))	; mapping equivalent keys to their values

(declaim (ftype (function (t) fixnum) Map-Key-Size))

(defun Map-Key-Size (key)
  "The number of domain values represented by `key', which can be more than 1 if
`key' is an `Equivalent-Map'."
  (if (Equivalent-Map? key)
      (length (Equivalent-Map-Alist key))
    1))


(defun WB-Map-Tree-Size (tree)
  "The number of key/value pairs contained in this tree."
  (cond ((null tree) 0)
	((consp tree) (length (car tree)))
	(t (WB-Map-Tree-Node-Size tree))))

(declaim (ftype (function (WB-Map-Tree) fixnum) WB-Map-Tree-Size))


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
	   (if (Equivalent-Map? key)
	       (let ((pr (car (Equivalent-Map-Alist key))))
		 (values (car pr) (cdr pr)))
	     (values key (WB-Map-Tree-Node-Value tree)))))))

(defun WB-Map-Tree-Least-Pair (tree)
  "Assumes `tree' is nonempty.  Returns the least key and its value, or an
arbitrary least key and its value if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (let ((key val (WB-Map-Tree-Minimum-Pair tree)))
    (if (Equivalent-Map? key)
	(let ((pr (car (Equivalent-Map-Alist key))))
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
	       (if (Equivalent-Map? key)
		   (let ((alist (Equivalent-Map-Alist key)))
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
	  (if (Equivalent-Map? key)
	      (let ((pr (car (lastcons (Equivalent-Map-Alist key)))))
		(values (car pr) (cdr pr)))
	    (values key (WB-Map-Tree-Node-Value tree))))))))


;;; ================================================================================
;;; Lookup

(defun WB-Map-Tree-Lookup (tree key)
  "If `tree' contains a pair whose key is `key', returns two values, true and
the associated value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key)))
	   (and (eq found? ':equal)
		(values t (svref (cdr tree) idx)))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (compare key node-key))))
	   (ecase comp
	     ((:equal :unequal)
	       (if (Equivalent-Map? node-key)
		   (let ((pr (assoc key (Equivalent-Map-Alist node-key) :test #'equal?)))
		     (and pr (values t (cdr pr))))
		 (and (eq comp ':equal)
		      (values t (WB-Map-Tree-Node-Value tree)))))
	     (:less
	       (WB-Map-Tree-Lookup (WB-Map-Tree-Node-Left tree) key))
	     (:greater
	       (WB-Map-Tree-Lookup (WB-Map-Tree-Node-Right tree) key)))))))

(defun WB-Map-Tree-Find-Equivalent (tree key)
  "If `tree' contains one or more keys equivalent to `value', returns (first
value) true, (second value) either the one key or an `Equivalent-Map'
containing the values, and (third value) if the second value was a single
key, the corresponding value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key)))
	   (and found? (values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (compare key node-key))))
	   (ecase comp
	     ((:equal :unequal) (values t node-key (WB-Map-Tree-Node-Value tree)))
	     (:less
	       (WB-Map-Tree-Find-Equivalent (WB-Map-Tree-Node-Left tree) key))
	     (:greater
	       (WB-Map-Tree-Find-Equivalent (WB-Map-Tree-Node-Right tree) key)))))))


;;; ================================================================================
;;; Map-with

(defun WB-Map-Tree-With (tree key value)
  "Returns a new tree like `tree' but with the pair < `key', `value' > added,
shadowing any previous pair with the same key."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree)
	 (if (not (Equivalent-Map? key))
	     (cons (vector key) (vector value))
	   (Make-WB-Map-Tree-Node key nil nil nil)))
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `key' is an `Equivalent-Map', because
	   ;; this routine is called by `WB-Map-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (Equivalent-Map? key)))
	       (cons (car tree)
		     (Vector-Update (cdr tree) idx value))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 *WB-Tree-Max-Vector-Length*)
		      (not (Equivalent-Map? key)))
		 (cons (Vector-Insert (car tree) idx key)
		       (Vector-Insert (cdr tree) idx value))
	       (Make-WB-Map-Tree-Node (if found?
					  (Equivalent-Map-Union (svref (car tree) idx)
								(svref (cdr tree) idx)
								key value)
					key)
				      value
				      (and (> idx 0)
					   (cons (Vector-Subseq (car tree) 0 idx)
						 (Vector-Subseq (cdr tree) 0 idx)))
				      (and (< right-start (length (the simple-vector
								    (car tree))))
					   (cons (Vector-Subseq (car tree) right-start)
						 (Vector-Subseq (cdr tree)
								right-start))))))))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (compare key node-key))))
	   (ecase comp
	     ((:equal :unequal)
	      ;; Since we're probably updating the value anyway, we don't bother trying
	      ;; to figure out whether we can reuse the node.
	      (Make-WB-Map-Tree-Node (Equivalent-Map-Union node-key
							   (WB-Map-Tree-Node-Value tree)
							   key value)
				     value
				     (WB-Map-Tree-Node-Left tree)
				     (WB-Map-Tree-Node-Right tree)))
	     ((:less)
	      (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key tree)
				      (WB-Map-Tree-Node-Value tree)
				      (WB-Map-Tree-With (WB-Map-Tree-Node-Left tree)
							key value)
				      (WB-Map-Tree-Node-Right tree)))
	     ((:greater)
	      (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key tree)
				      (WB-Map-Tree-Node-Value tree)
				      (WB-Map-Tree-Node-Left tree)
				      (WB-Map-Tree-With (WB-Map-Tree-Node-Right tree)
							key value))))))))

(defun Vector-Update (vec idx val)
  "Returns a new vector like `vec' but with `val' at `idx'."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec))
	((new-vec (make-array len))))
    (dotimes (i len)
      (setf (svref new-vec i) (svref vec i)))
    (setf (svref new-vec idx) val)
    new-vec))


;;; ================================================================================
;;; Map-less

(defun WB-Map-Tree-Less (tree key)
  "Returns a new tree like `tree', but with any entry for `key' removed."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (Vector-Set-Binary-Search (car tree) key)))
	   (if (eq found? ':equal)
	       (and (> (length (the simple-vector (car tree))) 1)
		    (cons (Vector-Remove-At (car tree) idx)
			  (Vector-Remove-At (cdr tree) idx)))
	     tree)))
	(t
	 (let ((node-key (WB-Map-Tree-Node-Key tree))
	       ((comp (compare key node-key))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (Equivalent-Map? node-key))
		  (if (eq comp ':unequal)
		      tree
		    (WB-Map-Tree-Join (WB-Map-Tree-Node-Left tree)
				      (WB-Map-Tree-Node-Right tree)))
		(let ((key val (Equivalent-Map-Less node-key key)))
		  (if (eq key node-key)
		      tree
		    (WB-Map-Tree-Build-Node key val (WB-Map-Tree-Node-Left tree)
					    (WB-Map-Tree-Node-Right tree))))))
	     ((:less)
	      (let ((left (WB-Map-Tree-Node-Left tree))
		    ((new-left (WB-Map-Tree-Less left key))))
		(if (eq new-left left)
		    tree
		  (WB-Map-Tree-Build-Node node-key (WB-Map-Tree-Node-Value tree)
					  new-left (WB-Map-Tree-Node-Right tree)))))
	     ((:greater)
	      (let ((right (WB-Map-Tree-Node-Right tree))
		    ((new-right (WB-Map-Tree-Less right key))))
		(if (eq new-right right)
		    tree
		  (WB-Map-Tree-Build-Node node-key (WB-Map-Tree-Node-Value tree)
					  (WB-Map-Tree-Node-Left tree) new-right)))))))))


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

(defun WB-Map-Tree-Less-Minimum (tree)
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
	  (WB-Map-Tree-Concat (WB-Map-Tree-Node-Key tree)
			      (WB-Map-Tree-Node-Value tree)
			      (WB-Map-Tree-Less-Minimum left)
			      (WB-Map-Tree-Node-Right tree))
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
	       ((elt (if (Equivalent-Map? key)
			 (Make-Equivalent-Set (mapcar #'car (Equivalent-Map-Alist key)))
		       key))))
	   (Make-WB-Set-Tree-Node elt
				  (WB-Map-Tree-Domain (WB-Map-Tree-Node-Left tree))
				  (WB-Map-Tree-Domain (WB-Map-Tree-Node-Right tree)))))))


;;; ================================================================================
;;; Union, intersection, and map difference

(defun WB-Map-Tree-Union (tree1 tree2 val-fn)
  (WB-Map-Tree-Union-Rng tree1 tree2 val-fn
			 Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Union-Rng (tree1 tree2 val-fn lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn)
	   (type WB-Map-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (WB-Map-Tree-Split tree1 lo hi))
	((null tree2)
	 (WB-Map-Tree-Split tree1 lo hi))
	((null tree1)
	 (WB-Map-Tree-Split tree2 lo hi))
	((and (consp tree1) (consp tree2))
	 (WB-Map-Tree-Vector-Pair-Union tree1 tree2 val-fn lo hi))
	((consp tree1)
	 ;; Can't use the swap-trees trick here, as the operation is noncommutative.
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2))
		((key val (if eqvk1? (Equivalent-Map-Union eqvk1 eqvv1 key2 val2 val-fn)
			    (values key2 val2))))))
	   (WB-Map-Tree-Concat
	     key val
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Trim tree1 lo key2)
				    (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2)
						      lo key2)
				    val-fn lo key2)
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Trim tree1 key2 hi)
				    (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2)
						      key2 hi)
				    val-fn key2 hi))))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1))
		((key val (if eqvk2? (Equivalent-Map-Union key1 val1 eqvk2 eqvv2 val-fn)
			    (values key1 val1))))))
	   (WB-Map-Tree-Concat
	     key val
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Node-Left tree1)
				    (WB-Map-Tree-Trim tree2 lo key1)
				    val-fn lo key1)
	     (WB-Map-Tree-Union-Rng (WB-Map-Tree-Node-Right tree1)
				    (WB-Map-Tree-Trim tree2 key1 hi)
				    val-fn key1 hi))))))

(defun WB-Map-Tree-Intersect (tree1 tree2 val-fn)
  (WB-Map-Tree-Intersect-Rng tree1 tree2 val-fn
			     Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Intersect-Rng (tree1 tree2 val-fn lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn)
	   (type WB-Map-Tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (WB-Map-Tree-Split tree1 lo hi))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Intersect tree1 tree2 val-fn lo hi))
	((consp tree1)
	 ;; Can't use the swap-trees trick here, as the operation is noncommutative.
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2))
		((nonnull? key val
		   (and eqvk1? (Equivalent-Map-Intersect eqvk1 eqvv1 key2 val2 val-fn))))))
	   (WB-Map-Tree-Concat-Maybe
	     nonnull? key val
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Trim tree1 lo key2)
					(WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2)
							  lo key2)
					val-fn lo key2)
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Trim tree1 key2 hi)
					(WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2)
							  key2 hi)
					val-fn key2 hi))))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1))
		((nonnull? key val
		   (and eqvk2? (Equivalent-Map-Intersect key1 val1 eqvk2 eqvv2 val-fn))))))
	   (WB-Map-Tree-Concat-Maybe
	     nonnull? key val
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Node-Left tree1)
					(WB-Map-Tree-Trim tree2 lo key1)
					val-fn lo key1)
	     (WB-Map-Tree-Intersect-Rng (WB-Map-Tree-Node-Right tree1)
					(WB-Map-Tree-Trim tree2 key1 hi)
					val-fn key1 hi))))))


(defun WB-Map-Tree-Diff-2 (tree1 tree2)
  "Returns two values: one containing the pairs that are in `tree1' but not
`tree2', and the other containing the pairs that are in `tree2' but not
`tree1'."
  (WB-Map-Tree-Diff-2-Rng tree1 tree2
			  Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Diff-2-Rng (tree1 tree2 lo hi)
  (cond ((eq tree1 tree2)		; historically-related tree optimization
	 (values nil nil))
	((or (null tree1) (null tree2))
	 (values (WB-Map-Tree-Split tree1 lo hi)
		 (WB-Map-Tree-Split tree2 lo hi)))
	((and (consp tree1) (consp tree2))
	 (Vector-Pair-Diff-2 tree1 tree2 lo hi))
	((consp tree1)
	 (let ((key2 (WB-Map-Tree-Node-Key tree2))
	       (val2 (WB-Map-Tree-Node-Value tree2))
	       ((new-left-1 new-left-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim tree1 lo key2)
					  (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree2)
							    lo key2)
					  lo key2))
		(new-right-1 new-right-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim tree1 key2 hi)
					  (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree2)
							    key2 hi)
					  key2 hi)))
	       ((eqvk1? eqvk1 eqvv1 (WB-Map-Tree-Find-Equivalent tree1 key2))
		((nonnull1? diffk1 diffv1
		   (and eqvk1? (Equivalent-Map-Difference eqvk1 eqvv1 key2 val2)))
		 (nonnull2? diffk2 diffv2
		   (if eqvk1? (Equivalent-Map-Difference key2 val2 eqvk1 eqvv1)
		     (values t key2 val2))))))
	   (values (if nonnull1? (WB-Map-Tree-Concat diffk1 diffv1 new-left-1 new-right-1)
		     (WB-Map-Tree-Join new-left-1 new-right-1))
		   (if nonnull2? (WB-Map-Tree-Concat diffk2 diffv2 new-left-2 new-right-2)
		     (WB-Map-Tree-Join new-left-2 new-right-2)))))
	(t
	 (let ((key1 (WB-Map-Tree-Node-Key tree1))
	       (val1 (WB-Map-Tree-Node-Value tree1))
	       ((new-left-1 new-left-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree1)
							    lo key1)
					  (WB-Map-Tree-Trim tree2 lo key1)
					  lo key1))
		(new-right-1 new-right-2
		  (WB-Map-Tree-Diff-2-Rng (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree1)
							    key1 hi)
					  (WB-Map-Tree-Trim tree2 key1 hi)
					  key1 hi)))
	       ((eqvk2? eqvk2 eqvv2 (WB-Map-Tree-Find-Equivalent tree2 key1))
		((nonnull1? diffk1 diffv1
		   (if eqvk2? (Equivalent-Map-Difference key1 val1 eqvk2 eqvv2)
		     (values t key1 val1)))
		 (nonnull2? diffk2 diffv2
		   (and eqvk2? (Equivalent-Map-Difference eqvk2 eqvv2 key1 val1))))))
	   (values (if nonnull1? (WB-Map-Tree-Concat diffk1 diffv1 new-left-1 new-right-1)
		     (WB-Map-Tree-Join new-left-1 new-right-1))
		   (if nonnull2? (WB-Map-Tree-Concat diffk2 diffv2 new-left-2 new-right-2)
		     (WB-Map-Tree-Join new-left-2 new-right-2)))))))


;;; ================================================================================
;;; Restrict and restrict-not

(defun WB-Map-Tree-Restrict (map-tree set-tree)
  (WB-Map-Tree-Restrict-Rng map-tree set-tree
			    Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Restrict-Rng (map-tree set-tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree map-tree)
	   (type WB-Set-Tree set-tree))
  (cond ((or (null map-tree) (null set-tree))
	 nil)
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (Vector-Pair-Restrict map-tree set-tree lo hi)
	   (let ((raw-elt (WB-Set-Tree-Node-Value set-tree))
		 ((set-elt (if (Equivalent-Set? raw-elt)
			       (car (Equivalent-Set-Members raw-elt))
			     raw-elt))
		  ((new-left (WB-Map-Tree-Restrict-Rng
			       (WB-Map-Tree-Trim map-tree lo set-elt)
			       (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left set-tree)
						 lo set-elt)
			       lo set-elt))
		   (new-right (WB-Map-Tree-Restrict-Rng
				(WB-Map-Tree-Trim map-tree set-elt hi)
				(WB-Set-Tree-Trim (WB-Set-Tree-Node-Right set-tree)
						  set-elt hi)
				set-elt hi))
		   (eqvk? eqvk eqvv (WB-Map-Tree-Find-Equivalent map-tree set-elt)))))
	     (if (not eqvk?)
		 (WB-Map-Tree-Join new-left new-right)
	       (let ((rpr? rkey rval (Equivalent-Map-Restrict eqvk eqvv raw-elt)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right)
		   (WB-Map-Tree-Join new-left new-right)))))))
	(t
	 (let ((raw-key (WB-Map-Tree-Node-Key map-tree))
	       ((map-key (if (Equivalent-Map? raw-key) ; for benefit of `compare'
			     (caar (Equivalent-Map-Alist raw-key))
			   raw-key))
		((new-left (WB-Map-Tree-Restrict-Rng (WB-Map-Tree-Node-Left map-tree)
						     (WB-Set-Tree-Trim set-tree
								       lo map-key)
						     lo map-key))
		 (new-right (WB-Map-Tree-Restrict-Rng (WB-Map-Tree-Node-Right map-tree)
						      (WB-Set-Tree-Trim set-tree
									map-key hi)
						      map-key hi))
		 (eqvv? eqvv (WB-Set-Tree-Find-Equivalent set-tree map-key)))))
	   (if (not eqvv?)
	       (WB-Map-Tree-Join new-left new-right)
	     (let ((map-val (WB-Map-Tree-Node-Value map-tree))
		   ((rpr? rkey rval (Equivalent-Map-Restrict raw-key map-val eqvv))))
	       (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right)
		 (WB-Map-Tree-Join new-left new-right))))))))

(defun WB-Map-Tree-Restrict-Not (map-tree set-tree)
  (WB-Map-Tree-Restrict-Not-Rng map-tree set-tree
				Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Restrict-Not-Rng (map-tree set-tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree map-tree)
	   (type WB-Set-Tree set-tree))
  (cond ((null map-tree)
	 nil)
	((null set-tree)
	 (WB-Map-Tree-Split map-tree lo hi))
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (Vector-Pair-Restrict-Not map-tree set-tree lo hi)
	   (let ((raw-elt (WB-Set-Tree-Node-Value set-tree))
		 ((set-elt (if (Equivalent-Set? raw-elt)
			       (car (Equivalent-Set-Members raw-elt))
			     raw-elt))
		  ((new-left (WB-Map-Tree-Restrict-Not-Rng
			       (WB-Map-Tree-Trim map-tree lo set-elt)
			       (WB-Set-Tree-Trim (WB-Set-Tree-Node-Left set-tree)
						 lo set-elt)
			       lo set-elt))
		   (new-right (WB-Map-Tree-Restrict-Not-Rng
				(WB-Map-Tree-Trim map-tree set-elt hi)
				(WB-Set-Tree-Trim (WB-Set-Tree-Node-Right set-tree)
						  set-elt hi)
				set-elt hi))
		   (eqvk? eqvk eqvv (WB-Map-Tree-Find-Equivalent map-tree set-elt)))))
	     (if (not eqvk?)
		 (WB-Map-Tree-Join new-left new-right)
	       (let ((rpr? rkey rval (Equivalent-Map-Restrict-Not eqvk eqvv raw-elt)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right)
		   (WB-Map-Tree-Join new-left new-right)))))))
	(t
	 (let ((raw-key (WB-Map-Tree-Node-Key map-tree))
	       ((map-key (if (Equivalent-Map? raw-key)
			     (caar (Equivalent-Map-Alist raw-key))
			   raw-key))
		((new-left (WB-Map-Tree-Restrict-Not-Rng
			     (WB-Map-Tree-Node-Left map-tree)
			     (WB-Set-Tree-Trim set-tree lo map-key)
			     lo map-key))
		 (new-right (WB-Map-Tree-Restrict-Not-Rng
			      (WB-Map-Tree-Node-Right map-tree)
			      (WB-Set-Tree-Trim set-tree map-key hi)
			      map-key hi))
		 (eqvv? eqvv (WB-Set-Tree-Find-Equivalent set-tree map-key)))))
	   (let ((map-val (WB-Map-Tree-Node-Value map-tree)))
	     (if (not eqvv?)
		 (WB-Map-Tree-Concat raw-key map-val new-left new-right)
	       (let ((rpr? rkey rval
		       (Equivalent-Map-Restrict-Not raw-key map-val eqvv)))
		 (if rpr? (WB-Map-Tree-Concat rkey rval new-left new-right)
		   (WB-Map-Tree-Join new-left new-right)))))))))

;;; ================================================================================
;;; Compare

(defun WB-Map-Tree-Compare (tree1 tree2 &optional (val-fn #'compare))
  (let ((size1 (WB-Map-Tree-Size tree1))
	(size2 (WB-Map-Tree-Size tree2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  (t (WB-Map-Tree-Compare-Rng tree1 0 tree2 0 0 size1 val-fn)))))

(defun WB-Map-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi val-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi)
	   (type function val-fn))
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related-map optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (consp tree1) (consp tree2))
	 (let ((unequal? nil))
	   (or (gmap :or #'(lambda (key1 val1 key2 val2)
			     (let ((key-comp (compare key1 key2)))
			       (when (eq key-comp ':unequal)
				 (setq unequal? t))
			       (if (or (eq key-comp ':less) (eq key-comp ':greater))
				   key-comp
				 (let ((val-comp (funcall val-fn val1 val2)))
				   (when (eq val-comp ':unequal)
				     (setq unequal? t))
				   (and (or (eq val-comp ':less) (eq val-comp ':greater))
					val-comp)))))
		     (:simple-vector (car tree1) :start (- lo base1) :stop (- hi base1))
		     (:simple-vector (cdr tree1) :start (- lo base1) :stop (- hi base1))
		     (:simple-vector (car tree2) :start (- lo base2) :stop (- hi base2))
		     (:simple-vector (cdr tree2) :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((consp tree1)
	 (let ((rev-comp (WB-Map-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi val-fn)))
	   (ecase rev-comp
	     (:less ':greater)
	     (:greater ':less)
	     ((:equal :unequal) rev-comp))))
	(t
	 (let ((left1 (WB-Map-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Map-Tree-Size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (WB-Map-Tree-Rank-Trim left1 base1 lo new-hi))
		  (tree2a base2a (WB-Map-Tree-Rank-Trim tree2 base2 lo new-hi))
		  ((left-comp (WB-Map-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-hi val-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((key1 (WB-Map-Tree-Node-Key tree1))
		   (val1 (WB-Map-Tree-Node-Value tree1))
		   (key2 val2
		      (WB-Map-Tree-Rank-Pair-Internal
			tree2 (the fixnum (- new-hi base2))))
		   ((comp (Equivalent-Map-Compare key1 val1 key2 val2 val-fn))))
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
						     new-lo hi val-fn))))))
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

(defun WB-Map-Tree-Rank (tree key)
  "Searches a map tree `tree' for `key'.  Returns two values, a boolean and an
index.  If `key', or a value equivalent to `key', is in `tree', the boolean
is true, and the index is the rank of the value; otherwise, the boolean is false
and the index is the rank `key' would have if it were to be added.  Note that
if the map contains equivalent-but-unequal keys, the rank of each of several
such keys is guaranteed consistent only within the same tree (by `eq'), not
between equal trees."
  (labels ((rec (tree key base)
	     (cond ((null tree) (values nil base))
		   ((consp tree)
		    (let ((found? idx (Vector-Set-Binary-Search (car tree) key)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (WB-Map-Tree-Node-Key tree))
			  (left (WB-Map-Tree-Node-Left tree))
			  ((left-size (WB-Map-Tree-Size left))
			   ((node-base (+ base left-size))))
			  ((comp (compare key node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (Equivalent-Map? node-val)
			     (let ((prs (Equivalent-Map-Alist node-val))
				   ((pos (cl:position key prs :test #'equal?
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
    (if (Equivalent-Map? key)
	(let ((pr (nth rem (Equivalent-Map-Alist key))))
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
;;; Support routines for the above (maps)

(defun WB-Map-Tree-Split (tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((and (eq lo Hedge-Negative-Infinity) (eq hi Hedge-Positive-Infinity))
	 tree)
	((consp tree)
	 (let ((keys (car tree))
	       (vals (cdr tree))
	       ((len (length (the simple-vector keys)))
		((split-point-lo (if (eq lo Hedge-Negative-Infinity)
				     0
				   (Vector-Set-Binary-Search-Lo keys lo)))
		 (split-point-hi (if (eq hi Hedge-Positive-Infinity)
				     len
				   (Vector-Set-Binary-Search-Hi keys hi))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (Vector-Subseq keys split-point-lo split-point-hi)
			(Vector-Subseq vals split-point-lo split-point-hi))))))
	((not (or (eq lo Hedge-Negative-Infinity)
		  (greater-than? (WB-Map-Tree-Node-Key tree) lo)))
	 (WB-Map-Tree-Split (WB-Map-Tree-Node-Right tree) lo hi))
	((not (or (eq hi Hedge-Positive-Infinity)
		  (less-than? (WB-Map-Tree-Node-Key tree) hi)))
	 (WB-Map-Tree-Split (WB-Map-Tree-Node-Left tree) lo hi))
	(t
	 (let ((new-left (WB-Map-Tree-Split (WB-Map-Tree-Node-Left tree)
					    lo Hedge-Positive-Infinity))
	       (new-right (WB-Map-Tree-Split (WB-Map-Tree-Node-Right tree)
					     Hedge-Negative-Infinity hi)))
	   (if (and (eq new-left (WB-Map-Tree-Node-Left tree))
		    (eq new-right (WB-Map-Tree-Node-Right tree)))
	       tree
	     (WB-Map-Tree-Concat (WB-Map-Tree-Node-Key tree)
				 (WB-Map-Tree-Node-Value tree)
				 new-left new-right))))))

(defun WB-Map-Tree-Trim (tree lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo Hedge-Negative-Infinity)
		  (greater-than? (svref (car tree)
					(1- (length (the simple-vector (car tree)))))
				 lo))
	      (or (eq hi Hedge-Positive-Infinity)
		  (less-than? (svref (car tree) 0) hi))
	      tree))
	(t
	 (let ((key (WB-Map-Tree-Node-Key tree)))
	   (if (or (eq lo Hedge-Negative-Infinity)
		   (greater-than? key lo))
	       (if (or (eq hi Hedge-Positive-Infinity)
		       (less-than? key hi))
		   tree
		 (WB-Map-Tree-Trim (WB-Map-Tree-Node-Left tree) lo hi))
	     (WB-Map-Tree-Trim (WB-Map-Tree-Node-Right tree) lo hi))))))

(defun WB-Map-Tree-Concat-Maybe (pair? key value left right)
  (declare (optimize (speed 3) (safety 0)))
  (if pair? (WB-Map-Tree-Concat key value left right)
    (WB-Map-Tree-Join left right)))

(defun WB-Map-Tree-Concat (key value left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree left right))
  (cond ((null left)
	 (WB-Map-Tree-With right key value))
	((null right)
	 (WB-Map-Tree-With left key value))
	((and (WB-Map-Tree-Node? left)
	      (> (WB-Map-Tree-Node-Size left)
		 (* (WB-Map-Tree-Size right) WB-Tree-Balance-Factor)))
	 (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key left)
				 (WB-Map-Tree-Node-Value left)
				 (WB-Map-Tree-Node-Left left)
				 (WB-Map-Tree-Concat key value
						     (WB-Map-Tree-Node-Right left)
						     right)))
	((and (WB-Map-Tree-Node? right)
	      (> (WB-Map-Tree-Node-Size right)
		 (* (WB-Map-Tree-Size left) WB-Tree-Balance-Factor)))
	 (WB-Map-Tree-Build-Node (WB-Map-Tree-Node-Key right)
				 (WB-Map-Tree-Node-Value right)
				 (WB-Map-Tree-Concat key value left
						     (WB-Map-Tree-Node-Left right))
				 (WB-Map-Tree-Node-Right right)))
	(t
	 (WB-Map-Tree-Build-Node key value left right))))

(defun WB-Map-Tree-Join (left right)
  (if (null left) right
    (if (null right) left
      (let ((min-key min-val (WB-Map-Tree-Minimum-Pair right)))
	(WB-Map-Tree-Concat min-key min-val
			    left (WB-Map-Tree-Less-Minimum right))))))

(defun WB-Map-Tree-Build-Node (key value left right)
  "Constructs a `WB-Map-Tree', performing one rebalancing step if required.
`key' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Map-Tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (Equivalent-Map? key))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  *WB-Tree-Max-Vector-Length*))
	  (cons (concatenate 'simple-vector (car left) (vector key) (car right))
		(concatenate 'simple-vector (cdr left) (vector value) (cdr right)))
	(Make-WB-Map-Tree-Node key value left right))
    (let ((sizl (WB-Map-Tree-Size left))
	  (sizr (WB-Map-Tree-Size right)))
      (cond ((and (WB-Map-Tree-Node? left)
		  (> sizl (* sizr WB-Tree-Balance-Factor)))
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
		  (> sizr (* sizl WB-Tree-Balance-Factor)))
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


(defun WB-Map-Tree-Verify (tree)
  (WB-Map-Tree-Verify-Rng tree Hedge-Negative-Infinity Hedge-Positive-Infinity))

(defun WB-Map-Tree-Verify-Rng (tree lo hi)
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len *WB-Tree-Max-Vector-Length*)
		(do ((i 0 (1+ i))
		     (prev Hedge-Negative-Infinity))
		    ((= i len)
		     (or (eq hi Hedge-Positive-Infinity)
			 (less-than? prev hi)))
		  (let ((key (svref (car tree) i)))
		    (unless (and (not (Equivalent-Map? key))
				 (or (eq prev Hedge-Negative-Infinity)
				     (less-than? prev key)))
		      (return nil))
		    (setq prev key))))))
	(t
	 (let ((sizl (WB-Map-Tree-Size (WB-Map-Tree-Node-Left tree)))
	       (sizr (WB-Map-Tree-Size (WB-Map-Tree-Node-Right tree)))
	       (key (WB-Map-Tree-Node-Key tree)))
	   (and (= (WB-Map-Tree-Node-Size tree) (+ sizl sizr (Map-Key-Size key)))
		(or (not (Equivalent-Map? key))
		    (> (length (Equivalent-Map-Alist key)) 1))
		(or (<= sizr 4)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 4)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Map-Tree-Verify-Rng (WB-Map-Tree-Node-Left tree) lo key)
		(WB-Map-Tree-Verify-Rng (WB-Map-Tree-Node-Right tree) key hi))))))


(defun WB-Map-Tree-Vector-Pair-Union (pr1 pr2 val-fn lo hi)
  (let ((new-pr any-equivalent? (Vector-Pair-Union pr1 pr2 val-fn lo hi)))
    (if any-equivalent?
	(let ((tree nil))
	  ;; Let's just do it the stupid way -- it's not supposed to happen often.
	  (dotimes (i (length (car new-pr)))
	    (setq tree (WB-Map-Tree-With tree (svref (car new-pr) i)
					 (svref (cdr new-pr) i))))
	  tree)
      (if (> (length (car new-pr)) *WB-Tree-Max-Vector-Length*)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (Make-WB-Map-Tree-Node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (Vector-Subseq (car new-pr) 0 split-point)
					 (Vector-Subseq (cdr new-pr) 0 split-point))
				   (cons (Vector-Subseq (car new-pr) (1+ split-point))
					 (Vector-Subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun Vector-Pair-Union (pr1 pr2 val-fn lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function val-fn))
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
      (do () ((or (= i1 len1) (less-than? lo (svref keys1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref keys2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref keys1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref keys2 (1- len2)) hi)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (coerce (nreverse keys) 'simple-vector)
		       (coerce (nreverse vals) 'simple-vector))
		 any-equivalent?))
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
		   ((comp (compare key1 key2))))
	       (ecase comp
		 ((:equal)
		  (push key1 keys)
		  (push (funcall val-fn (svref vals1 i1) (svref vals2 i2))
			vals)
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
		  (push (Equivalent-Map-Union key1 (svref vals1 i1)
					      key2 (svref vals2 i2) val-fn)
			keys)
		  (push nil vals)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun Vector-Pair-Intersect (pr1 pr2 val-fn lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function val-fn))
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
      (do () ((or (= i1 len1) (less-than? lo (svref keys1 i1))))
	(incf i1)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref keys1 (1- len1)) hi)))
	(decf len1)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (coerce (nreverse keys) 'simple-vector)
			 (coerce (nreverse vals) 'simple-vector))))
      (let ((key1 (svref keys1 i1))
	    (key2 (svref keys2 i2))
	    ((comp (compare key1 key2))))
	(ecase comp
	  ((:equal)
	   (push key1 keys)
	   (push (funcall val-fn (svref vals1 i1) (svref vals2 i2)) vals)
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (incf i1)
	   (incf i2)))))))

(defun Vector-Pair-Diff-2 (pr1 pr2 lo hi)
  (let ((keys1 (the simple-vector (car pr1)))
	(vals1 (the simple-vector (cdr pr1)))
	(keys2 (the simple-vector (car pr2)))
	(vals2 (the simple-vector (cdr pr2)))
	(i1 0)
	(i2 0)
	((len1 (length keys1))
	 (len2 (length keys2))))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref keys1 i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref keys2 i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref keys1 (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref keys2 (1- len2)) hi)))
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
	 (values (and diff-1-keys (cons (coerce (nreverse diff-1-keys) 'simple-vector)
					(coerce (nreverse diff-1-vals) 'simple-vector)))
		 (and diff-2-keys (cons (coerce (nreverse diff-2-keys) 'simple-vector)
					(coerce (nreverse diff-2-vals) 'simple-vector)))))
      (let ((key1 (svref keys1 i1))
	    (key2 (svref keys2 i2))
	    (val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (compare key1 key2))))
	(ecase comp
	  ((:equal)
	   (unless (equal? val1 val2)
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

(defun Vector-Pair-Restrict (map-pr set-vec lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons map-pr)
	   (type simple-vector set-vec))
  (let ((map-keys (the simple-vector (car map-pr)))
	(map-vals (the simple-vector (cdr map-pr)))
	(i1 0)
	(i2 0)
	((len1 (length map-keys))
	 (len2 (length set-vec))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref map-keys i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref set-vec i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref map-keys (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref set-vec (1- len2)) hi)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (coerce (nreverse keys) 'simple-vector)
			 (coerce (nreverse vals) 'simple-vector))))
      (let ((k (svref map-keys i1))
	    (e (svref set-vec i2))
	    ((comp (compare k e))))
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

(defun Vector-Pair-Restrict-Not (map-pr set-vec lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type cons map-pr)
	   (type simple-vector set-vec))
  (let ((map-keys (the simple-vector (car map-pr)))
	(map-vals (the simple-vector (cdr map-pr)))
	(i1 0)
	(i2 0)
	((len1 (length map-keys))
	 (len2 (length set-vec))))
    (declare (type fixnum i1 i2 len1 len2))
    (unless (eq lo Hedge-Negative-Infinity)
      (do () ((or (= i1 len1) (less-than? lo (svref map-keys i1))))
	(incf i1))
      (do () ((or (= i2 len2) (less-than? lo (svref set-vec i2))))
	(incf i2)))
    (unless (eq hi Hedge-Positive-Infinity)
      (do () ((or (= i1 len1) (less-than? (svref map-keys (1- len1)) hi)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than? (svref set-vec (1- len2)) hi)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref map-keys i1) keys)
	   (push (svref map-vals i1) vals)
	   (incf i1))
	 (and keys (cons (coerce (nreverse keys) 'simple-vector)
			 (coerce (nreverse vals) 'simple-vector))))
      (let ((k (svref map-keys i1))
	    (e (svref set-vec i2))
	    ((comp (compare k e))))
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
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
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
			  (if (Equivalent-Map? key)
			      (dolist (pr (Equivalent-Map-Alist key))
				(,body-fn (car pr) (cdr pr)))
			    (,body-fn key (WB-Map-Tree-Node-Value tree))))
			(,recur-fn (WB-Map-Tree-Node-Right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))


(defun WB-Map-Tree-Compose (tree fn)
  (and tree
       (if (consp tree)
	   (cons (car tree)
		 (gmap (:vector :length (length (cdr tree)))
		       fn (:simple-vector (cdr tree))))
	 (let ((key (WB-Map-Tree-Node-Key tree))
	       (val (WB-Map-Tree-Node-Value tree))
	       (new-left (WB-Map-Tree-Compose (WB-Map-Tree-Node-Left tree) fn))
	       (new-right (WB-Map-Tree-Compose (WB-Map-Tree-Node-Right tree) fn)))
	   (if (Equivalent-Map? key)
	       (Make-WB-Map-Tree-Node
		 (Make-Equivalent-Map (mapcar (lambda (pr)
						(cons (car pr) (funcall fn (cdr pr))))
					      (Equivalent-Map-Alist key)))
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
	     (cond ((< idx (length (the simple-array (car node))))
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
	(incf (the fixnum (svref iter (1+ sp))))
	(WB-Map-Tree-Iterator-Canonicalize iter)
	(if (consp node)
	    (values (svref (car node) idx) (svref (cdr node) idx) t)
	  (let ((key (WB-Map-Tree-Node-Key node)))
	    (if (Equivalent-Map? key)
		(let ((pr (nth (1- idx) (Equivalent-Map-Alist key))))
		  (values (car pr) (cdr pr) t))
	      (values key (WB-Map-Tree-Node-Value node) t))))))))


;;; ================================================================================
;;; Equivalent-Map routines

(defun Equivalent-Map-Union (key1 val1 key2 val2
			     &optional (val-fn #'(lambda (v1 v2)
						   (declare (ignore v1))
						   v2)))
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
Returns one or more new key/value pairs in which the \"2\" pairs override
the \"1\" pairs.  If the result is a single pair, it's returned as two values;
otherwise one value is returned, which is an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn))
  (if (Equivalent-Map? key1)
      (if (Equivalent-Map? key2)
	  (let ((alist1 (Equivalent-Map-Alist key1))
		(alist2 (Equivalent-Map-Alist key2))
		((result nil)))
	    (declare (type list alist1 alist2))
	    (dolist (pr1 alist1)
	      (let ((pr2 (find (car pr1) alist2 :test #'equal? :key #'car)))
		(if pr2
		    (push (cons (car pr1) (funcall val-fn (cdr pr1) (cdr pr2)))
			  result)
		  (push pr1 result))))
	    (dolist (pr2 alist2)
	      (let ((pr1 (find (car pr2) alist1 :test #'equal? :key #'car)))
		(when (null pr1)
		  (push pr2 result))))
	    (Make-Equivalent-Map result))
	(let ((alist1 (Equivalent-Map-Alist key1))
	      ((pr1 (find key2 alist1 :test #'equal? :key #'car))))
	  (declare (type list alist1))
	  (when pr1
	    (setq alist1 (remove pr1 alist1))
	    (setq val2 (funcall val-fn (cdr pr1) val2)))
	  (Make-Equivalent-Map (cons (cons key2 val2) alist1))))
    (if (Equivalent-Map? key2)
	(let ((alist2 (Equivalent-Map-Alist key2))
	      ((pr2 (find key1 alist2 :test #'equal? :key #'car))))
	  (declare (type list alist2))
	  (when pr2
	    (setq alist2 (remove pr2 alist2))
	    (setq val1 (funcall val-fn val1 (cdr pr2))))
	  (Make-Equivalent-Map (cons (cons key1 val1) alist2)))
      (if (equal? key1 key2)
	  (values key1 (funcall val-fn val1 val2))
	(Make-Equivalent-Map (list (cons key1 val1) (cons key2 val2)))))))

(defun Equivalent-Map-Intersect (key1 val1 key2 val2 val-fn)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
If the intersection is nonnull, returns two or three values: if it is a
single pair, returns true, the key, and the value; if it is more than one
pair, returns true and an `Equivalent-Map' of the pairs.  If the intersection
is null, returns false."
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn))
  (if (Equivalent-Map? key1)
      (if (Equivalent-Map? key2)
	  (let ((alist1 (Equivalent-Map-Alist key1))
		(alist2 (Equivalent-Map-Alist key2))
		(result nil))
	    (declare (type list alist1 alist2))
	    (dolist (pr1 alist1)
	      (let ((pr2 (cl:find (car pr1) alist2 :test #'equal? :key #'car)))
		(when pr2
		  (push (cons (car pr1) (funcall val-fn (cdr pr1) (cdr pr2))) result))))
	    (and result
		 (if (cdr result)
		     (values t (Make-Equivalent-Map result))
		   (values t (caar result) (cdar result)))))
	(let ((alist1 (Equivalent-Map-Alist key1))
	      ((pr1 (cl:find key2 alist1 :test #'equal? :key #'car))))
	  (declare (type list alist1))
	  (and pr1
	       (values t key2 (funcall val-fn (cdr pr1) val2)))))
    (if (Equivalent-Map? key2)
	(let ((alist2 (Equivalent-Map-Alist key2))
	      ((pr2 (cl:find key1 alist2 :test #'equal? :key #'car))))
	  (declare (type list alist2))
	  (and pr2
	       (values t key1 (funcall val-fn val1 (cdr pr2)))))
      (and (equal? key1 key2)
	   (values t key1 (funcall val-fn val1 val2))))))

(defun Equivalent-Map-Difference (key1 val1 key2 val2)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
If the difference is nonnull, returns two or three values: if it is a single
pair, returns true, the key, and the value; if it is more than one pair,
returns true and an `Equivalent-Map' of the pairs.  If the difference is
empty, returns false."
  (if (Equivalent-Map? key1)
      (let ((alist1 (Equivalent-Map-Alist key1)))
	(declare (type list alist1))
	(let ((alist2 (if (Equivalent-Map? key2) (Equivalent-Map-Alist key2)
			(list (cons key2 val2))))
	      (result nil))
	  (declare (type list alist2))
	  (dolist (pr1 alist1)
	    (let ((pr2 (cl:find (car pr1) alist2 :test #'equal? :key #'car)))
	      (when (or (null pr2) (not (equal? (cdr pr1) (cdr pr2))))
		(push pr1 result))))
	  (and result
	       (if (cdr result)
		   (values t (Make-Equivalent-Map result))
		 (values t (caar result) (cdar result))))))
    (if (Equivalent-Map? key2)
	(let ((alist2 (Equivalent-Map-Alist key2))
	      ((pr2 (cl:find key1 alist2 :test #'equal? :key #'car))))
	  (declare (type list alist2))
	  (and (or (null pr2) (not (equal? val1 (cdr pr2))))
	       (values t key1 val1)))
      (and (or (not (equal? key1 key2)) (not (equal? val1 val2)))
	   (values t key1 val1)))))

(defun Equivalent-Map-Less (eqvm key)
  "Removes the pair associated with `key' from `eqvm', an `Equivalent-Map'.  If
the result is a single pair, it's returned as two values; otherwise one value
is returned, which is an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0)))
  (let ((alist (Equivalent-Map-Alist eqvm))
	((pr (assoc key alist :test #'equal?))))
    (if pr
	(let ((result (cl:remove pr alist)))
	  (declare (type list result))
	  (if (= (length result) 1)
	      (values (caar result) (cdar result))
	    (Make-Equivalent-Map result)))
      eqvm)))

(defun Equivalent-Map-Restrict (key val set-elt)
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Map? key)
      (let ((alist1 (Equivalent-Map-Alist key))
	    (mems2 (if (Equivalent-Set? set-elt) (Equivalent-Set-Members set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if-not #'(lambda (pr)
					    (member (car pr) mems2 :test #'equal?))
					alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (Make-Equivalent-Map result) nil)))))
    (if (Equivalent-Set? set-elt)
	(and (member key (Equivalent-Set-Members set-elt) :test #'equal?)
	     (values t key val))
      (and (equal? key set-elt)
	   (values t key val)))))

(defun Equivalent-Map-Restrict-Not (key val set-elt)
  (declare (optimize (speed 3) (safety 0)))
  (if (Equivalent-Map? key)
      (let ((alist1 (Equivalent-Map-Alist key))
	    (mems2 (if (Equivalent-Set? set-elt) (Equivalent-Set-Members set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if #'(lambda (pr)
					(member (car pr) mems2 :test #'equal?))
				    alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (Make-Equivalent-Map result) nil)))))
    (if (Equivalent-Set? set-elt)
	(and (not (member key (Equivalent-Set-Members set-elt) :test #'equal?))
	     (values t key val))
      (and (not (equal? key set-elt))
	   (values t key val)))))

(defun Equivalent-Map-Compare (key1 val1 key2 val2 val-fn)
  "Compares two pairs where the key of either or both may be an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn))
  (let ((comp (compare key1 key2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (Equivalent-Map? key1)
	  (if (Equivalent-Map? key2)
	      (let ((mems1 (Equivalent-Map-Alist key1))
		    (mems2 (Equivalent-Map-Alist key2))
		    ((len1 (length mems1))
		     (len2 (length mems2))))
		(cond ((< len1 len2) ':greater)
		      ((> len1 len2) ':less)
		      ((cl:every #'(lambda (pr1)
				     (let ((pr2 (assoc (car pr1) mems2 :test #'equal?)))
				       (and pr2 (equal? (cdr pr1) (cdr pr2)))))
				 mems1)
		       ':equal)
		      (t
		       (let ((set1 (reduce #'WB-Set-Tree-With (mapcar #'cdr mems1)
					   :initial-value nil))
			     (set2 (reduce #'WB-Set-Tree-With (mapcar #'cdr mems2)
					   :initial-value nil))
			     ((comp (WB-Set-Tree-Compare set1 set2))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(if (Equivalent-Map? key2)
	    ':greater
	  (let ((val-comp (funcall val-fn val1 val2)))
	    (if (not (eq val-comp ':equal)) val-comp comp)))))))

(defmethod compare (key (eqvm Equivalent-Map))
  "Returns `:less' or `:greater' if `key' is less than resp. greater than the
domain values in `eqvm'; or EITHER `:equal' or `:unequal' if `x' is equivalent
to any domain value in `eqvm'."
  (compare key (caar (Equivalent-Map-Alist eqvm))))

(defmethod compare ((eqvm Equivalent-Map) key)
  "Returns `:less' or `:greater' if the domain values in `eqvm' are less than
resp. greater than `key'; or EITHER `:equal' or `:unequal' if `key' is
equivalent to any domain value in `eqvm'."
  (compare (caar (Equivalent-Map-Alist eqvm)) key))

(defmethod compare ((eqvm1 Equivalent-Map) (eqvm2 Equivalent-Map))
  "Returns `:less' or `:greater' if the domain values in `eqvm1' are less than
resp. greater than those in `eqvm2'; or EITHER `:equal' or `:unequal' if those
in `eqvm1' are equivalent to those in `eqvm2'."
  (compare (caar (Equivalent-Map-Alist eqvm1)) (caar (Equivalent-Map-Alist eqvm2))))


;;; ================================================================================
;;; ================================================================================
;;; Sequences

;;; Sequence tree nodes have no associated value.
(defstruct (WB-Seq-Tree-Node
	    (:constructor Make-Raw-WB-Seq-Tree-Node (Size Left Right))
	    (:predicate WB-Seq-Tree-Node?)
	    (:print-function WB-Seq-Tree-Node-Print))
  (Left  nil :type (or null WB-Seq-Tree-Node simple-string simple-vector))
  (Right nil :type (or null WB-Seq-Tree-Node simple-string simple-vector))
  (Size 0 :type fixnum))

(deftype WB-Seq-Tree ()
  '(or null WB-Seq-Tree-Node
       simple-string simple-vector))


(defun Make-WB-Seq-Tree-Node (left right)
  "The low-level constructor for a sequence tree node."
  (Make-Raw-WB-Seq-Tree-Node (+ (WB-Seq-Tree-Size left) (WB-Seq-Tree-Size right))
			     left right))

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


(declaim (ftype (function (WB-Seq-Tree) fixnum) WB-Seq-Tree-Size))

(defun WB-Seq-Tree-Size (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree))
  (cond ((null tree) 0)
	((stringp tree) (length tree))
	((simple-vector-p tree) (length tree))
	(t (WB-Seq-Tree-Node-Size tree))))


(defun WB-Seq-Tree-Subscript (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree) nil)
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

(defun WB-Seq-Tree-Append (tree value)
  (WB-Seq-Tree-Insert tree (WB-Seq-Tree-Size tree) value))

;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-Insert (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree)
	 (if (characterp value) (string value)
	   (vector value)))
	((stringp tree)
	 (if (characterp value)
	     (if (< (length tree) *WB-Tree-Max-String-Length*)
		 (String-Insert tree idx value)
	       (if (< (* idx 2) (length tree))
		   (Make-WB-Seq-Tree-Node (String-Subseq-Insert tree 0 idx idx value)
					  (String-Subseq tree idx))
		 (Make-WB-Seq-Tree-Node (String-Subseq tree 0 idx)
					(String-Subseq-Insert tree idx (length tree)
							      0 value))))
	   (if (< (length tree) *WB-Tree-Max-Vector-Length*)
	       (Vector-Insert-From-String tree idx value)
	     (let ((left (and (> idx 0) (String-Subseq tree 0 idx)))
		   (right (and (< idx (length tree)) (String-Subseq tree idx))))
	       (declare (type (or simple-string null) left right))
	       (if (< (length-nv left) (length-nv right))
		   (Make-WB-Seq-Tree-Node (Vector-Insert (coerce left 'simple-vector)
							 idx value)
					  right)
		 (Make-WB-Seq-Tree-Node left (Vector-Insert (coerce right 'simple-vector)
							    0 value)))))))
	((simple-vector-p tree)
	 (if (< (length tree) *WB-Tree-Max-Vector-Length*)
	     (Vector-Insert tree idx value)
	   (if (< (* idx 2) (length tree))
	       (Make-WB-Seq-Tree-Node (Vector-Subseq-Insert tree 0 idx idx value)
				      (Vector-Subseq tree idx))
	     (Make-WB-Seq-Tree-Node (Vector-Subseq tree 0 idx)
				    (Vector-Subseq-Insert tree idx (length tree) 0
							  value)))))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree)))
	   (if (< idx left-size)
	       (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Insert left idx value)
				       right)
	     (WB-Seq-Tree-Build-Node left
				     (WB-Seq-Tree-Insert right (- idx left-size)
							 value)))))))

(defun String-Insert (str idx ch)
  "Returns a new string like `str' but with `ch' inserted at `idx'.  Careful --
does no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-str (make-string (1+ len)
			       :element-type #-FSet-Ext-Strings 'base-char
			       #+FSet-Ext-Strings (if (and (typep str 'base-string)
							   (typep ch 'base-char))
						      'base-char
						    'character)))))
    (dotimes (i idx)
      (setf (schar new-str i) (schar str i)))
    (setf (schar new-str idx) ch)
    (dotimes (i (- len idx))
      (setf (schar new-str (+ idx i 1))
	    (schar str (+ idx i))))
    new-str))

(defun Vector-Insert-From-String (str idx val)
  "Returns a new vector like `str' (a string) but with `val' inserted at `idx'.
Careful -- does no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-vec (make-array (1+ len)))))
    (dotimes (i idx)
      (setf (svref new-vec i) (schar str i)))
    (setf (svref new-vec idx) val)
    (dotimes (i (- len idx))
      (setf (svref new-vec (+ idx i 1))
	    (schar str (+ idx i))))
    new-vec))

;;; Specialized version should be faster than `cl:subseq'.
(defun String-Subseq (str start &optional (end (length str)))
  "Returns a subsequence of `str' between `start' and `end'.  Careful -- does
no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end))
  (let ((len (- end start))
	((new-str (make-string len
			       :element-type #-FSet-Ext-Strings 'base-char
			       #+FSet-Ext-Strings (if (typep str 'base-string)
						      'base-char
						    'character)))))
    (dotimes (i len)
      (setf (schar new-str i) (schar str (+ i start))))
    new-str))

(defun String-Subseq-Insert (str start end idx ch)
  "Takes the subsequence of `str' from `start' to `end', then at `idx' within
the result, inserts `ch', returning the new string."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end idx))
  (let ((len (- end start))
	((new-str (make-string (1+ len)
			       :element-type #-FSet-Ext-Strings 'base-char
			       #+FSet-Ext-Strings (if (and (typep str 'base-string)
							   (typep ch 'base-char))
						      'base-char
						    'character)))))
    (dotimes (i idx)
      (setf (schar new-str i) (schar str (+ i start))))
    (setf (schar new-str idx) ch)
    (dotimes (i (- len idx))
      (setf (schar new-str (+ idx i 1))
	    (schar str (+ idx i start))))
    new-str))

(defun Vector-Subseq-Insert (vec start end idx val)
  "Takes the subsequence of `vec' from `start' to `end', then at `idx' within
the result, inserts `val', returning the new vector."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum start end idx))
  (let ((len (- end start))
	((new-vec (make-array (1+ len)))))
    (dotimes (i idx)
      (setf (svref new-vec i) (svref vec (+ i start))))
    (setf (svref new-vec idx) val)
    (dotimes (i (- len idx))
      (setf (svref new-vec (+ idx i 1))
	    (svref vec (+ idx i start))))
    new-vec))


;;; We assume bounds checking has already been done.
(defun WB-Seq-Tree-Remove (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum idx))
  (cond ((null tree) nil)
	((stringp tree)
	 (String-Remove-At tree idx))
	((simple-vector-p tree)
	 (Vector-Remove-At tree idx))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree)))
	   (if (< idx left-size)
	       (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Remove left idx) right)
	     (WB-Seq-Tree-Build-Node left
				     (WB-Seq-Tree-Remove right (- idx left-size))))))))

(defun String-Remove-At (str idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-str (make-string (1- len)
			       :element-type #-FSet-Ext-Strings 'base-char
			       #+FSet-Ext-Strings (if (typep str 'base-string)
						      'base-char
						    'character)))))
    (dotimes (i idx)
      (setf (schar new-str i) (schar str i)))
    (dotimes (i (- len idx 1))
      (setf (schar new-str (+ idx i)) (schar str (+ idx i 1))))
    new-str))


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
	   (Vector-Update-From-String tree idx value)))
	((simple-vector-p tree)
	 (Vector-Update tree idx value))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree)))
	   (if (< idx left-size)
	       (Make-WB-Seq-Tree-Node (WB-Seq-Tree-With left idx value)
				      right)
	     (Make-WB-Seq-Tree-Node left
				    (WB-Seq-Tree-With right (- idx left-size) value)))))))

(defun String-Update (str idx ch)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-str (make-string len
			       :element-type #-FSet-Ext-Strings 'base-char
			       #+FSet-Ext-Strings (if (and (typep str 'base-string)
							   (typep ch 'base-char))
						      'base-char
						    'character)))))
    (dotimes (i len)
      (setf (schar new-str i) (schar str i)))
    (setf (schar new-str idx) ch)
    new-str))

(defun Vector-Update-From-String (str idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((new-vec (make-array len))))
    (dotimes (i len)
      (setf (svref new-vec i) (schar str i)))
    (setf (svref new-vec idx) value)
    new-vec))


;;; We assume bounds checking has already been done.
;;; &&& The Java code for `PureTreeList.subseq' does this a little better, I think.
(defun WB-Seq-Tree-Subseq (tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree)
	   (type fixnum start end))
  (cond ((or (null tree) (>= start end)) nil)
	((simple-vector-p tree)
	 (if (and (= start 0) (= end (length tree)))
	     tree
	   (Vector-Subseq tree start end)))
	((stringp tree)
	 (if (and (= start 0) (= end (length tree)))
	     tree
	   (String-Subseq tree start end)))
	(t
	 (let ((left (WB-Seq-Tree-Node-Left tree))
	       ((left-size (WB-Seq-Tree-Size left)))
	       (right (WB-Seq-Tree-Node-Right tree))
	       ((right-size (WB-Seq-Tree-Size right))
	        ((new-left (if (and (= start 0) (<= left-size end))
			       left
			     (WB-Seq-Tree-Subseq left start (min end left-size))))
		 (new-right (if (and (<= start left-size)
				     (= (+ left-size right-size) end))
				right
			      (WB-Seq-Tree-Subseq right
						  (max 0 (the fixnum (- start left-size)))
						  (- end left-size)))))))
	   (if (and (eq new-left left) (eq new-right right))
	       tree
	     (WB-Seq-Tree-Concat new-left new-right))))))


(defun WB-Seq-Tree-Reverse (tree)
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (cl:reverse tree))
	(t
	  (Make-WB-Seq-Tree-Node (WB-Seq-Tree-Reverse (WB-Seq-Tree-Node-Right tree))
				 (WB-Seq-Tree-Reverse (WB-Seq-Tree-Node-Left tree))))))


;;; ================================================================================
;;; Conversion to/from vectors

(defun WB-Seq-Tree-From-Vector (vec)
  (declare (optimize (speed 1) (safety 1))
	   (type vector vec))
  (and (> (length vec) 0)
       ;; We walk the vector left-to-right, breaking it up into nearly-equal-sized
       ;; subsequences, and combining those into a tree.
       (let ((len (length vec))
	     ((npieces (ceiling len (if (stringp vec)
					*WB-Tree-Max-String-Length*
				      *WB-Tree-Max-Vector-Length*)))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (base 0)
	      (stack nil))
	     ((= ipiece npieces)
	      (do () ((null (cdr stack)))
		(let ((right (pop stack))
		      (left (pop stack)))
		  (push (Make-WB-Seq-Tree-Node left right) stack)))
	      (car stack))
	   (declare (type fixnum ipiece base))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap :and #'base-char-p
				      (:vector vec :start base :stop (+ base piece-len)))
				(let ((str (make-string piece-len
							:element-type 'base-char)))
				  (dotimes (i piece-len)
				    (setf (schar str i) (aref vec (+ base i))))
				  str))
			       #+FSet-Ext-Strings
			       ((gmap :and #'(lambda (x) (typep x 'character))
				      (:vector vec :start base :stop (+ base piece-len)))
				(let ((str (make-string piece-len
							:element-type 'character)))
				  (dotimes (i piece-len)
				    (setf (char str i) (aref vec (+ base i))))
				  str))
			       ((simple-vector-p vec)
				(Vector-Subseq vec base (+ base piece-len)))
			       (t
				(cl:subseq vec base (+ base piece-len)))))))
	     (push piece stack)
	     (incf base piece-len)
	     ;; This is the clever part -- by looking at the number of low-order 1s in
	     ;; `ipiece', we know how many nodes to make.
	     (do ((i ipiece (ash i -1)))
		 ((evenp i))
	       (declare (type fixnum i))
	       (let ((right (pop stack))
		     (left (pop stack)))
		 (push (Make-WB-Seq-Tree-Node left right) stack))))))))

(defun WB-Seq-Tree-To-Vector (tree)
  (let ((result (make-array (WB-Seq-Tree-Size tree))))
    (labels ((fillr (tree result idx)
	       (declare (optimize (speed 3) (safety 0))
			(fixnum idx))
	       (cond ((stringp tree)
		      (dotimes (i (length (the simple-string tree)))
			(setf (svref result (+ idx i)) (schar tree i))))
		     ((simple-vector-p tree)
		      (dotimes (i (length tree))
			(setf (svref result (+ idx i)) (svref tree i))))
		     (t
		      (let ((left (WB-Seq-Tree-Node-Left tree)))
			(fillr left result idx)
			(fillr (WB-Seq-Tree-Node-Right tree)
			       result (+ idx (WB-Seq-Tree-Size left))))))))
      (fillr tree result 0)
      result)))

(defun WB-Seq-Tree-To-String (tree)
  (declare (optimize (speed 3) (safety 0)))
  (if (null tree) ""
    (labels ((element-type (tree)
	       (cond ((null tree) 'base-char)
		     ((vectorp tree)
		      (cond #+FSet-Ext-Strings
			    ((typep tree 'base-string) 'base-char)
			    #+FSet-Ext-Strings
			    ((stringp tree) 'character)
			    #-FSet-Ext-Strings
			    ((stringp tree) 'base-char)
			    (t
			     (error 'type-error
				    :datum (find-if-not #'characterp tree)
				    :expected-type 'character))))
		     (t
		      (let ((left (element-type (WB-Seq-Tree-Node-Left tree)))
			    (right (element-type (WB-Seq-Tree-Node-Right tree))))
			(declare (ignorable left right))
			(cond #+FSet-Ext-Strings
			      ((or (eq left 'character) (eq right 'character))
			       'character)
			      (t 'base-char)))))))
      (let ((elt-type (element-type tree)))
	(let ((result (make-string (WB-Seq-Tree-Size tree) :element-type elt-type)))
	  (labels ((fillr (tree result idx)
		     (declare (optimize (speed 3) (safety 0))
			      (fixnum idx))
		     (cond ((stringp tree)
			    (dotimes (i (length (the simple-string tree)))
			      (setf (schar result (+ idx i)) (schar tree i))))
			   (t
			    (let ((left (WB-Seq-Tree-Node-Left tree)))
			      (fillr left result idx)
			      (fillr (WB-Seq-Tree-Node-Right tree)
				     result (+ idx (WB-Seq-Tree-Size left))))))))
	    (fillr tree result 0)
	    result))))))


;;; ================================================================================
;;; Conversion to/from lists

(defun WB-Seq-Tree-From-List (lst)
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (and lst
       (let ((len (length lst))
	     ((npieces (ceiling len *WB-Tree-Max-Vector-Length*))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (do () ((null (cdr stack)))
		(let ((right (pop stack))
		      (left (pop stack)))
		  (push (Make-WB-Seq-Tree-Node left right) stack)))
	      (car stack))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap :and #'(lambda (x y) (declare (ignore y))
					       (typep x 'base-char))
				      (:list lst)
				      (:index 0 piece-len))
				(let ((str (make-string piece-len
							:element-type 'base-char)))
				  (dotimes (i piece-len)
				    (setf (schar str i) (pop lst)))
				  str))
			       #+FSet-Ext-Strings
			       ((gmap :and #'(lambda (x y) (declare (ignore y))
					       (typep x 'character))
				      (:list lst)
				      (:index 0 piece-len))
				(let ((str (make-string piece-len
							:element-type 'character)))
				  (dotimes (i piece-len)
				    (setf (char str i) (pop lst)))
				  str))
			       (t
				(let ((vec (make-array piece-len)))
				  (dotimes (i piece-len)
				    (setf (svref vec i) (pop lst)))
				  vec))))))
	     (push piece stack)
	     (do ((i ipiece (ash i -1)))
		 ((evenp i))
	       (declare (type fixnum i))
	       (let ((right (pop stack))
		     (left (pop stack)))
		 (push (Make-WB-Seq-Tree-Node left right) stack))))))))

(defun WB-Seq-Tree-To-List (tree)
  (declare (optimize (speed 3) (safety 0)))
  (if (or (null tree) (vectorp tree))
      (coerce tree 'list)
    ;; We carefully build the list from the right so this runs in linear time.
    (labels ((build (tree result)
	       (cond ((null tree) result)
		     ((vectorp tree)
		      (nconc (coerce tree 'list) result))
		     (t
		      (build (WB-Seq-Tree-Node-Left tree)
			     (build (WB-Seq-Tree-Node-Right tree) result))))))
      (build tree nil))))


;;; ================================================================================
;;; Compare

(defun WB-Seq-Tree-Compare (tree1 tree2)
  (let ((size1 (WB-Seq-Tree-Size tree1))
	(size2 (WB-Seq-Tree-Size tree2)))
    (cond ((< size1 size2) ':less)
	  ((> size1 size2) ':greater)
	  (t (WB-Seq-Tree-Compare-Rng tree1 0 tree2 0 0 size1)))))

(defun WB-Seq-Tree-Compare-Lexicographically (tree1 tree2)
  (let ((size1 (WB-Seq-Tree-Size tree1))
	(size2 (WB-Seq-Tree-Size tree2)))
    (let ((comp (WB-Seq-Tree-Compare-Rng tree1 0 tree2 0 0 (min size1 size2))))
      (cond ((or (eq comp ':less) (eq comp ':greater))
	     comp)
	    ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t comp)))))

(defun WB-Seq-Tree-Compare-Rng (tree1 base1 tree2 base2 lo hi)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree tree1 tree2)
	   (type fixnum base1 base2 lo hi))
  (cond ((and (eq tree1 tree2) (= base1 base2))	; historically-related seq optimization
	 ':equal)
	((= lo hi) ':equal)
	((and (stringp tree1) (stringp tree2))
	 (or (gmap :or #'(lambda (ch1 ch2)
			   (cond ((char< ch1 ch2) ':less)
				 ((char> ch1 ch2) ':greater)))
		   (:simple-string tree1 :start (- lo base1) :stop (- hi base1))
		   (:simple-string tree2 :start (- lo base2) :stop (- hi base2)))
	     ':equal))
	((and (vectorp tree1) (vectorp tree2))
	 (let ((unequal? nil))
	   (or (gmap :or #'(lambda (val1 val2)
			     (let ((comp (compare val1 val2)))
			       (when (eq comp ':unequal)
				 (setq unequal? t))
			       (and (or (eq comp ':less) (eq comp ':greater))
				    comp)))
		     ;; We're doing a CLOS dispatch on each pair anyway, so I don't
		     ;; think the `aref's matter much.
		     (:vector tree1 :start (- lo base1) :stop (- hi base1))
		     (:vector tree2 :start (- lo base2) :stop (- hi base2)))
	       (if unequal? ':unequal ':equal))))
	((or (stringp tree1) (simple-vector-p tree1))
	 (let ((rev-comp (WB-Seq-Tree-Compare-Rng tree2 base2 tree1 base1 lo hi)))
	   (ecase rev-comp
	     (:less ':greater)
	     (:greater ':less)
	     ((:equal :unequal) rev-comp))))
	(t
	 (let ((left1 (WB-Seq-Tree-Node-Left tree1))
	       ((left1-size (the fixnum (WB-Seq-Tree-Size left1)))
		((new-mid (the fixnum (+ base1 left1-size)))
		 ;; See the comment beginning `Subtlety:' in `WB-Set-Tree-Union-Rng'.
		 ((left1a base1a (WB-Seq-Tree-Trim left1 base1 lo new-mid))
		  (tree2a base2a (WB-Seq-Tree-Trim tree2 base2 lo new-mid))
		  ((left-comp (WB-Seq-Tree-Compare-Rng left1a base1a tree2a base2a
						       lo new-mid)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((right1a base1a (WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Right tree1)
						     new-mid new-mid hi))
		   (tree2a base2a (WB-Seq-Tree-Trim tree2 base2 new-mid hi))
		   ((right-comp (WB-Seq-Tree-Compare-Rng right1a base1a tree2a base2a
							 new-mid hi))))
	       (if (not (eq right-comp ':equal))
		   right-comp
		 left-comp)))))))


;;; ================================================================================
;;; Conversion to set

(defun WB-Seq-Tree-To-Set-Tree (tree)
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (Vector-Seq-To-Set tree 0 (length tree)))
	((stringp tree)
	 (String-Seq-To-Set tree 0 (length tree)))
	(t (WB-Set-Tree-Union (WB-Seq-Tree-To-Set-Tree (WB-Seq-Tree-Node-Left tree))
			      (WB-Seq-Tree-To-Set-Tree (WB-Seq-Tree-Node-Right tree))))))

(defun Vector-Seq-To-Set (vec lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum lo hi))
  (cond ((= lo hi) nil)			; (shouldn't happen)
	((= hi (1+ lo))
	 (vector (svref vec lo)))
	(t
	 (let ((mid (ash (+ lo hi) -1)))
	   (WB-Set-Tree-Union (Vector-Seq-To-Set vec lo mid)
			      (Vector-Seq-To-Set vec mid hi))))))

(defun String-Seq-To-Set (vec lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string vec)
	   (type fixnum lo hi))
  (cond ((= lo hi) nil)			; (shouldn't happen)
	((= hi (1+ lo))
	 (vector (schar vec lo)))
	(t
	 (let ((mid (ash (+ lo hi) -1)))
	   (WB-Set-Tree-Union (String-Seq-To-Set vec lo mid)
			      (String-Seq-To-Set vec mid hi))))))


;;; ================================================================================
;;; Support routines for the above (sequences)

(defun WB-Seq-Tree-Concat (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree left right))
  (cond ((null left) right)
	((null right) left)
	((and (WB-Seq-Tree-Node? left)
	      (> (WB-Seq-Tree-Size left)
		 (* (WB-Seq-Tree-Size right) WB-Tree-Balance-Factor)))
	 (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Node-Left left)
				 (WB-Seq-Tree-Concat (WB-Seq-Tree-Node-Right left)
						     right)))
	((and (WB-Seq-Tree-Node? right)
	      (> (WB-Seq-Tree-Size right)
		 (* (WB-Seq-Tree-Size left) WB-Tree-Balance-Factor)))
	 (WB-Seq-Tree-Build-Node (WB-Seq-Tree-Concat left (WB-Seq-Tree-Node-Left right))
				 (WB-Seq-Tree-Node-Right right)))
	(t
	 (WB-Seq-Tree-Build-Node left right))))


(defun WB-Seq-Tree-Build-Node (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type WB-Seq-Tree left right))
  (cond ((and (or (null left) (stringp left))
	      (or (null right) (stringp right))
	      (< (+ (length-nv left) (length-nv right)) *WB-Tree-Max-String-Length*))
	 (if (and left right)
	     (concatenate #-FSet-Ext-Strings 'base-string
			  #+FSet-Ext-Strings (if (and (typep left 'base-string)
						      (typep right 'base-string))
						 'base-string
					       'string)
			  (the string left) (the string right))
	   (or left right)))
	((and (or (null left) (simple-vector-p left))
	      (or (null right) (simple-vector-p right)))
	 (if (< (+ (length-nv left) (length-nv right)) *WB-Tree-Max-Vector-Length*)
	     (concatenate 'simple-vector left right)
	   (Make-WB-Seq-Tree-Node left right)))
	(t
	 (let ((sizl (WB-Seq-Tree-Size left))
	       (sizr (WB-Seq-Tree-Size right)))
	   (cond ((and (WB-Seq-Tree-Node? left)
		       (> sizl (* sizr WB-Tree-Balance-Factor)))
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
		       (> sizr (* sizl WB-Tree-Balance-Factor)))
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
  (if (or (null tree) (stringp tree) (simple-vector-p tree))
      (values tree base)
    (let ((node-rank (the fixnum
		       (+ base (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Left tree))))))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Left tree) base lo hi))
	(WB-Seq-Tree-Trim (WB-Seq-Tree-Node-Right tree) node-rank lo hi)))))


;;; ================================================================================
;;; Iteration primitives

(defmacro Do-WB-Seq-Tree-Members ((var tree-form &optional value-form) &body body)
  ;; You may ask, why do we do this with a macro rather than a mapper when
  ;; we're going to have a function call for every invocation of the body anyway?
  ;; First, this local call is faster, or should be, than a general funcall; and
  ;; second, some compilers may decide to inline `body-fn' if the body is small.
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
    `(block nil
       (labels ((,body-fn (,var) . ,body)
		(,recur-fn (tree)
		  (when tree
		    (cond ((stringp tree)
			   (dotimes (i (length (the simple-string tree)))
			     (,body-fn (schar tree i))))
			  ((simple-vector-p tree)
			   (dotimes (i (length tree))
			     (,body-fn (svref tree i))))
			  (t
			   (,recur-fn (WB-Seq-Tree-Node-Left tree))
			   (,recur-fn (WB-Seq-Tree-Node-Right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))

(defmacro Do-WB-Seq-Tree-Members-Gen ((var tree-form start-form end-form from-end-form
				       &optional value-form)
				      &body body)
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-"))
	(start-var (gensym "START-"))
	(end-var (gensym "END-"))
	(from-end-var (gensym "FROM-END-")))
    `(block nil		; for `return' inside `body'
       (let ((,start-var ,start-form)
	     (,end-var ,end-form)
	     (,from-end-var ,from-end-form))
	 (declare (type fixnum ,start-var ,end-var))
	 (labels ((,body-fn (,var) . ,body)
		  (,recur-fn (tree start end)
		     (declare (type fixnum start end))
		     (when tree
		       (cond ((stringp tree)
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
					      (,recur-fn left start left-size)
					      (,recur-fn right 0 (- end left-size)))
					  (progn
					    (,recur-fn right 0 (- end left-size))
					    (,recur-fn left start left-size)))
				      (,recur-fn left start end))
				  (,recur-fn right (- start left-size)
					     (- end left-size)))))))))
	   (let ((tree ,tree-form))
	     (,recur-fn tree
			(max 0 ,start-var)
			(min ,end-var (WB-Seq-Tree-Size tree))))))
       ,value-form)))


#|| L8R...
(defun WB-Seq-Tree-Image (tree fn)
  (cond ((stringp tree)
	 (let ((len (length (the simple-string tree)))
	       (first-val (funcall fn (schar tree 0)))
	       ;; Heuristic: if the image of elt 0 is a character, figure they're
	       ;; all likely to be characters.  If not, we'll switch.
	       ((result char-type
		  (cond ((typep first-val 'base-char)
			 (values (make-string len :element-type 'base-char)
				 'base-char))
			#+FSet-Ext-Strings
			((typep first-val 'character)
			 (values (make-string len :element-type 'character)
				 'character))
			(t (values (make-array len) nil))))))
	   (dotimes (i len)
	     (let ((val (if (= i 0) first-val (funcall fn (schar tree i)))))
	       (when (and char-type (> i 0)
			  ;; I suspect this will optimize much better than
			  ;; (typep val char-type).
			  (not (if (eq char-type 'base-char) (typep val 'base-char)
				 (typep val 'character))))
		 (let (())))
	       (if char-type
		   (setf (schar result i) val)
		 (setf (svref result i) val))))))))
||#


;;; ----------------
;;; Stateful iterator

(defun Make-WB-Seq-Tree-Iterator (tree)
  (let ((iter (Make-WB-Seq-Tree-Iterator-Internal tree)))
    (lambda (op)
      (ecase op
	(:get (WB-Seq-Tree-Iterator-Get iter))
	(:done? (WB-Seq-Tree-Iterator-Done? iter))
	(:more? (not (WB-Seq-Tree-Iterator-Done? iter)))))))

(defun Make-WB-Seq-Tree-Iterator-Internal (tree)
  (WB-Seq-Tree-Iterator-Canonicalize
    (Make-WB-Tree-Iterator tree (WB-Seq-Tree-Size tree) 2 nil)))

(defun WB-Seq-Tree-Iterator-Canonicalize (iter)
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
	    ((simple-string-p node)
	     (cond ((< idx (length node))
		    (return))
		   ((= sp 1)
		    (setf (svref iter 1) nil)
		    (return))
		   (t
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
	     (setf (svref iter sp) (WB-Seq-Tree-Node-Left node))
	     (setf (svref iter (1+ sp)) 0))
	    (t
	     ;; Tail recursion
	     (setf (svref iter sp) (WB-Seq-Tree-Node-Right node))
	     (setf (svref iter (1+ sp)) 0)))))
  iter)

(defun WB-Seq-Tree-Iterator-Done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun WB-Seq-Tree-Iterator-Get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx (svref iter (1+ sp)))))
    (declare (fixnum idx))
    (if (null node)
	(values nil nil)
      (progn
	(incf (the fixnum (svref iter (1+ sp))))
	(WB-Seq-Tree-Iterator-Canonicalize iter)
	(values (if (simple-string-p node) (schar node idx) (svref node idx)) t)))))


;;; ================================================================================
;;; Verifier

(defun WB-Seq-Tree-Verify (tree)
  (cond ((null tree) t)
	((stringp tree) t)
	((simple-vector-p tree) t)
	(t
	 (let ((sizl (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Left tree)))
	       (sizr (WB-Seq-Tree-Size (WB-Seq-Tree-Node-Right tree))))
	   (and (= (WB-Seq-Tree-Node-Size tree) (+ sizl sizr))
		;; We suppress the balance test if one side is smaller than 8
		;; here, instead of 4, because of `*WB-Tree-Max-String-Length*',
		;; which makes the trees appear less balanced.
		(or (<= sizr 8)
		    (<= sizl (* sizr WB-Tree-Balance-Factor)))
		(or (<= sizl 8)
		    (<= sizr (* sizl WB-Tree-Balance-Factor)))
		(WB-Seq-Tree-Verify (WB-Seq-Tree-Node-Left tree))
		(WB-Seq-Tree-Verify (WB-Seq-Tree-Node-Right tree)))))))

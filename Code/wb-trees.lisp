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

Also of interest: https://arxiv.org/abs/1602.02120v4

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
(defconstant wb-tree-max-vector-length 8)

;;; This is longer because characters are smaller (on SBCL, 1 byte for base-chars,
;;; 4 for extended-chars).  We could reasonably go to 32 for 1-byte chars.
;;; It is assumed that this is no more than twice `WB-Tree-Max-Vector-Length'.
(defconstant wb-tree-max-string-length 16)

;;; The factor by which one subtree may outweigh another.
;;; Ref: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/balancing-weightbalanced-trees/7281C4DE7E56B74F2D13F06E31DCBC5B
(defconstant wb-tree-balance-factor-delta 2.5)  ; for choosing whether to rotate
(defconstant wb-tree-balance-factor-gamma 1.5)  ; for choosing between single and double rotation
;;; I tried imposing `Delta' as the limit, but it's too stringent.  The big problem is `Equivalent-Node's:
;;; they're not allowed in leaf vectors, so they tend to distort the tree.  I believe they're rare
;;; enough in practice that this is of no consequence, but for testing purposes, I generate a lot
;;; of them, so I have to loosen the verification constraint back to what it used to be.  -- Turns
;;; out that the leaf vectors can cause a little balance slop even for seqs, which have no
;;; `Equivalent-Node's.  I tried 3, but it was still too tight.
(defconstant wb-tree-balance-factor-limit 4)    ; imposed by verification

;;; Even SBCL doesn't optimize (floor (* fixnum 5/2)) down to this.
(defsubst wb-tree-balance-delta-fn (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (the fixnum (+ (the fixnum (* x 2)) (ash x -1))))   ; 5/2 * x

(defsubst wb-tree-balance-gamma-fn (x)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum x))
  (the fixnum (+ x (ash x -1))))   ; 3/2 * x



;;; ================================================================================
;;; ================================================================================
;;; Sets

(declaim (inline make-raw-wb-set-tree-node))

(deftype wb-set-tree ()
  '(or null wb-set-tree-node simple-vector))

(defstruct (wb-set-tree-node
	    (:constructor make-raw-wb-set-tree-node (size value left right))
	    (:predicate wb-set-tree-node?)
	    (:print-function wb-set-tree-node-print))
  (left  nil :type wb-set-tree :read-only t)
  (right nil :type wb-set-tree :read-only t)
  (value nil :read-only t)		; normally the value at the node, but see `Equivalent-Node' below.
  (size 0 :type fixnum :read-only t))	; the number of members in this subtree

(defun wb-set-tree-node-print (node stream depth)
  "Print function for `WB-Set-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      ;; Fun with `format'!
      (format stream "~<#set-node<~;~D, ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (wb-set-tree-node-size node)
		    (wb-set-tree-node-value node)
		    (let ((sub (wb-set-tree-node-left node)))
		      (if (simple-vector-p sub)
			  (list t (coerce sub 'list))
			(list nil sub)))
		    (let ((sub (wb-set-tree-node-right node)))
		      (if (simple-vector-p sub)
			  (list t (coerce sub 'list))
			(list nil sub)))))
    (format stream "#set-node<...>")))

(declaim (inline make-equivalent-node))

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
(defstruct (equivalent-node
	    (:constructor make-equivalent-node (set? list))
	    (:predicate equivalent-node?))
  set?
  ;; If `set?' is true, this is just a list of values; otherwise, it is an alist.
  (list nil :type list :read-only t))

;;; Speeds up `typep' slightly.
(declaim #+sbcl (sb-ext:freeze-type equivalent-node))

(declaim (inline unwrap-equivalent-node))

;;; Custom comparison functions that can ever return `:unequal' must call this on each argument.
;;; Note that if one of the arguments is an `Equivalent-Node', the resulting comparison might
;;; return `:equal' or `:unequal' based on which value happened to be at the head of the list.
;;; Functions in this file that call the comparison function on values that could be
;;; `Equivalent-Node's must handle these cases identically; such code has more work to do anyway,
;;; such as calling `Equivalent-Set-Union' etc.
(defun unwrap-equivalent-node (x)
  (if (equivalent-node? x)
      (if (equivalent-node-set? x)
	  (car (equivalent-node-list x))
	(caar (equivalent-node-list x)))
    x))

(declaim (inline make-equivalent-set))

(defun make-equivalent-set (list)
  (make-equivalent-node t list))

(declaim (ftype (function (t) fixnum) set-value-size))

(defun set-value-size (value)
  "The number of members represented by `value', which can be more than 1 if
`value' is an `Equivalent-Node'."
  (declare (optimize (speed 3) (safety 0)))
  (if (equivalent-node? value)
      (length (equivalent-node-list value))
    1))


(declaim (ftype (function (wb-set-tree) fixnum) wb-set-tree-size))
(declaim (inline wb-set-tree-size))

(defun wb-set-tree-size (tree)
  "The number of members contained in this tree."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (cond ((null tree) 0)
	((simple-vector-p tree) (length tree))
	(t (wb-set-tree-node-size tree))))


(declaim (inline make-wb-set-tree-node))

(defun make-wb-set-tree-node (value left right)
  "The low-level constructor for a set tree node."
  (declare (optimize (speed 3) (safety 0)))
  (make-raw-wb-set-tree-node (the fixnum
			       (+ (wb-set-tree-size left) (wb-set-tree-size right)
				  (set-value-size value)))
			     value left right))


(defun wb-set-tree-arb (tree)
  "Selects an arbitrary member of the set.  Assumes `tree' is nonnull."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (cond ((null tree)
	 (error "`WB-Set-Tree-Arb' called on empty tree"))
	((simple-vector-p tree)
	 (svref tree 0))
	(t
	 (let ((value (wb-set-tree-node-value tree)))
	   (if (equivalent-node? value)
	       (car (equivalent-node-list value))
	     value)))))

(defun wb-set-tree-least (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (let ((val (wb-set-tree-minimum-value tree)))
    (if (equivalent-node? val)
	(car (equivalent-node-list val))
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

(defun wb-set-tree-greatest (tree)
  "Assumes `tree' is nonempty.  Returns the greatest member, or an arbitrary
greatest member if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (if (simple-vector-p tree)
      (svref tree (1- (length tree)))
    (let ((right (wb-set-tree-node-right tree)))
      (if right
	  (wb-set-tree-greatest right)
	(let ((val (wb-set-tree-node-value tree)))
	  (if (equivalent-node? val)
	      (car (cl:last (equivalent-node-list val)))
	    val))))))

(defun wb-set-tree-contains? (tree value cmp-fn)
  "Returns true iff `value' is a member of `tree'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (eq (vector-set-binary-search tree value cmp-fn) ':equal))
	(t
	 (let ((node-val (wb-set-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal t)
	     ((:unequal)
	      (and (equivalent-node? node-val)
		   (member value (equivalent-node-list node-val) :test (equal?-fn cmp-fn))))
	     ((:less)
	      (wb-set-tree-contains? (wb-set-tree-node-left tree) value cmp-fn))
	     ((:greater)
	      (wb-set-tree-contains? (wb-set-tree-node-right tree) value cmp-fn)))))))

(defun wb-set-tree-find-equivalent (tree value cmp-fn)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true and (second value) either the one value or an `Equivalent-Set'
containing the values; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (vector-set-binary-search tree value cmp-fn)))
	   (and found? (values t (svref tree idx)))))
	(t
	 (let ((node-val (wb-set-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val))
	     ((:less)
	      (wb-set-tree-find-equivalent (wb-set-tree-node-left tree) value cmp-fn))
	     ((:greater)
	      (wb-set-tree-find-equivalent (wb-set-tree-node-right tree) value cmp-fn)))))))

(defun wb-set-tree-find-equal (tree value cmp-fn)
  "If `tree' contains a value equal to `value', returns (first value) true and
\(second value) the value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (vector-set-binary-search tree value cmp-fn)))
	   (and (eq found? ':equal)
		(values t (svref tree idx)))))
	(t
	 (let ((node-val (wb-set-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal (values t (if (equivalent-node? node-val)
				   (car (equivalent-node-list node-val))
				 node-val)))
	     (:unequal
	      (and (equivalent-node? node-val)
		   (let ((mem (member value (equivalent-node-list node-val) :test (equal?-fn cmp-fn))))
		     (and mem (values t (car mem))))))
	     ((:less)
	      (wb-set-tree-find-equal (wb-set-tree-node-left tree) value cmp-fn))
	     ((:greater)
	      (wb-set-tree-find-equal (wb-set-tree-node-right tree) value cmp-fn)))))))


;;; ================================================================================
;;; With

(declaim (ftype (function (simple-vector t function) (values t fixnum)) vector-set-binary-search))

(defun wb-set-tree-with (tree value cmp-fn)
  "If `value' is in `tree', returns `tree'; otherwise returns `tree' with
`value' added.  `value' may be an `Equivalent-Node'."
  ;; The case where `value' is an `Equivalent-Node' is used by `WB-Set-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree)
	 (if (not (equivalent-node? value))
	     (vector value)
	   (make-wb-set-tree-node value nil nil)))
	((simple-vector-p tree)
	 (let ((found? idx (vector-set-binary-search tree value cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Node', because
	   ;; this routine is called by `WB-Set-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (equivalent-node? value)))
	       tree
	     (if (and (not found?)
		      (< (length tree) wb-tree-max-vector-length)
		      (not (equivalent-node? value)))
		 (vector-insert tree idx value)
	       ;; Originally, I split the vector in half rather than at the point
	       ;; where `value' goes.  But in the not unlikely case where values
	       ;; are being inserted in order, this will give longer vectors.
	       (make-wb-set-tree-node (if found?
					  (equivalent-set-union (svref tree idx) value cmp-fn)
					value)
				      (and (> idx 0)
					   (vector-subseq tree 0 idx))
				      (and (< right-start (length tree))
					   (vector-subseq tree right-start)))))))
	(t
	 (let ((node-val (wb-set-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (and (not (equivalent-node? node-val)) (not (equivalent-node? value))
		       (eq comp ':equal))
		  tree
		(let ((eqvs (equivalent-set-union node-val value cmp-fn)))
		  (if (eq eqvs node-val)
		      tree
		    (make-wb-set-tree-node eqvs
					   (wb-set-tree-node-left tree)
					   (wb-set-tree-node-right tree))))))
	     ((:less)
	      (let ((left (wb-set-tree-node-left tree))
		    ((new-left (wb-set-tree-with left value cmp-fn))))
		(if (eq new-left left)
		    tree
		  (wb-set-tree-build-node node-val new-left
					  (wb-set-tree-node-right tree)))))
	     ((:greater)
	      (let ((right (wb-set-tree-node-right tree))
		    ((new-right (wb-set-tree-with right value cmp-fn))))
		(if (eq new-right right)
		    tree
		  (wb-set-tree-build-node node-val (wb-set-tree-node-left tree)
					  new-right)))))))))

(defun vector-insert (vec idx val)
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
(defun vector-subseq (vec start &optional (end (length vec)))
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
(defun wb-set-tree-less (tree value cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (let ((found? idx (vector-set-binary-search tree value cmp-fn)))
	   (if (eq found? ':equal)
	       (vector-remove-at tree idx)
	     tree)))
	(t
	 (let ((node-val (wb-set-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (equivalent-node? node-val))
		  (if (eq comp ':unequal)
		      tree
		    (wb-set-tree-join (wb-set-tree-node-left tree) (wb-set-tree-node-right tree) cmp-fn))
		(let ((ignore diff (equivalent-set-difference node-val value cmp-fn)))
		  (declare (ignore ignore))	; difference can't be null
		  (wb-set-tree-build-node diff
					  (wb-set-tree-node-left tree)
					  (wb-set-tree-node-right tree)))))
	     ((:less)
	      (let ((left (wb-set-tree-node-left tree))
		    ((new-left (wb-set-tree-less left value cmp-fn))))
		(if (eq new-left left)
		    tree
		  (wb-set-tree-build-node node-val new-left
					  (wb-set-tree-node-right tree)))))
	     ((:greater)
	      (let ((right (wb-set-tree-node-right tree))
		    ((new-right (wb-set-tree-less right value cmp-fn))))
		(if (eq new-right right)
		    tree
		  (wb-set-tree-build-node node-val (wb-set-tree-node-left tree)
					  new-right)))))))))

(defun vector-remove-at (vec idx)
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

(defconstant hedge-negative-infinity
  '|&*$ hedge negative infinity $*&|)

(defconstant hedge-positive-infinity
  '|&*$ hedge positive infinity $*&|)

(defun wb-set-tree-split-above (tree value cmp-fn)
  (wb-set-tree-split tree value hedge-positive-infinity cmp-fn))

(defun wb-set-tree-split-below (tree value cmp-fn)
  (wb-set-tree-split tree hedge-negative-infinity value cmp-fn))


;;; ================================================================================
;;; Union, intersection, and set difference

;;; Adams recommends using four versions of each of these routines, one for each
;;; boundedness case (no bound, a lower bound, an upper bound, or both).  He probably
;;; had to do that given that he was working in ML, but in Lisp it's easy to make
;;; up distinguished "negative infinity" and "positive infinity" values which, for
;;; all practical purposes, will never show up in sets.

(defun wb-set-tree-union (tree1 tree2 cmp-fn)
  "Returns the union of `tree1' and `tree2'.  Runs in time linear in the total
sizes of the two trees."
  (declare (type function cmp-fn))
  (if (eq tree1 tree2)
      tree1
    (wb-set-tree-union-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))

(defun wb-set-tree-union-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the union of `tree1' with `tree2', considering only those members
that are above `lo' and below `hi', and assuming that the root values of `tree1'
and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ;; If the sets are historically related -- one was produced by a sufficiently
	;; small number of `with' and `less' operations on the other, or they are both
	;; related in this way to a third set -- then we might get lucky and find
	;; ourselves with the same subtree on both sides.  This can reduce this
	;; linear-time algorithm to log-time.
	((eq tree1 tree2) (wb-set-tree-split tree1 lo hi cmp-fn))
	((null tree2)
	 (wb-set-tree-split tree1 lo hi cmp-fn))
	((null tree1)
	 (wb-set-tree-split tree2 lo hi cmp-fn))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (wb-set-tree-vector-union tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (wb-set-tree-node-value tree2))
	       ((eqvv1? eqvv1 (wb-set-tree-find-equivalent tree1 val2 cmp-fn))))
	   (wb-set-tree-concat (if eqvv1? (equivalent-set-union eqvv1 val2 cmp-fn)
				 val2)
			       (wb-set-tree-union-rng (wb-set-tree-trim tree1 lo val2 cmp-fn)
						      ;; We have to trim the children too, because their
						      ;; node values might be outside (lo, hi).
						      (wb-set-tree-trim (wb-set-tree-node-left tree2)
									lo val2 cmp-fn)
						      lo val2 cmp-fn)
			       (wb-set-tree-union-rng (wb-set-tree-trim tree1 val2 hi cmp-fn)
						      (wb-set-tree-trim (wb-set-tree-node-right tree2)
									val2 hi cmp-fn)
						      val2 hi cmp-fn)
			       cmp-fn)))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1))
	       ((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn))))
	   (wb-set-tree-concat (if eqvv2? (equivalent-set-union val1 eqvv2 cmp-fn)
				 val1)
			       (wb-set-tree-union-rng (wb-set-tree-node-left tree1)
						      (wb-set-tree-trim tree2 lo val1 cmp-fn)
						      lo val1 cmp-fn)
			       (wb-set-tree-union-rng (wb-set-tree-node-right tree1)
						      (wb-set-tree-trim tree2 val1 hi cmp-fn)
						      val1 hi cmp-fn)
			       cmp-fn)))))

(defun wb-set-tree-intersect (tree1 tree2 cmp-fn)
  "Returns the intersection of `tree1' and `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (declare (type function cmp-fn))
  (wb-set-tree-intersect-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-set-tree-intersect-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2)		; historically-related-set optimization
	 (wb-set-tree-split tree1 lo hi cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (vector-set-intersect tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (wb-set-tree-node-value tree2))
	       ((new-left
		  (wb-set-tree-intersect-rng (wb-set-tree-trim tree1 lo val2 cmp-fn)
					     (wb-set-tree-node-left tree2)
					     lo val2 cmp-fn))
		(new-right
		  (wb-set-tree-intersect-rng (wb-set-tree-trim tree1 val2 hi cmp-fn)
					     (wb-set-tree-node-right tree2)
					     val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (wb-set-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull? isect (and eqvv1? (equivalent-set-intersect eqvv1 val2 cmp-fn))))))
	   (if nonnull?
	       (wb-set-tree-concat isect new-left new-right cmp-fn)
	     (wb-set-tree-join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1))
	       ((new-left
		  (wb-set-tree-intersect-rng (wb-set-tree-node-left tree1)
					     (wb-set-tree-trim tree2 lo val1 cmp-fn)
					     lo val1 cmp-fn))
		(new-right
		  (wb-set-tree-intersect-rng (wb-set-tree-node-right tree1)
					     (wb-set-tree-trim tree2 val1 hi cmp-fn)
					     val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn))
		((nonnull? isect (and eqvv2? (equivalent-set-intersect val1 eqvv2 cmp-fn))))))
	   (if nonnull?
	       (wb-set-tree-concat isect new-left new-right cmp-fn)
	     (wb-set-tree-join new-left new-right cmp-fn))))))


(defun wb-set-tree-diff (tree1 tree2 cmp-fn)
  "Returns the set difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (declare (type function cmp-fn))
  (wb-set-tree-diff-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-set-tree-diff-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the set difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) nil)		; historically-related-set optimization
	((null tree1) nil)
	((null tree2)
	 (wb-set-tree-split tree1 lo hi cmp-fn))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (vector-set-diff tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (wb-set-tree-node-value tree2))
	       ((new-left (wb-set-tree-diff-rng (wb-set-tree-trim tree1 lo val2 cmp-fn)
						(wb-set-tree-trim (wb-set-tree-node-left tree2) lo val2 cmp-fn)
						lo val2 cmp-fn))
		(new-right (wb-set-tree-diff-rng (wb-set-tree-trim tree1 val2 hi cmp-fn)
						 (wb-set-tree-trim (wb-set-tree-node-right tree2) val2 hi cmp-fn)
						 val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (wb-set-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull? diff (and eqvv1? (equivalent-set-difference eqvv1 val2 cmp-fn))))))
	   (if nonnull?
	       (wb-set-tree-concat diff new-left new-right cmp-fn)
	     (wb-set-tree-join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1))
	       ((new-left (wb-set-tree-diff-rng (wb-set-tree-node-left tree1)
						(wb-set-tree-trim tree2 lo val1 cmp-fn)
						lo val1 cmp-fn))
		(new-right (wb-set-tree-diff-rng (wb-set-tree-node-right tree1)
						 (wb-set-tree-trim tree2 val1 hi cmp-fn)
						 val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn))
		((nonnull? diff (if eqvv2? (equivalent-set-difference val1 eqvv2 cmp-fn)
				  (values t val1))))))
	   (if nonnull?
	       (wb-set-tree-concat diff new-left new-right cmp-fn)
	     (wb-set-tree-join new-left new-right cmp-fn))))))


(defun wb-set-tree-diff-2 (tree1 tree2 cmp-fn)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1'.  Runs in time linear in the total sizes of the two trees."
  (wb-set-tree-diff-2-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-set-tree-diff-2-rng (tree1 tree2 lo hi cmp-fn)
  "Returns two values: the set difference of `tree1' less `tree2', and that of
`tree2' less `tree1', considering only those members that are above `lo' and
below `hi', and assuming that the root values of `tree1' and `tree2' are in
this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) (values nil nil)) ; historically-related tree optimization
	((or (null tree1) (null tree2))
	 (values (wb-set-tree-split tree1 lo hi cmp-fn)
		 (wb-set-tree-split tree2 lo hi cmp-fn)))
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (vector-set-diff-2 tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (wb-set-tree-node-value tree2))
	       ((new-left-1 new-left-2
		  (wb-set-tree-diff-2-rng (wb-set-tree-trim tree1 lo val2 cmp-fn)
					  (wb-set-tree-trim (wb-set-tree-node-left tree2) lo val2 cmp-fn)
					  lo val2 cmp-fn))
		(new-right-1 new-right-2
		  (wb-set-tree-diff-2-rng (wb-set-tree-trim tree1 val2 hi cmp-fn)
					  (wb-set-tree-trim (wb-set-tree-node-right tree2) val2 hi cmp-fn)
					  val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 (wb-set-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull1? diff1 (and eqvv1? (equivalent-set-difference eqvv1 val2 cmp-fn)))
		 (nonnull2? diff2 (if eqvv1? (equivalent-set-difference val2 eqvv1 cmp-fn)
				    (values t val2))))))
	   (values
	     (if nonnull1?
		 (wb-set-tree-concat diff1 new-left-1 new-right-1 cmp-fn)
	       (wb-set-tree-join new-left-1 new-right-1 cmp-fn))
	     (if nonnull2?
		 (wb-set-tree-concat diff2 new-left-2 new-right-2 cmp-fn)
	       (wb-set-tree-join new-left-2 new-right-2 cmp-fn)))))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1))
	       ((new-left-1 new-left-2
		  (wb-set-tree-diff-2-rng (wb-set-tree-node-left tree1)
					  (wb-set-tree-trim tree2 lo val1 cmp-fn)
					  lo val1 cmp-fn))
		(new-right-1 new-right-2
		  (wb-set-tree-diff-2-rng (wb-set-tree-node-right tree1)
					  (wb-set-tree-trim tree2 val1 hi cmp-fn)
					  val1 hi cmp-fn))
		((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn))
		 ((nonnull1? diff1 (if eqvv2? (equivalent-set-difference val1 eqvv2 cmp-fn)
				     (values t val1)))
		  (nonnull2? diff2 (and eqvv2? (equivalent-set-difference eqvv2 val1 cmp-fn)))))))
	   (values
	     (if nonnull1?
		 (wb-set-tree-concat diff1 new-left-1 new-right-1 cmp-fn)
	       (wb-set-tree-join new-left-1 new-right-1 cmp-fn))
	     (if nonnull2?
		 (wb-set-tree-concat diff2 new-left-2 new-right-2 cmp-fn)
	       (wb-set-tree-join new-left-2 new-right-2 cmp-fn)))))))


;;; ================================================================================
;;; Comparison

(defun wb-set-tree-compare (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((size1 (wb-set-tree-size tree1))
	  (size2 (wb-set-tree-size tree2)))
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t (wb-set-tree-compare-rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun wb-set-tree-compare-rng (tree1 base1 tree2 base2 lo hi cmp-fn)
  ;; This is similar to the other hedge algorithms, but there is a key difference:
  ;; it is concerned not with the values of nodes but with their rank, that is,
  ;; the number of values to their left.  The `base' parameters specify, for
  ;; each tree, the number of values to the left of the tree.
  ;; Another subtlety: we can return as soon as we get a comparison result of
  ;; ':less or ':greater, but ':unequal has to wait until the end.
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
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
	 (invert-comparison (wb-set-tree-compare-rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (wb-set-tree-node-left tree1))
	       ((left1-size (wb-set-tree-size left1))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ;; See the comment beginning `Subtlety:' in `WB-Set-Tree-Union-Rng'.
		 ((left1a base1a (wb-set-tree-rank-trim left1 base1 lo new-hi))
		  (tree2a base2a (wb-set-tree-rank-trim tree2 base2 lo new-hi))
		  ((left-comp (wb-set-tree-compare-rng left1a base1a tree2a base2a
						       lo new-hi cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (wb-set-tree-node-value tree1))
		   (val2 (wb-set-tree-rank-element-internal
			   tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (equivalent-set-compare val1 val2 cmp-fn))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (set-value-size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (wb-set-tree-rank-trim (wb-set-tree-node-right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (wb-set-tree-rank-trim tree2 base2 new-lo hi))
			 ((right-comp (wb-set-tree-compare-rng
					right1a base1a tree2a base2a new-lo hi cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal val-comp))))))))))

(defun wb-set-tree-rank-trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type fixnum base lo hi)
	   #+(or cmu scl)
	   (values wb-set-tree fixnum))
  (if (or (null tree) (simple-vector-p tree))
      (values tree base)
    (let ((node-rank (+ base (wb-set-tree-size (wb-set-tree-node-left tree)))))
      (declare (type fixnum node-rank))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (wb-set-tree-rank-trim (wb-set-tree-node-left tree) base lo hi))
	(wb-set-tree-rank-trim (wb-set-tree-node-right tree)
			       (+ node-rank
				  (set-value-size (wb-set-tree-node-value tree)))
			       lo hi)))))

(defun wb-set-tree-rank (tree value cmp-fn)
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
		    (let ((found? idx (vector-set-binary-search tree value cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (wb-set-tree-node-value tree))
			  (left (wb-set-tree-node-left tree))
			  ((left-size (wb-set-tree-size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall cmp-fn value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (equivalent-node? node-val)
			     (let ((mems (equivalent-node-list node-val))
				   ((pos (cl:position value mems :test (equal?-fn cmp-fn)))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left value base))
			((:greater)
			 (rec (wb-set-tree-node-right tree) value
			      (+ node-base (set-value-size node-val))))))))))
    (rec tree value 0)))

(defun wb-set-tree-rank-element (tree rank)
  (let ((elt rem (wb-set-tree-rank-element-internal tree rank)))
    (if (equivalent-node? elt)
	(nth rem (equivalent-node-list elt))
      elt)))

(defun wb-set-tree-rank-element-internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in set comparator"))
	((simple-vector-p tree)
	 (values (svref tree rank) 0))
	(t
	 (let ((left (wb-set-tree-node-left tree))
	       ((left-size (wb-set-tree-size left))))
	   (if (< rank left-size)
	       (wb-set-tree-rank-element-internal left rank)
	     (let ((val (wb-set-tree-node-value tree))
		   ((val-size (set-value-size val))
		    (rank (- rank left-size))))
	       (declare (type fixnum rank))
	       (if (< rank val-size)
		   (values val rank)
		 (wb-set-tree-rank-element-internal (wb-set-tree-node-right tree)
						    (- rank val-size)))))))))


;;; ================================================================================
;;; Subset testing

(defun wb-set-tree-subset? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (let ((size1 (wb-set-tree-size tree1))
	(size2 (wb-set-tree-size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (wb-set-tree-subset?-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))))

(defun wb-set-tree-subset?-rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) t)		; historically-related-set optimization
	((null tree1) t)
	((null tree2) nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (vector-set-subset? tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (let ((val2 (wb-set-tree-node-value tree2)))
	   (and (wb-set-tree-subset?-rng (wb-set-tree-trim tree1 lo val2 cmp-fn)
					 (wb-set-tree-node-left tree2)
					 lo val2 cmp-fn)
		(let ((eqvv1? eqvv1 (wb-set-tree-find-equivalent tree1 val2 cmp-fn)))
		  (and (or (not eqvv1?)
			   (equivalent-set-subset? eqvv1 val2 cmp-fn))
		       (wb-set-tree-subset?-rng (wb-set-tree-trim tree1 val2 hi cmp-fn)
						(wb-set-tree-node-right tree2)
						val2 hi cmp-fn))))))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1)))
	   (and (wb-set-tree-subset?-rng (wb-set-tree-node-left tree1)
					 (wb-set-tree-trim tree2 lo val1 cmp-fn)
					 lo val1 cmp-fn)
		(let ((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn)))
		  (and eqvv2?
		       (equivalent-set-subset? val1 eqvv2 cmp-fn)
		       (wb-set-tree-subset?-rng (wb-set-tree-node-right tree1)
						(wb-set-tree-trim tree2 val1 hi cmp-fn)
						val1 hi cmp-fn))))))))


;;; ================================================================================
;;; Disjointness testing

(defun wb-set-tree-disjoint? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (wb-set-tree-disjoint?-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-set-tree-disjoint?-rng (tree1 tree2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 t)
	((eq tree1 tree2)
	 nil)
	((and (simple-vector-p tree1) (simple-vector-p tree2))
	 (vector-set-disjoint? tree1 tree2 lo hi cmp-fn))
	((simple-vector-p tree1)
	 (wb-set-tree-disjoint?-rng (wb-set-tree-trim tree2 lo hi cmp-fn)
				    tree1 lo hi cmp-fn))
	(t
	 (let ((val1 (wb-set-tree-node-value tree1))
	       ((eqvv2? eqvv2 (wb-set-tree-find-equivalent tree2 val1 cmp-fn))))
	   (and (or (null eqvv2?) (equivalent-set-disjoint? val1 eqvv2 cmp-fn))
		(wb-set-tree-disjoint?-rng (wb-set-tree-node-left tree1)
					   (wb-set-tree-trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn)
		(wb-set-tree-disjoint?-rng (wb-set-tree-node-right tree1)
					   (wb-set-tree-trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn))))))

;;; ================================================================================
;;; Miscellany

(defun wb-set-tree-from-list (lst cmp-fn)
  (let ((tree nil))
    (dolist (x lst)
      (setq tree (wb-set-tree-with tree x cmp-fn)))
    tree))

(defun wb-set-tree-from-iterable (it cmp-fn)
  (declare (type function it))
  (let ((tree nil))
    (while (funcall it ':more?)
      (setq tree (wb-set-tree-with tree (funcall it ':get) cmp-fn)))
    tree))

;;; Much faster than repeated `with' if the input is sorted.  Still correct if the
;;; input is not actually sorted, but if it isn't even close to sorted, this is slower.
(defun wb-set-tree-from-sorted-iterable (it len cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function it cmp-fn)
	   (dynamic-extent it))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil hedge-positive-infinity hedge-negative-infinity))
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
			(:unequal (values (wb-set-tree-with (vector a) b cmp-fn) a a)))))
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
			  (values (wb-set-tree-build-node n2-elt left right) left-first right-last)
			;; Fall back to general case.
			(values (wb-set-tree-with (wb-set-tree-union left right cmp-fn) n2-elt cmp-fn)
				(if (less-than?-cmp left-first right-first cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last cmp-fn) right-last left-last))))))))
    (recur len)))


;;; ================================================================================
;;; Support routines for the above (sets)

(defun vector-set-binary-search (vec value cmp-fn)
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

(defun vector-set-binary-search-lo (vec lo cmp-fn)
  "Returns the index of the left edge of the first member of `vec' that is
above `lo'."
  (declare (type simple-vector vec)
	   (type function cmp-fn)
	   #+(or cmu sbcl scl)
	   (values fixnum))
  (let ((found? idx (vector-set-binary-search vec lo cmp-fn)))
    (if found? (1+ idx) idx)))

(defun vector-set-binary-search-hi (vec hi cmp-fn)
  "Returns the index of the right edge of the last member of `vec' that is
below `hi'."
  (declare (type simple-vector vec)
	   (type function cmp-fn)
	   #+(or cmu sbcl scl)
	   (values fixnum))
  (let ((found? idx (vector-set-binary-search vec hi cmp-fn)))
    (declare (ignore found?))
    idx))

(declaim (ftype (function (simple-vector t function) fixnum) vector-set-binary-search-lo))
(declaim (ftype (function (simple-vector t function) fixnum) vector-set-binary-search-hi))

(defun wb-set-tree-split (tree lo hi cmp-fn)
  "Corresponds to Adams' `split_lt' and `split_gt'.  Returns a tree containing
those members of `tree' above `lo' and below `hi'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo hedge-negative-infinity) (eq hi hedge-positive-infinity))
	 tree)
	((simple-vector-p tree)
	 (let ((len (length tree))
	       ((split-point-lo (if (eq lo hedge-negative-infinity)
				    0
				  (vector-set-binary-search-lo tree lo cmp-fn)))
		(split-point-hi (if (eq hi hedge-positive-infinity)
				     len
				   (vector-set-binary-search-hi tree hi cmp-fn)))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (vector-subseq tree split-point-lo split-point-hi)))))
	((and (not (eq lo hedge-negative-infinity))
	      (not (greater-than?-cmp (wb-set-tree-node-value tree) lo cmp-fn)))
	 (wb-set-tree-split (wb-set-tree-node-right tree) lo hi cmp-fn))
	((and (not (eq hi hedge-positive-infinity))
	      (not (less-than?-cmp (wb-set-tree-node-value tree) hi cmp-fn)))
	 (wb-set-tree-split (wb-set-tree-node-left tree) lo hi cmp-fn))
	(t
	 (let ((new-left (wb-set-tree-split (wb-set-tree-node-left tree)
					    lo hedge-positive-infinity cmp-fn))
	       (new-right (wb-set-tree-split (wb-set-tree-node-right tree)
					     hedge-negative-infinity hi cmp-fn)))
	   (if (and (eq new-left (wb-set-tree-node-left tree))
		    (eq new-right (wb-set-tree-node-right tree)))
	       tree
	     (wb-set-tree-concat (wb-set-tree-node-value tree) new-left new-right cmp-fn))))))

(defun wb-set-tree-trim (tree lo hi cmp-fn)
  "Corresponds to Adams' `trim' and variants.  Removes any tree nodes whose
values are less than `lo' or greater than `hi'.  Note, this does _not_ guarantee
that the result only contains values between `lo' and `hi'; use `-Split' for
that.  This, however, doesn't cons."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 ;; If the vector is completely out of range, drop it.
	 (and (or (eq lo hedge-negative-infinity)
		  (greater-than?-cmp (svref tree (1- (length tree))) lo cmp-fn))
	      (or (eq hi hedge-positive-infinity)
		  (less-than?-cmp (svref tree 0) hi cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo hedge-negative-infinity)
					0
				      (vector-set-binary-search-lo tree lo cmp-fn)))
		    (split-point-hi (if (eq hi hedge-positive-infinity)
					(length tree)
				      (vector-set-binary-search-hi tree hi cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((val (wb-set-tree-node-value tree)))
	   (if (or (eq lo hedge-negative-infinity)
		   (greater-than?-cmp val lo cmp-fn))
	       (if (or (eq hi hedge-positive-infinity)
		       (less-than?-cmp val hi cmp-fn))
		   tree
		 (wb-set-tree-trim (wb-set-tree-node-left tree) lo hi cmp-fn))
	     (wb-set-tree-trim (wb-set-tree-node-right tree) lo hi cmp-fn))))))

(defun wb-set-tree-concat (value left right cmp-fn)
  "Corresponds to Adams' `concat3'.  Assumes that (all values in `left') <=
`value' <= (all values in `right'); returns a new tree containing all values.
This does less rebalancing than `WB-Set-Tree-Build-Node', which otherwise
has the same contract.  `value' may be an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree left right))
  (cond ((null left)
	 (wb-set-tree-with right value cmp-fn))
	((null right)
	 (wb-set-tree-with left value cmp-fn))
	((and (wb-set-tree-node? left)
	      (> (1+ (wb-set-tree-size left))
		 (wb-tree-balance-delta-fn (1+ (wb-set-tree-size right)))))
	 (wb-set-tree-build-node (wb-set-tree-node-value left)
				 (wb-set-tree-node-left left)
				 (wb-set-tree-concat value (wb-set-tree-node-right left) right cmp-fn)))
	((and (wb-set-tree-node? right)
	      (> (1+ (wb-set-tree-size right))
		 (wb-tree-balance-delta-fn (1+ (wb-set-tree-size left)))))
	 (wb-set-tree-build-node (wb-set-tree-node-value right)
				 (wb-set-tree-concat value left (wb-set-tree-node-left right) cmp-fn)
				 (wb-set-tree-node-right right)))
	(t
	 (wb-set-tree-build-node value left right))))

(defun wb-set-tree-join (left right cmp-fn)
  "Returns the union of `left' and `right' under the assumption that all values
in `left' are less than any value in `right'."
  (if (null left) right
    (if (null right) left
      (let ((val (wb-set-tree-minimum-value right)))
	(wb-set-tree-concat val left (wb-set-tree-less-minimum right cmp-fn) cmp-fn)))))

(defun wb-set-tree-minimum-value (tree)
  "Assumes `tree' is nonempty.  Returns the minimum value.  This may be an
`Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (if (simple-vector-p tree)
      (svref tree 0)
    (let ((left (wb-set-tree-node-left tree)))
      (if left
	  (wb-set-tree-minimum-value left)
	(wb-set-tree-node-value tree)))))

(defun wb-set-tree-less-minimum (tree cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum value
or `Equivalent-Set' removed."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (if (simple-vector-p tree)
      (and (> (length (the simple-vector tree)) 1) (vector-subseq tree 1))
    (let ((left (wb-set-tree-node-left tree)))
      (if left
	  (wb-set-tree-concat (wb-set-tree-node-value tree) (wb-set-tree-less-minimum left cmp-fn)
			      (wb-set-tree-node-right tree) cmp-fn)
	(wb-set-tree-node-right tree)))))

(defun wb-set-tree-build-node (value left right)
  "Constructs a `WB-Set-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree left right))
  (cond ((and (or (null left) (simple-vector-p left))
	      (or (null right) (simple-vector-p right)))
	 (if (and (not (equivalent-node? value))
		  (< (+ (length-nv left) (length-nv right))
		     wb-tree-max-vector-length))
	     (concatenate 'simple-vector left (vector value) right)
	   (make-wb-set-tree-node value left right)))
	(t
	 (let ((wgtl (1+ (wb-set-tree-size left)))
	       (wgtr (1+ (wb-set-tree-size right))))
	   (declare (fixnum wgtl wgtr))
	   ;; This code is subtly different from Adams's in order to create more
	   ;; opportunities to coalesce adjacent short vectors.
	   (cond ((and (wb-set-tree-node? left) (> wgtl (wb-tree-balance-delta-fn wgtr)))
		  (let ((ll (wb-set-tree-node-left left))
			(rl (wb-set-tree-node-right left)))
		    (if (or (null rl) (simple-vector-p rl)
			    (< (1+ (wb-set-tree-size rl))
			       (wb-tree-balance-gamma-fn (1+ (wb-set-tree-size ll)))))
			(make-wb-set-tree-node (wb-set-tree-node-value left)
					       ll
					       (wb-set-tree-build-node value rl right))
		      (make-wb-set-tree-node (wb-set-tree-node-value rl)
					     (wb-set-tree-build-node
					       (wb-set-tree-node-value left)
					       ll
					       (wb-set-tree-node-left rl))
					     (wb-set-tree-build-node
					       value (wb-set-tree-node-right rl)
					       right)))))
		 ((and (wb-set-tree-node? right) (> wgtr (wb-tree-balance-delta-fn wgtl)))
		  (let ((lr (wb-set-tree-node-left right))
			(rr (wb-set-tree-node-right right)))
		    (if (or (null lr) (simple-vector-p lr)
			    (< (1+ (wb-set-tree-size lr))
			       (wb-tree-balance-gamma-fn (1+ (wb-set-tree-size rr)))))
			(make-wb-set-tree-node (wb-set-tree-node-value right)
					       (wb-set-tree-build-node value left lr)
					       rr)
		      (make-wb-set-tree-node (wb-set-tree-node-value lr)
					     (wb-set-tree-build-node
					       value left (wb-set-tree-node-left lr))
					     (wb-set-tree-build-node
					       (wb-set-tree-node-value right)
					       (wb-set-tree-node-right lr)
					       rr)))))
		 (t
		  (make-wb-set-tree-node value left right)))))))


(defun wb-set-tree-verify (tree cmp-fn)
  (macrolet ((test (form)
	       `(or ,form
		    (progn
		      (cerror "Ignore and proceed."
			      "WB-Set verification check failed: ~S" ',form)
		      t))))
    (rlabels (rec tree hedge-negative-infinity hedge-positive-infinity)
      (rec (tree lo hi)
	(cond ((null tree) t)
	      ((simple-vector-p tree)
	       (let ((len (length tree)))
		 (and (<= len wb-tree-max-vector-length)
		      (let ((prev lo))
			(and (gmap :and
				   (fn (elt)
				     (prog1 (and (test (not (equivalent-node? elt)))
						 (or (eq prev hedge-negative-infinity)
						     (test (less-than?-cmp prev elt cmp-fn))))
				       (setq prev elt)))
				   (:arg simple-vector tree))
			     (or (eq hi hedge-positive-infinity)
				 (test (less-than?-cmp prev hi cmp-fn))))))))
	      (t
	       (let ((sizl (wb-set-tree-size (wb-set-tree-node-left tree)))
		     (sizr (wb-set-tree-size (wb-set-tree-node-right tree)))
		     (value (wb-set-tree-node-value tree)))
		 (and (test (= (wb-set-tree-node-size tree) (+ sizl sizr (set-value-size value))))
		      (test (or (eq lo hedge-negative-infinity) (less-than?-cmp lo value cmp-fn)))
		      (test (or (eq hi hedge-positive-infinity) (less-than?-cmp value hi cmp-fn)))
		      (test (or (not (equivalent-node? value))
				(> (length (equivalent-node-list value)) 1)))
		      (test (or (<= sizr 4)
				(<= sizl (* wb-tree-balance-factor-limit sizr))))
		      (test (or (<= sizl 4)
				(<= sizr (* wb-tree-balance-factor-limit sizl))))
		      (rec (wb-set-tree-node-left tree) lo value)
		      (rec (wb-set-tree-node-right tree) value hi)))))))))


;;; ================================================================================
;;; Vector set operations

(defun wb-set-tree-vector-union (vec1 vec2 lo hi cmp-fn)
  "Returns the union of vectors `vec1' and `vec2', restricted to those members
above `lo' and below `hi'.  Creates new set tree nodes if needed, either
because the result exceeds the vector threshold size, or because one or more
pairs of equivalent members were found."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec1 vec2)
	   (type function cmp-fn))
  (let ((new-vec any-equivalent? (vector-set-union vec1 vec2 lo hi cmp-fn)))
    (declare (type simple-vector new-vec))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(reduce (fn (st x) (wb-set-tree-with st x cmp-fn)) new-vec :initial-value nil)
      (if (> (length new-vec) wb-tree-max-vector-length)
	  (let ((split-point (floor (length new-vec) 2)))
	    (make-wb-set-tree-node (svref new-vec split-point)
				   (vector-subseq new-vec 0 split-point)
				   (vector-subseq new-vec (1+ split-point))))
	new-vec))))

(defun vector-set-union (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      ;; We do these with linear rather than binary search because frequently,
      ;; the ends of the vectors will already be in range (the worst case for
      ;; binary search).
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vec2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vec2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((res nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (reverse-list-to-vector res)
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
		  (push (equivalent-set-union v1 v2 cmp-fn) res)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun reverse-list-to-vector (lst)
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

(defun vector-set-intersect (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (and res (reverse-list-to-vector res)))
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

(defun vector-set-diff (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vec1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((res nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vec1 i1) res)
	   (incf i1))
	 (and res (reverse-list-to-vector res)))
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

(defun vector-set-diff-2 (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vec2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
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
	 (values (and res1 (reverse-list-to-vector res1))
		 (and res2 (reverse-list-to-vector res2))))
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

(defun vector-set-subset? (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
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

(defun vector-set-disjoint? (vec1 vec2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vec1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
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

(defmacro do-wb-set-tree-members ((var tree-form &optional value-form) &body body)
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
			   (,recur-fn (wb-set-tree-node-left tree))
			   (let ((val (wb-set-tree-node-value tree)))
			     (if (equivalent-node? val)
				 (dolist (val (equivalent-node-list val))
				   (,body-fn val))
			       (,body-fn val)))
			   (,recur-fn (wb-set-tree-node-right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))


;;; ----------------
;;; Stateful iterator

(defun make-wb-set-tree-iterator (tree)
  (let ((iter (make-wb-set-tree-iterator-internal tree)))
    (lambda (op)
      (ecase op
	(:get (wb-set-tree-iterator-get iter))
	(:done? (wb-set-tree-iterator-done? iter))
	(:more? (not (wb-set-tree-iterator-done? iter)))))))

(defun make-wb-set-tree-iterator-internal (tree)
  (wb-set-tree-iterator-canonicalize
    (make-wb-tree-iterator tree (wb-set-tree-size tree) 2 t)))

(defun wb-set-tree-iterator-canonicalize (iter)
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
	    ((and (= idx 0) (wb-set-tree-node-left node))
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (wb-set-tree-node-left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx 0)
	     (setf (svref iter (1+ sp)) 1))
	    ((= idx (1+ (set-value-size (wb-set-tree-node-value node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (wb-set-tree-node-right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun wb-set-tree-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun wb-set-tree-iterator-get (iter)
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
		(wb-set-tree-iterator-canonicalize iter))
	      (values (svref node idx) t))
	  (let ((val (wb-set-tree-node-value node)))
	    (if (equivalent-node? val)
		(progn
		  (when (= idx (length (equivalent-node-list val)))
		    (wb-set-tree-iterator-canonicalize iter))
		  (values (nth (1- idx) (equivalent-node-list val)) t))
	      (progn
		(when (= idx 1)
		  (wb-set-tree-iterator-canonicalize iter))
		(values val t)))))))))


;;; ----------------
;;; Functional iterators.  Fun!!!

(defun wb-set-tree-fun-iter (tree &optional (cont (lambda (op)
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
	     (walk (wb-set-tree-node-left node)
		   (let ((value (wb-set-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (equivalent-node-list value))
			   (iter (mems)
			     (if mems
				 (lambda (op)
				   (ecase op
				     (:first (values (car mems) t))
				     (:rest (iter (cdr mems)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-set-tree-node-right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value t))
			   (:rest (walk (wb-set-tree-node-right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun wb-set-tree-rev-fun-iter (tree &optional (cont (lambda (op)
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
	     (walk (wb-set-tree-node-right node)
		   (let ((value (wb-set-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (reverse (equivalent-node-list value)))
			   (iter (mems)
			     (if mems
				 (lambda (op)
				   (ecase op
				     (:first (values (car mems) t))
				     (:rest (iter (cdr mems)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-set-tree-node-left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value t))
			   (:rest (walk (wb-set-tree-node-left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ----------------
;;; Utilities used by all tree types in this file

(defun make-wb-tree-iterator (tree size frame-size nodes-have-values?)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum frame-size))
  (let ((depth (the fixnum (wb-tree-max-depth size nodes-have-values?)))
	((stack (make-array (the fixnum (1+ (the fixnum (* frame-size depth))))))))
    (setf (svref stack 0) 1)
    (setf (svref stack 1) tree)
    (dotimes (i (1- frame-size))
      (setf (svref stack (+ i 2)) 0))
    stack))

(defun wb-tree-true-max-depth (size nodes-have-values?)
  (cond ((= size 0) 1)		; not really, but this is convenient
	((= size 1) 1)
	((= size 2) 2)
	(t
	 (let ((size (if nodes-have-values? (1- size) size))
	       ((subtree-max (min (1- size)
				  (floor (* size (/ wb-tree-balance-factor-delta
						    (1+ wb-tree-balance-factor-delta))))))))
	   (1+ (wb-tree-true-max-depth subtree-max nodes-have-values?))))))

(defconstant wb-tree-precomputed-max-depths 1000)

(deflex +wb-tree-max-depths-without-values+
    (gmap (:result vector) (fn (i) (wb-tree-true-max-depth i nil))
	  (:arg index 0 wb-tree-precomputed-max-depths)))

(deflex +wb-tree-max-depths-with-values+
    (gmap (:result vector) (fn (i) (wb-tree-true-max-depth i t))
	  (:arg index 0 wb-tree-precomputed-max-depths)))

(defun wb-tree-max-depth (size nodes-have-values?)
  ;; For purposes of this worst-case analysis I ignore the leaf vectors, though I
  ;; think it would be possible to prove that they are always at least half full.
  ;; There's almost no cost to overestimating this by a few, so this tries to be
  ;; very fast and conservative.
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum size))
  (if (< size wb-tree-precomputed-max-depths)
      (svref (if nodes-have-values?
		 +wb-tree-max-depths-with-values+
	       +wb-tree-max-depths-without-values+)
	     size)
    (ceiling (* (1- (integer-length size))
		;; constant:
		(/ (log 2) (log (/ (+ 1 wb-tree-balance-factor-delta) wb-tree-balance-factor-delta)))))))


;;; ================================================================================
;;; Equivalent-Set routines

(defun equivalent-set-union (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns their union represented as a single
value if a singleton, else as an `Equivalent-Set'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (if (equivalent-node? val2)
	  (let ((mems1 (equivalent-node-list val1))
		(mems2 (equivalent-node-list val2))
		((union mems1)) ; prefer `val1' values
		(any-new? nil))
	    (dolist (m2 mems2)
	      (unless (member m2 mems1 :test (equal?-fn cmp-fn))
		(push m2 union)
		(setq any-new? t)))
	    (if any-new? (make-equivalent-set union)
	      val1))
	(if (member val2 (equivalent-node-list val1) :test (equal?-fn cmp-fn))
	    val1
	  (make-equivalent-set (cons val2 (equivalent-node-list val1)))))
    (if (equivalent-node? val2)
	(let ((mems2 (equivalent-node-list val2))
	      ((pos (position val1 mems2 :test (equal?-fn cmp-fn)))))
	  (if pos
	      ;; prefer `val1', even in this case
	      (make-equivalent-set (cons val1 (append (cl:subseq mems2 0 pos) (nthcdr (1+ pos) mems2))))
	    (make-equivalent-set (cons val1 mems2))))
      (if (equal?-cmp val1 val2 cmp-fn) val1
	(make-equivalent-set (list val1 val2))))))

(defun equivalent-set-intersect (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is nonnull, returns
two values: true, and the intersection, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the intersection is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (if (equivalent-node? val2)
	  (let ((mems1 (equivalent-node-list val1))
		(mems2 (equivalent-node-list val2))
		((isect nil)))
	    (dolist (m1 mems1)
	      (when (member m1 mems2 :test (equal?-fn cmp-fn))
		(push m1 isect)))
	    (let ((isect-len (length isect)))
	      (cond ((null isect) nil)
		    ((= isect-len (length mems1)) (values t val1))
		    ((= isect-len 1) (values t (car isect)))
		    (t (values t (make-equivalent-set isect))))))
	;; prefer `val1' value
	(let ((pos (position val2 (equivalent-node-list val1) :test (equal?-fn cmp-fn))))
	  (and pos (values t (nth pos (equivalent-node-list val1))))))
    (if (equivalent-node? val2)
	(and (member val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))
	     (values t val1))
      (and (equal?-cmp val1 val2 cmp-fn) (values t val1)))))

(defun equivalent-set-difference (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their difference is nonnull, returns
two values: true, and the difference, represented as a single value if a
singleton, else as an `Equivalent-Set'.  If the difference is null, returns
`nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((mems1 (equivalent-node-list val1))
	    (mems2 (if (equivalent-node? val2) (equivalent-node-list val2)
		     (list val2)))
	    ((diff (cl:set-difference mems1 mems2 :test (equal?-fn cmp-fn)))
	     ((diff-len (length diff)))))
	(cond ((null diff) nil)
	      ((= diff-len (length mems1)) (values t val1))
	      ((= diff-len 1) (values t (car diff)))
	      (t (values t (make-equivalent-set diff)))))
    (if (equivalent-node? val2)
	(and (not (member val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn)))
	     (values t val1))
      (and (not (equal?-cmp val1 val2 cmp-fn)) (values t val1)))))

(defun equivalent-set-subset? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  Returns true iff `val2' contains all members
of `val1'."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (and (equivalent-node? val2)
	   (let ((mems2 (equivalent-node-list val2)))
	     (dolist (m1 (equivalent-node-list val1) t)
	       (unless (member m1 mems2 :test (equal?-fn cmp-fn))
		 (return nil)))))
    (if (equivalent-node? val2)
	(member val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))
      (equal?-cmp val1 val2 cmp-fn))))

(defun equivalent-set-disjoint? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values (representing singleton sets)
or `Equivalent-Set's of values.  If their intersection is null, returns
true, else false."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (if (equivalent-node? val2)
	  (dolist (m1 (equivalent-node-list val1) t)
	    (when (member m1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))
	      (return nil)))
	(not (member val2 (equivalent-node-list val1) :test (equal?-fn cmp-fn))))
    (if (equivalent-node? val2)
	(not (member val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn)))
      (not (equal?-cmp val1 val2 cmp-fn)))))

(defun equivalent-set-compare (val1 val2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (let ((comp (funcall cmp-fn val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (equivalent-node? val1)
	  (if (equivalent-node? val2)
	      (let ((mems1 (equivalent-node-list val1))
		    (mems2 (equivalent-node-list val2))
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
	(if (equivalent-node? val2)
	    ':greater
	  comp)))))


;;; When called on a value and an `Equivalent-Node', or on two `Equivalent-Node's,
;;; the result of `compare' is meaningful only for ordering; the distinction between
;;; `:equal' and `:unequal' is not meaningful.  Code that may care about the latter
;;; has more work to do anyway, such as calling `Equivalent-Set-Union' etc.
(defmethod compare (x (eqvn equivalent-node))
  "Returns `:less' or `:greater' if `x' is less than resp. greater than the
values in `eqvs'; or EITHER `:equal' or `:unequal' if `x' is equivalent to any
value in `eqvs'."
  (compare x (if (equivalent-node-set? eqvn)
		 (car (equivalent-node-list eqvn))
	       (caar (equivalent-node-list eqvn)))))

(defmethod compare ((eqvn equivalent-node) x)
  "Returns `:less' or `:greater' if the values in `eqvs' are less than resp.
greater than `x'; or EITHER `:equal' or `:unequal' if `x' is equivalent to
any value in `eqvs'."
  (compare (if (equivalent-node-set? eqvn)
	       (car (equivalent-node-list eqvn))
	     (caar (equivalent-node-list eqvn)))
	   x))

(defmethod compare ((eqvn1 equivalent-node) (eqvn2 equivalent-node))
  "Returns `:less' or `:greater' if the values in `eqvs1' are less than resp.
greater than those in `eqvs2'; returns EITHER `:equal' or `:unequal' if those
in `eqvs1' are equivalent to those in `eqvs2'."
  (compare (if (equivalent-node-set? eqvn1)
	       (car (equivalent-node-list eqvn1))
	     (caar (equivalent-node-list eqvn1)))
	   (if (equivalent-node-set? eqvn2)
	       (car (equivalent-node-list eqvn2))
	     (caar (equivalent-node-list eqvn2)))))


;;; ================================================================================
;;; ================================================================================
;;; Bags

(declaim (inline make-raw-wb-bag-tree-node))

;;; A bag tree is either null, a node, or a cons of two simple-vectors.
(deftype wb-bag-tree ()
  '(or null wb-bag-tree-node cons))

(defstruct (wb-bag-tree-node
	    (:constructor make-raw-wb-bag-tree-node (size total-count value count
						     left right))
	    (:predicate wb-bag-tree-node?)
	    (:print-function wb-bag-tree-node-print))
  (left  nil :type wb-bag-tree :read-only t)
  (right nil :type wb-bag-tree :read-only t)
  ;; If we get equivalent values, then the `Value' is an `Equivalent-Bag', and the
  ;; `Count' is unused.
  (value nil :read-only t)			; the value at the node, or an `Equivalent-Bag'
  (count 0 :type integer :read-only t)		; the count (multiplicity) for this value
  (total-count 0 :type integer :read-only t)	; total count of all values in this subtree
  (size 0 :type fixnum :read-only t))		; the number of < value, count > pairs


(defun wb-bag-tree-node-print (node stream depth)
  "Print function for `WB-Bag-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#bag-node<~;~D/~D, ~S -> ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (wb-bag-tree-node-size node)
		    (wb-bag-tree-node-total-count node)
		    (wb-bag-tree-node-value node)
		    (wb-bag-tree-node-count node)
		    (let ((sub (wb-bag-tree-node-left node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))
		    (let ((sub (wb-bag-tree-node-right node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))))
    (format stream "#bag-node<...>")))

(declaim (inline make-equivalent-bag))

(defun make-equivalent-bag (alist)
  (make-equivalent-node nil alist))

(declaim (ftype (function (t) fixnum) bag-value-size))
(declaim (inline bag-value-size))

(defun bag-value-size (value)
  "The number of values represented by `value', which can be more than 1 if
`key' is an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0)))
  (if (equivalent-node? value)
      (length (equivalent-node-list value))
    1))

(declaim (inline wb-bag-tree-size))

(defun wb-bag-tree-size (tree)
  "The number of value/count pairs contained in this tree."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (cond ((null tree) 0)
	((consp tree) (length (the simple-vector (car tree))))
	(t (wb-bag-tree-node-size tree))))

(declaim (ftype (function (wb-bag-tree) fixnum) wb-bag-tree-size))

(defun wb-bag-tree-total-count (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (cond ((null tree) 0)
	((consp tree)
	 (the integer (muffle-notes (gmap (:result sum) nil (:arg simple-vector (cdr tree))))))
	(t (wb-bag-tree-node-total-count tree))))

(declaim (ftype (function (wb-bag-tree) integer) wb-bag-tree-total-count))


;;; This is just to get rid of compiler optimization notes.
(gmap:def-result-type gen-sum (&key filterp)
  "Returns the sum of the values, optionally filtered by `filterp', using
generic arithmetic."
  `(0 #'(lambda (x y) (gen + x y)) nil ,filterp))

(defun make-wb-bag-tree-node (value count left right)
  "The low-level constructor for a bag tree node.  `count' is ignored and can be
`nil' if value is an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree left right))
  (make-raw-wb-bag-tree-node (gen + (wb-bag-tree-size left) (wb-bag-tree-size right)
				  (bag-value-size value))
			     (gen + (wb-bag-tree-total-count left)
				  (wb-bag-tree-total-count right)
				  (if (equivalent-node? value)
				      (gmap (:result gen-sum) #'cdr
					    (:arg list (equivalent-node-list value)))
				    (or count 0)))
			     value (or count 0) left right))


(defun wb-bag-tree-arb-pair (tree)
  "Returns an arbitrary member of the bag and its count.  Assumes the bag is
nonempty."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (if (consp tree)
      (values (svref (car tree) 0) (svref (cdr tree) 0))
    (let ((value (wb-bag-tree-node-value tree)))
      (if (equivalent-node? value)
	  (let ((alist (equivalent-node-list value)))
	    (values (caar alist) (cdar alist)))
	(values value (wb-bag-tree-node-count tree))))))

(defun wb-bag-tree-least-pair (tree)
  "Assumes `tree' is nonempty.  Returns the least member, or an arbitrary
least member if there are more than one; the second value is the associated
count."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (let ((val count (wb-bag-tree-minimum-pair tree)))
    (if (equivalent-node? val)
	(let ((pr (car (equivalent-node-list val))))
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

(defun wb-bag-tree-greatest-pair (tree)
  "Assumes `tree' is nonempty.  Returns the greatest member, or an arbitrary
greatest member if there are more than one; the second value is the associated
multiplicity."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (if (consp tree)
      (let ((idx (1- (length (the simple-vector (car tree))))))
	(values (svref (car tree) idx)
		(svref (cdr tree) idx)))
    (let ((right (wb-bag-tree-node-right tree)))
      (if right
	  (wb-bag-tree-greatest-pair right)
	(let ((val (wb-bag-tree-node-value tree)))
	  (if (equivalent-node? val)
	      (let ((pr (car (cl:last (equivalent-node-list val)))))
		(values (car pr) (cdr pr)))
	    (values val (wb-bag-tree-node-count tree))))))))

(declaim (ftype (function (wb-bag-tree t function) integer) wb-bag-tree-multiplicity))
(defun wb-bag-tree-multiplicity (tree value cmp-fn)
  "Returns the multiplicity of `value' in `tree', or 0 if `value' does not
appear in `tree'.  As a second value, returns the value found, if any."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) (values 0 nil))
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) value cmp-fn)))
	   (if (eq found? ':equal)
	       (values (svref (cdr tree) idx) (svref (car tree) idx))
	     (values 0 nil))))
	(t
	 (let ((node-val (wb-bag-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     (:equal
	      (if (equivalent-node? node-val)
		  (let ((pr (car (equivalent-node-list node-val))))
		    (values (cdr pr) (car pr)))
		(values (wb-bag-tree-node-count tree) node-val)))
	     (:unequal
	      (if (equivalent-node? node-val)
		  (let ((pr (assoc value (equivalent-node-list node-val)
				   :test (equal?-fn cmp-fn))))
		    (if pr (values (cdr pr) (car pr))
		      (values 0 nil)))
		(values 0 nil)))
	     ((:less)
	      (wb-bag-tree-multiplicity (wb-bag-tree-node-left tree) value cmp-fn))
	     ((:greater)
	      (wb-bag-tree-multiplicity (wb-bag-tree-node-right tree) value cmp-fn)))))))

(defun wb-bag-tree-find-equivalent (tree value cmp-fn)
  "If `tree' contains one or more values equivalent to `value', returns (first
value) true, (second value) either the one value or an `Equivalent-Bag'
containing the values, and (third value) if the second value was a single
value, the corresponding count; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) value cmp-fn)))
	   (and found?
		(values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-val (wb-bag-tree-node-value tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal) (values t node-val (wb-bag-tree-node-count tree)))
	     (:less
	       (wb-bag-tree-find-equivalent (wb-bag-tree-node-left tree) value cmp-fn))
	     (:greater
	       (wb-bag-tree-find-equivalent (wb-bag-tree-node-right tree) value cmp-fn)))))))

;;; ================================================================================
;;; With

(defun wb-bag-tree-with (tree value cmp-fn &optional (count 1))
  "Returns `tree' with `value' added with a count of `count' (if it was already
present, its count is incremented by `count').  `value' may be an `Equivalent-Bag'."
  ;; The case where `value' is an `Equivalent-Bag' is used by `WB-Bag-Tree-Concat',
  ;; which may be passed one by various callers.
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type function cmp-fn))
  (cond ((null tree)
	 (if (not (equivalent-node? value))
	     (cons (vector value) (vector count))
	   (make-wb-bag-tree-node value count nil nil)))
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) value cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `value' is an `Equivalent-Bag', because
	   ;; this routine is called by `WB-Bag-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (equivalent-node? value)))
	       (cons (car tree)
		     (vector-update (cdr tree) idx (gen + (svref (cdr tree) idx)
							count)))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 wb-tree-max-vector-length)
		      (not (equivalent-node? value)))
		 (cons (vector-insert (car tree) idx value)
		       (vector-insert (cdr tree) idx count))
	       (let ((new-val new-count
			(if found? (equivalent-bag-sum (svref (car tree) idx)
						       (svref (cdr tree) idx)
						       value count cmp-fn)
			  (values value count))))
		 (make-wb-bag-tree-node new-val new-count
					(and (> idx 0)
					     (cons (vector-subseq (car tree) 0 idx)
						   (vector-subseq (cdr tree) 0 idx)))
					(and (< right-start (length (the simple-vector
								      (car tree))))
					     (cons (vector-subseq (car tree) right-start)
						   (vector-subseq (cdr tree)
								  right-start)))))))))
	(t
	 (let ((node-val (wb-bag-tree-node-value tree))
	       (node-count (wb-bag-tree-node-count tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((new-val new-count
		       (equivalent-bag-sum node-val node-count value count cmp-fn)))
		(make-wb-bag-tree-node new-val new-count
				       (wb-bag-tree-node-left tree)
				       (wb-bag-tree-node-right tree))))
	     ((:less)
	      (wb-bag-tree-build-node node-val node-count
				      (wb-bag-tree-with (wb-bag-tree-node-left tree)
							value cmp-fn count)
				      (wb-bag-tree-node-right tree)))
	     ((:greater)
	      (wb-bag-tree-build-node node-val node-count
				      (wb-bag-tree-node-left tree)
				      (wb-bag-tree-with (wb-bag-tree-node-right tree)
							value cmp-fn count))))))))


;;; ================================================================================
;;; Less

(defun wb-bag-tree-less (tree value cmp-fn &optional (count 1))
  "Returns `tree' with the count for `value' decremented; if that count was
1, `value' is removed entirely."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type integer count)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) value cmp-fn)))
	   (if (eq found? ':equal)
	       (let ((prev-count (the integer (svref (cdr tree) idx))))
		 (if (gen > prev-count count)
		     (cons (car tree) (vector-update (cdr tree) idx
						     (gen - prev-count count)))
		   (and (> (length (the simple-vector (car tree))) 1)
			(cons (vector-remove-at (car tree) idx)
			      (vector-remove-at (cdr tree) idx)))))
	     tree)))
	(t
	 (let ((node-val (wb-bag-tree-node-value tree))
	       (node-count (wb-bag-tree-node-count tree))
	       ((comp (funcall cmp-fn value node-val))))
	   (ecase comp
	     ((:equal :unequal)
	      (let ((nonnull? value count
		      (equivalent-bag-difference node-val node-count value count cmp-fn)))
		(cond ((and (eq value node-val) (eq count node-count))
		       tree)
		      (nonnull?
		       (make-wb-bag-tree-node value count
					      (wb-bag-tree-node-left tree)
					      (wb-bag-tree-node-right tree)))
		      (t
		       (wb-bag-tree-join (wb-bag-tree-node-left tree) (wb-bag-tree-node-right tree) cmp-fn)))))
	     ((:less)
	      (let ((left (wb-bag-tree-node-left tree))
		    ((new-left (wb-bag-tree-less left value cmp-fn count))))
		(if (eq new-left left) tree
		  (wb-bag-tree-build-node node-val node-count new-left
					  (wb-bag-tree-node-right tree)))))
	     ((:greater)
	      (let ((right (wb-bag-tree-node-right tree))
		    ((new-right (wb-bag-tree-less right value cmp-fn count))))
		(if (eq new-right right) tree
		  (wb-bag-tree-build-node node-val node-count
					  (wb-bag-tree-node-left tree)
					  new-right)))))))))


;;; ================================================================================
;;; Union, sum, intersection, and bag difference

(defun wb-bag-tree-union (tree1 tree2 cmp-fn)
  "Returns the union of `tree' and `tree2'."
  (if (eq tree1 tree2)
      tree1
    (wb-bag-tree-union-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))

(defun wb-bag-tree-union-rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (wb-bag-tree-split tree1 lo hi cmp-fn))
	((null tree2)
	 (wb-bag-tree-split tree1 lo hi cmp-fn))
	((null tree1)
	 (wb-bag-tree-split tree2 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (wb-bag-tree-vector-pair-union tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2))
	       ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn))
		((val count (if eqvv1? (equivalent-bag-union eqvv1 eqvc1 val2 count2 cmp-fn)
			      (values val2 count2))))))
	   (wb-bag-tree-concat
	     val count
	     (wb-bag-tree-union-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
				    (wb-bag-tree-trim (wb-bag-tree-node-left tree2) lo val2 cmp-fn)
				    lo val2 cmp-fn)
	     (wb-bag-tree-union-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
				    (wb-bag-tree-trim (wb-bag-tree-node-right tree2) val2 hi cmp-fn)
				    val2 hi cmp-fn)
	     cmp-fn)))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1))
	       ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))
		((val count (if eqvv2? (equivalent-bag-union val1 count1 eqvv2 eqvc2 cmp-fn)
			      (values val1 count1))))))
	   (wb-bag-tree-concat
	     val count
	     (wb-bag-tree-union-rng (wb-bag-tree-trim (wb-bag-tree-node-left tree1) lo val1 cmp-fn)
				    (wb-bag-tree-trim tree2 lo val1 cmp-fn)
				    lo val1 cmp-fn)
	     (wb-bag-tree-union-rng (wb-bag-tree-trim (wb-bag-tree-node-right tree1) val1 hi cmp-fn)
				    (wb-bag-tree-trim tree2 val1 hi cmp-fn)
				    val1 hi cmp-fn)
	     cmp-fn)))))

(defun wb-bag-tree-sum (tree1 tree2 cmp-fn)
  "Returns the sum of `tree' and `tree2'."
  (wb-bag-tree-sum-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-bag-tree-sum-rng (tree1 tree2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((null tree2)
	 (wb-bag-tree-split tree1 lo hi cmp-fn))
	((null tree1)
	 (wb-bag-tree-split tree2 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (wb-bag-tree-vector-pair-sum tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2))
	       ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn))
		((val count (if eqvv1? (equivalent-bag-sum eqvv1 eqvc1 val2 count2 cmp-fn)
			      (values val2 count2))))))
	   (wb-bag-tree-concat
	     val count
	     (wb-bag-tree-sum-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
				  (wb-bag-tree-trim (wb-bag-tree-node-left tree2) lo val2 cmp-fn)
				  lo val2 cmp-fn)
	     (wb-bag-tree-sum-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
				  (wb-bag-tree-trim (wb-bag-tree-node-right tree2) val2 hi cmp-fn)
				  val2 hi cmp-fn)
	     cmp-fn)))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1))
	       ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))
		((val count (if eqvv2? (equivalent-bag-sum val1 count1 eqvv2 eqvc2 cmp-fn)
			      (values val1 count1))))))
	   (wb-bag-tree-concat
	     val count
	     (wb-bag-tree-sum-rng (wb-bag-tree-trim (wb-bag-tree-node-left tree1) lo val1 cmp-fn)
				  (wb-bag-tree-trim tree2 lo val1 cmp-fn)
				  lo val1 cmp-fn)
	     (wb-bag-tree-sum-rng (wb-bag-tree-trim (wb-bag-tree-node-right tree1) val1 hi cmp-fn)
				  (wb-bag-tree-trim tree2 val1 hi cmp-fn)
				  val1 hi cmp-fn)
	     cmp-fn)))))


(defun wb-bag-tree-intersect (tree1 tree2 cmp-fn)
  (if (eq tree1 tree2)
      tree1
    (wb-bag-tree-intersect-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))

(defun wb-bag-tree-intersect-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the intersection of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2)		; historically-related-bag optimization
	 (wb-bag-tree-split tree1 lo hi cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (vector-pair-bag-intersect tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2))
	       ((new-left
		  (wb-bag-tree-intersect-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
					     (wb-bag-tree-node-left tree2)
					     lo val2 cmp-fn))
		(new-right
		  (wb-bag-tree-intersect-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
					     (wb-bag-tree-node-right tree2)
					     val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (equivalent-bag-intersect eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1))
	       ((new-left
		  (wb-bag-tree-intersect-rng (wb-bag-tree-node-left tree1)
					     (wb-bag-tree-trim tree2 lo val1 cmp-fn)
					     lo val1 cmp-fn))
		(new-right
		  (wb-bag-tree-intersect-rng (wb-bag-tree-node-right tree1)
					     (wb-bag-tree-trim tree2 val1 hi cmp-fn)
					     val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (and eqvv2? (equivalent-bag-intersect val1 count1 eqvv2 eqvc2 cmp-fn))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))))


(defun wb-bag-tree-product (tree1 tree2 cmp-fn)
  (wb-bag-tree-product-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-bag-tree-product-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the Production of `tree1' with `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (vector-pair-bag-product tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2))
	       ((new-left
		  (wb-bag-tree-product-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
					   (wb-bag-tree-node-left tree2)
					   lo val2 cmp-fn))
		(new-right
		  (wb-bag-tree-product-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
					   (wb-bag-tree-node-right tree2)
					   val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (equivalent-bag-product eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1))
	       ((new-left
		  (wb-bag-tree-product-rng (wb-bag-tree-node-left tree1)
					   (wb-bag-tree-trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn))
		(new-right
		  (wb-bag-tree-product-rng (wb-bag-tree-node-right tree1)
					   (wb-bag-tree-trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (and eqvv2? (equivalent-bag-product val1 count1 eqvv2 eqvc2 cmp-fn))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))))


(defun wb-bag-tree-diff (tree1 tree2 cmp-fn)
  "Returns the bag difference of `tree1' less `tree2'.  Runs in time linear in
the total sizes of the two trees."
  (and (not (eq tree1 tree2))
       (wb-bag-tree-diff-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))

(defun wb-bag-tree-diff-rng (tree1 tree2 lo hi cmp-fn)
  "Returns the bag difference of `tree1' less `tree2', considering only those
members that are above `lo' and below `hi', and assuming that the root values
of `tree1' and `tree2' are in this range."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((eq tree1 tree2) nil)		; historically-related-bag optimization
	((null tree1) nil)
	((null tree2)
	 (wb-bag-tree-split tree1 lo hi cmp-fn))
	((and (consp tree1) (consp tree2))
	 (vector-pair-bag-diff tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2))
	       ((new-left (wb-bag-tree-diff-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
						(wb-bag-tree-trim (wb-bag-tree-node-left tree2) lo val2 cmp-fn)
						lo val2 cmp-fn))
		(new-right (wb-bag-tree-diff-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
						 (wb-bag-tree-trim (wb-bag-tree-node-right tree2) val2 hi cmp-fn)
						 val2 hi cmp-fn)))
	       ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn))
		((nonnull? value count
		   (and eqvv1? (equivalent-bag-difference eqvv1 eqvc1 val2 count2 cmp-fn))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1))
	       ((new-left (wb-bag-tree-diff-rng (wb-bag-tree-node-left tree1)
						(wb-bag-tree-trim tree2 lo val1 cmp-fn)
						lo val1 cmp-fn))
		(new-right (wb-bag-tree-diff-rng (wb-bag-tree-node-right tree1)
						 (wb-bag-tree-trim tree2 val1 hi cmp-fn)
						 val1 hi cmp-fn)))
	       ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))
		((nonnull? value count
		   (if eqvv2? (equivalent-bag-difference val1 count1 eqvv2 eqvc2 cmp-fn)
		     (values t val1 count1))))))
	   (if nonnull?
	       (wb-bag-tree-concat value count new-left new-right cmp-fn)
	     (wb-bag-tree-join new-left new-right cmp-fn))))))


;;; ================================================================================
;;; Comparison

(defun wb-bag-tree-compare (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((totct1 (wb-bag-tree-total-count tree1))
	  (totct2 (wb-bag-tree-total-count tree2))
	  (size1 (wb-bag-tree-size tree1))
	  (size2 (wb-bag-tree-size tree2)))
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    ((< totct1 totct2) ':less)
	    ((> totct1 totct2) ':greater)
	    (t (wb-bag-tree-compare-rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun wb-bag-tree-compare-rng (tree1 base1 tree2 base2 lo hi cmp-fn)
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
	 (invert-comparison (wb-bag-tree-compare-rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (wb-bag-tree-node-left tree1))
	       ((left1-size (the fixnum (wb-bag-tree-size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (wb-bag-tree-rank-trim left1 base1 lo new-hi))
		  (tree2a base2a (wb-bag-tree-rank-trim tree2 base2 lo new-hi))
		  ((left-comp (wb-bag-tree-compare-rng left1a base1a tree2a base2a
						       lo new-hi cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((val1 (wb-bag-tree-node-value tree1))
		   (count1 (wb-bag-tree-node-count tree1))
		   (val2 count2
		      (wb-bag-tree-rank-pair-internal
			tree2 (the fixnum (- new-hi base2))))
		   ((val-comp (equivalent-bag-compare val1 count1 val2 count2 cmp-fn))))
	       (if (or (eq val-comp ':less) (eq val-comp ':greater))
		   val-comp
		 (let ((val1-size (bag-value-size val1))
		       ((new-lo (the fixnum (+ base1 left1-size val1-size)))
			((right1a base1a
			   (wb-bag-tree-rank-trim (wb-bag-tree-node-right tree1)
						  new-lo new-lo hi))
			 (tree2a base2a (wb-bag-tree-rank-trim tree2 base2 new-lo hi))
			 ((right-comp (wb-bag-tree-compare-rng right1a base1a tree2a
							       base2a new-lo hi cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal val-comp))))))))))

(defun wb-bag-tree-rank-trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type fixnum base lo hi)
	   #+(or cmu scl)
	   (values wb-bag-tree fixnum))
  (if (or (null tree) (consp tree))
      (values tree base)
    (let ((node-rank (+ base (wb-bag-tree-size (wb-bag-tree-node-left tree)))))
      (declare (type fixnum node-rank))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (wb-bag-tree-rank-trim (wb-bag-tree-node-left tree) base lo hi))
	(wb-bag-tree-rank-trim (wb-bag-tree-node-right tree)
			       (+ node-rank
				  (bag-value-size (wb-bag-tree-node-value tree)))
			       lo hi)))))

(defun wb-bag-tree-rank (tree value cmp-fn)
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
		    (let ((found? idx (vector-set-binary-search (car tree) value cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (wb-bag-tree-node-value tree))
			  (left (wb-bag-tree-node-left tree))
			  ((left-size (wb-bag-tree-size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall cmp-fn value node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (equivalent-node? node-val)
			     (let ((mems (equivalent-node-list node-val))
				   ((pos (cl:position value mems :test (equal?-fn cmp-fn) :key #'car))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left value base))
			((:greater)
			 (rec (wb-bag-tree-node-right tree) value
			      (+ node-base (bag-value-size node-val))))))))))
    (rec tree value 0)))

(defun wb-bag-tree-rank-pair (tree rank)
  (let ((elt count rem (wb-bag-tree-rank-pair-internal tree rank)))
    (if (equivalent-node? elt)
	(let ((pr (nth rem (equivalent-node-list elt))))
	  (values (car pr) (cdr pr)))
      (values elt count))))

(defun wb-bag-tree-rank-pair-internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in bag comparator"))
	((consp tree)
	 (values (svref (car tree) rank) (svref (cdr tree) rank) 0))
	(t
	 (let ((left (wb-bag-tree-node-left tree))
	       ((left-size (wb-bag-tree-size left))))
	   (if (< rank left-size)
	       (wb-bag-tree-rank-pair-internal left rank)
	     (let ((val (wb-bag-tree-node-value tree))
		   ((val-size (bag-value-size val))
		    (rank (- rank left-size))))
	       (declare (type fixnum rank))
	       (if (< rank val-size)
		   (values val (wb-bag-tree-node-count tree) rank)
		 (wb-bag-tree-rank-pair-internal (wb-bag-tree-node-right tree)
						 (the fixnum (- rank val-size))))))))))


;;; ================================================================================
;;; Subbag testing

(defun wb-bag-tree-subbag? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (let ((size1 (wb-bag-tree-size tree1))
	(size2 (wb-bag-tree-size tree2)))
    (or (eq tree1 tree2)
	(and (<= size1 size2)
	     (wb-bag-tree-subbag?-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn)))))

(defun wb-bag-tree-subbag?-rng (tree1 tree2 lo hi cmp-fn)
  (declare ;(optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree1 tree2)
	   (type function cmp-fn))
  (cond ((null tree1) t)
	((null tree2) nil)
	((eq tree1 tree2) t)		; historically-related-tree optimization
	((and (consp tree1) (consp tree2))
	 (vector-pair-bag-subbag? tree1 tree2 lo hi cmp-fn))
	((consp tree1)
	 (let ((val2 (wb-bag-tree-node-value tree2))
	       (count2 (wb-bag-tree-node-count tree2)))
	   (and (wb-bag-tree-subbag?-rng (wb-bag-tree-trim tree1 lo val2 cmp-fn)
					 (wb-bag-tree-node-left tree2)
					 lo val2 cmp-fn)
		(let ((eqvv1? eqvv1 eqvc1 (wb-bag-tree-find-equivalent tree1 val2 cmp-fn)))
		  (and (or (not eqvv1?)
			   (equivalent-bag-subbag? eqvv1 eqvc1 val2 count2 cmp-fn))
		       (wb-bag-tree-subbag?-rng (wb-bag-tree-trim tree1 val2 hi cmp-fn)
						(wb-bag-tree-node-right tree2)
						val2 hi cmp-fn))))))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       (count1 (wb-bag-tree-node-count tree1)))
	   (and (wb-bag-tree-subbag?-rng (wb-bag-tree-node-left tree1)
					 (wb-bag-tree-trim tree2 lo val1 cmp-fn)
					 lo val1 cmp-fn)
		(let ((eqvv2? eqvv2 eqvc2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn)))
		  (and eqvv2?
		       (equivalent-bag-subbag? val1 count1 eqvv2 eqvc2 cmp-fn)
		       (wb-bag-tree-subbag?-rng (wb-bag-tree-node-right tree1)
						(wb-bag-tree-trim tree2 val1 hi cmp-fn)
						val1 hi cmp-fn))))))))


;;; ================================================================================
;;; Disjointness testing

(defun wb-bag-tree-disjoint? (tree1 tree2 cmp-fn)
  (declare (type function cmp-fn))
  (wb-bag-tree-disjoint?-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-bag-tree-disjoint?-rng (tree1 tree2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (cond ((or (null tree1) (null tree2))
	 t)
	((eq tree1 tree2)
	 nil)
	((and (consp tree1) (consp tree2))
	 (vector-set-disjoint? (car tree1) (car tree2) lo hi cmp-fn))
	((consp tree1)
	 (wb-bag-tree-disjoint?-rng (wb-bag-tree-trim tree2 lo hi cmp-fn)
				    tree1 lo hi cmp-fn))
	(t
	 (let ((val1 (wb-bag-tree-node-value tree1))
	       ((eqvv2? eqvv2 (wb-bag-tree-find-equivalent tree2 val1 cmp-fn))))
	   (and (or (null eqvv2?) (equivalent-bag-disjoint? val1 eqvv2 cmp-fn))
		(wb-bag-tree-disjoint?-rng (wb-bag-tree-node-left tree1)
					   (wb-bag-tree-trim tree2 lo val1 cmp-fn)
					   lo val1 cmp-fn)
		(wb-bag-tree-disjoint?-rng (wb-bag-tree-node-right tree1)
					   (wb-bag-tree-trim tree2 val1 hi cmp-fn)
					   val1 hi cmp-fn))))))


;;; ================================================================================
;;; Miscellany

(defun wb-bag-tree-from-list (lst pairs? cmp-fn)
  (let ((tree nil))
    (if pairs?
	(dolist (x lst)
	  (unless (and (integerp (cdr x)) (< 0 (cdr x)))
	    (error 'simple-type-error :datum (cdr x) :expected-type '(integer 1 *)
				      :format-control "Supplied count is not a positive integer: ~S"
				      :format-arguments (list (cdr x))))
	  (setq tree (wb-bag-tree-with tree (car x) cmp-fn (cdr x))))
      (dolist (x lst)
	(setq tree (wb-bag-tree-with tree x cmp-fn))))
    tree))

(defun wb-bag-tree-from-iterable (it pairs? cmp-fn)
  (declare (type function it))
  (let ((tree nil))
    (if pairs?
	(while (funcall it ':more?)
	  (let ((x (funcall it ':get)))
	    (unless (and (integerp (cdr x)) (< 0 (cdr x)))
	      (error 'simple-type-error :datum (cdr x) :expected-type '(integer 1 *)
					:format-control "Supplied count is not a positive integer: ~S"
					:format-arguments (list (cdr x))))
	    (setq tree (wb-bag-tree-with tree (car x) cmp-fn (cdr x)))))
      (while (funcall it ':more?)
	(setq tree (wb-bag-tree-with tree (funcall it ':get) cmp-fn))))
    tree))

;;; See `WB-Set-Tree-From-Sorted-Iterable'.
(defun wb-bag-tree-from-sorted-iterable (it size pairs? cmp-fn)
  (declare (type function it cmp-fn))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil hedge-positive-infinity hedge-negative-infinity))
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
			(:unequal (values (wb-bag-tree-with (cons (vector a) (vector na)) b cmp-fn nb) a a)))))
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
			  (values (wb-bag-tree-build-node e ne left right) left-first right-last)
			;; Fall back to general case.
			(values (wb-bag-tree-with (wb-bag-tree-sum left right cmp-fn) e cmp-fn ne)
				(if (less-than?-cmp left-first right-first cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last cmp-fn) right-last left-last)))))))
	   (check-count (n)
	     (unless (and (integerp n) (< 0 n))
	       (error 'simple-type-error :datum n :expected-type '(integer 0 *)
					 :format-control "Supplied count is not a positive integer: ~S"
					 :format-arguments (list n)))))
    (recur size)))


(defun wb-set-tree-to-bag-tree (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-set-tree tree))
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (cons tree (make-array (length tree) :initial-element 1)))
	(t
	 (let ((value (wb-set-tree-node-value tree))
	       (size (wb-set-tree-node-size tree))
	       (new-left (wb-set-tree-to-bag-tree (wb-set-tree-node-left tree)))
	       (new-right (wb-set-tree-to-bag-tree (wb-set-tree-node-right tree))))
	   (if (equivalent-node? value)
	       (make-raw-wb-bag-tree-node
		 size size
		 (make-equivalent-bag (mapcar #'(lambda (x) (cons x 1))
					      (equivalent-node-list value)))
		 0 new-left new-right)
	     (make-raw-wb-bag-tree-node size size value 1 new-left new-right))))))

(defun wb-bag-tree-to-set-tree (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (cond ((null tree) nil)
	((consp tree)
	 (car tree))
	(t
	 (let ((value (wb-bag-tree-node-value tree))
	       (size (wb-bag-tree-node-size tree))
	       (new-left (wb-bag-tree-to-set-tree (wb-bag-tree-node-left tree)))
	       (new-right (wb-bag-tree-to-set-tree (wb-bag-tree-node-right tree))))
	   (if (equivalent-node? value)
	       (make-raw-wb-set-tree-node
		 size (make-equivalent-set (mapcar #'car (equivalent-node-list value)))
		 new-left new-right)
	     (make-raw-wb-set-tree-node size value new-left new-right))))))

(defun wb-bag-tree-split-above (tree value cmp-fn)
  (wb-bag-tree-split tree value hedge-positive-infinity cmp-fn))

(defun wb-bag-tree-split-below (tree value cmp-fn)
  (wb-bag-tree-split tree hedge-negative-infinity value cmp-fn))


;;; ================================================================================
;;; Support routines for the above (bags)

(defun wb-bag-tree-split (tree lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo hedge-negative-infinity) (eq hi hedge-positive-infinity))
	 tree)
	((consp tree)
	 (let ((vals (the simple-vector (car tree)))
	       (counts (the simple-vector (cdr tree)))
	       ((len (length vals))
		((split-point-lo (if (eq lo hedge-negative-infinity)
				     0
				   (vector-set-binary-search-lo vals lo cmp-fn)))
		 (split-point-hi (if (eq hi hedge-positive-infinity)
				     len
				   (vector-set-binary-search-hi vals hi cmp-fn))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (vector-subseq vals split-point-lo split-point-hi)
			(vector-subseq counts split-point-lo split-point-hi))))))
	((not (or (eq lo hedge-negative-infinity)
		  (greater-than?-cmp (wb-bag-tree-node-value tree) lo cmp-fn)))
	 (wb-bag-tree-split (wb-bag-tree-node-right tree) lo hi cmp-fn))
	((not (or (eq hi hedge-positive-infinity)
		  (less-than?-cmp (wb-bag-tree-node-value tree) hi cmp-fn)))
	 (wb-bag-tree-split (wb-bag-tree-node-left tree) lo hi cmp-fn))
	(t
	 (let ((new-left (wb-bag-tree-split (wb-bag-tree-node-left tree)
					    lo hedge-positive-infinity cmp-fn))
	       (new-right (wb-bag-tree-split (wb-bag-tree-node-right tree)
					     hedge-negative-infinity hi cmp-fn)))
	   (if (and (eq new-left (wb-bag-tree-node-left tree))
		    (eq new-right (wb-bag-tree-node-right tree)))
	       tree
	     (wb-bag-tree-concat (wb-bag-tree-node-value tree)
				 (wb-bag-tree-node-count tree)
				 new-left new-right cmp-fn))))))

(defun wb-bag-tree-trim (tree lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree)
	   (type function cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo hedge-negative-infinity)
		  (greater-than?-cmp (svref (car tree) (1- (length (the simple-vector (car tree)))))
				     lo cmp-fn))
	      (or (eq hi hedge-positive-infinity)
		  (less-than?-cmp (svref (car tree) 0) hi cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo hedge-negative-infinity)
					0
				      (vector-set-binary-search-lo (car tree) lo cmp-fn)))
		    (split-point-hi (if (eq hi hedge-positive-infinity)
					(length (the simple-vector (car tree)))
				      (vector-set-binary-search-hi (car tree) hi cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((val (wb-bag-tree-node-value tree)))
	   (if (or (eq lo hedge-negative-infinity)
		   (greater-than?-cmp val lo cmp-fn))
	       (if (or (eq hi hedge-positive-infinity)
		       (less-than?-cmp val hi cmp-fn))
		   tree
		 (wb-bag-tree-trim (wb-bag-tree-node-left tree) lo hi cmp-fn))
	     (wb-bag-tree-trim (wb-bag-tree-node-right tree) lo hi cmp-fn))))))

(defun wb-bag-tree-concat (value count left right cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree left right))
  (cond ((null left)
	 (wb-bag-tree-with right value cmp-fn count))
	((null right)
	 (wb-bag-tree-with left value cmp-fn count))
	((and (wb-bag-tree-node? left)
	      (> (1+ (wb-bag-tree-size left))
		 (wb-tree-balance-delta-fn (1+ (wb-bag-tree-size right)))))
	 (wb-bag-tree-build-node (wb-bag-tree-node-value left)
				 (wb-bag-tree-node-count left)
				 (wb-bag-tree-node-left left)
				 (wb-bag-tree-concat value count
						     (wb-bag-tree-node-right left)
						     right cmp-fn)))
	((and (wb-bag-tree-node? right)
	      (> (1+ (wb-bag-tree-size right))
		 (wb-tree-balance-delta-fn (1+ (wb-bag-tree-size left)))))
	 (wb-bag-tree-build-node (wb-bag-tree-node-value right)
				 (wb-bag-tree-node-count right)
				 (wb-bag-tree-concat value count left (wb-bag-tree-node-left right) cmp-fn)
				 (wb-bag-tree-node-right right)))
	(t
	 (wb-bag-tree-build-node value count left right))))

(defun wb-bag-tree-join (left right cmp-fn)
  (if (null left) right
    (if (null right) left
      (let ((min-val min-count (wb-bag-tree-minimum-pair right)))
	(wb-bag-tree-concat min-val min-count
			    left (wb-bag-tree-less-minimum right cmp-fn) cmp-fn)))))

(defun wb-bag-tree-minimum-pair (tree)
  "Assumes `tree' is nonempty.  Returns the minimum value and count as two
values.  The value may be an `Equivalent-Bag', in which case, as usual, the
count is not meaningful."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (if (consp tree)
      (values (svref (car tree) 0)
	      (svref (cdr tree) 0))
    (let ((left (wb-bag-tree-node-left tree)))
      (if left
	  (wb-bag-tree-minimum-pair left)
	(values (wb-bag-tree-node-value tree)
		(wb-bag-tree-node-count tree))))))

(defun wb-bag-tree-less-minimum (tree cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum value
removed."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree tree))
  (if (consp tree)
      (and (> (length (the simple-vector (car tree))) 1)
	   (cons (vector-subseq (car tree) 1)
		 (vector-subseq (cdr tree) 1)))
    (let ((left (wb-bag-tree-node-left tree)))
      (if left
	  (wb-bag-tree-concat (wb-bag-tree-node-value tree) (wb-bag-tree-node-count tree)
			      (wb-bag-tree-less-minimum left cmp-fn) (wb-bag-tree-node-right tree) cmp-fn)
	(wb-bag-tree-node-right tree)))))

(defun wb-bag-tree-build-node (value count left right)
  "Constructs a `WB-Bag-Tree', performing one rebalancing step if required.
`value' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-bag-tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (equivalent-node? value))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  wb-tree-max-vector-length))
	  (cons (concatenate 'simple-vector (car left) (vector value) (car right))
		(concatenate 'simple-vector (cdr left) (vector count) (cdr right)))
	(make-wb-bag-tree-node value count left right))
    (let ((wgtl (1+ (wb-bag-tree-size left)))
	  (wgtr (1+ (wb-bag-tree-size right))))
      (declare (fixnum wgtl wgtr))
      (cond ((and (wb-bag-tree-node? left) (> wgtl (wb-tree-balance-delta-fn wgtr)))
	     (let ((ll (wb-bag-tree-node-left left))
		   (rl (wb-bag-tree-node-right left)))
	       (if (or (null rl) (consp rl)
		       (< (1+ (wb-bag-tree-size rl))
			  (wb-tree-balance-gamma-fn (1+ (wb-bag-tree-size ll)))))
		   (make-wb-bag-tree-node (wb-bag-tree-node-value left)
					  (wb-bag-tree-node-count left)
					  ll
					  (wb-bag-tree-build-node value count rl right))
		 (make-wb-bag-tree-node (wb-bag-tree-node-value rl)
					(wb-bag-tree-node-count rl)
					(wb-bag-tree-build-node
					  (wb-bag-tree-node-value left)
					  (wb-bag-tree-node-count left)
					  ll
					  (wb-bag-tree-node-left rl))
					(wb-bag-tree-build-node
					  value count (wb-bag-tree-node-right rl)
					  right)))))
	    ((and (wb-bag-tree-node? right) (> wgtr (wb-tree-balance-delta-fn wgtl)))
	     (let ((lr (wb-bag-tree-node-left right))
		   (rr (wb-bag-tree-node-right right)))
	       (if (or (null lr) (consp lr)
		       (< (1+ (wb-bag-tree-size lr))
			  (wb-tree-balance-gamma-fn (1+ (wb-bag-tree-size rr)))))
		   (make-wb-bag-tree-node (wb-bag-tree-node-value right)
					  (wb-bag-tree-node-count right)
					  (wb-bag-tree-build-node value count left lr)
					  rr)
		 (make-wb-bag-tree-node (wb-bag-tree-node-value lr)
					(wb-bag-tree-node-count lr)
					(wb-bag-tree-build-node
					  value count left (wb-bag-tree-node-left lr))
					(wb-bag-tree-build-node
					  (wb-bag-tree-node-value right)
					  (wb-bag-tree-node-count right)
					  (wb-bag-tree-node-right lr)
					  rr)))))
	    (t
	     (make-wb-bag-tree-node value count left right))))))


(defun wb-bag-tree-verify (tree cmp-fn)
  (wb-bag-tree-verify-rng tree hedge-negative-infinity hedge-positive-infinity cmp-fn))

(defun wb-bag-tree-verify-rng (tree lo hi cmp-fn)
  (declare (type function cmp-fn) (optimize (debug 3)))
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len wb-tree-max-vector-length)
		(do ((i 0 (1+ i))
		     (prev lo))
		    ((= i len)
		     (or (eq hi hedge-positive-infinity)
			 (less-than?-cmp prev hi cmp-fn)))
		  (let ((elt (svref (car tree) i)))
		    (unless (and (not (equivalent-node? elt))
				 (or (eq prev hedge-negative-infinity)
				     (less-than?-cmp prev elt cmp-fn)))
		      (return nil))
		    (setq prev elt))))))
	(t
	 (let ((left (wb-bag-tree-node-left tree))
	       (right (wb-bag-tree-node-right tree))
	       ((sizl (wb-bag-tree-size left))
		(sizr (wb-bag-tree-size right)))
	       (value (wb-bag-tree-node-value tree)))
	   (and (wb-bag-tree-node-count tree)
		(= (wb-bag-tree-node-size tree) (+ sizl sizr (bag-value-size value)))
		(= (wb-bag-tree-node-total-count tree)
		   (+ (wb-bag-tree-total-count left)
		      (wb-bag-tree-total-count right)
		      (if (equivalent-node? value)
			  (gmap (:result sum) #'cdr (:arg list (equivalent-node-list value)))
			(wb-bag-tree-node-count tree))))
		(or (not (equivalent-node? value))
		    (> (length (equivalent-node-list value)) 1))
		(or (<= sizr 4)
		    (<= sizl (* wb-tree-balance-factor-limit sizr)))
		(or (<= sizl 4)
		    (<= sizr (* wb-tree-balance-factor-limit sizl)))
		(wb-bag-tree-verify-rng (wb-bag-tree-node-left tree) lo value cmp-fn)
		(wb-bag-tree-verify-rng (wb-bag-tree-node-right tree) value hi cmp-fn))))))


;;; ================================================================================
;;; Vector pair bag operations

(defun wb-bag-tree-vector-pair-union (pr1 pr2 lo hi cmp-fn)
  (declare (type function cmp-fn))
  (let ((new-pr any-equivalent? (vector-pair-bag-union pr1 pr2 lo hi cmp-fn)))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i (length (car new-pr)))
	    (setq result (wb-bag-tree-with result (svref (car new-pr) i)
					   cmp-fn (svref (cdr new-pr) i))))
	  result)
      (if (> (length (car new-pr)) wb-tree-max-vector-length)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (make-wb-bag-tree-node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (vector-subseq (car new-pr) 0 split-point)
					 (vector-subseq (cdr new-pr) 0 split-point))
				   (cons (vector-subseq (car new-pr) (1+ split-point))
					 (vector-subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun vector-pair-bag-union (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (reverse-list-to-vector vals)
		       (reverse-list-to-vector counts))
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
		  (push (equivalent-bag-union val1 (svref counts1 i1)
					      val2 (svref counts2 i2) cmp-fn)
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun wb-bag-tree-vector-pair-sum (pr1 pr2 lo hi cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type cons pr1 pr2)
	   (type function cmp-fn))
  (let ((new-pr any-equivalent? (vector-pair-bag-sum pr1 pr2 lo hi cmp-fn))
	((len (length (the simple-vector (car new-pr))))))
    (if any-equivalent?
	;; Let's just do it the slow way -- it's not supposed to happen often.
	(let ((result nil))
	  ;; Hmm -- need a generalization of `reduce' to multiple sequences.
	  (dotimes (i len)
	    (setq result (wb-bag-tree-with result (svref (car new-pr) i)
					   cmp-fn (svref (cdr new-pr) i))))
	  result)
      (if (> len wb-tree-max-vector-length)
	  (let ((split-point (floor len 2)))
	    (make-wb-bag-tree-node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (vector-subseq (car new-pr) 0 split-point)
					 (vector-subseq (cdr new-pr) 0 split-point))
				   (cons (vector-subseq (car new-pr) (1+ split-point))
					 (vector-subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun vector-pair-bag-sum (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 (values (cons (reverse-list-to-vector vals)
		       (reverse-list-to-vector counts))
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
		  (push (equivalent-bag-union val1 (svref counts1 i1)
					      val2 (svref counts2 i2) cmp-fn)
			vals)
		  (push 0 counts)
		  (incf i1)
		  (incf i2)
		  (setq any-equivalent? t)))))))))

(defun vector-pair-bag-intersect (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (reverse-list-to-vector vals)
			 (reverse-list-to-vector counts))))
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

(defun vector-pair-bag-product (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref vals2 i2) cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref vals2 (1- len2)) hi cmp-fn)))
	(decf len2)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (and vals (cons (reverse-list-to-vector vals)
			 (reverse-list-to-vector counts))))
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

(defun vector-pair-bag-diff (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref vals1 (1- len1)) hi cmp-fn)))
	(decf len1)))
    (do ((vals nil)
	 (counts nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref vals1 i1) vals)
	   (push (svref counts1 i1) counts)
	   (incf i1))
	 (and vals (cons (reverse-list-to-vector vals)
			 (reverse-list-to-vector counts))))
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

(defun vector-pair-bag-subbag? (pr1 pr2 lo hi cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref vals1 i1) cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
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

(defmacro do-wb-bag-tree-pairs ((val-var count-var tree-form &optional value-form)
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
			(,recur-fn (wb-bag-tree-node-left tree))
			(let ((value (wb-bag-tree-node-value tree)))
			  (if (equivalent-node? value)
			      (dolist (pr (equivalent-node-list value))
				(,body-fn (car pr) (cdr pr)))
			    (,body-fn value (wb-bag-tree-node-count tree))))
			(,recur-fn (wb-bag-tree-node-right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))


;;; ----------------
;;; Stateful iterator

(defun make-wb-bag-tree-iterator (tree)
  (let ((iter (make-wb-bag-tree-iterator-internal tree)))
    (lambda (op)
      (ecase op
	(:get (wb-bag-tree-iterator-get iter))
	(:done? (wb-bag-tree-iterator-done? iter))
	(:more? (not (wb-bag-tree-iterator-done? iter)))))))

(defun make-wb-bag-tree-iterator-internal (tree)
  (wb-bag-tree-iterator-canonicalize
    (make-wb-tree-iterator tree (wb-bag-tree-size tree) 3 t)))

(defun wb-bag-tree-iterator-canonicalize (iter)
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
	    ((and (= idx1 0) (wb-bag-tree-node-left node))
	     (unless (< (+ sp 5) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 3)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (wb-bag-tree-node-left node))
	     (setf (svref iter (+ sp 1)) 0)
	     (setf (svref iter (+ sp 2)) 0))
	    ((= idx1 0)
	     (setf (svref iter (+ sp 1)) 1))
	    (t
	     (let ((val (wb-bag-tree-node-value node)))
	       (if (equivalent-node? val)
		   (let ((alist (equivalent-node-list val)))
		     (if (< (1- idx1) (length alist))
			 (if (< idx2 (the fixnum (cdr (nth (1- idx1) alist))))
			     (return)
			   (progn
			     (incf (the fixnum (svref iter (+ sp 1))))
			     (setf (svref iter (+ sp 2)) 0)))
		       (progn
			 ;; Tail recursion
			 (setf (svref iter sp) (wb-bag-tree-node-right node))
			 (setf (svref iter (+ sp 1)) 0)
			 (setf (svref iter (+ sp 2)) 0))))
		 (if (= idx1 1)
		     (if (< idx2 (the fixnum (wb-bag-tree-node-count node)))
			 (return)
		       (incf (the fixnum (svref iter (+ sp 1)))))
		   (progn
		     ;; Tail recursion
		     (setf (svref iter sp) (wb-bag-tree-node-right node))
		     (setf (svref iter (+ sp 1)) 0)
		     (setf (svref iter (+ sp 2)) 0)))))))))
  iter)

(defun wb-bag-tree-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun wb-bag-tree-iterator-get (iter)
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
	     (wb-bag-tree-iterator-canonicalize iter)
	     (values (svref (car node) idx1) t)))
	  (t
	   (let ((val (wb-bag-tree-node-value node)))
	     (if (equivalent-node? val)
		 (let ((alist (equivalent-node-list val)))
		   (incf (the fixnum (svref iter (+ sp 2))))
		   (wb-bag-tree-iterator-canonicalize iter)
		   (values (car (nth (1- idx1) alist)) t))
	       (progn
		 (incf (the fixnum (svref iter (+ sp 2))))
		 (wb-bag-tree-iterator-canonicalize iter)
		 (values val t))))))))


;;; Map-style bag iterator

(defun make-wb-bag-tree-pair-iterator (tree)
  (let ((iter (make-wb-bag-tree-pair-iterator-internal tree)))
    (lambda (op)
      (ecase op
	(:get (wb-bag-tree-pair-iterator-get iter))
	(:done? (wb-bag-tree-pair-iterator-done? iter))
	(:more? (not (wb-bag-tree-pair-iterator-done? iter)))))))

(defun make-wb-bag-tree-pair-iterator-internal (tree)
  (wb-bag-tree-pair-iterator-canonicalize
    (make-wb-tree-iterator tree (wb-bag-tree-size tree) 2 t)))

(defun wb-bag-tree-pair-iterator-canonicalize (iter)
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
	    ((and (= idx 0) (wb-bag-tree-node-left node))
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (wb-bag-tree-node-left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx 0)
	     (setf (svref iter (1+ sp)) 1))
	    ((= idx (1+ (bag-value-size (wb-bag-tree-node-value node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (wb-bag-tree-node-right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun wb-bag-tree-pair-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun wb-bag-tree-pair-iterator-get (iter)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sp (svref iter 0))
	((node (svref iter sp))
	 (idx (svref iter (1+ sp)))))
    (declare (fixnum idx))
    (if (null node)
	(values nil 0 nil)
      (progn
	(setf (svref iter (1+ sp)) (1+ idx))
	(if (consp node)
	    (progn
	      (when (= (1+ idx) (length (the simple-vector (car node))))
		(wb-bag-tree-pair-iterator-canonicalize iter))
	      (values (svref (car node) idx) (svref (cdr node) idx) t))
	  (let ((val (wb-bag-tree-node-value node)))
	    (when (= idx (bag-value-size val))
	      (wb-bag-tree-pair-iterator-canonicalize iter))
	    (if (equivalent-node? val)
		(let ((pr (nth (1- idx) (equivalent-node-list val))))
		  (values (car pr) (cdr pr) t))
	      (values val (wb-bag-tree-node-count node) t))))))))

;;; ----------------
;;; Functional iterators.  Fun!!!

(defun wb-bag-tree-fun-iter (tree)
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
	     (walk (wb-bag-tree-node-left node)
		   (let ((value (wb-bag-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (equivalent-node-list value))
			   (iter (prs)
			     (if prs
				 (copies prs 0)
			       (walk (wb-bag-tree-node-right node) cont)))
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
			   (if (gen < j (wb-bag-tree-node-count node))
			       (lambda (op)
				 (ecase op
				   (:first (values value t))
				   (:rest (copies (1+ j)))
				   (:empty? nil)
				   (:more? t)))
			     (walk (wb-bag-tree-node-right node) cont))))))))))))

(defun wb-bag-tree-rev-fun-iter (tree)
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
	     (walk (wb-bag-tree-node-right node)
		   (let ((value (wb-bag-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (reverse (equivalent-node-list value)))
			   (iter (prs)
			     (if prs
				 (copies prs 0)
			       (walk (wb-bag-tree-node-left node) cont)))
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
			   (if (gen < j (wb-bag-tree-node-count node))
			       (lambda (op)
				 (ecase op
				   (:first (values value t))
				   (:rest (copies (1+ j)))
				   (:empty? nil)
				   (:more? t)))
			     (walk (wb-bag-tree-node-left node) cont))))))))))))

(defun wb-bag-tree-pair-fun-iter (tree &optional (cont (lambda (op)
							 (ecase op
							   (:first (values nil nil))
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
	     (walk (wb-bag-tree-node-left node)
		   (let ((value (wb-bag-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (equivalent-node-list value))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-bag-tree-node-right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value (wb-bag-tree-node-count node) t))
			   (:rest (walk (wb-bag-tree-node-right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun wb-bag-tree-pair-rev-fun-iter (tree &optional (cont (lambda (op)
							     (ecase op
							       (:first (values nil nil))
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
	     (walk (wb-bag-tree-node-right node)
		   (let ((value (wb-bag-tree-node-value node)))
		     (if (equivalent-node? value)
			 (rlabels (iter (reverse (equivalent-node-list value)))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-bag-tree-node-left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values value (wb-bag-tree-node-count node) t))
			   (:rest (walk (wb-bag-tree-node-left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ================================================================================
;;; Equivalent-Bag routines

(defun equivalent-bag-sum (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((alist1 (equivalent-node-list val1)))
	(if (equivalent-node? val2)
	    (let ((alist2 (copy-list (equivalent-node-list val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (if pr2
		      (progn (push (cons (car pr1) (gen + (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (make-equivalent-bag result))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (if pr1
		(make-equivalent-bag (cons (cons (car pr1) (gen + (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (make-equivalent-bag (cons (cons val2 count2) alist1))))))
    (if (equivalent-node? val2)
	(let ((alist2 (equivalent-node-list val2))
	      ((pr2 (assoc val1 alist2 :test (equal?-fn cmp-fn)))))
	  (if pr2
	      (make-equivalent-bag (cons (cons val1 (gen + count1 (cdr pr2)))
					 (cl:remove pr2 alist2)))
	    (make-equivalent-bag (cons (cons val1 count1) alist2))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (values val1 (gen + count1 count2))
	(make-equivalent-bag (list (cons val1 count1) (cons val2 count2)))))))

(defun equivalent-bag-union (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((alist1 (equivalent-node-list val1)))
	(if (equivalent-node? val2)
	    (let ((alist2 (copy-list (equivalent-node-list val2)))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (if pr2
		      (progn (push (cons (car pr1) (gen max (cdr pr1) (cdr pr2)))
				   result)
			     (setq alist2 (delete pr2 alist2)))
		    (push pr1 result))))
	      (setq result (nconc alist2 result))
	      (make-equivalent-bag result))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (if pr1
		(make-equivalent-bag (cons (cons (car pr1) (gen max (cdr pr1) count2))
					   (cl:remove pr1 alist1)))
	      (make-equivalent-bag (cons (cons val2 count2) alist1))))))
    (if (equivalent-node? val2)
	(let ((alist2 (equivalent-node-list val2))
	      ((pr2 (assoc val1 alist2 :test (equal?-fn cmp-fn)))))
	  (if pr2
	      (make-equivalent-bag (cons (cons val1 (gen max count1 (cdr pr2)))
					 (cl:remove pr2 alist2)))
	    (make-equivalent-bag (cons (cons val1 count1) alist2))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (values val1 (gen max count1 count2))
	(make-equivalent-bag (list (cons val1 count1) (cons val2 count2)))))))

(defun equivalent-bag-intersect (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((alist1 (equivalent-node-list val1)))
	(if (equivalent-node? val2)
	    (let ((alist2 (equivalent-node-list val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (when pr2
		    (push (cons (car pr1) (gen min (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (make-equivalent-bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (and pr1
		 (values t (car pr1) (gen min (cdr pr1) count2))))))
    (if (equivalent-node? val2)
	(let ((pr2 (assoc val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (values t val1 (gen min count1 (cdr pr2)))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (values t val1 (gen min count1 count2))))))

(defun equivalent-bag-product (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((alist1 (equivalent-node-list val1)))
	(if (equivalent-node? val2)
	    (let ((alist2 (equivalent-node-list val2))
		  (result nil))
	      (dolist (pr1 alist1)
		(let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		  (when pr2
		    (push (cons (car pr1) (gen * (cdr pr1) (cdr pr2)))
			  result))))
	      (cond ((null result) nil)
		    ((null (cdr result)) (values t (caar result) (cdar result)))
		    (t (values t (make-equivalent-bag result)))))
	  (let ((pr1 (assoc val2 alist1 :test (equal?-fn cmp-fn))))
	    (and pr1
		 (values t (car pr1) (gen * (cdr pr1) count2))))))
    (if (equivalent-node? val2)
	(let ((pr2 (assoc val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (values t val1 (gen * count1 (cdr pr2)))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (values t val1 (gen * count1 count2))))))

(defun equivalent-bag-difference (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (let ((alist1 (equivalent-node-list val1))
	    (alist2 (if (equivalent-node? val2) (equivalent-node-list val2)
		      (list (cons val2 count2))))
	    (result nil)
	    ;; Because `WB-Bag-Tree-Less' uses this, we have to return `val1' when appropriate.
	    (any-diff? nil))
	(dolist (pr1 alist1)
	  (let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
	    (if (null pr2)
		(push pr1 result)
	      (progn
		(setq any-diff? t)
		(when (gen > (cdr pr1) (cdr pr2))
		  (push (cons (car pr1)
			      (gen - (cdr pr1) (cdr pr2)))
			result))))))
	(cond ((not any-diff?) (values t val1 count1))
	      ((null result) nil)
	      ((null (cdr result)) (values t (caar result) (cdar result)))
	      (t (values t (make-equivalent-bag result)))))
    (if (equivalent-node? val2)
	(let ((pr2 (assoc val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))))
	  (cond ((null pr2)
		 (values t val1 count1))
		((gen > count1 (cdr pr2))
		 (values t val1 (gen - count1 (cdr pr2))))))
      (if (equal?-cmp val1 val2 cmp-fn)
	  (and (gen > count1 count2) (values t val1 (gen - count1 count2)))
	(values t val1 count1)))))

(defun equivalent-bag-subbag? (val1 count1 val2 count2 cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (and (equivalent-node? val2)
	   (let ((alist2 (equivalent-node-list val2)))
	     (dolist (pr1 (equivalent-node-list val1) t)
	       (let ((pr2 (assoc (car pr1) alist2 :test (equal?-fn cmp-fn))))
		 (unless (and pr2 (gen <= (cdr pr1) (cdr pr2)))
		   (return nil))))))
    (if (equivalent-node? val2)
	(let ((pr2 (assoc val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))))
	  (and pr2 (gen <= count1 (cdr pr2))))
      (and (equal?-cmp val1 val2 cmp-fn)
	   (gen <= count1 count2)))))

(defun equivalent-bag-disjoint? (val1 val2 cmp-fn)
  "Both `val1' and `val2' may be single values or `Equivalent-Node's of values.
If their intersection is null, returns true, else false."
  (declare (optimize (speed 3) (safety 0))
	   (type function cmp-fn))
  (if (equivalent-node? val1)
      (if (equivalent-node? val2)
	  (dolist (m1 (equivalent-node-list val1) t)
	    (when (assoc m1 (equivalent-node-list val2) :test (equal?-fn cmp-fn))
	      (return nil)))
	(not (assoc val2 (equivalent-node-list val1) :test (equal?-fn cmp-fn))))
    (if (equivalent-node? val2)
	(not (assoc val1 (equivalent-node-list val2) :test (equal?-fn cmp-fn)))
      (not (equal?-cmp val1 val2 cmp-fn)))))

(defun equivalent-bag-compare (val1 count1 val2 count2 cmp-fn)
  "Compares two pairs where the key of either or both may be an `Equivalent-Bag'."
  (declare (optimize (speed 3) (safety 0))
	   (type integer count1 count2)
	   (type function cmp-fn))
  (let ((comp (funcall cmp-fn val1 val2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (equivalent-node? val1)
	  (if (equivalent-node? val2)
	      (let ((mems1 (equivalent-node-list val1))
		    (mems2 (equivalent-node-list val2))
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
		       (let ((set1 (cl:reduce (fn (s x) (wb-set-tree-with s x #'compare)) (mapcar #'cdr mems1)
					      :initial-value nil))
			     (set2 (cl:reduce (fn (s x) (wb-set-tree-with s x #'compare)) (mapcar #'cdr mems2)
					      :initial-value nil))
			     ((comp (wb-set-tree-compare set1 set2 #'compare))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(cond ((equivalent-node? val2)
	       ':greater)
	      ((gen < count1 count2) ':less)
	      ((gen > count1 count2) ':greater)
	      (t comp))))))


;;; ================================================================================
;;; ================================================================================
;;; Maps

(declaim (inline make-raw-wb-map-tree-node))

;;; A map tree is either null, a node, or a cons of two simple-vectors.
(deftype wb-map-tree ()
  '(or null wb-map-tree-node cons))

(defstruct (wb-map-tree-node
	    (:constructor make-raw-wb-map-tree-node (size key value
						     left right))
	    (:predicate wb-map-tree-node?)
	    (:print-function wb-map-tree-node-print))
  (left  nil :type wb-map-tree :read-only t)
  (right nil :type wb-map-tree :read-only t)
  ;; If we get equivalent keys, then the `Key' is an `Equivalent-Node', and the
  ;; `Value' is unused.
  (key nil :read-only t)		; the domain value
  (value nil :read-only t)		; the range value
  (size 0 :type fixnum :read-only t))	; the number of < key, value > pairs


(defun wb-map-tree-node-print (node stream depth)
  "Print function for `WB-Map-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#map-node<~;~D, ~S -> ~S, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^, ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~{~S -> ~S~}~^, ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (wb-map-tree-node-size node)
		    (wb-map-tree-node-key node)
		    (wb-map-tree-node-value node)
		    (let ((sub (wb-map-tree-node-left node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))
		    (let ((sub (wb-map-tree-node-right node)))
		      (if (consp sub)
			  (list t (mapcar #'list (coerce (car sub) 'list)
					  (coerce (cdr sub) 'list)))
			(list nil sub)))))
    (format stream "#map-node<...>")))

(declaim (inline make-equivalent-map))

(defun make-equivalent-map (alist)
  (make-equivalent-node nil alist))

(declaim (ftype (function (t) fixnum) map-key-size))
(declaim (inline map-key-size))

(defun map-key-size (key)
  "The number of domain values represented by `key', which can be more than 1 if
`key' is an `Equivalent-Map'."
  (if (equivalent-node? key)
      (length (equivalent-node-list key))
    1))

(declaim (ftype (function (wb-map-tree) fixnum) wb-map-tree-size))
(declaim (inline wb-map-tree-size))

(defun wb-map-tree-size (tree)
  "The number of key/value pairs contained in this tree."
  (cond ((null tree) 0)
	((consp tree) (length (the simple-vector (car tree))))
	(t (wb-map-tree-node-size tree))))

(declaim (inline make-wb-map-tree-node))

(defun make-wb-map-tree-node (key value left right)
  "The low-level constructor for a map tree node."
  (declare (optimize (speed 3) (safety 0)))
  (make-raw-wb-map-tree-node (the fixnum
			       (+ (wb-map-tree-size left) (wb-map-tree-size right)
				  (map-key-size key)))
			     key value left right))


(defun wb-map-tree-arb-pair (tree)
  "Selects an arbitrary pair of the map.  Assumes it is nonnull."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (cond ((null tree)
	 (error "`WB-Map-Tree-Arb-Pair' called on empty tree"))
	((consp tree)
	 (values (svref (car tree) 0) (svref (cdr tree) 0)))
	(t
	 (let ((key (wb-map-tree-node-key tree)))
	   (if (equivalent-node? key)
	       (let ((pr (car (equivalent-node-list key))))
		 (values (car pr) (cdr pr)))
	     (values key (wb-map-tree-node-value tree)))))))

(defun wb-map-tree-least-pair (tree)
  "Assumes `tree' is nonempty.  Returns the least key and its value, or an
arbitrary least key and its value if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (let ((key val (wb-map-tree-minimum-pair tree)))
    (if (equivalent-node? key)
	(let ((pr (car (equivalent-node-list key))))
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

(defun wb-map-tree-greatest-pair (tree)
  "Assumes `tree' is nonempty.  Returns the greatest key and its value, or an
arbitrary greatest key and its value if there are more than one."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (if (consp tree)
      (let ((idx (1- (length (the simple-vector (car tree))))))
	(values (svref (car tree) idx)
		(svref (cdr tree) idx)))
    (let ((right (wb-map-tree-node-right tree)))
      (if right
	  (wb-map-tree-greatest-pair right)
	(let ((key (wb-map-tree-node-key tree)))
	  (if (equivalent-node? key)
	      (let ((pr (car (cl:last (equivalent-node-list key)))))
		(values (car pr) (cdr pr)))
	    (values key (wb-map-tree-node-value tree))))))))


;;; ================================================================================
;;; Lookup

(defun wb-map-tree-lookup (tree key key-cmp-fn)
  "If `tree' contains a pair whose key is equal to `key', returns three values:
true, the associated value, and the key found; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn)))
	   (and (eq found? ':equal)
		(values t (svref (cdr tree) idx) (svref (car tree) idx)))))
	(t
	 (let ((node-key (wb-map-tree-node-key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     (:equal (if (equivalent-node? node-key)
			 (let ((alist (equivalent-node-list node-key)))
			   (values t (cdar alist) (caar alist)))
		       (values t (wb-map-tree-node-value tree) node-key)))
	     (:unequal
	       (and (equivalent-node? node-key)
		    (let ((pr (assoc key (equivalent-node-list node-key) :test (equal?-fn key-cmp-fn))))
		      (and pr (values t (cdr pr) (car pr))))))
	     (:less
	       (wb-map-tree-lookup (wb-map-tree-node-left tree) key key-cmp-fn))
	     (:greater
	       (wb-map-tree-lookup (wb-map-tree-node-right tree) key key-cmp-fn)))))))

(defun wb-map-tree-find-equivalent (tree key key-cmp-fn)
  "If `tree' contains one or more keys equivalent to `value', returns (first
value) true, (second value) either the one key or an `Equivalent-Map'
containing the values, and (third value) if the second value was a single
key, the corresponding value; otherwise `nil'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn)))
	   (and found? (values t (svref (car tree) idx) (svref (cdr tree) idx)))))
	(t
	 (let ((node-key (wb-map-tree-node-key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     ((:equal :unequal) (values t node-key (wb-map-tree-node-value tree)))
	     (:less
	       (wb-map-tree-find-equivalent (wb-map-tree-node-left tree) key key-cmp-fn))
	     (:greater
	       (wb-map-tree-find-equivalent (wb-map-tree-node-right tree) key key-cmp-fn)))))))


;;; ================================================================================
;;; Map-with

;;; This could almost just call `WB-Map-Tree-Update', except that it has to handle the case
;;; where `key' is an `Equivalent-Node', which can be generated internally by `WB-Map-Tree-Split'
;;; via `WB-Map-Tree-Concat'.
(defun wb-map-tree-with (tree key value key-cmp-fn val-cmp-fn)
  "Returns a new tree like `tree' but with the pair < `key', `value' > added,
shadowing any previous pair with the same key."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn val-cmp-fn))
  (cond ((null tree)
	 (if (not (equivalent-node? key))
	     (cons (vector key) (vector value))
	   (make-wb-map-tree-node key nil nil nil)))
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   ;; We have to handle the case where `key' is an `Equivalent-Node' because
	   ;; this routine is called by `WB-Map-Tree-Concat'.
	   (if (and (eq found? ':equal) (not (equivalent-node? key)))
	       (if (equal?-cmp (svref (cdr tree) idx) value val-cmp-fn)
		   tree
		 (cons (car tree) (vector-update (cdr tree) idx value)))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 wb-tree-max-vector-length)
		      (not (equivalent-node? key)))
		 (cons (vector-insert (car tree) idx key)
		       (vector-insert (cdr tree) idx value))
	       (make-wb-map-tree-node (if found?
					  (equivalent-map-with (svref (car tree) idx) (svref (cdr tree) idx)
							       key value key-cmp-fn val-cmp-fn)
					key)
				      value
				      (and (> idx 0)
					   (cons (vector-subseq (car tree) 0 idx)
						 (vector-subseq (cdr tree) 0 idx)))
				      (and (< right-start (length (the simple-vector (car tree))))
					   (cons (vector-subseq (car tree) right-start)
						 (vector-subseq (cdr tree) right-start))))))))
	(t
	 (let ((node-key (wb-map-tree-node-key tree))
	       (node-val (wb-map-tree-node-value tree))
	       (node-left (wb-map-tree-node-left tree))
	       (node-right (wb-map-tree-node-right tree))
	       ((key-comp (funcall key-cmp-fn key node-key))))
	   (ecase key-comp
	     ((:equal :unequal)
	       (if (and (eq key-comp ':equal) (not (equivalent-node? key)) (not (equivalent-node? node-key))
			(equal?-cmp value node-val val-cmp-fn))
		  tree
		(make-wb-map-tree-node (equivalent-map-with node-key node-val key value key-cmp-fn val-cmp-fn)
				       value node-left node-right)))
	     ((:less)
	      (let ((new-left (wb-map-tree-with node-left key value key-cmp-fn val-cmp-fn)))
		(if (eq new-left node-left)
		    tree
		  (wb-map-tree-build-node node-key node-val new-left node-right))))
	     ((:greater)
	       (let ((new-right (wb-map-tree-with node-right key value key-cmp-fn val-cmp-fn)))
		 (if (eq new-right node-right)
		     tree
		   (wb-map-tree-build-node node-key node-val node-left new-right)))))))))

(defun vector-update (vec idx val)
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
(defun wb-map-tree-update (tree key value-fn default
   			   &optional (key-cmp-fn #'compare) (val-cmp-fn #'compare))
  "Returns a new tree like `tree', except that the value associated with `key'
is the result of calling `value-fn' on either the existing such value, if any,
or else `default'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function value-fn key-cmp-fn val-cmp-fn)
	   (dynamic-extent value-fn))
  (cond ((null tree)
	 (cons (vector key) (vector (funcall value-fn default))))
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn))
	       ((right-start (if found? (1+ idx) idx))))
	   (if (eq found? ':equal)
	       (let ((value (funcall value-fn (svref (cdr tree) idx))))
		 (if (equal?-cmp (svref (cdr tree) idx) value val-cmp-fn)
		     tree
		   (cons (car tree) (vector-update (cdr tree) idx value))))
	     (if (and (not found?)
		      (< (length (the simple-vector (car tree)))
			 wb-tree-max-vector-length))
		 (cons (vector-insert (car tree) idx key)
		       (vector-insert (cdr tree) idx (funcall value-fn default)))
	       (let ((new-key new-val (equivalent-map-update (svref (car tree) idx) (svref (cdr tree) idx)
							     key value-fn default key-cmp-fn val-cmp-fn)))
		 (make-wb-map-tree-node new-key new-val
					(and (> idx 0)
					     (cons (vector-subseq (car tree) 0 idx)
						   (vector-subseq (cdr tree) 0 idx)))
					(and (< right-start (length (the simple-vector (car tree))))
					     (cons (vector-subseq (car tree) right-start)
						   (vector-subseq (cdr tree) right-start)))))))))
	(t
	 (let ((node-key (wb-map-tree-node-key tree))
	       (node-val (wb-map-tree-node-value tree))
	       (node-left (wb-map-tree-node-left tree))
	       (node-right (wb-map-tree-node-right tree))
	       ((key-comp (funcall key-cmp-fn key node-key))))
	   (ecase key-comp
	     ((:equal :unequal)
	      (if (and (eq key-comp ':equal) (not (equivalent-node? node-val)))
		  (let ((new-val (funcall value-fn node-val)))
		    (if (equal?-cmp new-val node-val val-cmp-fn)
			tree
		      (make-wb-map-tree-node node-key new-val node-left node-right)))
		(let ((new-key new-val
			(equivalent-map-update node-key node-val key value-fn default key-cmp-fn val-cmp-fn)))
		  (make-wb-map-tree-node new-key new-val node-left node-right))))
	     ((:less)
	      (wb-map-tree-build-node node-key node-val
				      (wb-map-tree-update node-left key value-fn default key-cmp-fn val-cmp-fn)
				      node-right))
	     ((:greater)
	      (wb-map-tree-build-node node-key node-val node-left
				      (wb-map-tree-update node-right key value-fn default key-cmp-fn val-cmp-fn))))))))


;;; ================================================================================
;;; Map-less

(defun wb-map-tree-less (tree key key-cmp-fn)
  "Returns a new tree like `tree', but with any entry for `key' removed.
If such an entry was found, returns the associated value as a second value."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn)))
	   (if (eq found? ':equal)
	       (values (and (> (length (the simple-vector (car tree))) 1)
			    (cons (vector-remove-at (car tree) idx)
				  (vector-remove-at (cdr tree) idx)))
		       (svref (cdr tree) idx))
	     tree)))
	(t
	 (let ((node-key (wb-map-tree-node-key tree))
	       ((comp (funcall key-cmp-fn key node-key))))
	   (ecase comp
	     ((:equal :unequal)
	      (if (not (equivalent-node? node-key))
		  (if (eq comp ':unequal)
		      tree
		    (values (wb-map-tree-join (wb-map-tree-node-left tree)
					      (wb-map-tree-node-right tree) key-cmp-fn)
			    (wb-map-tree-node-value tree)))
		(let ((less-key less-val range-val (equivalent-map-less node-key key key-cmp-fn)))
		  (if (eq less-key node-key)
		      tree
		    (values (wb-map-tree-build-node less-key less-val (wb-map-tree-node-left tree)
						    (wb-map-tree-node-right tree))
			    range-val)))))
	     ((:less)
	      (let ((left (wb-map-tree-node-left tree))
		    ((new-left range-val (wb-map-tree-less left key key-cmp-fn))))
		(if (eq new-left left)
		    tree
		  (values (wb-map-tree-build-node node-key (wb-map-tree-node-value tree)
						  new-left (wb-map-tree-node-right tree))
			  range-val))))
	     ((:greater)
	      (let ((right (wb-map-tree-node-right tree))
		    ((new-right range-val (wb-map-tree-less right key key-cmp-fn))))
		(if (eq new-right right)
		    tree
		  (values (wb-map-tree-build-node node-key (wb-map-tree-node-value tree)
						  (wb-map-tree-node-left tree) new-right)
			  range-val)))))))))


(defun wb-map-tree-minimum-pair (tree)
  "Assumes `tree' is nonempty.  Returns the minimum key and value as two
values.  The key may be an `Equivalent-Map', in which case, as usual, the
value is not meaningful."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (if (consp tree)
      (values (svref (car tree) 0)
	      (svref (cdr tree) 0))
    (let ((left (wb-map-tree-node-left tree)))
      (if left
	  (wb-map-tree-minimum-pair left)
	(values (wb-map-tree-node-key tree)
		(wb-map-tree-node-value tree))))))

(defun wb-map-tree-less-minimum (tree key-cmp-fn)
  "Assumes `tree' is nonempty.  Returns a new tree with the minimum key/value
pair removed."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (if (consp tree)
      (and (> (length (the simple-vector (car tree))) 1)
	   (cons (vector-subseq (car tree) 1)
		 (vector-subseq (cdr tree) 1)))
    (let ((left (wb-map-tree-node-left tree)))
      (if left
	  (wb-map-tree-concat (wb-map-tree-node-key tree) (wb-map-tree-node-value tree)
			      (wb-map-tree-less-minimum left key-cmp-fn) (wb-map-tree-node-right tree)
			      key-cmp-fn)
	(wb-map-tree-node-right tree)))))


;;; ================================================================================
;;; Domain

;;; We've chosen a representation that makes this pretty fast (though still linear
;;; time).
(defun wb-map-tree-domain (tree)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree))
  (cond ((null tree) nil)
	((consp tree) (car tree))
	(t
	 (let ((key (wb-map-tree-node-key tree))
	       ((elt (if (equivalent-node? key)
			 (make-equivalent-set (mapcar #'car (equivalent-node-list key)))
		       key))))
	   (make-wb-set-tree-node elt
				  (wb-map-tree-domain (wb-map-tree-node-left tree))
				  (wb-map-tree-domain (wb-map-tree-node-right tree)))))))


;;; ================================================================================
;;; Union, intersection, and map difference

(defun wb-map-tree-union (tree1 tree2 val-fn key-cmp-fn val-cmp-fn)
  (wb-map-tree-union-rng tree1 tree2 val-fn hedge-negative-infinity hedge-positive-infinity key-cmp-fn val-cmp-fn))

(defun wb-map-tree-union-rng (tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn key-cmp-fn)
	   (type wb-map-tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (wb-map-tree-split tree1 lo hi key-cmp-fn))
	((null tree2)
	 (wb-map-tree-split tree1 lo hi key-cmp-fn))
	((null tree1)
	 (wb-map-tree-split tree2 lo hi key-cmp-fn))
	((and (consp tree1) (consp tree2))
	 (wb-map-tree-vector-pair-union tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (wb-map-tree-node-key tree2))
	       (val2 (wb-map-tree-node-value tree2))
	       ((eqvk1? eqvk1 eqvv1 (wb-map-tree-find-equivalent tree1 key2 key-cmp-fn))
		((nonnull? key val (if eqvk1? (equivalent-map-union eqvk1 eqvv1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
				     (values t key2 val2))))))
	   (wb-map-tree-concat-maybe
	     nonnull? key val
	     (wb-map-tree-union-rng (wb-map-tree-trim tree1 lo key2 key-cmp-fn)
				    (wb-map-tree-trim (wb-map-tree-node-left tree2) lo key2 key-cmp-fn)
				    val-fn lo key2 key-cmp-fn val-cmp-fn)
	     (wb-map-tree-union-rng (wb-map-tree-trim tree1 key2 hi key-cmp-fn)
				    (wb-map-tree-trim (wb-map-tree-node-right tree2) key2 hi key-cmp-fn)
				    val-fn key2 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))
	(t
	 (let ((key1 (wb-map-tree-node-key tree1))
	       (val1 (wb-map-tree-node-value tree1))
	       ((eqvk2? eqvk2 eqvv2 (wb-map-tree-find-equivalent tree2 key1 key-cmp-fn))
		((nonnull? key val (if eqvk2? (equivalent-map-union key1 val1 eqvk2 eqvv2 val-fn key-cmp-fn val-cmp-fn)
				     (values t key1 val1))))))
	   (wb-map-tree-concat-maybe
	     nonnull? key val
	     (wb-map-tree-union-rng (wb-map-tree-node-left tree1)
				    (wb-map-tree-trim tree2 lo key1 key-cmp-fn)
				    val-fn lo key1 key-cmp-fn val-cmp-fn)
	     (wb-map-tree-union-rng (wb-map-tree-node-right tree1)
				    (wb-map-tree-trim tree2 key1 hi key-cmp-fn)
				    val-fn key1 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))))

(defun wb-map-tree-intersect (tree1 tree2 val-fn key-cmp-fn val-cmp-fn)
  (wb-map-tree-intersect-rng tree1 tree2 val-fn hedge-negative-infinity hedge-positive-infinity key-cmp-fn val-cmp-fn))

(defun wb-map-tree-intersect-rng (tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function val-fn key-cmp-fn val-cmp-fn)
	   (type wb-map-tree tree1 tree2))
  (cond ((eq tree1 tree2)		; historically-related-map optimization
	 (wb-map-tree-split tree1 lo hi key-cmp-fn))
	((or (null tree1) (null tree2))
	 nil)
	((and (consp tree1) (consp tree2))
	 (vector-pair-intersect tree1 tree2 val-fn lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (wb-map-tree-node-key tree2))
	       (val2 (wb-map-tree-node-value tree2))
	       ((eqvk1? eqvk1 eqvv1 (wb-map-tree-find-equivalent tree1 key2 key-cmp-fn))
		((nonnull? key val
		   (and eqvk1? (equivalent-map-intersect eqvk1 eqvv1 key2 val2 val-fn key-cmp-fn val-cmp-fn))))))
	   (wb-map-tree-concat-maybe
	     nonnull? key val
	     (wb-map-tree-intersect-rng (wb-map-tree-trim tree1 lo key2 key-cmp-fn)
					(wb-map-tree-trim (wb-map-tree-node-left tree2) lo key2 key-cmp-fn)
					val-fn lo key2 key-cmp-fn val-cmp-fn)
	     (wb-map-tree-intersect-rng (wb-map-tree-trim tree1 key2 hi key-cmp-fn)
					(wb-map-tree-trim (wb-map-tree-node-right tree2) key2 hi key-cmp-fn)
					val-fn key2 hi key-cmp-fn val-cmp-fn)
	     key-cmp-fn)))
	(t
	 (let ((key1 (wb-map-tree-node-key tree1))
	       (val1 (wb-map-tree-node-value tree1))
	       ((eqvk2? eqvk2 eqvv2 (wb-map-tree-find-equivalent tree2 key1 key-cmp-fn))
		((nonnull? key val
		   (and eqvk2? (equivalent-map-intersect key1 val1 eqvk2 eqvv2 val-fn key-cmp-fn val-cmp-fn))))))
	   (wb-map-tree-concat-maybe nonnull? key val
				     (wb-map-tree-intersect-rng (wb-map-tree-node-left tree1)
								(wb-map-tree-trim tree2 lo key1 key-cmp-fn)
								val-fn lo key1 key-cmp-fn val-cmp-fn)
				     (wb-map-tree-intersect-rng (wb-map-tree-node-right tree1)
								(wb-map-tree-trim tree2 key1 hi key-cmp-fn)
								val-fn key1 hi key-cmp-fn val-cmp-fn)
				     key-cmp-fn)))))


(defun wb-map-tree-diff (tree1 tree2 key-cmp-fn val-cmp-fn)
  "Returns the pairs that are in `tree1' but not `tree2'."
  (wb-map-tree-diff-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity key-cmp-fn val-cmp-fn))

(defun wb-map-tree-diff-rng (tree1 tree2 lo hi key-cmp-fn val-cmp-fn)
  (cond ((eq tree1 tree2)		; historically-related tree optimization
	 nil)
	((or (null tree1) (null tree2))
	 (wb-map-tree-split tree1 lo hi key-cmp-fn))
	((and (consp tree1) (consp tree2))
	 (vector-pair-diff tree1 tree2 lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (wb-map-tree-node-key tree2))
	       (val2 (wb-map-tree-node-value tree2))
	       ((new-left
		  (wb-map-tree-diff-rng (wb-map-tree-trim tree1 lo key2 key-cmp-fn)
					(wb-map-tree-trim (wb-map-tree-node-left tree2) lo key2 key-cmp-fn)
					lo key2 key-cmp-fn val-cmp-fn))
		(new-right
		  (wb-map-tree-diff-rng (wb-map-tree-trim tree1 key2 hi key-cmp-fn)
					(wb-map-tree-trim (wb-map-tree-node-right tree2) key2 hi key-cmp-fn)
					key2 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk1? eqvk1 eqvv1 (wb-map-tree-find-equivalent tree1 key2 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (and eqvk1? (equivalent-map-difference eqvk1 eqvv1 key2 val2 key-cmp-fn val-cmp-fn))))))
	   (if nonnull1? (wb-map-tree-concat diffk1 diffv1 new-left new-right key-cmp-fn)
	     (wb-map-tree-join new-left new-right key-cmp-fn))))
	(t
	 (let ((key1 (wb-map-tree-node-key tree1))
	       (val1 (wb-map-tree-node-value tree1))
	       ((new-left
		  (wb-map-tree-diff-rng (wb-map-tree-trim (wb-map-tree-node-left tree1) lo key1 key-cmp-fn)
					(wb-map-tree-trim tree2 lo key1 key-cmp-fn)
					lo key1 key-cmp-fn val-cmp-fn))
		(new-right
		  (wb-map-tree-diff-rng (wb-map-tree-trim (wb-map-tree-node-right tree1) key1 hi key-cmp-fn)
					(wb-map-tree-trim tree2 key1 hi key-cmp-fn)
					key1 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk2? eqvk2 eqvv2 (wb-map-tree-find-equivalent tree2 key1 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (if eqvk2? (equivalent-map-difference key1 val1 eqvk2 eqvv2 key-cmp-fn val-cmp-fn)
		     (values t key1 val1))))))
	   (if nonnull1? (wb-map-tree-concat diffk1 diffv1 new-left new-right key-cmp-fn)
	     (wb-map-tree-join new-left new-right key-cmp-fn))))))

(defun wb-map-tree-diff-2 (tree1 tree2 key-cmp-fn val-cmp-fn)
  "Returns two values: one containing the pairs that are in `tree1' but not
`tree2', and the other containing the pairs that are in `tree2' but not
`tree1'."
  (wb-map-tree-diff-2-rng tree1 tree2 hedge-negative-infinity hedge-positive-infinity key-cmp-fn val-cmp-fn))

(defun wb-map-tree-diff-2-rng (tree1 tree2 lo hi key-cmp-fn val-cmp-fn)
  (cond ((eq tree1 tree2)		; historically-related tree optimization
	 (values nil nil))
	((or (null tree1) (null tree2))
	 (values (wb-map-tree-split tree1 lo hi key-cmp-fn)
		 (wb-map-tree-split tree2 lo hi key-cmp-fn)))
	((and (consp tree1) (consp tree2))
	 (vector-pair-diff-2 tree1 tree2 lo hi key-cmp-fn val-cmp-fn))
	((consp tree1)
	 (let ((key2 (wb-map-tree-node-key tree2))
	       (val2 (wb-map-tree-node-value tree2))
	       ((new-left-1 new-left-2
		  (wb-map-tree-diff-2-rng (wb-map-tree-trim tree1 lo key2 key-cmp-fn)
					  (wb-map-tree-trim (wb-map-tree-node-left tree2) lo key2 key-cmp-fn)
					  lo key2 key-cmp-fn val-cmp-fn))
		(new-right-1 new-right-2
		  (wb-map-tree-diff-2-rng (wb-map-tree-trim tree1 key2 hi key-cmp-fn)
					  (wb-map-tree-trim (wb-map-tree-node-right tree2) key2 hi key-cmp-fn)
					  key2 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk1? eqvk1 eqvv1 (wb-map-tree-find-equivalent tree1 key2 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (and eqvk1? (equivalent-map-difference eqvk1 eqvv1 key2 val2 key-cmp-fn val-cmp-fn)))
		 (nonnull2? diffk2 diffv2
		   (if eqvk1? (equivalent-map-difference key2 val2 eqvk1 eqvv1 key-cmp-fn val-cmp-fn)
		     (values t key2 val2))))))
	   (values (if nonnull1? (wb-map-tree-concat diffk1 diffv1 new-left-1 new-right-1 key-cmp-fn)
		     (wb-map-tree-join new-left-1 new-right-1 key-cmp-fn))
		   (if nonnull2? (wb-map-tree-concat diffk2 diffv2 new-left-2 new-right-2 key-cmp-fn)
		     (wb-map-tree-join new-left-2 new-right-2 key-cmp-fn)))))
	(t
	 (let ((key1 (wb-map-tree-node-key tree1))
	       (val1 (wb-map-tree-node-value tree1))
	       ((new-left-1 new-left-2
		  (wb-map-tree-diff-2-rng (wb-map-tree-trim (wb-map-tree-node-left tree1) lo key1 key-cmp-fn)
					  (wb-map-tree-trim tree2 lo key1 key-cmp-fn)
					  lo key1 key-cmp-fn val-cmp-fn))
		(new-right-1 new-right-2
		  (wb-map-tree-diff-2-rng (wb-map-tree-trim (wb-map-tree-node-right tree1) key1 hi key-cmp-fn)
					  (wb-map-tree-trim tree2 key1 hi key-cmp-fn)
					  key1 hi key-cmp-fn val-cmp-fn)))
	       ((eqvk2? eqvk2 eqvv2 (wb-map-tree-find-equivalent tree2 key1 key-cmp-fn))
		((nonnull1? diffk1 diffv1
		   (if eqvk2? (equivalent-map-difference key1 val1 eqvk2 eqvv2 key-cmp-fn val-cmp-fn)
		     (values t key1 val1)))
		 (nonnull2? diffk2 diffv2
		   (and eqvk2? (equivalent-map-difference eqvk2 eqvv2 key1 val1 key-cmp-fn val-cmp-fn))))))
	   (values (if nonnull1? (wb-map-tree-concat diffk1 diffv1 new-left-1 new-right-1 key-cmp-fn)
		     (wb-map-tree-join new-left-1 new-right-1 key-cmp-fn))
		   (if nonnull2? (wb-map-tree-concat diffk2 diffv2 new-left-2 new-right-2 key-cmp-fn)
		     (wb-map-tree-join new-left-2 new-right-2 key-cmp-fn)))))))


;;; ================================================================================
;;; Restrict and restrict-not

(defun wb-map-tree-restrict (map-tree set-tree key-cmp-fn)
  (wb-map-tree-restrict-rng map-tree set-tree hedge-negative-infinity hedge-positive-infinity key-cmp-fn))

(defun wb-map-tree-restrict-rng (map-tree set-tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree map-tree)
	   (type wb-set-tree set-tree))
  (cond ((or (null map-tree) (null set-tree))
	 nil)
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (vector-pair-restrict map-tree set-tree lo hi key-cmp-fn)
	   (let ((raw-elt (wb-set-tree-node-value set-tree))
		 ((set-elt (if (equivalent-node? raw-elt)
			       (car (equivalent-node-list raw-elt))
			     raw-elt))
		  ((new-left (wb-map-tree-restrict-rng
			       (wb-map-tree-trim map-tree lo set-elt key-cmp-fn)
			       (wb-set-tree-trim (wb-set-tree-node-left set-tree) lo set-elt key-cmp-fn)
			       lo set-elt key-cmp-fn))
		   (new-right (wb-map-tree-restrict-rng
				(wb-map-tree-trim map-tree set-elt hi key-cmp-fn)
				(wb-set-tree-trim (wb-set-tree-node-right set-tree) set-elt hi key-cmp-fn)
				set-elt hi key-cmp-fn))
		   (eqvk? eqvk eqvv (wb-map-tree-find-equivalent map-tree set-elt key-cmp-fn)))))
	     (if (not eqvk?)
		 (wb-map-tree-join new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (equivalent-map-restrict eqvk eqvv raw-elt key-cmp-fn)))
		 (if rpr? (wb-map-tree-concat rkey rval new-left new-right key-cmp-fn)
		   (wb-map-tree-join new-left new-right key-cmp-fn)))))))
	(t
	 (let ((raw-key (wb-map-tree-node-key map-tree))
	       ((map-key (if (equivalent-node? raw-key) ; for benefit of `compare'
			     (caar (equivalent-node-list raw-key))
			   raw-key))
		((new-left (wb-map-tree-restrict-rng (wb-map-tree-node-left map-tree)
						     (wb-set-tree-trim set-tree lo map-key key-cmp-fn)
						     lo map-key key-cmp-fn))
		 (new-right (wb-map-tree-restrict-rng (wb-map-tree-node-right map-tree)
						      (wb-set-tree-trim set-tree map-key hi key-cmp-fn)
						      map-key hi key-cmp-fn))
		 (eqvv? eqvv (wb-set-tree-find-equivalent set-tree map-key key-cmp-fn)))))
	   (if (not eqvv?)
	       (wb-map-tree-join new-left new-right key-cmp-fn)
	     (let ((map-val (wb-map-tree-node-value map-tree))
		   ((rpr? rkey rval (equivalent-map-restrict raw-key map-val eqvv key-cmp-fn))))
	       (if rpr? (wb-map-tree-concat rkey rval new-left new-right key-cmp-fn)
		 (wb-map-tree-join new-left new-right key-cmp-fn))))))))

(defun wb-map-tree-restrict-not (map-tree set-tree key-cmp-fn)
  (wb-map-tree-restrict-not-rng map-tree set-tree hedge-negative-infinity hedge-positive-infinity key-cmp-fn))

(defun wb-map-tree-restrict-not-rng (map-tree set-tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree map-tree)
	   (type wb-set-tree set-tree))
  (cond ((null map-tree)
	 nil)
	((null set-tree)
	 (wb-map-tree-split map-tree lo hi key-cmp-fn))
	((consp map-tree)
	 (if (simple-vector-p set-tree)
	     (vector-pair-restrict-not map-tree set-tree lo hi key-cmp-fn)
	   (let ((raw-elt (wb-set-tree-node-value set-tree))
		 ((set-elt (if (equivalent-node? raw-elt)
			       (car (equivalent-node-list raw-elt))
			     raw-elt))
		  ((new-left (wb-map-tree-restrict-not-rng
			       (wb-map-tree-trim map-tree lo set-elt key-cmp-fn)
			       (wb-set-tree-trim (wb-set-tree-node-left set-tree) lo set-elt key-cmp-fn)
			       lo set-elt key-cmp-fn))
		   (new-right (wb-map-tree-restrict-not-rng
				(wb-map-tree-trim map-tree set-elt hi key-cmp-fn)
				(wb-set-tree-trim (wb-set-tree-node-right set-tree) set-elt hi key-cmp-fn)
				set-elt hi key-cmp-fn))
		   (eqvk? eqvk eqvv (wb-map-tree-find-equivalent map-tree set-elt key-cmp-fn)))))
	     (if (not eqvk?)
		 (wb-map-tree-join new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (equivalent-map-restrict-not eqvk eqvv raw-elt key-cmp-fn)))
		 (if rpr? (wb-map-tree-concat rkey rval new-left new-right key-cmp-fn)
		   (wb-map-tree-join new-left new-right key-cmp-fn)))))))
	(t
	 (let ((raw-key (wb-map-tree-node-key map-tree))
	       ((map-key (if (equivalent-node? raw-key)
			     (caar (equivalent-node-list raw-key))
			   raw-key))
		((new-left (wb-map-tree-restrict-not-rng (wb-map-tree-node-left map-tree)
							 (wb-set-tree-trim set-tree lo map-key key-cmp-fn)
							 lo map-key key-cmp-fn))
		 (new-right (wb-map-tree-restrict-not-rng (wb-map-tree-node-right map-tree)
							  (wb-set-tree-trim set-tree map-key hi key-cmp-fn)
							  map-key hi key-cmp-fn))
		 (eqvv? eqvv (wb-set-tree-find-equivalent set-tree map-key key-cmp-fn)))))
	   (let ((map-val (wb-map-tree-node-value map-tree)))
	     (if (not eqvv?)
		 (wb-map-tree-concat raw-key map-val new-left new-right key-cmp-fn)
	       (let ((rpr? rkey rval (equivalent-map-restrict-not raw-key map-val eqvv key-cmp-fn)))
		 (if rpr? (wb-map-tree-concat rkey rval new-left new-right key-cmp-fn)
		   (wb-map-tree-join new-left new-right key-cmp-fn)))))))))

;;; ================================================================================
;;; Compare

(defun wb-map-tree-compare (tree1 tree2 key-cmp-fn val-cmp-fn)
  (if (eq tree1 tree2) ':equal
    (let ((size1 (wb-map-tree-size tree1))
	  (size2 (wb-map-tree-size tree2)))
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t (wb-map-tree-compare-rng tree1 0 tree2 0 0 size1 key-cmp-fn val-cmp-fn))))))

(defun wb-map-tree-compare-rng (tree1 base1 tree2 base2 lo hi key-cmp-fn val-cmp-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree1 tree2)
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
	 (invert-comparison (wb-map-tree-compare-rng tree2 base2 tree1 base1 lo hi key-cmp-fn val-cmp-fn)))
	(t
	 (let ((left1 (wb-map-tree-node-left tree1))
	       ((left1-size (the fixnum (wb-map-tree-size left1)))
		((new-hi (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (wb-map-tree-rank-trim left1 base1 lo new-hi))
		  (tree2a base2a (wb-map-tree-rank-trim tree2 base2 lo new-hi))
		  ((left-comp (wb-map-tree-compare-rng left1a base1a tree2a base2a
						       lo new-hi key-cmp-fn val-cmp-fn)))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((key1 (wb-map-tree-node-key tree1))
		   (val1 (wb-map-tree-node-value tree1))
		   (key2 val2
		      (wb-map-tree-rank-pair-internal
			tree2 (the fixnum (- new-hi base2))))
		   ((comp (equivalent-map-compare key1 val1 key2 val2 key-cmp-fn val-cmp-fn))))
	       (if (or (eq comp ':less) (eq comp ':greater))
		   comp
		 (let ((key1-size (map-key-size key1))
		       ((new-lo (the fixnum (+ base1 left1-size key1-size)))
			((right1a base1a
			   (wb-map-tree-rank-trim (wb-map-tree-node-right tree1)
						  (the fixnum
						    (+ base1 left1-size key1-size))
						  new-lo hi))
			 (tree2a base2a (wb-map-tree-rank-trim tree2 base2 new-lo hi))
			 ((right-comp
			    (wb-map-tree-compare-rng right1a base1a tree2a base2a
						     new-lo hi key-cmp-fn val-cmp-fn))))))
		   (if (not (eq right-comp ':equal))
		       right-comp
		     (if (eq left-comp ':unequal) ':unequal comp))))))))))

(defun wb-map-tree-rank-trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type fixnum base lo hi))
  (if (or (null tree) (consp tree))
      (values tree base)
    (let ((node-rank (the fixnum
		       (+ base (wb-map-tree-size (wb-map-tree-node-left tree))))))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (wb-map-tree-rank-trim (wb-map-tree-node-left tree) base lo hi))
	(wb-map-tree-rank-trim (wb-map-tree-node-right tree)
			       (+ node-rank (map-key-size (wb-map-tree-node-key tree)))
			       lo hi)))))

(defun wb-map-tree-rank (tree key key-cmp-fn)
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
		    (let ((found? idx (vector-set-binary-search (car tree) key key-cmp-fn)))
		      (values found? (+ idx base))))
		   (t
		    (let ((node-val (wb-map-tree-node-key tree))
			  (left (wb-map-tree-node-left tree))
			  ((left-size (wb-map-tree-size left))
			   ((node-base (+ base left-size))))
			  ((comp (funcall key-cmp-fn key node-val))))
		      (ecase comp
			(:equal (values t node-base))
			((:unequal)
			 (if (equivalent-node? node-val)
			     (let ((prs (equivalent-node-list node-val))
				   ((pos (cl:position key prs :test (equal?-fn key-cmp-fn)
						      :key #'car))))
			       (if pos (values t (+ node-base pos))
				 (values nil node-base)))
			   (values nil node-base)))
			((:less)
			 (rec left key base))
			((:greater)
			 (rec (wb-map-tree-node-right tree) key
			      (+ node-base (map-key-size node-val))))))))))
    (rec tree key 0)))

(defun wb-map-tree-rank-pair (tree rank)
  (let ((key value rem (wb-map-tree-rank-pair-internal tree rank)))
    (if (equivalent-node? key)
	(let ((pr (nth rem (equivalent-node-list key))))
	  (values (car pr) (cdr pr)))
      (values key value))))

(defun wb-map-tree-rank-pair-internal (tree rank)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type fixnum rank))
  (cond ((null tree)
	 (error "Bug in map comparator"))
	((consp tree)
	 (values (svref (car tree) rank) (svref (cdr tree) rank) 0))
	(t
	 (let ((left (wb-map-tree-node-left tree))
	       ((left-size (wb-map-tree-size left))))
	   (if (< rank left-size)
	       (wb-map-tree-rank-pair-internal left rank)
	     (let ((key (wb-map-tree-node-key tree))
		   ((key-size (map-key-size key)))
		   (rank (- rank left-size)))
	       (declare (type fixnum rank key-size))
	       (if (< rank key-size)
		   (values key (wb-map-tree-node-value tree) rank)
		 (wb-map-tree-rank-pair-internal (wb-map-tree-node-right tree)
						 (the fixnum (- rank key-size))))))))))


;;; ================================================================================
;;; Miscellany

(defun wb-map-tree-from-list (lst key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (type function key-fn value-fn))
  (let ((tree nil))
    (dolist (pr lst)
      (setq tree (wb-map-tree-with tree (funcall key-fn pr) (funcall value-fn pr) key-cmp-fn val-cmp-fn)))
    tree))

(defun wb-map-tree-from-iterable (it key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (type function it key-fn value-fn))
  (let ((tree nil))
    (while (funcall it ':more?)
      (let ((pr (funcall it ':get)))
	(setq tree (wb-map-tree-with tree (funcall key-fn pr) (funcall value-fn pr) key-cmp-fn val-cmp-fn))))
    tree))

;;; See `WB-Set-Tree-From-Sorted-Iterable'.
(defun wb-map-tree-from-sorted-iterable (it len key-fn value-fn key-cmp-fn val-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function it key-fn value-fn key-cmp-fn))
  (labels ((recur (n)
	     (declare (fixnum n))
	     (cond ((= n 0) (values nil hedge-positive-infinity hedge-negative-infinity))
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
			(:unequal (values (wb-map-tree-with (cons (vector ka) (vector va)) kb vb key-cmp-fn val-cmp-fn)
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
			  (values (wb-map-tree-build-node k v left right) left-first right-last)
			;; Fall back to the general case, being careful to keep the rightmost value for
			;; a duplicated key.
			(values (wb-map-tree-union (wb-map-tree-with left k v key-cmp-fn val-cmp-fn)
						   right (fn (_a b) b) key-cmp-fn val-cmp-fn)
				(if (less-than?-cmp left-first right-first key-cmp-fn) left-first right-first)
				(if (less-than?-cmp left-last right-last key-cmp-fn) right-last left-last))))))))
    (recur len)))

(defun wb-map-tree-split-above (tree value cmp-fn)
  (wb-map-tree-split tree value hedge-positive-infinity cmp-fn))

(defun wb-map-tree-split-below (tree value cmp-fn)
  (wb-map-tree-split tree hedge-negative-infinity value cmp-fn))


;;; ================================================================================
;;; Support routines for the above (maps)

(defun wb-map-tree-split (tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((and (eq lo hedge-negative-infinity) (eq hi hedge-positive-infinity))
	 tree)
	((consp tree)
	 (let ((keys (car tree))
	       (vals (cdr tree))
	       ((len (length (the simple-vector keys)))
		((split-point-lo (if (eq lo hedge-negative-infinity)
				     0
				   (vector-set-binary-search-lo keys lo key-cmp-fn)))
		 (split-point-hi (if (eq hi hedge-positive-infinity)
				     len
				   (vector-set-binary-search-hi keys hi key-cmp-fn))))))
	   (and (> split-point-hi split-point-lo)
		(if (and (= split-point-lo 0)
			 (= split-point-hi len))
		    tree
		  (cons (vector-subseq keys split-point-lo split-point-hi)
			(vector-subseq vals split-point-lo split-point-hi))))))
	((not (or (eq lo hedge-negative-infinity)
		  (greater-than?-cmp (wb-map-tree-node-key tree) lo key-cmp-fn)))
	 (wb-map-tree-split (wb-map-tree-node-right tree) lo hi key-cmp-fn))
	((not (or (eq hi hedge-positive-infinity)
		  (less-than?-cmp (wb-map-tree-node-key tree) hi key-cmp-fn)))
	 (wb-map-tree-split (wb-map-tree-node-left tree) lo hi key-cmp-fn))
	(t
	 (let ((new-left (wb-map-tree-split (wb-map-tree-node-left tree)
					    lo hedge-positive-infinity key-cmp-fn))
	       (new-right (wb-map-tree-split (wb-map-tree-node-right tree)
					     hedge-negative-infinity hi key-cmp-fn)))
	   (if (and (eq new-left (wb-map-tree-node-left tree))
		    (eq new-right (wb-map-tree-node-right tree)))
	       tree
	     (wb-map-tree-concat (wb-map-tree-node-key tree)
				 (wb-map-tree-node-value tree)
				 new-left new-right key-cmp-fn))))))

(defun wb-map-tree-trim (tree lo hi key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree tree)
	   (type function key-cmp-fn))
  (cond ((null tree) nil)
	((consp tree)
	 ;; If the vector pair is completely out of range, drop it.
	 (and (or (eq lo hedge-negative-infinity)
		  (greater-than?-cmp (svref (car tree) (1- (length (the simple-vector (car tree)))))
				     lo key-cmp-fn))
	      (or (eq hi hedge-positive-infinity)
		  (less-than?-cmp (svref (car tree) 0) hi key-cmp-fn))
	      ;; If it contains no elements within the range, also drop it.
	      (let ((split-point-lo (if (eq lo hedge-negative-infinity)
					0
				      (vector-set-binary-search-lo (car tree) lo key-cmp-fn)))
		    (split-point-hi (if (eq hi hedge-positive-infinity)
					(length (the simple-vector (car tree)))
				      (vector-set-binary-search-hi (car tree) hi key-cmp-fn))))
		(> split-point-hi split-point-lo))
	      tree))
	(t
	 (let ((key (wb-map-tree-node-key tree)))
	   (if (or (eq lo hedge-negative-infinity)
		   (greater-than?-cmp key lo key-cmp-fn))
	       (if (or (eq hi hedge-positive-infinity)
		       (less-than?-cmp key hi key-cmp-fn))
		   tree
		 (wb-map-tree-trim (wb-map-tree-node-left tree) lo hi key-cmp-fn))
	     (wb-map-tree-trim (wb-map-tree-node-right tree) lo hi key-cmp-fn))))))

(defun wb-map-tree-concat-maybe (pair? key value left right key-cmp-fn)
  (declare (optimize (speed 3) (safety 0)))
  (if pair? (wb-map-tree-concat key value left right key-cmp-fn)
    (wb-map-tree-join left right key-cmp-fn)))

(defun wb-map-tree-concat (key value left right key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree left right)
	   (type function key-cmp-fn))
  (cond ((null left)
	 ;; To avoid needing `val-cmp-fn' absolutely everywhere, and because the only reason it's
	 ;; needed here is to optimize out a redundant `with', and we know this `with' can't be
	 ;; redundant, we just pass a `val-cmp-fn' that disables the optimization.
	 (wb-map-tree-with right key value key-cmp-fn (fn (_x _y) ':unequal)))
	((null right)
	 (wb-map-tree-with left key value key-cmp-fn (fn (_x _y) ':unequal)))
	((and (wb-map-tree-node? left)
	      (> (1+ (wb-map-tree-node-size left))
		 (wb-tree-balance-delta-fn (1+ (wb-map-tree-size right)))))
	 (wb-map-tree-build-node (wb-map-tree-node-key left)
				 (wb-map-tree-node-value left)
				 (wb-map-tree-node-left left)
				 (wb-map-tree-concat key value
						     (wb-map-tree-node-right left)
						     right key-cmp-fn)))
	((and (wb-map-tree-node? right)
	      (> (1+ (wb-map-tree-node-size right))
		 (wb-tree-balance-delta-fn (1+ (wb-map-tree-size left)))))
	 (wb-map-tree-build-node (wb-map-tree-node-key right)
				 (wb-map-tree-node-value right)
				 (wb-map-tree-concat key value left
						     (wb-map-tree-node-left right) key-cmp-fn)
				 (wb-map-tree-node-right right)))
	(t
	 (wb-map-tree-build-node key value left right))))

(defun wb-map-tree-join (left right key-cmp-fn)
  (if (null left) right
    (if (null right) left
      (let ((min-key min-val (wb-map-tree-minimum-pair right)))
	(wb-map-tree-concat min-key min-val
			    left (wb-map-tree-less-minimum right key-cmp-fn) key-cmp-fn)))))

(defun wb-map-tree-build-node (key value left right)
  "Constructs a `WB-Map-Tree', performing one rebalancing step if required.
`key' must already be known to go between `left' and `right'."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-map-tree left right))
  (if (and (or (null left) (consp left))
	   (or (null right) (consp right)))
      (if (and (not (equivalent-node? key))
	       (< (+ (length-nv (the (or null simple-vector) (car left)))
		     (length-nv (the (or null simple-vector) (car right))))
		  wb-tree-max-vector-length))
	  (cons (concatenate 'simple-vector (car left) (vector key) (car right))
		(concatenate 'simple-vector (cdr left) (vector value) (cdr right)))
	(make-wb-map-tree-node key value left right))
    (let ((wgtl (1+ (wb-map-tree-size left)))
	  (wgtr (1+ (wb-map-tree-size right))))
      (declare (fixnum wgtl wgtr))
      (cond ((and (wb-map-tree-node? left) (> wgtl (wb-tree-balance-delta-fn wgtr)))
	     (let ((ll (wb-map-tree-node-left left))
		   (rl (wb-map-tree-node-right left)))
	       (if (or (null rl) (consp rl)
		       (< (1+ (wb-map-tree-size rl))
			  (wb-tree-balance-gamma-fn (1+ (wb-map-tree-size ll)))))
		   (make-wb-map-tree-node (wb-map-tree-node-key left)
					  (wb-map-tree-node-value left)
					  ll
					  (wb-map-tree-build-node key value rl right))
		 (make-wb-map-tree-node (wb-map-tree-node-key rl)
					(wb-map-tree-node-value rl)
					(wb-map-tree-build-node
					  (wb-map-tree-node-key left)
					  (wb-map-tree-node-value left)
					  ll
					  (wb-map-tree-node-left rl))
					(wb-map-tree-build-node
					  key value (wb-map-tree-node-right rl) right)))))
	    ((and (wb-map-tree-node? right) (> wgtr (wb-tree-balance-delta-fn wgtl)))
	     (let ((lr (wb-map-tree-node-left right))
		   (rr (wb-map-tree-node-right right)))
	       (if (or (null lr) (consp lr)
		       (< (1+ (wb-map-tree-size lr))
			  (wb-tree-balance-gamma-fn (1+ (wb-map-tree-size rr)))))
		   (make-wb-map-tree-node (wb-map-tree-node-key right)
					  (wb-map-tree-node-value right)
					  (wb-map-tree-build-node key value left lr)
					  rr)
		 (make-wb-map-tree-node (wb-map-tree-node-key lr)
					(wb-map-tree-node-value lr)
					(wb-map-tree-build-node
					  key value left (wb-map-tree-node-left lr))
					(wb-map-tree-build-node
					  (wb-map-tree-node-key right)
					  (wb-map-tree-node-value right)
					  (wb-map-tree-node-right lr)
					  rr)))))
	    (t
	     (make-wb-map-tree-node key value left right))))))


(defun wb-map-tree-verify (tree key-cmp-fn)
  (wb-map-tree-verify-rng tree hedge-negative-infinity hedge-positive-infinity key-cmp-fn))

(defun wb-map-tree-verify-rng (tree lo hi key-cmp-fn)
  (cond ((null tree) t)
	((consp tree)
	 (let ((len (length (car tree))))
	   (and (> len 0)
		(<= len wb-tree-max-vector-length)
		(do ((i 0 (1+ i))
		     (prev lo))
		    ((= i len)
		     (or (eq hi hedge-positive-infinity)
			 (less-than?-cmp prev hi key-cmp-fn)))
		  (let ((key (svref (car tree) i)))
		    (unless (and (not (equivalent-node? key))
				 (or (eq prev hedge-negative-infinity)
				     (less-than?-cmp prev key key-cmp-fn)))
		      (return nil))
		    (setq prev key))))))
	(t
	 (let ((sizl (wb-map-tree-size (wb-map-tree-node-left tree)))
	       (sizr (wb-map-tree-size (wb-map-tree-node-right tree)))
	       (key (wb-map-tree-node-key tree)))
	   (and (= (wb-map-tree-node-size tree) (+ sizl sizr (map-key-size key)))
		(or (not (equivalent-node? key))
		    (> (length (equivalent-node-list key)) 1))
		(or (<= sizr 4)
		    (<= sizl (* wb-tree-balance-factor-limit sizr)))
		(or (<= sizl 4)
		    (<= sizr (* wb-tree-balance-factor-limit sizl)))
		(wb-map-tree-verify-rng (wb-map-tree-node-left tree) lo key key-cmp-fn)
		(wb-map-tree-verify-rng (wb-map-tree-node-right tree) key hi key-cmp-fn))))))

(defun wb-map-tree-vector-pair-union (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
  (let ((new-pr any-equivalent? (vector-pair-union pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)))
    (if any-equivalent?
	(let ((tree nil))
	  ;; Let's just do it the stupid way -- it's not supposed to happen often.
	  (dotimes (i (length (car new-pr)))
	    ;; As above -- the funny `val-cmp-fn' value just blocks an optimization that can't fire anyway.
	    (setq tree (wb-map-tree-with tree (svref (car new-pr) i)
					 (svref (cdr new-pr) i) key-cmp-fn (fn (_x _y) ':unequal))))
	  tree)
      (if (> (length (car new-pr)) wb-tree-max-vector-length)
	  (let ((split-point (floor (length (car new-pr)) 2)))
	    (make-wb-map-tree-node (svref (car new-pr) split-point)
				   (svref (cdr new-pr) split-point)
				   (cons (vector-subseq (car new-pr) 0 split-point)
					 (vector-subseq (cdr new-pr) 0 split-point))
				   (cons (vector-subseq (car new-pr) (1+ split-point))
					 (vector-subseq (cdr new-pr) (1+ split-point)))))
	new-pr))))

(defun vector-pair-union (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref keys2 i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref keys2 (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil)
	 (any-equivalent? nil))
	((and (= i1 len1) (= i2 len2))
	 ;; The use of `:no-value' could produce an empty result.
	 (and keys (values (cons (reverse-list-to-vector keys)
				 (reverse-list-to-vector vals))
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
			  (equivalent-map-union key1 (svref vals1 i1)
			   key2 (svref vals2 i2) val-fn key-cmp-fn val-cmp-fn)))
		    (when nonnull?
		      (push key keys)
		      (push val vals)
		      (when (equivalent-node? key)
			(setq any-equivalent? t))))
		   (incf i1)
		   (incf i2)))))))))

(defun vector-pair-intersect (pr1 pr2 val-fn lo hi key-cmp-fn val-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (reverse-list-to-vector keys)
			 (reverse-list-to-vector vals))))
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

(defun vector-pair-diff (pr1 pr2 lo hi key-cmp-fn val-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref keys2 i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref keys1 (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref keys2 (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((diff-1-keys nil)
	 (diff-1-vals nil))
	((or (= i1 len1) (= i2 len2))
	 (do () ((= i1 len1))
	   (push (svref keys1 i1) diff-1-keys)
	   (push (svref vals1 i1) diff-1-vals)
	   (incf i1))
	 (and diff-1-keys (cons (reverse-list-to-vector diff-1-keys)
				(reverse-list-to-vector diff-1-vals))))
      (let ((key1 (svref keys1 i1))
	    (key2 (svref keys2 i2))
	    (val1 (svref vals1 i1))
	    (val2 (svref vals2 i2))
	    ((comp (funcall key-cmp-fn key1 key2))))
	(ecase comp
	  ((:equal)
	   (unless (equal?-cmp val1 val2 val-cmp-fn)
	     (push key1 diff-1-keys)
	     (push val1 diff-1-vals))
	   (incf i1)
	   (incf i2))
	  ((:less)
	   (push key1 diff-1-keys)
	   (push val1 diff-1-vals)
	   (incf i1))
	  ((:greater)
	   (incf i2))
	  ((:unequal)
	   (push key1 diff-1-keys)
	   (push val1 diff-1-vals)
	   (incf i1)
	   (incf i2)))))))

(defun vector-pair-diff-2 (pr1 pr2 lo hi key-cmp-fn val-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref keys1 i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref keys2 i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
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
	 (values (and diff-1-keys (cons (reverse-list-to-vector diff-1-keys)
					(reverse-list-to-vector diff-1-vals)))
		 (and diff-2-keys (cons (reverse-list-to-vector diff-2-keys)
					(reverse-list-to-vector diff-2-vals)))))
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

(defun vector-pair-restrict (map-pr set-vec lo hi key-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref map-keys i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref set-vec i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp (svref map-keys (1- len1)) hi key-cmp-fn)))
	(decf len1))
      (do () ((or (= i2 len2) (less-than?-cmp (svref set-vec (1- len2)) hi key-cmp-fn)))
	(decf len2)))
    (do ((keys nil)
	 (vals nil))
	((or (= i1 len1) (= i2 len2))
	 (and keys (cons (reverse-list-to-vector keys)
			 (reverse-list-to-vector vals))))
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

(defun vector-pair-restrict-not (map-pr set-vec lo hi key-cmp-fn)
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
    (unless (eq lo hedge-negative-infinity)
      (do () ((or (= i1 len1) (less-than?-cmp lo (svref map-keys i1) key-cmp-fn)))
	(incf i1))
      (do () ((or (= i2 len2) (less-than?-cmp lo (svref set-vec i2) key-cmp-fn)))
	(incf i2)))
    (unless (eq hi hedge-positive-infinity)
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
	 (and keys (cons (reverse-list-to-vector keys)
			 (reverse-list-to-vector vals))))
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

(defmacro do-wb-map-tree-pairs ((key-var value-var tree-form &optional value-form)
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
			(,recur-fn (wb-map-tree-node-left tree))
			(let ((key (wb-map-tree-node-key tree)))
			  (if (equivalent-node? key)
			      (dolist (pr (equivalent-node-list key))
				(,body-fn (car pr) (cdr pr)))
			    (,body-fn key (wb-map-tree-node-value tree))))
			(,recur-fn (wb-map-tree-node-right tree)))))))
	 (,recur-fn ,tree-form))
       ,value-form)))

(defun wb-map-tree-compose (tree fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  (and tree
       (if (consp tree)
	   (cons (car tree)
		 (gmap (:result vector :length (length (the simple-vector (cdr tree))))
		       fn (:arg simple-vector (cdr tree))))
	 (let ((key (wb-map-tree-node-key tree))
	       (val (wb-map-tree-node-value tree))
	       (new-left (wb-map-tree-compose (wb-map-tree-node-left tree) fn))
	       (new-right (wb-map-tree-compose (wb-map-tree-node-right tree) fn)))
	   (if (equivalent-node? key)
	       (make-wb-map-tree-node
		 (make-equivalent-map (mapcar (lambda (pr)
						(cons (car pr) (funcall fn (cdr pr))))
					      (equivalent-node-list key)))
		 val new-left new-right)
	     (make-wb-map-tree-node key (funcall fn val) new-left new-right))))))


;;; ----------------
;;; Stateful iterator

(defun make-wb-map-tree-iterator (tree)
  (let ((iter (make-wb-map-tree-iterator-internal tree)))
    (lambda (op)
      (ecase op
	(:get (wb-map-tree-iterator-get iter))
	(:done? (wb-map-tree-iterator-done? iter))
	(:more? (not (wb-map-tree-iterator-done? iter)))))))

(defun make-wb-map-tree-iterator-internal (tree)
  (wb-map-tree-iterator-canonicalize
    (make-wb-tree-iterator tree (wb-map-tree-size tree) 2 t)))

(defun wb-map-tree-iterator-canonicalize (iter)
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
	    ((and (= idx 0) (wb-map-tree-node-left node))
	     (unless (< (+ sp 3) (length iter))
	       (error "Internal FSet error: iterator stack overflow.  Please report this bug."))
	     (incf sp 2)
	     (setf (svref iter 0) sp)
	     (setf (svref iter sp) (wb-map-tree-node-left node))
	     (setf (svref iter (1+ sp)) 0))
	    ((= idx 0)
	     (setf (svref iter (1+ sp)) 1))
	    ((= idx (1+ (map-key-size (wb-map-tree-node-key node))))
	     ;; Tail recursion
	     (setf (svref iter sp) (wb-map-tree-node-right node))
	     (setf (svref iter (1+ sp)) 0))
	    (t (return)))))
  iter)

(defun wb-map-tree-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (null (svref iter (svref iter 0))))

(defun wb-map-tree-iterator-get (iter)
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
		(wb-map-tree-iterator-canonicalize iter))
	      (values (svref (car node) idx) (svref (cdr node) idx) t))
	  (let ((key (wb-map-tree-node-key node)))
	    (if (equivalent-node? key)
		(let ((pr (nth (1- idx) (equivalent-node-list key))))
		  (when (= idx (length (equivalent-node-list key)))
		    (wb-map-tree-iterator-canonicalize iter))
		  (values (car pr) (cdr pr) t))
	      (progn
		(when (= idx 1)
		  (wb-map-tree-iterator-canonicalize iter))
		(values key (wb-map-tree-node-value node) t)))))))))


;;; ----------------
;;; Functional iterators.  Fun!!!

(defun wb-map-tree-fun-iter (tree &optional (cont (lambda (op)
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
	     (walk (wb-map-tree-node-left node)
		   (let ((key (wb-map-tree-node-key node)))
		     (if (equivalent-node? key)
			 (rlabels (iter (equivalent-node-list key))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-map-tree-node-right node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values key (wb-map-tree-node-value node) t))
			   (:rest (walk (wb-map-tree-node-right node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))

(defun wb-map-tree-rev-fun-iter (tree &optional (cont (lambda (op)
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
	     (walk (wb-map-tree-node-right node)
		   (let ((key (wb-map-tree-node-key node)))
		     (if (equivalent-node? key)
			 (rlabels (iter (reverse (equivalent-node-list key)))
			   (iter (prs)
			     (if prs
				 (lambda (op)
				   (ecase op
				     (:first (values (caar prs) (cdar prs) t))
				     (:rest (iter (cdr prs)))
				     (:empty? nil)
				     (:more? t)))
			       (walk (wb-map-tree-node-left node) cont))))
		       (lambda (op)
			 (ecase op
			   (:first (values key (wb-map-tree-node-value node) t))
			   (:rest (walk (wb-map-tree-node-left node) cont))
			   (:empty? nil)
			   (:more? t)))))))))))


;;; ================================================================================
;;; Equivalent-Map routines

(defun equivalent-map-with (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  (let ((nonnull? key val (equivalent-map-union key1 val1 key2 val2 (fn (_a b) b) key-cmp-fn val-cmp-fn)))
    (declare (ignore nonnull?))
    ;; `nonnull?' must be true given `val-fn'.
    (values key val)))

(defun equivalent-map-union (key1 val1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
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
    (if (equivalent-node? key1)
	(if (equivalent-node? key2)
	    (let ((alist1 (equivalent-node-list key1))
		  (alist2 (copy-list (equivalent-node-list key2)))
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
		     (values t (make-equivalent-map result)))))
	  (let ((alist1 (equivalent-node-list key1))
		((pr1 (find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist1))
	    (if pr1
		(progn
		  (setq alist1 (remove pr1 alist1))
		  (let ((new-val second-val (compute-new-val (cdr pr1) val2)))
		    (if (eq second-val ':no-value)
			(if (null (cdr alist1))
			    (values t (caar alist1) (cdar alist1))
			  (values t (make-equivalent-map alist1)))
		      (values t (make-equivalent-map (cons (cons (car pr1) new-val) alist1))))))
	      (values t (make-equivalent-map (cons (cons key2 val2) alist1))))))
      (if (equivalent-node? key2)
	  (let ((alist2 (equivalent-node-list key2))
		((pr2 (find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist2))
	    (if pr2
		(progn
		  (setq alist2 (remove pr2 alist2))
		  (let ((new-val second-val (compute-new-val val1 (cdr pr2))))
		    (if (eq second-val ':no-value)
			(if (null (cdr alist2))
			    (values t (caar alist2) (cdar alist2))
			  (values t (make-equivalent-map alist2)))
		      (values t (make-equivalent-map (cons (cons key1 new-val) alist2))))))
	      (values t (make-equivalent-map (cons (cons key1 val1) alist2)))))
	(if (equal?-cmp key1 key2 key-cmp-fn)
	    (let ((new-val second-val (compute-new-val val1 val2)))
	      (and (not (eq second-val ':no-value))
		   (values t key1 new-val)))
	  (values t (make-equivalent-map (list (cons key1 val1) (cons key2 val2)))))))))

(defun equivalent-map-update (key1 val1 key2 value-fn default second-arg key-cmp-fn)
  "`key1' may be either a single value (representing a single key/value pair)
or an `Equivalent-Map' of key/value pairs.  That is, if `key1' is an
`Equivalent-Map', `val1' is ignored.  Returns one or more new key/value pairs
in which the value associated with `key2' is the result of calling `value-fn'
on (a) either the previous such value, if any, or else `default', and (b)
`second-arg'.  If the result is a single pair, it's returned as two values;
otherwise one value is returned, which is an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function value-fn key-cmp-fn))
  (if (equivalent-node? key1)
      (let ((alist1 (equivalent-node-list key1))
	    ((pr1 (find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	(declare (type list alist1))
	(if pr1
	    (make-equivalent-map (cons (cons key1 (funcall value-fn (cdr pr1) second-arg))
				       (remove pr1 alist1)))
	  (make-equivalent-map (cons (cons key2 (funcall value-fn default second-arg)) alist1))))
    (if (equal?-cmp key1 key2 key-cmp-fn)
	(values key1 (funcall value-fn val1 second-arg))
      (make-equivalent-map (list (cons key1 val1)
				 (cons key2 (funcall value-fn default second-arg)))))))

(defun equivalent-map-intersect (key1 val1 key2 val2 val-fn key-cmp-fn val-cmp-fn)
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
    (if (equivalent-node? key1)
	(if (equivalent-node? key2)
	    (let ((alist1 (equivalent-node-list key1))
		  (alist2 (equivalent-node-list key2))
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
		       (values t (make-equivalent-map result))
		     (values t (caar result) (cdar result)))))
	  (let ((alist1 (equivalent-node-list key1))
		((pr1 (cl:find key2 alist1 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist1))
	    (and pr1
		 (let ((new-val second-val (compute-new-val (cdr pr1) val2)))
		   (and (not (eq second-val ':no-value)) (values t (car pr1) new-val))))))
      (if (equivalent-node? key2)
	  (let ((alist2 (equivalent-node-list key2))
		((pr2 (cl:find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	    (declare (type list alist2))
	    (and pr2
		 (let ((new-val second-val (compute-new-val val1 (cdr pr2))))
		   (and (not (eq second-val ':no-value)) (values t key1 new-val)))))
	(and (equal?-cmp key1 key2 key-cmp-fn)
	     (let ((new-val second-val (compute-new-val val1 val2)))
	       (and (not (eq second-val ':no-value)) (values t key1 new-val))))))))

(defun equivalent-map-difference (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  "Both `key1' and `key2' may be single values (representing a single key/value
pair) or `Equivalent-Map's of key/value pairs.  That is, if `key1' is a
`Equivalent-Map', `val1' is ignored, and similarly for `key2' and `val2'.
If the difference is nonnull, returns two or three values: if it is a single
pair, returns true, the key, and the value; if it is more than one pair,
returns true and an `Equivalent-Map' of the pairs.  If the difference is
empty, returns false."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn val-cmp-fn))
  (if (equivalent-node? key1)
      (let ((alist1 (equivalent-node-list key1)))
	(declare (type list alist1))
	(let ((alist2 (if (equivalent-node? key2) (equivalent-node-list key2)
			(list (cons key2 val2))))
	      (result nil))
	  (declare (type list alist2))
	  (dolist (pr1 alist1)
	    (let ((pr2 (cl:find (car pr1) alist2 :test (equal?-fn key-cmp-fn) :key #'car)))
	      (when (or (null pr2) (not (equal?-cmp (cdr pr1) (cdr pr2) val-cmp-fn)))
		(push pr1 result))))
	  (and result
	       (if (cdr result)
		   (values t (make-equivalent-map result))
		 (values t (caar result) (cdar result))))))
    (if (equivalent-node? key2)
	(let ((alist2 (equivalent-node-list key2))
	      ((pr2 (cl:find key1 alist2 :test (equal?-fn key-cmp-fn) :key #'car))))
	  (declare (type list alist2))
	  (and (or (null pr2) (not (equal?-cmp val1 (cdr pr2) val-cmp-fn)))
	       (values t key1 val1)))
      (and (or (not (equal?-cmp key1 key2 key-cmp-fn)) (not (equal?-cmp val1 val2 val-cmp-fn)))
	   (values t key1 val1)))))

(defun equivalent-map-less (eqvm key key-cmp-fn)
  "Removes the pair associated with `key' from `eqvm', an `Equivalent-Map'.  If
the result is a single pair, it's returned as two values; otherwise one value
is returned, which is an `Equivalent-Map'.  If a pair is removed, its value is
returned as the third value."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (let ((alist (equivalent-node-list eqvm))
	((pr (assoc key alist :test (equal?-fn key-cmp-fn)))))
    (if pr
	(let ((result (cl:remove pr alist)))
	  (declare (type list result))
	  (if (= (length result) 1)
	      (values (caar result) (cdar result) (cdr pr))
	    (values (make-equivalent-map result) nil (cdr pr))))
      eqvm)))

(defun equivalent-map-restrict (key val set-elt key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (if (equivalent-node? key)
      (let ((alist1 (equivalent-node-list key))
	    (mems2 (if (equivalent-node? set-elt) (equivalent-node-list set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if-not #'(lambda (pr)
					    (member (car pr) mems2 :test (equal?-fn key-cmp-fn)))
					alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (make-equivalent-map result) nil)))))
    (if (equivalent-node? set-elt)
	(and (member key (equivalent-node-list set-elt) :test (equal?-fn key-cmp-fn))
	     (values t key val))
      (and (equal?-cmp key set-elt key-cmp-fn)
	   (values t key val)))))

(defun equivalent-map-restrict-not (key val set-elt key-cmp-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn))
  (if (equivalent-node? key)
      (let ((alist1 (equivalent-node-list key))
	    (mems2 (if (equivalent-node? set-elt) (equivalent-node-list set-elt)
		     (list set-elt))))
	(let ((result (cl:remove-if #'(lambda (pr)
					(member (car pr) mems2 :test (equal?-fn key-cmp-fn)))
				    alist1)))
	  (cond ((null result) nil)
		((null (cdr result))
		 (values t (caar result) (cdar result)))
		(t
		 (values t (make-equivalent-map result) nil)))))
    (if (equivalent-node? set-elt)
	(and (not (member key (equivalent-node-list set-elt) :test (equal?-fn key-cmp-fn)))
	     (values t key val))
      (and (not (equal?-cmp key set-elt key-cmp-fn))
	   (values t key val)))))

(defun equivalent-map-compare (key1 val1 key2 val2 key-cmp-fn val-cmp-fn)
  "Compares two pairs where the key of either or both may be an `Equivalent-Map'."
  (declare (optimize (speed 3) (safety 0))
	   (type function key-cmp-fn val-cmp-fn))
  (let ((comp (funcall key-cmp-fn key1 key2)))
    (if (or (eq comp ':less) (eq comp ':greater))
	comp
      (if (equivalent-node? key1)
	  (if (equivalent-node? key2)
	      (let ((mems1 (equivalent-node-list key1))
		    (mems2 (equivalent-node-list key2))
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
		       (let ((set1 (reduce (fn (s x) (wb-set-tree-with s (cdr x) val-cmp-fn))
					   mems1 :initial-value nil))
			     (set2 (reduce (fn (s x) (wb-set-tree-with s (cdr x) val-cmp-fn))
					   mems2 :initial-value nil))
			     ((comp (wb-set-tree-compare set1 set2 val-cmp-fn))))
			 (if (eq comp ':equal) ':unequal comp)))))
	    ':less)
	(if (equivalent-node? key2)
	    ':greater
	  (let ((val-comp (funcall val-cmp-fn val1 val2)))
	    (if (not (eq val-comp ':equal)) val-comp comp)))))))


;;; ================================================================================
;;; ================================================================================
;;; Sequences

(defconstant wb-seq-tree-ht-threshold 32
  "The size threshold above which we convert the root node to HT form.")
(defconstant wb-seq-tree-non-ht-threshold 28
  "The size threshold below which we convert an HT root to non-HT form.")

(declaim (inline make-raw-wb-seq-tree-node make-raw-wb-ht-seq-tree))

(deftype wb-seq-tree ()
  '(or null wb-seq-tree-node
       simple-string simple-vector))

(deftype symmetric-fixnum ()
  "Excludes `most-negative-fixnum', so negating it doesn't need to check for overflow."
  `(integer ,(- most-positive-fixnum) ,most-positive-fixnum))

;;; Seq tree nodes have no associated value.
(defstruct (wb-seq-tree-node
	     (:constructor make-raw-wb-seq-tree-node (raw-size left right))
	     (:predicate wb-seq-tree-node?)
	     (:print-function wb-seq-tree-node-print))
  ;; If negative, the seq contains only characters; size is the absolute value.
  (raw-size 0 :type symmetric-fixnum :read-only t)
  ;; These should never actually be null, since that would make the node itself redundant.
  (left  nil :type wb-seq-tree :read-only t)
  (right nil :type wb-seq-tree :read-only t))

(declaim (inline wb-seq-tree-node-size))
(defun wb-seq-tree-node-size (node)
  (abs (wb-seq-tree-node-raw-size node)))

;;; "HT" for "Head/Tail".
(defstruct (wb-ht-seq-tree
	     (:constructor make-raw-wb-ht-seq-tree (raw-size head body tail))
	     (:predicate wb-ht-seq-tree?)
	     (:print-function wb-ht-seq-tree-print))
  ;; If negative, the seq contains only characters; size is the absolute value.
  (raw-size 0 :type symmetric-fixnum :read-only t)
  (head nil :type (or null simple-vector simple-string))
  (body nil :type wb-seq-tree)
  (tail nil :type (or null simple-vector simple-string)))

;;; Speeds up `typep' slightly.
(declaim #+sbcl (sb-ext:freeze-type wb-ht-seq-tree))

(deflex +empty-simple-vector+ #())

(declaim (ftype (function (wb-seq-tree) (values fixnum boolean)) wb-seq-tree-size))
(declaim (inline wb-seq-tree-size))
(defun wb-seq-tree-size (tree)
  "As a second value, returns true iff `tree' contains only characters."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree))
  (cond ((null tree) (values 0 nil))
	((stringp tree) (values (length tree) t))
	((simple-vector-p tree) (values (length tree) nil))
	(t (let ((raw-size (wb-seq-tree-node-raw-size tree)))
	     (if (minusp raw-size)
		 (values (- raw-size) t)
	       (values raw-size nil))))))

(declaim (inline wb-ht-seq-tree-size))
(defun wb-ht-seq-tree-size (tree)
  "As a second value, returns true iff `tree' contains only characters."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree))
  (let ((raw-size (wb-ht-seq-tree-raw-size tree)))
    (if (minusp raw-size)
	(values (- raw-size) t)
      (values raw-size nil))))

(defun wb-ht?-seq-tree-size (tree)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-size tree)
    (wb-seq-tree-size tree)))

(defun make-wb-seq-tree-node (left right)
  "The low-level constructor for a sequence tree node."
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree left right))
  (let ((left-size left-chars? (wb-seq-tree-size left))
	(right-size right-chars? (wb-seq-tree-size right))
	((total-size (+ left-size right-size))))
    (make-raw-wb-seq-tree-node (if (and left-chars? right-chars?) (- total-size) total-size)
			       left right)))

(defun make-wb-ht-seq-tree (head body tail)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree head body tail))
  (flet ((split-for-head (tree)
	   (let ((leaf (wb-seq-tree-first-leaf tree)))
	     (values leaf (wb-seq-tree-subseq tree (wb-seq-tree-size leaf) (wb-seq-tree-size tree)))))
	 (split-for-tail (tree)
	   (let ((leaf (wb-seq-tree-last-leaf tree)))
	     (values leaf (wb-seq-tree-subseq tree 0 (- (wb-seq-tree-size tree) (wb-seq-tree-size leaf)))))))
    (when (null head)
      (setf (values head body) (split-for-head body)))
    (when (null tail)
      (setf (values tail body) (split-for-tail body)))
    (when (wb-seq-tree-node? head)
      (let ((new-head body-left (split-for-head head)))
	(setq head new-head)
	(setq body (wb-seq-tree-concat body-left body))))
    (when (wb-seq-tree-node? tail)
      (let ((new-tail body-right (split-for-tail tail)))
	(setq tail new-tail)
	(setq body (wb-seq-tree-concat body body-right)))))
  (let ((head-size left-chars? (wb-seq-tree-size head))
	(body-size body-chars? (wb-seq-tree-size body))
	(tail-size tail-chars? (wb-seq-tree-size tail))
	((total-size (the fixnum (+ head-size body-size tail-size)))))
    (make-raw-wb-ht-seq-tree (if (and left-chars? body-chars? tail-chars?) (- total-size) total-size)
			     head body tail)))

(defun wb-seq-tree-node-print (node stream depth)
  "Print function for `WB-Seq-Tree-Node', q.v."
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "~<#seq-node<~;~D, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}, ~
		      ~_~{~:[~S~;~<#(~;~@{~S~^ ~:_~:}~;)~:>~]~}~;>~:>"
	      (list (wb-seq-tree-node-size node)
		    (let ((sub (wb-seq-tree-node-left node)))
		      (if (and (simple-vector-p sub) (not (stringp sub)))
			  (list t (coerce sub 'list))
			(list nil sub)))
		    (let ((sub (wb-seq-tree-node-right node)))
		      (if (and (simple-vector-p sub) (not (stringp sub)))
			  (list t (coerce sub 'list))
			(list nil sub)))))
    (format stream "#seq-node<...>")))

(defun wb-ht-seq-tree-print (node stream depth)
  (if (or (null *print-level*) (<= depth *print-level*))
      (format stream "#seq-ht-tree<~D, ~S, ~S, ~S>"
	      (wb-ht-seq-tree-size node)
	      (wb-ht-seq-tree-head node)
	      (wb-ht-seq-tree-body node)
	      (wb-ht-seq-tree-tail node))
    (format stream "#seq-ht-node<...>")))

(declaim (inline character-type))
(defun character-type (ch)
  (declare (ignorable ch))
  #-fset-ext-strings 'base-char
  #+fset-ext-strings (if (base-char-p ch) 'base-char 'character))

(declaim (inline object-or-char-type))
(defun object-or-char-type (x)
  (cond ((base-char-p x) 'base-char)
	#+fset-ext-strings
	((characterp x) 'character)
	(t 't)))

(declaim (inline string-plus-char-type))
(defun string-plus-char-type (str ch)
  "The element-type of a new string with `ch' being added to the contents of `str'."
  (declare (ignorable str ch))
  #-fset-ext-strings 'base-char
  #+fset-ext-strings (if (and (or (null str) (typep str 'base-string))
			      (base-char-p ch))
			 'base-char
		       'character))

(declaim (inline string-plus-string-type))
(defun string-plus-string-type (str1 str2)
  (declare (ignorable str1 str2))
  #-fset-ext-strings 'base-string
  #+fset-ext-strings (if (and (typep str1 'base-string) (typep str2 'base-string))
			 'base-string
		       'string))

;;; Workaround for https://gitlab.com/embeddable-common-lisp/ecl/-/issues/812
;;; Do not inline!
#+ecl
(defun base-char-p-ecl (x) (typep x 'base-char))

(declaim (inline string-subrange-element-type))
(defun string-subrange-element-type (str start end &optional (prev-type 'base-char))
  (declare (optimize (speed 3) (safety 0))
	   (simple-string str)
	   (fixnum start end)
	   (ignorable str start end prev-type))
  #-fset-ext-strings 'base-char
  #+fset-ext-strings
  (if (eq prev-type 'character) prev-type
    (if (typep str 'simple-base-string)
	'base-char
      (dotimes (i (- end start) 'base-char)
	(unless #-ecl (base-char-p (schar str (+ i start)))
		#+ecl (base-char-p-ecl (schar str (+ i start)))
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
		#+fset-ext-strings
		((and (eq result 'base-char) (not (base-char-p e)))
		 (setq result 'character)))))
      result)))


(defun wb-seq-tree-subscript (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
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
	 (let ((left (wb-seq-tree-node-left tree))
	       ((left-size (wb-seq-tree-size left))))
	   (if (< idx left-size)
	       (wb-seq-tree-subscript left idx)
	     (wb-seq-tree-subscript (wb-seq-tree-node-right tree)
				    (- idx left-size)))))))

(defun wb-ht-seq-tree-subscript (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree)
	   (type fixnum idx))
  (let ((whole-size (wb-ht-seq-tree-size tree)))
    (and (>= idx 0) (< idx whole-size)
	 (let ((head (wb-ht-seq-tree-head tree))
	       ((head-size (length-nv head))))
	   (if (< idx head-size)
	       (values t (if (simple-vector-p head) (svref head idx) (schar head idx)))
	     (let ((tail (wb-ht-seq-tree-tail tree))
		   ((tail-start (- whole-size (length-nv tail)))))
	       (if (>= idx tail-start)
		   (values t (if (simple-vector-p tail) (svref tail (- idx tail-start))
			       (schar tail (- idx tail-start))))
		 (wb-seq-tree-subscript (wb-ht-seq-tree-body tree) (- idx head-size)))))))))

(declaim (inline wb-ht?-seq-tree-subscript))
(defun wb-ht?-seq-tree-subscript (tree idx)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-subscript tree idx)
    (wb-seq-tree-subscript tree idx)))


;;; We assume bounds checking has already been done.
(defun wb-seq-tree-insert (tree idx value &optional not-at-root?)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
	   (type fixnum idx))
  (cond ((null tree)
	 (if (characterp value)
	     (make-string 1 :element-type (character-type value) :initial-element value)
	   (vector value)))
	((stringp tree)
	 (let ((len (length tree)))
	   (if (characterp value)
	       (if (< len wb-tree-max-string-length)
		   (string-insert tree idx value)
		 (let ((midpt (floor len 2)))
		   (if (<= idx midpt)
		       (make-wb-seq-tree-node (string-subseq-insert tree 0 midpt idx value)
					      (string-subseq tree midpt))
		     (make-wb-seq-tree-node (string-subseq tree 0 midpt)
					    (string-subseq-insert tree midpt len (- idx midpt) value)))))
	     (if (< len wb-tree-max-vector-length)
		 (vector-insert-from-string tree idx value)
	       (let ((midpt (floor len 2)))
		 ;; The recursive calls are necessary in the case where the original string is
		 ;; of length 15 or 16 and so the substring we're inserting into is of length 8
		 ;; (the max vector length).
		 (if (<= idx midpt)
		     (make-wb-seq-tree-node (wb-seq-tree-insert (string-subseq tree 0 midpt) idx value)
					    (string-subseq tree midpt))
		   (make-wb-seq-tree-node (string-subseq tree 0 midpt)
					  (wb-seq-tree-insert (string-subseq tree midpt)
							      (- idx midpt) value))))))))
	((simple-vector-p tree)
	 (let ((len (length tree)))
	   (if (< len wb-tree-max-vector-length)
	       (vector-insert tree idx value)
	     (if (< (* idx 2) len)
		 (make-wb-seq-tree-node (vector-subseq-maybe-string-insert tree 0 idx idx value)
					(vector-subseq-maybe-string tree idx len))
	       (make-wb-seq-tree-node (vector-subseq-maybe-string tree 0 idx)
				      (vector-subseq-maybe-string-insert tree idx len 0 value))))))
	(t
	 (let ((left (wb-seq-tree-node-left tree))
	       (right (wb-seq-tree-node-right tree))
	       ((left-size (wb-seq-tree-size left))
		((new-tree
		   (if (< idx left-size)
		       (wb-seq-tree-build-node (wb-seq-tree-insert left idx value t) right)
		     (wb-seq-tree-build-node left (wb-seq-tree-insert right (- idx left-size) value t)))))))
	   (if not-at-root?
	       new-tree
	     (wb-seq-tree-canonicalize-up new-tree))))))

(defun wb-ht-seq-tree-insert (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree)
	   (type fixnum idx))
  (let ((head (wb-ht-seq-tree-head tree))
	((head-size (length-nv head)))
	(body (wb-ht-seq-tree-body tree))
	(tail (wb-ht-seq-tree-tail tree)))
    (if (<= idx head-size)
	(if (and (characterp value) (or (null head) (stringp head)) (< head-size wb-tree-max-string-length))
	    (make-wb-ht-seq-tree (string-insert head idx value) body tail)
	  (make-wb-ht-seq-tree (wb-seq-tree-insert head idx value t) body tail))
      (let ((size (wb-ht-seq-tree-size tree))
	    (tail-size (length-nv tail))
	    ((tail-start (- size tail-size))
	     ((idx1 (- idx tail-start)))))
	(declare (fixnum tail-size tail-start idx1))
	(if (>= idx1 0)
	    (if (and (characterp value) (or (null tail) (stringp tail)) (< tail-size wb-tree-max-string-length))
		(make-wb-ht-seq-tree head body (string-insert tail idx1 value))
	      (make-wb-ht-seq-tree head body (wb-seq-tree-insert tail idx1 value t)))
	  (make-wb-ht-seq-tree head (wb-seq-tree-insert body (- idx head-size) value t) tail))))))

(declaim (inline wb-ht?-seq-tree-insert))
(defun wb-ht?-seq-tree-insert (tree idx value)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-insert tree idx value)
    (wb-seq-tree-insert tree idx value)))

(defun wb-seq-tree-append (tree value)
  (wb-seq-tree-insert tree (wb-seq-tree-size tree) value))

(defun wb-ht-seq-tree-append (tree value)
  (wb-ht-seq-tree-insert tree (wb-ht-seq-tree-size tree) value))

(declaim (inline wb-ht?-seq-tree-append))
(defun wb-ht?-seq-tree-append (tree value)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-append tree value)
    (wb-seq-tree-append tree value)))

;;; We assume bounds checking has already been done.
(defun wb-seq-tree-with (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
	   (type fixnum idx))
  (cond ((null tree)
	 (error "This case shouldn't happen"))
	((stringp tree)
	 (if (characterp value)
	     (string-update tree idx value)
	   (let ((len (length tree)))
	     (if (<= len wb-tree-max-vector-length)
		 (vector-update-from-string tree idx value)
	       (let ((split-point (ash len -1)))
		 (if (< idx split-point)
		     (make-wb-seq-tree-node (wb-seq-tree-with (string-subseq tree 0 split-point) idx value)
					    (string-subseq tree split-point))
		   (make-wb-seq-tree-node (string-subseq tree 0 split-point)
					  (wb-seq-tree-with (string-subseq tree split-point)
							    (- idx split-point) value))))))))
	((simple-vector-p tree)
	 (vector-update-maybe-to-string tree idx value))
	(t
	 (let ((left (wb-seq-tree-node-left tree))
	       ((left-size (wb-seq-tree-size left)))
	       (right (wb-seq-tree-node-right tree)))
	   (if (< idx left-size)
	       (make-wb-seq-tree-node (wb-seq-tree-with left idx value) right)
	     (make-wb-seq-tree-node left (wb-seq-tree-with right (- idx left-size) value)))))))

(defun wb-ht-seq-tree-with (tree idx value)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree)
	   (type fixnum idx))
  (let ((head (wb-ht-seq-tree-head tree))
	((head-size (length-nv head))))
    (if (< idx head-size)
	(make-wb-ht-seq-tree (wb-seq-tree-with head idx value) (wb-ht-seq-tree-body tree)
			     (wb-ht-seq-tree-tail tree))
      (let ((whole-size (wb-ht-seq-tree-size tree))
	    (tail (wb-ht-seq-tree-tail tree))
	    ((tail-size (length-nv tail))
	     ((tail-start (- whole-size tail-size)))))
	(if (>= idx tail-start)
	    (make-wb-ht-seq-tree head (wb-ht-seq-tree-body tree)
				 (wb-seq-tree-with tail (the fixnum (- idx tail-start)) value))
	  (make-wb-ht-seq-tree head (wb-seq-tree-with (wb-ht-seq-tree-body tree) (- idx head-size) value)
			       tail))))))

(declaim (inline wb-ht?-seq-tree-with))
(defun wb-ht?-seq-tree-with (tree idx value)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-with tree idx value)
    (wb-seq-tree-with tree idx value)))


(defun vector-subseq-maybe-string (vec start end)
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
	 (split-cases-on-var (elt-type base-char #+fset-ext-strings character t)
	   (let ((result (make-array len :element-type elt-type)))
	     (dotimes (i len)
	       ;; Have to use `aref' here to cover all three cases, but SBCL, at least,
	       ;; still optimizes away the type tests.
	       (setf (aref result i) (svref vec (+ i start))))
	     result)))))

(defun string-insert (str idx ch)
  "Returns a new string like `str' but with `ch' inserted at `idx'.  Careful --
does no bounds checking on `str', which it assumes is simple.  As a convenience,
accepts `nil' for an empty `str'."
  (declare (optimize (speed 3) (safety 0))
	   (type (or null simple-string) str)
	   (type fixnum idx))
  (let ((len (length-nv str))
	(elt-type (string-plus-char-type str ch)))
    (declare (ignorable elt-type))
    (split-cases-on-var (elt-type base-char #+fset-ext-strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string (1+ len) :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str i)))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx))
	    (setf (schar new-str (+ idx i 1))
		  (schar str (+ idx i))))
	  new-str)))))

(defun vector-insert-from-string (str idx val)
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
(defun string-subseq (str start &optional (end (length str)))
  "Returns a subsequence of `str' between `start' and `end'.  Careful -- does
no bounds checking on `str', which it assumes is simple."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end))
  (let ((len (- end start))
	(elt-type (string-subrange-element-type str start end)))
    (declare (ignorable elt-type)
	     (fixnum len))
    (split-cases-on-var (elt-type base-char #+fset-ext-strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string len :element-type elt-type)))
	  (dotimes (i len)
	    (setf (schar new-str i) (schar str (+ i start))))
	  new-str)))))

(defun string-subseq-insert (str start end idx ch)
  "Takes the subsequence of `str' from `start' to `end', then at `idx' within
the result, inserts `ch', returning the new string."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum start end idx))
  (let ((len (- end start))
	(elt-type (string-subrange-element-type str start end (character-type ch))))
    (declare (ignorable elt-type)
	     (type fixnum len))
    (split-cases-on-var (elt-type base-char #+fset-ext-strings character)
      (split-cases ((typep str 'simple-base-string))
	(let ((new-str (make-string (1+ len) :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str (+ i start))))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx))
	    (setf (schar new-str (+ idx i 1))
		  (schar str (+ idx i start))))
	  new-str)))))

(defun vector-subseq-maybe-string-insert (vec start end idx val)
  "Returns a subsequence of `vec' between `start' and `end', or `nil' if
the result would be of zero length.  Careful -- does no bounds checking
on `vec', which it assumes is simple.  Returns a string or base-string if
possible."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum start end idx))
  (let ((len (- end start))
	(elt-type (if (not (characterp val)) 't
		    (vector-subrange-element-type vec start end (character-type val)))))
    (declare (fixnum len))
    (split-cases-on-var (elt-type base-char #+fset-ext-strings character t)
      (let ((result (make-array (1+ len) :element-type elt-type)))
	(dotimes (i idx)
	  (setf (aref result i) (svref vec (+ start i))))
	(setf (aref result idx) val)
	(dotimes (i (- len idx))
	  (setf (aref result (+ idx i 1)) (svref vec (+ start idx i))))
	result))))


;;; We assume bounds checking has already been done.
(defun wb-seq-tree-remove (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
	   (type fixnum idx))
  (cond ((null tree) nil)
	((stringp tree)
	 (string-remove-at tree idx))
	((simple-vector-p tree)
	 (vector-remove-to-maybe-string tree idx))
	(t
	 (let ((left (wb-seq-tree-node-left tree))
	       ((left-size (wb-seq-tree-size left)))
	       (right (wb-seq-tree-node-right tree)))
	   (if (< idx left-size)
	       (wb-seq-tree-build-node (wb-seq-tree-remove left idx) right)
	     (wb-seq-tree-build-node left
				     (wb-seq-tree-remove right (- idx left-size))))))))

(defun wb-ht-seq-tree-remove (tree idx)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree)
	   (type fixnum idx))
  (wb-seq-tree-canonicalize-down
    (let ((head (wb-ht-seq-tree-head tree))
	  ((head-size (length-nv head))))
      (if (< idx head-size)
	  (make-wb-ht-seq-tree (wb-seq-tree-remove head idx) (wb-ht-seq-tree-body tree)
			       (wb-ht-seq-tree-tail tree))
	(let ((whole-size (wb-ht-seq-tree-size tree))
	      (tail (wb-ht-seq-tree-tail tree))
	      ((tail-start (- whole-size (length-nv tail)))))
	  (if (>= idx tail-start)
	      (make-wb-ht-seq-tree head (wb-ht-seq-tree-body tree)
				   (wb-seq-tree-remove tail (the fixnum (- idx tail-start))))
	    (make-wb-ht-seq-tree head (wb-seq-tree-remove (wb-ht-seq-tree-body tree) (- idx head-size))
				 tail)))))))

(declaim (inline wb-ht?-seq-tree-remove))
(defun wb-ht?-seq-tree-remove (tree idx)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-remove tree idx)
    (wb-seq-tree-remove tree idx)))

(defun wb-seq-tree-canonicalize-down (tree)
  (declare (type wb-ht-seq-tree tree))
  (if (>= (wb-ht-seq-tree-size tree) wb-seq-tree-non-ht-threshold)
      tree
    (wb-seq-tree-remove-ht tree)))

(defun wb-seq-tree-remove-ht (tree)
  (declare (type wb-ht-seq-tree tree))
  (let ((head (wb-ht-seq-tree-head tree))
	(body (wb-ht-seq-tree-body tree))
	(tail (wb-ht-seq-tree-tail tree))
	((hb (if (null head) body
	       (wb-seq-tree-concat head body)))))
    (if (null tail) hb
      (wb-seq-tree-concat hb tail))))

;;; For some use cases, we don't need a balanced normal tree.
(defun wb-seq-tree-canonicalize-down-unbalanced (tree)
  (declare (type (or wb-seq-tree wb-ht-seq-tree) tree))
  (if (wb-ht-seq-tree? tree)
      (let ((head (wb-ht-seq-tree-head tree))
	    (body (wb-ht-seq-tree-body tree))
	    (tail (wb-ht-seq-tree-tail tree))
	    ((head+body (if head (make-wb-seq-tree-node head body)
			  body))))
	(if tail (make-wb-seq-tree-node head+body tail)
	  head+body))
    tree))

(defun wb-seq-tree-canonicalize-up (tree)
  "Canonicalizes after insertion by possibly converting to an HT node."
  (declare (type wb-seq-tree tree))
  (let ((size chars? (wb-seq-tree-size tree)))
    (if (< size (if chars? (* wb-seq-tree-ht-threshold 2) wb-seq-tree-ht-threshold))
	tree
      (make-wb-ht-seq-tree nil tree nil))))

(defun wb-seq-tree-first-leaf (tree)
  (if (or (stringp tree) (simple-vector-p tree))
      tree
    (wb-seq-tree-first-leaf (wb-seq-tree-node-left tree))))

(defun wb-seq-tree-last-leaf (tree)
  (if (or (stringp tree) (simple-vector-p tree))
      tree
    (wb-seq-tree-last-leaf (wb-seq-tree-node-right tree))))

(defun string-remove-at (str idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type (unsigned-byte 24) idx))
  (let ((len (length str))
	((elt-type (string-subrange-element-type str 0 idx (string-subrange-element-type str (1+ idx) len)))))
    (declare (ignorable elt-type))
    (and (> len 1)
	 (split-cases ((typep str 'simple-base-string))
	   (split-cases-on-var (elt-type base-char #+fset-ext-strings character)
	     (let ((new-str (make-string (1- len) :element-type elt-type)))
	       (declare (fixnum len))
	       (dotimes (i idx)
		 (setf (schar new-str i) (schar str i)))
	       (dotimes (i (- len idx 1))
		 (setf (schar new-str (+ idx i)) (schar str (+ idx i 1))))
	       new-str))))))

(defun vector-remove-to-maybe-string (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type (unsigned-byte 24) idx))
  (let ((len (length vec))
	((elt-type (vector-subrange-element-type vec 0 idx (vector-subrange-element-type vec (1+ idx) len)))))
    (declare (type fixnum len))
    (and (> len 1)
	 (split-cases-on-var (elt-type base-char #+fset-ext-strings character t)
	   (let ((new-vec (make-array (1- len) :element-type elt-type)))
	     (dotimes (i idx)
	       (setf (aref new-vec i) (svref vec i)))
	     (dotimes (i (- len idx 1))
	       (setf (aref new-vec (+ idx i)) (svref vec (+ idx i 1))))
	     new-vec)))))

(defun string-update (str idx ch)
  (declare (optimize (speed 3) (safety 0))
	   (type simple-string str)
	   (type fixnum idx))
  (let ((len (length str))
	((elt-type-0 (string-subrange-element-type str 0 idx (character-type ch)))
	 ((elt-type (string-subrange-element-type str (1+ idx) len elt-type-0)))))
    (declare (ignorable elt-type)
	     (fixnum len))
    (split-cases ((typep str 'simple-base-string))
      (split-cases-on-var (elt-type base-char #+fset-ext-strings character)
	(let ((new-str (make-string len :element-type elt-type)))
	  (dotimes (i idx)
	    (setf (schar new-str i) (schar str i)))
	  (setf (schar new-str idx) ch)
	  (dotimes (i (- len idx 1))
	    (let ((j (+ i idx 1)))
	      (setf (schar new-str j) (schar str j))))
	  new-str)))))

(defun vector-update-from-string (str idx value)
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

(defun vector-update-maybe-to-string (vec idx val)
  "Returns a new vector like `vec' but with `val' at `idx'.  If the elements
of the result are all characters, returns a string instead."
  (declare (optimize (speed 3) (safety 0))
	   (type simple-vector vec)
	   (type fixnum idx))
  (let ((len (length vec))
	((elt-type (if (not (typep val 'character))
		       't
		     (let ((vec-elt-type (if (base-char-p val) 'base-char 'character)))
		       (dotimes (i len)
			 (unless (= i idx)
			   (let ((e (svref vec i)))
			     (unless (characterp e)
			       (setq vec-elt-type 't)
			       (return))
			     (when (and (eq vec-elt-type 'base-char) (not (base-char-p e)))
			       (setq vec-elt-type 'character)))))
		       vec-elt-type)))))
    (declare (fixnum len))
    (split-cases-on-var (elt-type base-char #+fset-ext-strings character t)
      (let ((new-vec (make-array len :element-type elt-type)))
	(dotimes (i idx)
	  (setf (aref new-vec i) (svref vec i)))
	(setf (aref new-vec idx) val)
	(dotimes (i (- len idx 1))
	  (let ((j (+ i idx 1)))
	    (setf (aref new-vec j) (svref vec j))))
	new-vec))))


;;; We assume bounds checking has already been done.
(defun wb-seq-tree-subseq (tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
	   (type fixnum start end))
  (cond ((or (null tree) (>= start end)) nil)
	((and (= start 0) (= end (wb-seq-tree-size tree))) tree)
	((simple-vector-p tree)
	 (vector-subseq-maybe-string tree start end))
	((stringp tree)
	 (string-subseq tree start end))
	(t
	 (let ((left (wb-seq-tree-node-left tree))
	       (right (wb-seq-tree-node-right tree))
	       ((left-size (wb-seq-tree-size left))
		((new-left (wb-seq-tree-subseq left start (min end left-size)))
		 (new-right (wb-seq-tree-subseq right (max 0 (the fixnum (- start left-size)))
						(- end left-size))))))
	   (if (and (eq new-left left) (eq new-right right))
	       tree
	     (wb-seq-tree-concat new-left new-right))))))

(defun wb-ht-seq-tree-subseq (tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-ht-seq-tree tree)
	   (type fixnum start end))
  (let ((head (wb-ht-seq-tree-head tree))
	((head-size (length-nv head)))
	(body (wb-ht-seq-tree-body tree))
	(tail (wb-ht-seq-tree-tail tree))
	((body-size (wb-seq-tree-size body))
	 ((tail-start (+ head-size body-size))
	  ((new-head (and (< start head-size)
			  (if (simple-vector-p head)
			      (vector-subseq-maybe-string head start (min head-size end))
			    (string-subseq head start (min head-size end)))))
	   (new-body (and (> end head-size) (< start tail-start)
			  (wb-seq-tree-subseq body (the fixnum (max 0 (- start head-size)))
					      (the fixnum (min body-size (- end head-size))))))
	   (new-tail (and (> end tail-start)
			  (if (simple-vector-p tail)
			      (vector-subseq-maybe-string tail (max 0 (- start tail-start)) (- end tail-start))
			    (string-subseq tail (max 0 (- start tail-start)) (- end tail-start)))))))))
    (if (> (- end start) wb-seq-tree-ht-threshold)
	(make-wb-ht-seq-tree new-head new-body new-tail)
      (wb-seq-tree-concat (wb-seq-tree-concat new-head new-body) new-tail))))

(declaim (inline wb-ht?-seq-tree-subseq))
(defun wb-ht?-seq-tree-subseq (tree start end)
  (if (wb-ht-seq-tree? tree)
      (wb-ht-seq-tree-subseq tree start end)
    (wb-seq-tree-subseq tree start end)))


(defun wb-seq-tree-reverse (tree)
  (cond ((null tree) nil)
	((simple-vector-p tree)
	 (cl:reverse tree))
	((stringp tree)
	 (cl:reverse tree))
	(t
	 (make-wb-seq-tree-node (wb-seq-tree-reverse (wb-seq-tree-node-right tree))
				(wb-seq-tree-reverse (wb-seq-tree-node-left tree))))))

(defun wb-ht-seq-tree-reverse (tree)
  (declare (type wb-ht-seq-tree tree))
  (make-wb-ht-seq-tree (cl:reverse (wb-ht-seq-tree-tail tree))
		       (wb-seq-tree-reverse (wb-ht-seq-tree-body tree))
		       (cl:reverse (wb-ht-seq-tree-head tree))))


;;; ================================================================================
;;; Conversion to/from vectors

;;; An alternative would be for this not to do anything but copy the vector.  Breaking it into
;;; pieces could then be done lazily on update, and skipped altogether if there aren't any updates.
;;; OTOH, this takes only about twice as long as a vector copy.
(defun wb-seq-tree-from-vector (vec)
  (declare (optimize (speed 3) (safety 0))
	   (type vector vec))
  (and (> (length vec) 0)
       ;; We walk the vector left-to-right, breaking it up into nearly-equal-sized
       ;; subsequences, and combining those into a tree.
       (let ((len (length vec))
	     ((ht? (>= len wb-seq-tree-ht-threshold))
	      (npieces (ceiling len (if (stringp vec)
					wb-tree-max-string-length
				      wb-tree-max-vector-length)))
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
		    (push (make-wb-seq-tree-node left right) stack)))
		(if ht? (make-wb-ht-seq-tree (cadr stack) (car stack) tail)
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
			       #+fset-ext-strings
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
				(vector-subseq-maybe-string vec base (the fixnum (+ base piece-len))))
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
		   (push (make-wb-seq-tree-node left right) stack)))))))))

(defun wb-seq-tree-to-vector (tree)
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
			(fillr (wb-seq-tree-node-left tree) result)
			(fillr (wb-seq-tree-node-right tree) result)))))
	(if (wb-ht-seq-tree? tree)
	    (let ((head (wb-ht-seq-tree-head tree))
		  (body (wb-ht-seq-tree-body tree))
		  (tail (wb-ht-seq-tree-tail tree))
		  (result (make-array (wb-ht-seq-tree-size tree))))
	      (fillr head result)
	      (fillr body result)
	      (fillr tail result)
	      result)
	  (let ((result (make-array (wb-seq-tree-size tree))))
	    (fillr tree result)
	    result))))))

(defun wb-seq-tree-to-string (tree &optional (element-type 'character))
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
				(unless (base-char-p c)
				  (error 'type-error :datum c :expected-type 'base-char))
				(setf (schar result idx) c))
			      (incf idx)))))
		       (t
			(fillr (wb-seq-tree-node-left tree) result)
			(fillr (wb-seq-tree-node-right tree) result)))))
	(if (wb-ht-seq-tree? tree)
	    (let ((head (wb-ht-seq-tree-head tree))
		  (body (wb-ht-seq-tree-body tree))
		  (tail (wb-ht-seq-tree-tail tree))
		  (result (make-string (wb-ht-seq-tree-size tree) :element-type element-type)))
	      (fillr head result)
	      (fillr body result)
	      (fillr tail result)
	      result)
	  (let ((result (make-string (wb-seq-tree-size tree) :element-type element-type)))
	    (fillr tree result)
	    result))))))


;;; ================================================================================
;;; Conversion to/from lists

(defun wb-seq-tree-from-list (lst)
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (and lst
       (let ((len (length lst))
	     ((ht? (>= len wb-seq-tree-ht-threshold))
	      (npieces (ceiling len wb-tree-max-vector-length))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((tail (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((right (pop stack))
			(left (pop stack)))
		    (push (make-wb-seq-tree-node left right) stack)))
		(if ht? (make-wb-ht-seq-tree (cadr stack) (car stack) tail)
		  (car stack))))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len))
		 ((piece (cond ((gmap (:result and) (fn (x _y) (base-char-p x))
				      (:arg list lst)
				      (:arg index 0 piece-len))
				(let ((str (make-string piece-len :element-type 'base-char)))
				  (dotimes (i piece-len)
				    (setf (schar str i) (pop lst)))
				  str))
			       #+fset-ext-strings
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
		   (push (make-wb-seq-tree-node left right) stack)))))))))

(defun wb-seq-tree-from-list-reverse (lst)
  "Always returns a `WB-Seq-Tree', not an HT tree."
  (declare (optimize (speed 3) (safety 0))
	   (type list lst))
  (and lst
       (let ((len (length lst))
	     ((ht? (>= len wb-seq-tree-ht-threshold))
	      (npieces (ceiling len wb-tree-max-vector-length))
	      ((piece-len remainder (floor len npieces)))))
	 (declare (type fixnum npieces piece-len remainder))
	 (do ((ipiece 0 (1+ ipiece))
	      (stack nil))
	     ((= ipiece npieces)
	      (let ((head (and ht? (pop stack))))
		(do () ((null (if ht? (cddr stack) (cdr stack))))
		  (let ((left (pop stack))
			(right (pop stack)))
		    (push (make-wb-seq-tree-node left right) stack)))
		(if ht? (make-wb-ht-seq-tree head (car stack) (cadr stack))
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
			       #+fset-ext-strings
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
		   (push (make-wb-seq-tree-node left right) stack)))))))))

(defun wb-seq-tree-to-list (tree)
  "Accepts HT trees."
  (declare (optimize (speed 3) (safety 0))
	   #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (rlabels (if (wb-ht-seq-tree? tree)
	       (build (wb-ht-seq-tree-head tree)
		      (build (wb-ht-seq-tree-body tree)
			     (build (wb-ht-seq-tree-tail tree) nil)))
	     (build tree nil))
    (build (tree result)
      (cond ((null tree) result)
	    ;; SBCL wants the type breakdown to optimize the `coerce'.
	    ((simple-vector-p tree)
	     (nconc (coerce tree 'list) result))
	    #+fset-ext-strings
	    ((typep tree 'base-string)
	     (nconc (coerce tree 'list) result))
	    ((typep tree 'string)
	     (nconc (coerce tree 'list) result))
	    (t
	     ;; We build the list from the right so we don't need an `nreverse'.
	     (build (wb-seq-tree-node-left tree)
		    (build (wb-seq-tree-node-right tree) result)))))))

(defun wb-seq-tree-fill (len value)
  (wb-seq-tree-from-iterable (fn (op) (ecase op (:get value))) len))

(defun wb-seq-tree-from-iterable (it len)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum len)
	   (type function it))
  (and (> len 0)
       (let ((ht? (>= len wb-seq-tree-ht-threshold))
	     (npieces (ceiling len wb-tree-max-vector-length))
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
		    (push (make-wb-seq-tree-node left right) stack)))
		(if ht? (make-wb-ht-seq-tree (cadr stack) (car stack) tail)
		  (car stack))))
	   (declare (type fixnum ipiece))
	   (let ((piece-len (if (< ipiece remainder) (1+ piece-len) piece-len)))
	     (dotimes (i piece-len)
	       (setf (svref tmp-piece i) (funcall it ':get)))
	     (let (((piece (cond ((gmap (:result and) (fn (x _y) (base-char-p x))
					(:arg simple-vector tmp-piece)
					(:arg index 0 piece-len))
				  (let ((str (make-string piece-len :element-type 'base-char)))
				    (dotimes (i piece-len)
				      (setf (schar str i) (svref tmp-piece i)))
				    str))
				 #+fset-ext-strings
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
		     (push (make-wb-seq-tree-node left right) stack))))))))))


;;; ================================================================================
;;; Compare

(defun wb-seq-tree-compare (tree1 tree2 cmp-fn &key lexicographic?)
  "Accepts HT trees."
  (declare (optimize (speed 3) (safety 0))
	   (type (or wb-seq-tree wb-ht-seq-tree) tree1 tree2)
	   (type function cmp-fn))
  (if (eq tree1 tree2) ':equal
    (let ((size1 (if (wb-ht-seq-tree? tree1) (wb-ht-seq-tree-size tree1) (wb-seq-tree-size tree1)))
	  (size2 (if (wb-ht-seq-tree? tree2) (wb-ht-seq-tree-size tree2) (wb-seq-tree-size tree2))))
      (cond ((and (not lexicographic?) (< size1 size2)) ':less)
	    ((and (not lexicographic?) (> size1 size2)) ':greater)
	    ((wb-ht-seq-tree? tree1)
	     (let ((head1 (wb-ht-seq-tree-head tree1))
		   (head2 body2 tail2
		     (if (wb-ht-seq-tree? tree2)
			 (values (wb-ht-seq-tree-head tree2) (wb-ht-seq-tree-body tree2)
				 (wb-ht-seq-tree-tail tree2))
		       (values nil tree2 nil)))
		   ((head1-size (length-nv head1))
		    (head2-size (length-nv head2))
		    ((head-comp (wb-seq-tree-compare-rng head1 0 head2 0 0 (min head1-size head2-size) cmp-fn)))))
	       ;; Yikes!  Five comparisons: head-head, head-body, body-body, body-tail, tail-tail.
	       ;; (We don't have to handle head-tail because HT nodes aren't used below a certain minimum size.)
	       (if (or (eq head-comp ':less) (eq head-comp ':greater))
		   head-comp
		 (let ((body1 (wb-ht-seq-tree-body tree1))
		       ((head-body-comp
			  (if (< head1-size head2-size)
			      (wb-seq-tree-compare-rng body1 head1-size head2 0 head1-size head2-size cmp-fn)
			    (wb-seq-tree-compare-rng head1 0 body2 head2-size head2-size head1-size cmp-fn)))))
		   (if (or (eq head-body-comp ':less) (eq head-body-comp ':greater))
		       head-body-comp
		     (let ((body1-end (the fixnum (+ head1-size (wb-seq-tree-size body1))))
			   (body2-end (the fixnum (+ head2-size (wb-seq-tree-size body2))))
			   ((body-comp
			      (wb-seq-tree-compare-rng body1 head1-size body2 head2-size
						       (max head1-size head2-size) (min body1-end body2-end) cmp-fn))))
		       (if (or (eq body-comp ':less) (eq body-comp ':greater))
			   body-comp
			 (let ((tail1 (wb-ht-seq-tree-tail tree1))
			       ((body-tail-comp
				  (if (< body1-end body2-end)
				      (wb-seq-tree-compare-rng tail1 body1-end body2 head2-size
							       body1-end body2-end cmp-fn)
				    (wb-seq-tree-compare-rng body1 head1-size tail2 body2-end
							     body2-end body1-end cmp-fn)))))
			   (if (or (eq body-tail-comp ':less) (eq body-tail-comp ':greater))
			       body-tail-comp
			     (let ((tail-comp (wb-seq-tree-compare-rng tail1 body1-end tail2 body2-end
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
	    ((wb-ht-seq-tree? tree2)
	     (invert-comparison (wb-seq-tree-compare tree2 tree1 cmp-fn :lexicographic? lexicographic?)))
	    (lexicographic?
	     (let ((comp (wb-seq-tree-compare-rng tree1 0 tree2 0 0 (min size1 size2) cmp-fn)))
	       (cond ((or (eq comp ':less) (eq comp ':greater))
		      comp)
		     ((< size1 size2) ':less)
		     ((> size1 size2) ':greater)
		     (t comp))))
	    (t (wb-seq-tree-compare-rng tree1 0 tree2 0 0 size1 cmp-fn))))))

(defun wb-seq-tree-compare-lexicographically (tree1 tree2 cmp-fn)
  (wb-seq-tree-compare tree1 tree2 cmp-fn :lexicographic? t))

(defun wb-seq-tree-compare-rng (tree1 base1 tree2 base2 lo hi cmp-fn)
  ;; See notes at `WB-Set-Tree-Compare-Rng'.
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree1 tree2)
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
			     #+fset-ext-strings (typep tree1 '(simple-array character (*))))
	       (e-split-cases ((simple-vector-p tree2) (typep tree2 'simple-base-string)
			       #+fset-ext-strings (typep tree2 '(simple-array character (*))))
		 (dotimes (i (the fixnum (- hi lo)) result)
		   (let ((comp (funcall cmp-fn (aref tree1 (+ i start1)) (aref tree2 (+ i start2)))))
		     (cond ((or (eq comp ':less) (eq comp ':greater))
			    (return comp))
			   ((eq comp ':unequal)
			    (setq result comp))))))))))
	((or (stringp tree1) (simple-vector-p tree1))
	 (invert-comparison (wb-seq-tree-compare-rng tree2 base2 tree1 base1 lo hi cmp-fn)))
	(t
	 (let ((left1 (wb-seq-tree-node-left tree1))
	       ((left1-size (the fixnum (wb-seq-tree-size left1)))
		((node-rank (the fixnum (+ base1 left1-size)))
		 ((left1a base1a (wb-seq-tree-trim left1 base1 lo node-rank))
		  (tree2a base2a (wb-seq-tree-trim tree2 base2 lo node-rank))
		  ((left-comp (if (not (less-than?-cmp lo node-rank cmp-fn))
				  ':equal
				(wb-seq-tree-compare-rng left1a base1a tree2a base2a
							 lo (min hi node-rank) cmp-fn))))))))
	   (if (or (eq left-comp ':less) (eq left-comp ':greater))
	       left-comp
	     (let ((right1a base1a (wb-seq-tree-trim (wb-seq-tree-node-right tree1)
						     node-rank node-rank hi))
		   (tree2a base2a (wb-seq-tree-trim tree2 base2 node-rank hi))
		   ((right-comp (if (not (less-than?-cmp node-rank hi cmp-fn))
				    ':equal
				  (wb-seq-tree-compare-rng right1a base1a tree2a base2a
							   (max lo node-rank) hi cmp-fn)))))
	       (if (not (eq right-comp ':equal))
		   right-comp
		 left-comp)))))))


;;; ================================================================================
;;; Support routines for the above (sequences)

(defun wb-seq-tree-concat (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree left right))
  (cond ((null left) right)
	((null right) left)
	((and (wb-seq-tree-node? left)
	      (> (1+ (wb-seq-tree-size left))
		 (wb-tree-balance-delta-fn (1+ (wb-seq-tree-size right)))))
	 (wb-seq-tree-build-node (wb-seq-tree-node-left left)
				 (wb-seq-tree-concat (wb-seq-tree-node-right left)
						     right)))
	((and (wb-seq-tree-node? right)
	      (> (1+ (wb-seq-tree-size right))
		 (wb-tree-balance-delta-fn (1+ (wb-seq-tree-size left)))))
	 (wb-seq-tree-build-node (wb-seq-tree-concat left (wb-seq-tree-node-left right))
				 (wb-seq-tree-node-right right)))
	(t
	 (wb-seq-tree-build-node left right))))

(defun wb-ht?-seq-tree-concat (left right)
  "The arguments can be HT trees, but do not have to be.  The result also
may or may not be an HT tree."
  (declare (optimize (speed 3) (safety 0)))
  (if (wb-ht-seq-tree? left)
      (if (wb-ht-seq-tree? right)
	  (make-wb-ht-seq-tree (wb-ht-seq-tree-head left)
			       (wb-seq-tree-concat (wb-seq-tree-concat (wb-ht-seq-tree-body left)
								       (wb-ht-seq-tree-tail left))
						   (wb-seq-tree-concat (wb-ht-seq-tree-head right)
								       (wb-ht-seq-tree-body right)))
			       (wb-ht-seq-tree-tail right))
	(make-wb-ht-seq-tree (wb-ht-seq-tree-head left)
			     (wb-seq-tree-concat (wb-seq-tree-concat (wb-ht-seq-tree-body left)
								     (wb-ht-seq-tree-tail left))
						 right)
			     nil))
    (if (wb-ht-seq-tree? right)
	(make-wb-ht-seq-tree nil (wb-seq-tree-concat left (wb-seq-tree-concat (wb-ht-seq-tree-head right)
									      (wb-ht-seq-tree-body right)))
			     (wb-ht-seq-tree-tail right))
      (wb-seq-tree-canonicalize-up (wb-seq-tree-concat left right)))))


(defun wb-seq-tree-build-node (left right)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree left right))
  (cond ((null left) right)
	((null right) left)
	((and (stringp left) (stringp right)
	      (<= (+ (length-nv left) (length-nv right)) wb-tree-max-string-length))
	 (concatenate (string-plus-string-type left right) left right))
	((and (or (stringp left) (simple-vector-p left))
	      (or (stringp right) (simple-vector-p right)))
	 (if (<= (+ (length-nv left) (length-nv right)) wb-tree-max-vector-length)
	     (concatenate 'simple-vector left right)
	   (make-wb-seq-tree-node left right)))
	(t
	 (let ((wgtl (1+ (wb-seq-tree-size left)))
	       (wgtr (1+ (wb-seq-tree-size right))))
	   (cond ((and (wb-seq-tree-node? left) (> wgtl (wb-tree-balance-delta-fn wgtr)))
		  (let ((ll (wb-seq-tree-node-left left))
			(rl (wb-seq-tree-node-right left)))
		    (if (or (null rl) (simple-string-p rl) (simple-vector-p rl)
			    (< (1+ (wb-seq-tree-size rl))
			       (wb-tree-balance-gamma-fn (1+ (wb-seq-tree-size ll)))))
			(make-wb-seq-tree-node ll (wb-seq-tree-build-node rl right))
		      (make-wb-seq-tree-node (wb-seq-tree-build-node
					       ll (wb-seq-tree-node-left rl))
					     (wb-seq-tree-build-node
					       (wb-seq-tree-node-right rl) right)))))
		 ((and (wb-seq-tree-node? right) (> wgtr (wb-tree-balance-delta-fn wgtl)))
		  (let ((lr (wb-seq-tree-node-left right))
			(rr (wb-seq-tree-node-right right)))
		    (if (or (null lr) (simple-string-p lr) (simple-vector-p lr)
			    (< (1+ (wb-seq-tree-size lr))
			       (wb-tree-balance-gamma-fn (1+ (wb-seq-tree-size rr)))))
			(make-wb-seq-tree-node (wb-seq-tree-build-node left lr)
					       rr)
		      (make-wb-seq-tree-node (wb-seq-tree-build-node
					       left (wb-seq-tree-node-left lr))
					     (wb-seq-tree-build-node
					       (wb-seq-tree-node-right lr) rr)))))
		 (t
		  (make-wb-seq-tree-node left right)))))))

(defun wb-seq-tree-trim (tree base lo hi)
  (declare (optimize (speed 3) (safety 0))
	   (type wb-seq-tree tree)
	   (type fixnum base lo hi))
  (assert (not (wb-ht-seq-tree? tree)))
  (if (or (null tree) (stringp tree) (simple-vector-p tree))
      (values tree base)
    (let ((node-rank (the fixnum (+ base (wb-seq-tree-size (wb-seq-tree-node-left tree))))))
      (if (>= node-rank lo)
	  (if (< node-rank hi)
	      (values tree base)
	    (wb-seq-tree-trim (wb-seq-tree-node-left tree) base lo hi))
	(wb-seq-tree-trim (wb-seq-tree-node-right tree) node-rank lo hi)))))


;;; ================================================================================
;;; Iteration primitives

(defmacro do-wb-seq-tree-members ((var tree-form &optional value-form) &body body)
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
			 (,walk-fn (wb-seq-tree-node-left tree))
			 (,walk-fn (wb-seq-tree-node-right tree))))))
	 (let ((tree ,tree-form))
	   (if (wb-ht-seq-tree? tree)
	       (progn
		 (,walk-fn (wb-ht-seq-tree-head tree))
		 (,walk-fn (wb-ht-seq-tree-body tree))
		 (,walk-fn (wb-ht-seq-tree-tail tree)))
	     (,walk-fn tree))))
       ,value-form)))

(defmacro do-wb-seq-tree-members-gen ((var tree-form start-form end-form from-end-form
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
			    (let ((left (wb-seq-tree-node-left tree))
				  ((left-size (wb-seq-tree-size left)))
				  (right (wb-seq-tree-node-right tree)))
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
	     (if (wb-ht-seq-tree? tree)
		 (let ((,end-var (min ,end-var (wb-ht-seq-tree-size tree))))
		   (when (< ,start-var ,end-var)
		     (let ((head (wb-ht-seq-tree-head tree))
			   (body (wb-ht-seq-tree-body tree))
			   (tail (wb-ht-seq-tree-tail tree))
			   ((head-size (length-nv head))
			    ((body-end (+ head-size (wb-seq-tree-size body))))))
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
	       (,walk-fn tree ,start-var (min ,end-var (wb-seq-tree-size tree)))))))
       ,value-form)))

(defmacro do-wb-seq-tree-leaves ((var tree-form &optional value-form) &body body)
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
			 (,walk-fn (wb-seq-tree-node-left tree))
			 (,walk-fn (wb-seq-tree-node-right tree))))))
	 (let ((tree ,tree-form))
	   (if (wb-ht-seq-tree? tree)
	       (progn
		 (,body-fn (wb-ht-seq-tree-head tree))
		 (,walk-fn (wb-ht-seq-tree-body tree))
		 (,body-fn (wb-ht-seq-tree-tail tree)))
	     (,walk-fn tree))))
       ,value-form)))


(defun wb-seq-tree-image (tree fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function fn))
  (cond ((null tree) nil)
	((stringp tree)
	 (let ((len (length (the simple-string tree)))
	       ((result (make-array len))))
	   (split-cases ((typep tree 'base-string))
	     (dotimes (i len)
	       (setf (svref result i) (funcall fn (schar tree i)))))
	   (cond ((gmap :and #'characterp (:arg simple-vector result))
		  (vector-subseq-maybe-string result 0 len))
		 ((> len wb-tree-max-vector-length)
		  (wb-seq-tree-from-vector result))
		 (t result))))
	((simple-vector-p tree)
	 (let ((len (length tree))
	       ((result (make-array len))))
	   (dotimes (i len)
	     (setf (svref result i) (funcall fn (svref tree i))))
	   (if (gmap :and #'characterp (:arg simple-vector result))
	       (vector-subseq-maybe-string result 0 len)
	     result)))
	(t
	 (wb-seq-tree-build-node (wb-seq-tree-image (wb-seq-tree-node-left tree) fn)
				 (wb-seq-tree-image (wb-seq-tree-node-right tree) fn)))))

(defun wb-ht?-seq-tree-image (tree fn)
  (if (wb-ht-seq-tree? tree)
      (make-wb-ht-seq-tree (wb-seq-tree-image (wb-ht-seq-tree-head tree) fn)
			   (wb-seq-tree-image (wb-ht-seq-tree-body tree) fn)
			   (wb-seq-tree-image (wb-ht-seq-tree-tail tree) fn))
    (wb-seq-tree-image tree fn)))


(defun wb-ht?-seq-tree-position-if (tree pred)
  (declare (optimize (speed 3))
	   (type function pred))
  (let ((pos 0))
    (declare (fixnum pos))
    (do-wb-seq-tree-members (x tree)
      (when (funcall pred x)
	(return pos))
      (incf pos))))


;;; ----------------
;;; Stateful iterator

(defun make-wb-seq-tree-iterator (tree &optional start end)
  (let ((iter (make-wb-seq-tree-iterator-internal tree start end)))
    (lambda (op)
      (ecase op
	(:get (wb-seq-tree-iterator-get iter))
	(:done? (wb-seq-tree-iterator-done? iter))
	(:more? (not (wb-seq-tree-iterator-done? iter)))
	(:reset (wb-seq-tree-iterator-initialize iter tree start end))))))

(defun make-wb-seq-tree-iterator-internal (tree &optional start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree (wb-seq-tree-canonicalize-down-unbalanced tree))
	((tree-size (wb-seq-tree-size tree))
	 ((stack (make-wb-tree-iterator tree tree-size 2 nil))
	  ((iter (cons stack nil))))))
    (wb-seq-tree-iterator-initialize iter tree start end)))

(defun wb-seq-tree-iterator-initialize (iter tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (wb-seq-tree-size tree))
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
	    (let ((left (wb-seq-tree-node-left node))
		  ((left-size (wb-seq-tree-size left))))
	      (if (< tmp-start left-size)
		  (progn
		    (incf sp 2)
		    (setf (svref stack sp) node)
		    (setf (svref stack (1+ sp)) 0)
		    (setq node left))
		(progn
		  (decf tmp-start left-size)
		  (setq node (wb-seq-tree-node-right node)))))))))
    (setf (cdr iter) (- end start))
    iter))

(defun wb-seq-tree-iterator-canonicalize (iter)
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
		 (setf (svref stack sp) (wb-seq-tree-node-left node))
		 (setf (svref stack (1+ sp)) 0))
		(t
		 ;; Tail recursion
		 (setf (svref stack sp) (wb-seq-tree-node-right node))
		 (setf (svref stack (1+ sp)) 0))))))))

(defun wb-seq-tree-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (zerop (the fixnum (cdr iter))))

(defun wb-seq-tree-iterator-get (iter)
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
	(wb-seq-tree-iterator-canonicalize iter))
      (values (if (simple-string-p node) (schar node idx) (svref node idx)) t))))


(defun make-wb-seq-tree-rev-iterator (tree &optional start end)
  (let ((tree (wb-seq-tree-canonicalize-down-unbalanced tree))
	((iter (make-wb-seq-tree-rev-iterator-internal tree start end))))
    (lambda (op)
      (ecase op
	(:get (wb-seq-tree-rev-iterator-get iter))
	(:done? (wb-seq-tree-rev-iterator-done? iter))
	(:more? (not (wb-seq-tree-rev-iterator-done? iter)))
	(:reset (wb-seq-tree-rev-iterator-initialize iter tree start end))))))

(defun make-wb-seq-tree-rev-iterator-internal (tree &optional start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (wb-seq-tree-size tree))
	((stack (make-wb-tree-iterator tree tree-size 2 nil))
	 ((iter (cons stack nil)))))
    (wb-seq-tree-rev-iterator-initialize iter tree start end)))

(defun wb-seq-tree-rev-iterator-initialize (iter tree start end)
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) start end))
  (let ((tree-size (wb-seq-tree-size tree))
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
	  (let ((right (wb-seq-tree-node-right node))
		((right-size (wb-seq-tree-size right))))
	    (if (< tmp-end right-size)
		(progn
		  (incf sp 2)
		  (setf (svref stack sp) node)
		  (setf (svref stack (1+ sp)) 1)
		  (setq node right))
	      (progn
		(decf tmp-end right-size)
		(setq node (wb-seq-tree-node-left node))))))))
    (setf (cdr iter) (- start end))
    iter))

(defun wb-seq-tree-rev-iterator-canonicalize (iter)
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
		 (setf (svref stack sp) (wb-seq-tree-node-right node))
		 (setf (svref stack (1+ sp)) nil))
		(t
		 ;; Tail recursion
		 (setf (svref stack sp) (wb-seq-tree-node-left node))
		 (setf (svref stack (1+ sp)) nil))))))))

(defun wb-seq-tree-rev-iterator-done? (iter)
  (declare (optimize (speed 3) (safety 0)))
  (zerop (the fixnum (cdr iter))))

(defun wb-seq-tree-rev-iterator-get (iter)
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
	(wb-seq-tree-rev-iterator-canonicalize iter))
      (values (if (simple-string-p node) (schar node idx) (svref node idx)) t))))

;;; ----------------
;;; Functional iterators.  Fun!!!

(defun wb-seq-tree-fun-iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk (wb-seq-tree-canonicalize-down-unbalanced tree)
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
	     (walk (wb-seq-tree-node-left node)
		   (walk (wb-seq-tree-node-right node) cont)))))))

(defun wb-seq-tree-rev-fun-iter (tree)
  (declare (optimize (speed 3) (safety 0)))
  (rlabels (walk (wb-seq-tree-canonicalize-down-unbalanced tree)
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
	     (walk (wb-seq-tree-node-right node)
		   (walk (wb-seq-tree-node-left node) cont)))))))


;;; ================================================================================
;;; Verifier

(defun wb-seq-tree-verify (tree)
  (declare (optimize (debug 3)))
  (rlabels (cond ((null tree) t)
		 ((wb-ht-seq-tree? tree)
		  (and (walk-leaf (wb-ht-seq-tree-head tree))
		       (walk (wb-ht-seq-tree-body tree))
		       (walk-leaf (wb-ht-seq-tree-tail tree))))
		 (t (walk tree)))
    (walk (tree)
      (cond ((null tree) nil) ; nil should appear only if the tree is empty
	    ((vectorp tree) (walk-leaf tree))
	    (t
	     (let ((sizl lchars? (wb-seq-tree-size (wb-seq-tree-node-left tree)))
		   (sizr rchars? (wb-seq-tree-size (wb-seq-tree-node-right tree))))
	       (and (= (wb-seq-tree-node-size tree) (+ sizl sizr))
		    (eqv (and lchars? rchars?) (minusp (wb-seq-tree-node-raw-size tree)))
		    ;; We suppress the balance test if one side is smaller than 8
		    ;; here, instead of 4, because of `WB-Tree-Max-String-Length',
		    ;; which makes the trees appear less balanced.
		    (or (<= sizr 8)
			(<= sizl (* wb-tree-balance-factor-limit sizr)))
		    (or (<= sizl 8)
			(<= sizr (* wb-tree-balance-factor-limit sizl)))
		    (walk (wb-seq-tree-node-left tree))
		    (walk (wb-seq-tree-node-right tree)))))))
    (walk-leaf (leaf)
      (let ((correct-type (reduce (fn (str-type c)
				    (cond ((and (eq str-type 'base-string) (base-char-p c))
					   'base-string)
					  ((and (member str-type '(base-string string))
						(typep c 'character))
					   'string)
					  (t 'vector)))
				  leaf :initial-value 'base-string)))
	(cond ((stringp leaf)
	       (or (and (<= (length leaf) wb-tree-max-string-length)
			(or (eq correct-type 'string)
			    (and (eq correct-type 'base-string) (typep leaf 'base-string))))
		   (error "String leaf failed check")))
	      ((simple-vector-p leaf)
	       (or (and (<= (length leaf) wb-tree-max-vector-length)
			(eq correct-type 'vector))
		   (error "Vector leaf failed check"))))))))


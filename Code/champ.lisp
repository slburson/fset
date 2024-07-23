;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: champ.lisp
;;; Contents: CHAMP implementation for FSet.
;;; See: https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf
;;;
;;; This file is part of FSet.  Copyright (c) 2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(in-package :fset)

;;; ================================================================================
;;; Global constants

(defconstant champ-hash-bits-per-level 5)
(defconstant champ-node-radix (ash 1 champ-hash-bits-per-level))
(defconstant champ-hash-level-mask (1- champ-node-radix))

(defmacro do-bit-indices ((idx-var val &optional result) &body body)
  (let ((val-var (gensym "VAL-")))
    `(flet ((least-1-bit (n)
	      (1- (logcount (logxor n (1- n))))))
       (declare (inline least-1-bit))
       (let ((,val-var ,val))
	 (do ((,idx-var (least-1-bit ,val-var) (least-1-bit ,val-var)))
	     ((= 0 ,val-var) ,result)
	   (logandc2f ,val-var (ash 1 ,idx-var))
	  . ,body)))))

(defmacro postincf (v)
  `(prog1 ,v (incf ,v)))

;;; Verification utility, used to recover hash bits from masks.
(defun bit-indices (mask)
  "A list of the indices of the 1 bits of `mask', in ascending order."
  (let ((result nil))
    (do-bit-indices (i mask (nreverse result))
      (push i result))))


;;; Proclaiming a generic function to return a specific type doesn't stick -- at least on SBCL,
;;; recompiling a method removes the type.
(defun hash-value-fixnum (x)
  (let ((h (hash-value x)))
    (if (typep h 'fixnum)
	h
      ;; Anyway, using this wrapper lets us force the hash to be a fixnum.
      (let ((len (integer-length h))
	    (fixnum-len (integer-length most-positive-fixnum))
	    (fh 0))
	(dotimes (i (ceiling len fixnum-len))
	  (logxorf fh (ldb (byte fixnum-len (* fixnum-len i)) h)))
	fh))))
(declaim (ftype (function (t) fixnum) hash-value-fixnum))


;;; ================================================================================
;;; Sets

(defstruct (ch-set-node
	     (:type vector))
  ;; We could cram both masks into a single fixnum, giving them 30 bits each (on a native 64-bit architecture),
  ;; at the price of having to do integer div/mod instead of shifting and masking to split the hash value.
  ;; I wonder how much that would cost.
  entry-mask
  subnode-mask
  size ; total number of elements at or below this node
  hash-value)
(defconstant ch-set-node-header-size 4)
(declaim (ftype (function (t) fixnum)
		ch-set-node-entry-mask ch-set-node-subnode-mask ch-set-node-size ch-set-node-hash-value))

(defun ch-set-tree-with (tree value &optional (depth 0))
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((value-hash (hash-value-fixnum value)))
    (declare (fixnum value-hash))
    (rlabels (rec tree (ash value-hash (- (* depth champ-hash-bits-per-level))) depth)
      (rec (node hash-shifted depth)
	(declare (fixnum hash-shifted)
		 (type (integer 0 64) depth))
	(let ((hash-bits (logand hash-shifted champ-hash-level-mask)))
	  (if (null node)
	      (vector (ash 1 hash-bits) 0 1 value-hash value)
	    (if (consp node)
		;; We can create collision nodes at any level (see below).  -- Well, not at the root.  But
		;; this is called from elsewhere (e.g. `-union') on subtrees, which might be collision nodes.
		(let ((wb-hash-shifted (ash (hash-value-fixnum (wb-set-tree-arb (cdr node)))
					    (* (- champ-hash-bits-per-level) depth)))
		      (size (1+ (wb-set-tree-size (cdr node)))))
		  (declare (fixnum wb-hash-shifted hash-shifted size))
		  (if (= hash-shifted wb-hash-shifted)
		      ;; Update the collision node.
		      (let ((new-wb-tree (wb-set-tree-with (cdr node) value)))
			(if (eq new-wb-tree (cdr node))
			    node
			  ;; New entry in collision node
			  (cons (logxor (the fixnum (car node)) value-hash)
				new-wb-tree)))
		    ;; Oh, this is fun.  We add enough levels to get to where the hashes differ.
		    (rlabels (build hash-shifted wb-hash-shifted)
		      (build (hash-shifted wb-hash-shifted)
			(declare (fixnum hash-shifted wb-hash-shifted))
			(let ((hash-bits (logand hash-shifted champ-hash-level-mask))
			      (wb-hash-bits (logand wb-hash-shifted champ-hash-level-mask))
			      (content-hash (logxor (the fixnum (car node)) value-hash)))
			  (if (= hash-bits wb-hash-bits)
			      (vector 0 (ash 1 hash-bits) size content-hash
				      (build (ash hash-shifted (- champ-hash-bits-per-level))
					     (ash wb-hash-shifted (- champ-hash-bits-per-level))))
			    (vector (ash 1 hash-bits) (ash 1 wb-hash-bits) size content-hash
				    value node)))))))
	      (let ((entry-mask (ch-set-node-entry-mask node))
		    ((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		     ((entry-idx (+ ch-set-node-header-size entry-raw-idx))))
		    (subnode-mask (ch-set-node-subnode-mask node))
		    ((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		     ((subnode-idx (- (length node) 1 subnode-raw-idx)))))
		(if (logbitp hash-bits entry-mask)
		    ;; Entry found
		    (let ((ex-value (svref node entry-idx)))
		      (if (equal? value ex-value)
			  node
			;; Entry with different value found: make a subnode
			(let ((hash-shifted (ash hash-shifted (- champ-hash-bits-per-level)))
			      (ex-value-hash (hash-value-fixnum ex-value))
			      ((ex-value-hash-shifted (ash ex-value-hash (* (- champ-hash-bits-per-level) (1+ depth))))
			       ((n2 (if (= hash-shifted ex-value-hash-shifted)
					;; Collision!  Fall back to WB-tree.
					(cons (logxor value-hash ex-value-hash)
					      (wb-set-tree-with (wb-set-tree-with nil ex-value) value))
				      ;; Creates a little garbage; &&& hand-integrate later.
				      (rec (vector (ash 1 (logand ex-value-hash-shifted champ-hash-level-mask))
						   0 1 ex-value-hash ex-value)
					   hash-shifted (1+ depth))))
			      ;; The `1+' is because we're inserting _after_ `subnode-idx', because the subnodes
			      ;; are in reverse order.
				((n (vector-rem-1-ins-1 node entry-idx (1+ subnode-idx) n2))))))
			  (logandc2f (ch-set-node-entry-mask n) (ash 1 hash-bits))
			  (logiorf (ch-set-node-subnode-mask n) (ash 1 hash-bits))
			  (incf (ch-set-node-size n))
			  (logxorf (ch-set-node-hash-value n) value-hash)
			  n)))
		  ;; No entry found: check for subnode
		  (if (logbitp hash-bits subnode-mask)
		      ;; Subnode found
		      (let ((subnode (svref node subnode-idx))
			    ((new-subnode (rec subnode (ash hash-shifted (- champ-hash-bits-per-level)) (1+ depth)))))
			(if (eq new-subnode subnode)
			    node ; no change
			  ;; New subnode
			  (let ((n (vector-update node subnode-idx new-subnode)))
			    (setf (ch-set-node-size n)
				  (the fixnum (+ (ch-set-node-size n) (- (the fixnum (ch-set-tree-size new-subnode))
									 (the fixnum (ch-set-tree-size subnode))))))
			    (logxorf (ch-set-node-hash-value n) (the fixnum (ch-set-tree-hash-value subnode))
				     (the fixnum (ch-set-tree-hash-value new-subnode)))
			    n)))
		    ;; Neither entry nor subnode found: make new entry
		    (let ((n (vector-insert node entry-idx value)))
		      (logiorf (ch-set-node-entry-mask n) (ash 1 hash-bits))
		      (incf (ch-set-node-size n))
		      (logxorf (ch-set-node-hash-value n) value-hash)
		      n)))))))))))

(defun vector-rem-1-ins-1 (vec rem-idx ins-idx ins-val)
  (declare ;(optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum rem-idx ins-idx))
  (assert (<= rem-idx (the fixnum (1- ins-idx))))
  (let ((v (make-array (length vec))))
    (dotimes (i rem-idx)
      (setf (svref v i) (svref vec i)))
    (dotimes (i (- ins-idx rem-idx 1))
      (declare (fixnum i))
      (setf (svref v (+ rem-idx i)) (svref vec (+ rem-idx i 1))))
    (setf (svref v (1- ins-idx)) ins-val)
    (dotimes (i (- (length vec) ins-idx))
      (declare (fixnum i))
      (setf (svref v (+ ins-idx i)) (svref vec (+ ins-idx i))))
    v))

(defun ch-set-tree-size (tree)
  (cond ((null tree) 0)
	((consp tree)
	 (wb-set-tree-size (cdr tree)))
	(t (ch-set-node-size tree))))

(defun ch-set-tree-hash-value (tree)
  (cond ((null tree) 0)
	((consp tree) (car tree))
	(t (ch-set-node-hash-value tree))))

(defun ch-set-tree-less (tree value)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((value-hash (hash-value-fixnum value)))
    (declare (fixnum value-hash))
    (rlabels (rec tree value-hash)
      (rec (node hash-shifted)
	(declare (fixnum hash-shifted))
	(and node
	     (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
		   (entry-mask (ch-set-node-entry-mask node))
		   ((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		    ((entry-idx (+ ch-set-node-header-size entry-raw-idx))))
		   (subnode-mask (ch-set-node-subnode-mask node)))
	       (if (logbitp hash-bits entry-mask)
		   (let ((ex-value (svref node entry-idx)))
		     (cond ((not (equal? ex-value value))
			    (values node 0))
			   ((and (= (logcount entry-mask) 1) (= 0 subnode-mask))
			    (values nil value-hash))
			   (t
			    (let ((n (vector-remove-at node entry-idx)))
			      (logandc2f (ch-set-node-entry-mask n) (ash 1 hash-bits))
			      (decf (ch-set-node-size n))
			      (logxorf (ch-set-node-hash-value n) value-hash)
			      (values n value-hash)))))
		 (let ((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		       ((subnode-idx (- (length node) 1 subnode-raw-idx))))
		   (if (not (logbitp hash-bits subnode-mask))
		       (values node 0)
		     (let ((subnode (svref node subnode-idx)))
		       (if (consp subnode)
			   (let ((new-wb-tree (wb-set-tree-less (cdr subnode) value)))
			     (if (eq new-wb-tree (cdr subnode))
				 (values node 0)
			       (if (= 1 (wb-set-tree-size new-wb-tree))
				   ;; Removing next-to-last entry of collision node: turn remaining value into entry
				   (let ((rem-val (wb-set-tree-arb new-wb-tree))
					 ;; `entry-idx' is still valid from above; the hash bits must be the same
					 ((n (vector-ins-1-rem-1 node entry-idx rem-val subnode-idx))))
				     (logiorf (ch-set-node-entry-mask n) (ash 1 hash-bits))
				     (logandc2f (ch-set-node-subnode-mask n) (ash 1 hash-bits))
				     (decf (ch-set-node-size n))
				     (logxorf (ch-set-node-hash-value n) value-hash)
				     (values n value-hash))
				 (let ((n (vector-update node subnode-idx
							 (cons (logxor (the fixnum (car subnode)) value-hash)
							       new-wb-tree))))
				   (decf (ch-set-node-size n))
				   (logxorf (ch-set-node-hash-value n) value-hash)
				   (values n value-hash)))))
			 (let ((new-subnode content-hash-delta
				 (rec subnode (ash hash-shifted (- champ-hash-bits-per-level)))))
			   (declare (fixnum content-hash-delta))
			   (if (eq new-subnode subnode)
			       (values node 0)
			     (let ((n (cond ((null new-subnode)
					     (let ((n (vector-remove-at node subnode-idx)))
					       (logandc2f (ch-set-node-subnode-mask n) (ash 1 hash-bits))
					       n))
					    ((and (= 1 (logcount (ch-set-node-entry-mask new-subnode)))
						  (= 0 (ch-set-node-subnode-mask new-subnode)))
					     ;; New subnode contains only a single entry: pull it up into this node
					     (let ((new-entry-idx
						     (+ ch-set-node-header-size
							(logcount (logand (1- (ash 1 hash-bits)) entry-mask))))
						   ((n (vector-ins-1-rem-1 node new-entry-idx
									   (svref new-subnode ch-set-node-header-size)
									   subnode-idx))))
					       (logiorf (ch-set-node-entry-mask n) (ash 1 hash-bits))
					       (logandc2f (ch-set-node-subnode-mask n) (ash 1 hash-bits))
					       n))
					    ((and (= 0 (ch-set-node-entry-mask new-subnode))
						  (= 1 (logcount (ch-set-node-subnode-mask new-subnode)))
						  (consp (svref new-subnode ch-set-node-header-size)))
					     ;; New subnode contains only a collision node: pull it up into this node
					     (vector-update node subnode-idx
							    (svref new-subnode ch-set-node-header-size)))
					    (t (vector-update node subnode-idx new-subnode)))))
			       (decf (ch-set-node-size n))
			       (logxorf (ch-set-node-hash-value n) content-hash-delta)
			       (values n content-hash-delta)))))))))))))))

(defun vector-ins-1-rem-1 (vec ins-idx ins-val rem-idx)
  (declare ;(optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum ins-idx rem-idx))
  (assert (<= ins-idx rem-idx))
  (let ((v (make-array (length vec))))
    (dotimes (i ins-idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v ins-idx) ins-val)
    (dotimes (i (- rem-idx ins-idx))
      (setf (svref v (+ ins-idx i 1)) (svref vec (+ ins-idx i))))
    (dotimes (i (- (length vec) rem-idx 1))
      (declare (fixnum i))
      (setf (svref v (+ rem-idx i 1)) (svref vec (+ rem-idx i 1))))
    v))

(defun ch-set-tree-arb (tree)
  (cond ((null tree)
	 (error "'ch-set-tree-arb' called on empty tree"))
	((consp tree)
	 (wb-set-tree-arb (cdr tree)))
	((= 0 (ch-set-node-entry-mask tree))
	 ;; If there are no entries, there must be at least one subnode
	 (ch-set-tree-arb (svref tree (1- (length tree)))))
	(t
	 (svref tree ch-set-node-header-size))))

(defun ch-set-tree-contains? (tree value)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((value-hash (hash-value-fixnum value)))
    (declare (fixnum value-hash))
    (rlabels (rec tree value value-hash)
      (rec (node value hash-shifted)
	(declare (fixnum hash-shifted))
	(and node
	     (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
		   (entry-mask (ch-set-node-entry-mask node)))
	       (if (logbitp hash-bits entry-mask)
		   (let ((entry-idx (+ ch-set-node-header-size
				       (logcount (logand (1- (ash 1 hash-bits)) entry-mask))))
			 ((ex-value (svref node entry-idx))))
		     (equal? ex-value value))
		 (let ((subnode-mask (ch-set-node-subnode-mask node)))
		   (and (logbitp hash-bits subnode-mask)
			(let ((subnode-idx (- (length node) 1 (logcount (logand (1- (ash 1 hash-bits)) subnode-mask))))
			      ((subnode (svref node subnode-idx))))
			  (if (consp subnode)
			      (wb-set-tree-member? (cdr subnode) value)
			    (rec subnode value (ash hash-shifted (- champ-hash-bits-per-level))))))))))))))

(defun ch-set-tree-compare (tree1 tree2)
  (if (eq tree1 tree2) ':equal
    (let ((size1 (ch-set-tree-size tree1))
	  (size2 (ch-set-tree-size tree2)))
      ;; There are various ways we could do this, but traditionally, FSet sorts on size first.
      (cond ((< size1 size2) ':less)
	    ((> size1 size2) ':greater)
	    (t
	     (let ((hash1 (ch-set-tree-hash-value tree1))
		   (hash2 (ch-set-tree-hash-value tree2)))
	       (cond ((< hash1 hash2) ':less)
		     ((> hash1 hash2) ':greater)
		     ((consp tree1)
		      (if (consp tree2) (wb-set-tree-compare (cdr tree1) (cdr tree2))
			':less))
		     ((consp tree2) ':greater)
		     (t
		      (let ((entries1 (ch-set-node-entry-mask tree1))
			    (entries2 (ch-set-node-entry-mask tree2))
			    (subnodes1 (ch-set-node-subnode-mask tree1))
			    (subnodes2 (ch-set-node-subnode-mask tree2)))
			(cond ((< entries1 entries2) ':less)
			      ((> entries1 entries2) ':greater)
			      ((< subnodes1 subnodes2) ':less)
			      ((> subnodes1 subnodes2) ':greater)
			      (t
			       ;; It's astronomically unlikely that we'll get here unless the trees are actually
			       ;; equal... but we still have to check.
			       (let ((default ':equal)
				     (len (length tree1)))
				 (dotimes (i (logcount entries1))
				   (let ((v1 (svref tree1 (+ ch-set-node-header-size i)))
					 (v2 (svref tree2 (+ ch-set-node-header-size i)))
					 ((cmp (compare v1 v2))))
				     (ecase cmp
				       (:equal)
				       (:unequal
					 (setq default ':unequal))
				       ((:less :greater)
					 (return-from ch-set-tree-compare cmp)))))
				 (dotimes (i (logcount subnodes1))
				   (let ((cmp (ch-set-tree-compare (svref tree1 (- len i 1))
								   (svref tree2 (- len i 1)))))
				     (ecase cmp
				       (:equal)
				       (:unequal
					 (setq default ':unequal))
				       ((:less :greater)
					 (return-from ch-set-tree-compare cmp)))))
				 default))))))))))))

(defun ch-set-tree-index-element (tree index)
  (declare (optimize (debug 3)))
  (rlabels (rec tree index)
    (rec (tree index)
      (if (consp tree)
	  (wb-set-tree-rank-element (cdr tree) index)
	(let ((len (length tree))
	      (entry-mask (ch-set-node-entry-mask tree))
	      (subnode-mask (ch-set-node-subnode-mask tree))
	      ((merged-mask (logior entry-mask subnode-mask)))
	      (entry-idx 0)
	      (subnode-idx 0))
	  (do-bit-indices (idx merged-mask (error "'Bug in ch-set-tree-index-element'"))
	    (cond ((logbitp idx entry-mask)
		   (when (= index 0)
		     (return (svref tree (+ ch-set-node-header-size entry-idx))))
		   (decf index)
		   (incf entry-idx))
		  ((logbitp idx subnode-mask)
		   (let ((subnode (svref tree (- len subnode-idx 1)))
			 ((subnode-size (ch-set-tree-size subnode))))
		     (when (< index subnode-size)
		       (return (rec subnode index)))
		     (decf index subnode-size)
		     (incf subnode-idx))))))))))

(defun ch-set-tree-union (tree1 tree2)
  (declare (optimize (debug 3)))
  (rlabels (rec tree1 tree2 0)
    (rec (tree1 tree2 depth)
      (cond ((null tree1) tree2)
	    ((null tree2) tree1)
	    ((and (consp tree1) (consp tree2))
	     (let ((tree-result (wb-set-tree-union (cdr tree1) (cdr tree2)))
		   (chash 0))
	       (do-wb-set-tree-members (v tree-result)
		 (logxorf chash (hash-value-fixnum v)))
	       (cons chash tree-result)))
	    ((or (consp tree1) (consp tree2))
	     (let ((collision-node normal-node (if (consp tree1) (values tree1 tree2) (values tree2 tree1))))
	       ;; Garbagey, but nontrivial to improve on, AFAICS.
	       (do-wb-set-tree-members (x (cdr collision-node))
		 (setq normal-node (ch-set-tree-with normal-node x depth)))
	       normal-node))
	    (t
	     (let ((entries1 (ch-set-node-entry-mask tree1))
		   (entries2 (ch-set-node-entry-mask tree2))
		   (subnodes1 (ch-set-node-subnode-mask tree1))
		   (subnodes2 (ch-set-node-subnode-mask tree2))
		   ((all-entries (logior entries1 entries2))
		    (all-subnodes (logior subnodes1 subnodes2))
		    ((n-subnodes (logcount all-subnodes))
		     (everything (logior all-entries all-subnodes))
		     ;; Entry collisions can change to subnodes, but they can't then collide with existing subnodes.
		     ;; But we do have to subtract out entry/subnode collisions.
		     ((new-len (- (+ ch-set-node-header-size (logcount all-entries) n-subnodes)
				  (logcount (logand entries1 subnodes2)) (logcount (logand entries2 subnodes1))))
		      ((n (make-array new-len))))))
		   (ientry1 0)
		   (isubnode1 0)
		   (ientry2 0)
		   (isubnode2 0)
		   ;; About the result...
		   (entries 0)
		   (subnodes 0)
		   (ientry 0)
		   (isubnode 0)
		   (size 0)
		   (content-hash 0))
	       (flet ((add-entry (idx val)
			(setf (svref n (+ ch-set-node-header-size (postincf ientry))) val)
			(logiorf entries (ash 1 idx))
			(incf size)
			(logxorf content-hash (hash-value-fixnum val)))
		      (add-subnode (idx subnode)
			;; It's possible for `subnode' to contain only a collision subnode -- we must pull it up.
			(when (and (not (consp subnode))
				   (= 0 (ch-set-node-entry-mask subnode))
				   (= 1 (logcount (ch-set-node-subnode-mask subnode)))
				   (consp (svref subnode (1- (length subnode)))))
			  (setq subnode (svref subnode (1- (length subnode)))))
			(setf (svref n (- new-len (postincf isubnode) 1)) subnode)
			(logiorf subnodes (ash 1 idx))
			(incf size (ch-set-tree-size subnode))
			(logxorf content-hash (ch-set-tree-hash-value subnode))))
		 (do-bit-indices (idx everything)
		   (cond ((and (logbitp idx entries1) (logbitp idx entries2))
			  (let ((val1 (svref tree1 (+ ch-set-node-header-size (postincf ientry1))))
				(val2 (svref tree2 (+ ch-set-node-header-size (postincf ientry2)))))
			    (if (equal? val1 val2)
				(add-entry idx val1)
			      (add-subnode idx (ch-set-tree-with (ch-set-tree-with nil val1 (1+ depth))
								 val2 (1+ depth))))))
			 ((logbitp idx entries1)
			  (let ((val (svref tree1 (+ ch-set-node-header-size (postincf ientry1)))))
			    (if (logbitp idx subnodes2)
				(let ((subnode (svref tree2 (- (length tree2) (postincf isubnode2) 1))))
				  (add-subnode idx (ch-set-tree-with subnode val (1+ depth))))
			      (add-entry idx val))))
			 ((logbitp idx entries2)
			  (let ((val (svref tree2 (+ ch-set-node-header-size (postincf ientry2)))))
			    (if (logbitp idx subnodes1)
				(let ((subnode (svref tree1 (- (length tree1) (postincf isubnode1) 1))))
				  (add-subnode idx (ch-set-tree-with subnode val (1+ depth))))
			      (add-entry idx val))))
			 ((and (logbitp idx subnodes1) (logbitp idx subnodes2))
			  (add-subnode idx (rec (svref tree1 (- (length tree1) (postincf isubnode1) 1))
						(svref tree2 (- (length tree2) (postincf isubnode2) 1))
						(1+ depth))))
			 ((logbitp idx subnodes1)
			  (add-subnode idx (svref tree1 (- (length tree1) (postincf isubnode1) 1))))
			 ((logbitp idx subnodes2)
			  (add-subnode idx (svref tree2 (- (length tree2) (postincf isubnode2) 1))))
			 (t (error "Bug in 'ch-set-tree-union'")))))
	       (assert (= new-len (+ ch-set-node-header-size ientry isubnode)))
	       (setf (ch-set-node-entry-mask n) entries)
	       (setf (ch-set-node-subnode-mask n) subnodes)
	       (setf (ch-set-node-size n) size)
	       (setf (ch-set-node-hash-value n) content-hash)
	       n))))))

(defmacro do-ch-set-tree-members ((value-var tree-form &optional value-form)
				  &body body)
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
    `(block nil
       (labels ((,body-fn (,value-var)
		  . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(do-wb-set-tree-members (,value-var (cdr tree))
			  (,body-fn ,value-var))
		      (progn
			(dotimes (i (logcount (ch-set-node-entry-mask tree)))
			  (,body-fn (svref tree (+ i ch-set-node-header-size))))
			(dotimes (i (logcount (ch-set-node-subnode-mask tree)))
			  (,recur-fn (svref tree (- (length tree) 1 i)))))))))
	 (,recur-fn ,tree-form))
       ,value-form)))

(defun ch-set-tree-verify (tree)
  (declare (optimize (debug 3)))
  (or (null tree)
      (rlabels (rec tree 0 0)
	(rec (node depth partial-hash)
	  (macrolet ((test (form)
		       `(or ,form
			    (error "Test failed at ~:A: ~S" (path depth partial-hash) ',form))))
	    (let ((entry-mask (ch-set-node-entry-mask node))
		  (subnode-mask (ch-set-node-subnode-mask node))
		  ((entry-bits (bit-indices entry-mask))
		   (subnode-bits (bit-indices subnode-mask))))
	      (and (test (= 0 (logand entry-mask subnode-mask)))
		   (test (= 0 (ash entry-mask (- champ-node-radix))))
		   (test (= 0 (ash subnode-mask (- champ-node-radix))))
		   (test (= (length node) (+ ch-set-node-header-size (logcount entry-mask) (logcount subnode-mask))))
		   ;; Check that unless root, this node does not contain only an entry ...
		   (test (not (and (> depth 0) (= 1 (logcount entry-mask)) (= 0 subnode-mask))))
		   ;; ... or only a collision subnode
		   (test (not (and (> depth 0) (= 0 entry-mask) (= 1 (logcount subnode-mask))
				   (consp (svref node (1- (length node)))))))
		   (let ((size (logcount entry-mask))
			 (content-hash 0))
		     (and
		       ;; Check entry value hashes
		       (gmap :and (fn (value-idx hash-bits)
				    (let ((value (svref node (+ ch-set-node-header-size value-idx)))
					  ((value-hash (hash-value-fixnum value))))
				      (logxorf content-hash value-hash)
				      (test (= (ldb (byte (* champ-hash-bits-per-level (1+ depth)) 0)
						    value-hash)
					       (new-partial-hash hash-bits depth partial-hash)))))
			     (:arg index 0)
			     (:arg list entry-bits))
		       ;; Verify subnodes
		       (gmap :and (fn (subnode-idx hash-bits)
				    (let ((subnode (svref node (- (length node) 1 subnode-idx))))
				      (if (consp subnode)
					  (let ((chash 0))
					    (do-wb-set-tree-members (v (cdr subnode))
					      (logxorf chash (hash-value-fixnum v)))
					    (logxorf content-hash chash)
					    (incf size (wb-set-tree-size (cdr subnode)))
					    (test (= (car subnode) chash)))
					(and (rec subnode (1+ depth) (new-partial-hash hash-bits depth partial-hash))
					     (progn
					       (incf size (ch-set-node-size subnode))
					       (logxorf content-hash (ch-set-node-hash-value subnode))
					       t)))))
			     (:arg index 0)
			     (:arg list subnode-bits))
		       ;; Finally, check size and hash
		       (test (= (ch-set-node-size node) size))
		       (test (= (ch-set-node-hash-value node) content-hash))))))))
	(new-partial-hash (hash-bits depth partial-hash)
	  (dpb hash-bits (byte champ-hash-bits-per-level (* champ-hash-bits-per-level depth))
	       partial-hash))
	(path (depth partial-hash)
	  (let ((path nil))
	    (dotimes (i depth)
	      (push (ldb (byte champ-hash-bits-per-level (* i champ-hash-bits-per-level))
			 partial-hash)
		    path))
	    (nreverse path))))))


;;; ================================================================================
;;; Maps

(defstruct (ch-map-node
	     (:type vector))
  entry-mask
  subnode-mask
  size ; total number of pairs at or below this node
  hash-value)
(defconstant ch-map-node-header-size 4)
(declaim (ftype (function (t) fixnum)
		ch-map-node-entry-mask ch-map-node-subnode-mask ch-map-node-size ch-map-node-hash-value))

#||
This differs slightly from the published CHAMP algorithm.  It is agnostic as to the number
of bits in the hash values; it will use as many as are provided.  A collision can be
detected at any level, when two keys hash to the same value but `equal?' is false on them.
When that happens, we fall back to using a WB-tree for those entries.  The resulting
collision node will be as high in the tree as possible given that it can contain only keys
with that hash value; this can require pushing it down an arbitrary number of levels when
adding a new key.
||#

(defun ch-map-tree-with (tree key value)
  (declare (optimize (speed 3) (safety 0)))
  (let ((key-hash (hash-value-fixnum key)))
    (declare (fixnum key-hash))
    (rlabels (rec tree key-hash 0)
      (rec (node hash-shifted depth)
	(declare (fixnum hash-shifted)
		 (type (integer 0 64) depth))
	(let ((hash-bits (logand hash-shifted champ-hash-level-mask)))
	  (if (null node)
	      (vector (ash 1 hash-bits) 0 1 (logxor key-hash (hash-value-fixnum value)) key value)
	    (let ((entry-mask (ch-map-node-entry-mask node))
		  ((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		   ((entry-idx (+ ch-map-node-header-size (* 2 entry-raw-idx)))))
		  (subnode-mask (ch-map-node-subnode-mask node))
		  ((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		   ((subnode-idx (- (length node) 1 subnode-raw-idx)))))
	      (if (logbitp hash-bits entry-mask)
		  ;; Entry found
		  (let ((ex-key (svref node entry-idx))
			(ex-val (svref node (1+ entry-idx))))
		    (if (equal? key ex-key)
			(if (equal? value ex-val)
			    node ; Key found, value equal: nothing to do
			  ;; Key found, value differs: just update value
			  (let ((n (vector-update node (+ entry-idx 1) value)))
			    (logxorf (ch-map-node-hash-value n) (hash-value-fixnum ex-val) (hash-value-fixnum value))
			    n))
		      ;; Entry with different key found: make a subnode
		      (let ((hash-shifted (ash hash-shifted (- champ-hash-bits-per-level)))
			    (ex-key-hash (hash-value-fixnum ex-key))
			    ((ex-key-hash-shifted (ash ex-key-hash (* (- champ-hash-bits-per-level) (1+ depth))))
			     ((n2 (if (= hash-shifted ex-key-hash-shifted)
				      ;; Collision!  Fall back to WB-tree.
				      (cons (logxor key-hash (hash-value-fixnum value) ex-key-hash
						    (hash-value-fixnum ex-val))
					    (wb-map-tree-with (wb-map-tree-with nil ex-key ex-val) key value))
				    ;; Creates a little garbage; &&& hand-integrate later.
				    (rec (vector (ash 1 (logand ex-key-hash-shifted champ-hash-level-mask))
						 0 1 (logxor ex-key-hash (hash-value-fixnum ex-val)) ex-key ex-val)
					 hash-shifted (1+ depth))))
			    ;; The `1+' is because we're inserting _after_ `subnode-idx', because the subnodes
			    ;; are in reverse order.
			      ((n (vector-rem-2-ins-1 node entry-idx (1+ subnode-idx) n2))))))
			(logandc2f (ch-map-node-entry-mask n) (ash 1 hash-bits))
			(logiorf (ch-map-node-subnode-mask n) (ash 1 hash-bits))
			(incf (ch-map-node-size n))
			(logxorf (ch-map-node-hash-value n) key-hash (hash-value-fixnum value))
			n)))
		;; No entry found: check for subnode
		(if (logbitp hash-bits subnode-mask)
		    ;; Subnode found
		    (let ((subnode (svref node subnode-idx))
			  ((new-subnode
			     ;; We can create collision nodes at any level (see above).
			     (if (consp subnode)
				 (let ((wb-hash-shifted (ash (hash-value-fixnum (wb-map-tree-arb-pair (cdr subnode)))
							     (* (- champ-hash-bits-per-level) (1+ depth))))
				       (hash-shifted (ash hash-shifted (- champ-hash-bits-per-level)))
				       (size (1+ (wb-map-tree-size (cdr subnode)))))
				   (declare (fixnum wb-hash-shifted hash-shifted size))
				   (if (= hash-shifted wb-hash-shifted)
				       ;; Update the collision node.
				       (let ((new-wb-tree (wb-map-tree-with (cdr subnode) key value)))
					 (cond (;; The map code doesn't promise not to return an equal-but-not-eq tree.
						(eq (wb-map-tree-compare new-wb-tree (cdr subnode)) ':equal)
						subnode)
					       ((= (wb-map-tree-size new-wb-tree) (wb-map-tree-size (cdr subnode)))
						;; The key was already present, but the value was updated.
						(let ((ig old-value (wb-map-tree-lookup (cdr subnode) key)))
						  (declare (ignore ig))
						  (cons (logxor (the fixnum (car subnode))
								(hash-value-fixnum old-value) (hash-value-fixnum value))
							new-wb-tree)))
					       (t ; New entry in collision node
						(cons (logxor (the fixnum (car subnode)) key-hash
							      (hash-value-fixnum value))
						      new-wb-tree))))
				     ;; Oh, this is fun.  We add enough levels to get to where the hashes differ.
				     (rlabels (build hash-shifted wb-hash-shifted)
				       (build (hash-shifted wb-hash-shifted)
					 (declare (fixnum hash-shifted wb-hash-shifted))
					 (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
					       (wb-hash-bits (logand wb-hash-shifted champ-hash-level-mask))
					       (content-hash (logxor (the fixnum (car subnode)) key-hash
								     (hash-value-fixnum value))))
					   (if (= hash-bits wb-hash-bits)
					       (vector 0 (ash 1 hash-bits) size content-hash
						       (build (ash hash-shifted (- champ-hash-bits-per-level))
							      (ash wb-hash-shifted (- champ-hash-bits-per-level))))
					     (vector (ash 1 hash-bits) (ash 1 wb-hash-bits) size content-hash
						     key value subnode)))))))
			       (rec subnode (ash hash-shifted (- champ-hash-bits-per-level)) (1+ depth))))))
		      (if (eq new-subnode subnode)
			  node ; no change
			;; New subnode
			(let ((n (vector-update node subnode-idx new-subnode)))
			  (setf (ch-map-node-size n)
				(the fixnum (+ (ch-map-node-size n) (- (the fixnum (ch-map-tree-size new-subnode))
								       (the fixnum (ch-map-tree-size subnode))))))
			  (logxorf (ch-map-node-hash-value n) (the fixnum (ch-map-tree-hash-value subnode))
				   (the fixnum (ch-map-tree-hash-value new-subnode)))
			  n)))
		  ;; Neither entry nor subnode found: make new entry
		  (let ((n (vector-insert-2 node entry-idx key value)))
		    (logiorf (ch-map-node-entry-mask n) (ash 1 hash-bits))
		    (incf (ch-map-node-size n))
		    (logxorf (ch-map-node-hash-value n) key-hash (hash-value-fixnum value))
		    n))))))))))

(defun vector-rem-2-ins-1 (vec rem-idx ins-idx ins-val)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum rem-idx ins-idx))
  (assert (<= rem-idx (the fixnum (- ins-idx 2))))
  (let ((v (make-array (1- (length vec)))))
    (dotimes (i rem-idx)
      (setf (svref v i) (svref vec i)))
    (dotimes (i (- ins-idx rem-idx 2))
      (declare (fixnum i))
      (setf (svref v (+ rem-idx i)) (svref vec (+ rem-idx i 2))))
    (setf (svref v (- ins-idx 2)) ins-val)
    (dotimes (i (- (length vec) ins-idx))
      (declare (fixnum i))
      (setf (svref v (+ ins-idx i -1)) (svref vec (+ ins-idx i))))
    v))

(defun vector-insert-2 (vec idx ins-0 ins-1)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum idx))
  (let ((v (make-array (+ 2 (length vec)))))
    (dotimes (i idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v idx) ins-0)
    (setf (svref v (1+ idx)) ins-1)
    (dotimes (i (- (length vec) idx))
      (setf (svref v (+ idx i 2)) (svref vec (+ idx i))))
    v))

(defun ch-map-tree-size (tree)
  (cond ((null tree) 0)
	((consp tree)
	 (wb-map-tree-size (cdr tree)))
	(t (ch-map-node-size tree))))

(defun ch-map-tree-hash-value (tree)
  (cond ((null tree) 0)
	((consp tree) (car tree))
	(t (ch-map-node-hash-value tree))))

(defun ch-map-tree-less (tree key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((key-hash (hash-value-fixnum key)))
    (declare (fixnum key-hash))
    (rlabels (rec tree key-hash)
      (rec (node hash-shifted)
	(declare (fixnum hash-shifted))
	(and node
	     (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
		   (entry-mask (ch-map-node-entry-mask node))
		   ((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		    ((entry-idx (+ ch-map-node-header-size (* 2 entry-raw-idx)))))
		   (subnode-mask (ch-map-node-subnode-mask node)))
	       (if (logbitp hash-bits entry-mask)
		   (let ((ex-key (svref node entry-idx)))
		     (cond ((not (equal? ex-key key))
			    (values node 0))
			   ((and (= (logcount entry-mask) 1) (= 0 subnode-mask))
			    (values nil (logxor key-hash (hash-value-fixnum (svref node (1+ entry-idx))))))
			   (t
			    (let ((n (vector-remove-2-at node entry-idx))
				  (ex-value-hash (hash-value-fixnum (svref node (1+ entry-idx)))))
			      (logandc2f (ch-map-node-entry-mask n) (ash 1 hash-bits))
			      (decf (ch-map-node-size n))
			      (let ((kv-hash (logxor key-hash ex-value-hash)))
				(logxorf (ch-map-node-hash-value n) kv-hash)
				(values n kv-hash))))))
		 (let ((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		       ((subnode-idx (- (length node) 1 subnode-raw-idx))))
		   (if (not (logbitp hash-bits subnode-mask))
		       (values node 0)
		     (let ((subnode (svref node subnode-idx)))
		       (if (consp subnode)
			   (let ((new-wb-tree (wb-map-tree-less (cdr subnode) key)))
			     (if (eq new-wb-tree (cdr subnode))
				 (values node 0)
			       (let ((ig ex-value (wb-map-tree-lookup (cdr subnode) key))
				     ((ex-value-hash (hash-value-fixnum ex-value))))
				 (declare (ignore ig))
				 (if (= 1 (wb-map-tree-size new-wb-tree))
				     ;; Removing next-to-last pair of collision node: turn remaining key/val into entry
				     (let ((rem-key rem-val (wb-map-tree-arb-pair new-wb-tree))
					   ;; `entry-idx' is still valid from above; the hash bits must be the same
					   ((n (vector-ins-2-rem-1 node entry-idx rem-key rem-val subnode-idx))))
				       (logiorf (ch-map-node-entry-mask n) (ash 1 hash-bits))
				       (logandc2f (ch-map-node-subnode-mask n) (ash 1 hash-bits))
				       (decf (ch-map-node-size n))
				       (let ((kv-hash (logxor key-hash ex-value-hash)))
					 (logxorf (ch-map-node-hash-value n) kv-hash)
					 (values n kv-hash)))
				   (let ((n (vector-update node subnode-idx
							   (cons (logxor (the fixnum (car subnode)) key-hash
									 ex-value-hash)
								 new-wb-tree))))
				     (decf (ch-map-node-size n))
				     (logxorf (ch-map-node-hash-value n) key-hash ex-value-hash)
				     (values n (logxor key-hash ex-value-hash)))))))
			 (let ((new-subnode content-hash-delta
				 (rec subnode (ash hash-shifted (- champ-hash-bits-per-level)))))
			   (declare (fixnum content-hash-delta))
			   (if (eq new-subnode subnode)
			       (values node 0)
			     (let ((n (cond ((null new-subnode)
					     (let ((n (vector-remove-at node subnode-idx)))
					       (logandc2f (ch-map-node-subnode-mask n) (ash 1 hash-bits))
					       n))
					    ((and (= 1 (logcount (ch-map-node-entry-mask new-subnode)))
						  (= 0 (ch-map-node-subnode-mask new-subnode)))
					     ;; New subnode contains only a single entry: pull it up into this node
					     (let ((new-entry-idx
						     (+ ch-map-node-header-size
							(* 2 (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))))
						   ((n (vector-ins-2-rem-1
							 node new-entry-idx (svref new-subnode ch-map-node-header-size)
							 (svref new-subnode (1+ ch-map-node-header-size))
							 subnode-idx))))
					       (logiorf (ch-map-node-entry-mask n) (ash 1 hash-bits))
					       (logandc2f (ch-map-node-subnode-mask n) (ash 1 hash-bits))
					       n))
					    ((and (= 0 (ch-map-node-entry-mask new-subnode))
						  (= 1 (logcount (ch-map-node-subnode-mask new-subnode)))
						  (consp (svref new-subnode ch-map-node-header-size)))
					     ;; New subnode contains only a collision node: pull it up into this node
					     (vector-update node subnode-idx
							    (svref new-subnode ch-map-node-header-size)))
					    (t (vector-update node subnode-idx new-subnode)))))
			       (decf (ch-map-node-size n))
			       (logxorf (ch-map-node-hash-value n) content-hash-delta)
			       (values n content-hash-delta)))))))))))))))

(defun vector-remove-2-at (vec idx)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum idx))
  (let ((v (make-array (- (length vec) 2))))
    (dotimes (i idx)
      (setf (svref v i) (svref vec i)))
    (dotimes (i (- (length vec) idx 2))
      (declare (fixnum i))
      (setf (svref v (+ idx i)) (svref vec (+ idx i 2))))
    v))

(defun vector-ins-2-rem-1 (vec ins-idx ins-0 ins-1 rem-idx)
  (declare (optimize (speed 3) (safety 0))
	   (simple-vector vec)
	   (fixnum ins-idx rem-idx))
  (assert (<= ins-idx rem-idx))
  (let ((v (make-array (1+ (length vec)))))
    (dotimes (i ins-idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v ins-idx) ins-0)
    (setf (svref v (1+ ins-idx)) ins-1)
    (dotimes (i (- rem-idx ins-idx))
      (setf (svref v (+ ins-idx i 2)) (svref vec (+ ins-idx i))))
    (dotimes (i (- (length vec) rem-idx 1))
      (declare (fixnum i))
      (setf (svref v (+ rem-idx i 2)) (svref vec (+ rem-idx i 1))))
    v))

(defun ch-map-tree-lookup (tree key)
  (declare (optimize (speed 3) (safety 0)))
  (let ((key-hash (hash-value-fixnum key)))
    (declare (fixnum key-hash))
    (rlabels (rec tree key key-hash)
      (rec (node key hash-shifted)
	(declare (fixnum hash-shifted))
	(and node
	     (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
		   (entry-mask (ch-map-node-entry-mask node)))
	       (if (logbitp hash-bits entry-mask)
		   (let ((entry-idx (+ ch-map-node-header-size
				       (* 2 (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))))
			 ((ex-key (svref node entry-idx))))
		     (and (equal? ex-key key)
			  (values t (svref node (1+ entry-idx)))))
		 (let ((subnode-mask (ch-map-node-subnode-mask node)))
		   (and (logbitp hash-bits subnode-mask)
			(let ((subnode-idx (- (length node) 1 (logcount (logand (1- (ash 1 hash-bits)) subnode-mask))))
			      ((subnode (svref node subnode-idx))))
			  (if (consp subnode)
			      (wb-map-tree-lookup (cdr subnode) key)
			    (rec subnode key (ash hash-shifted (- champ-hash-bits-per-level))))))))))))))

(defmacro do-ch-map-tree-pairs ((key-var value-var tree-form &optional value-form)
				&body body)
  (let ((body-fn (gensym "BODY-"))
	(recur-fn (gensym "RECUR-")))
    `(block nil
       (labels ((,body-fn (,key-var ,value-var)
		  . ,body)
		(,recur-fn (tree)
		  (when tree
		    (if (consp tree)
			(do-wb-map-tree-pairs (,key-var ,value-var (cdr tree))
			  (,body-fn ,key-var ,value-var))
		      (progn
			(dotimes (i (logcount (ch-map-node-entry-mask tree)))
			  (let ((idx (+ (* 2 i) ch-map-node-header-size)))
			    (,body-fn (svref tree idx) (svref tree (1+ idx)))))
			(dotimes (i (logcount (ch-map-node-subnode-mask tree)))
			  (,recur-fn (svref tree (- (length tree) 1 i)))))))))
	 (,recur-fn ,tree-form))
       ,value-form)))

(defun ch-map-tree-verify (tree)
  (declare (optimize (debug 3)))
  (or (null tree)
      (rlabels (rec tree 0 0)
	(rec (node depth partial-hash)
	  (macrolet ((test (form)
		       `(or ,form
			    (error "Test failed at ~:A: ~S" (path depth partial-hash) ',form))))
	    (let ((entry-mask (ch-map-node-entry-mask node))
		  (subnode-mask (ch-map-node-subnode-mask node))
		  ((entry-bits (bit-indices entry-mask))
		   (subnode-bits (bit-indices subnode-mask))))
	      (and (test (= 0 (logand entry-mask subnode-mask)))
		   (test (= 0 (ash entry-mask (- champ-node-radix))))
		   (test (= 0 (ash subnode-mask (- champ-node-radix))))
		   (test (= (length node)
			    (+ ch-map-node-header-size (* 2 (logcount entry-mask)) (logcount subnode-mask))))
		   ;; Check that unless root, this node does not contain only an entry ...
		   (test (not (and (> depth 0) (= 1 (logcount entry-mask)) (= 0 subnode-mask))))
		   ;; ... or only a collision subnode
		   (test (not (and (> depth 0) (= 0 entry-mask) (= 1 (logcount subnode-mask))
				   (consp (svref node (1- (length node)))))))
		   (let ((size (logcount entry-mask))
			 (content-hash 0))
		     (and
		       ;; Check entry key hashes
		       (gmap :and (fn (key-idx hash-bits)
				    (let ((key (svref node (+ ch-map-node-header-size (* 2 key-idx))))
					  ((key-hash (hash-value-fixnum key)))
					  (value (svref node (+ ch-map-node-header-size (1+ (* 2 key-idx))))))
				      (setq content-hash (logxor content-hash key-hash (hash-value-fixnum value)))
				      (test (= (ldb (byte (* champ-hash-bits-per-level (1+ depth)) 0)
						    key-hash)
					       (new-partial-hash hash-bits depth partial-hash)))))
			     (:arg index 0)
			     (:arg list entry-bits))
		       ;; Verify subnodes
		       (gmap :and (fn (subnode-idx hash-bits)
				    (let ((subnode (svref node (- (length node) 1 subnode-idx))))
				      (if (consp subnode)
					  (let ((chash 0))
					    (do-wb-map-tree-pairs (k v (cdr subnode))
					      (setf chash (logxor chash (hash-value-fixnum k) (hash-value-fixnum v))))
					    (setf content-hash (logxor content-hash chash))
					    (incf size (wb-map-tree-size (cdr subnode)))
					    (test (= (car subnode) chash)))
					(and (rec subnode (1+ depth) (new-partial-hash hash-bits depth partial-hash))
					     (progn
					       (incf size (ch-map-node-size subnode))
					       (setf content-hash
						     (logxor content-hash (ch-map-node-hash-value subnode)))
					       t)))))
			     (:arg index 0)
			     (:arg list subnode-bits))
		       ;; Finally, check size and hash
		       (test (= (ch-map-node-size node) size))
		       (test (= (ch-map-node-hash-value node) content-hash))))))))
	(new-partial-hash (hash-bits depth partial-hash)
	  (dpb hash-bits (byte champ-hash-bits-per-level (* champ-hash-bits-per-level depth))
	       partial-hash))
	(path (depth partial-hash)
	  (let ((path nil))
	    (dotimes (i depth)
	      (push (ldb (byte champ-hash-bits-per-level (* i champ-hash-bits-per-level))
			 partial-hash)
		    path))
	    (nreverse path))))))


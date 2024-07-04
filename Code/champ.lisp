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

;;; Verification utility, used to recover hash bits from masks.
(defun bit-indices (mask)
  "A list of the indices of the 1 bits of `mask', in ascending order."
  (do ((mask mask (ash mask -1))
       (i 0 (1+ i))
       (result nil))
      ((= mask 0) (nreverse result))
    (when (logbitp 0 mask)
      (push i result))))

(define-modify-macro logiorf (&rest args)
  logior)

(define-modify-macro logandc2f (arg-2)
  logandc2)

(define-modify-macro logxorf (&rest args)
  logxor)


;;; ================================================================================
;;; Sets

#||
(defstruct (ch-set
	     (:include set)
	     (:constructor make-ch-set (contents))
	     (:predicate ch-set?)
	     (:print-function print-ch-set)
	     (:copier nil))
  contents)

(defparameter *empty-ch-set* (make-ch-set nil))
||#


;;; ================================================================================
;;; Maps

(defstruct (ch-map-node
	     (:type vector))
  ;; We could cram both masks into a single fixnum, giving them 30 bits each (on a native 64-bit architecture),
  ;; at the price of having to do integer div/mod instead of shifting and masking to split the hash value.
  ;; I wonder how much that would cost.
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

(defun ch-map-tree-with (tree key key-hash value)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum key-hash))
  (rlabels (rec tree key-hash 0)
    (rec (node hash-shifted depth)
      (declare (fixnum hash-shifted)
	       (type (integer 0 64) depth))
      (let ((hash-bits (logand hash-shifted champ-hash-level-mask)))
	(if (null node)
	    (vector (ash 1 hash-bits) 0 1 (logxor key-hash (hash-value value)) key value)
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
			  (logxorf (ch-map-node-hash-value n) (hash-value ex-val) (hash-value value))
			  n))
		    ;; Entry with different key found: make a subnode
		    (let ((hash-shifted (ash hash-shifted (- champ-hash-bits-per-level)))
			  (ex-key-hash (hash-value ex-key))
			  ((ex-key-hash-shifted (ash ex-key-hash (* (- champ-hash-bits-per-level) (1+ depth))))
			   ((n2 (if (= hash-shifted ex-key-hash-shifted)
				    ;; Collision!  Fall back to WB-tree.
				    (cons (logxor key-hash (hash-value value) ex-key-hash (hash-value ex-val))
					  (wb-map-tree-with (wb-map-tree-with nil ex-key ex-val) key value))
				  ;; Creates a little garbage; maybe hand-integrate later.
				  (rec (vector (ash 1 (logand ex-key-hash-shifted champ-hash-level-mask))
					       0 1 (logxor ex-key-hash (hash-value ex-val)) ex-key ex-val)
				       hash-shifted (1+ depth))))
			    ;; The `1+' is because we're inserting _after_ `subnode-idx', because the subnodes
			    ;; are in reverse order.
			    ((n (vector-rem-2-ins-1 node entry-idx (1+ subnode-idx) n2))))))
		      (logandc2f (ch-map-node-entry-mask n) (ash 1 hash-bits))
		      (logiorf (ch-map-node-subnode-mask n) (ash 1 hash-bits))
		      (incf (ch-map-node-size n))
		      (logxorf (ch-map-node-hash-value n) key-hash (hash-value value))
		      n)))
	      ;; No entry found: check for subnode
	      (if (logbitp hash-bits subnode-mask)
		  ;; Subnode found
		  (let ((subnode (svref node subnode-idx))
			((new-subnode
			   ;; We can create collision nodes at any level (see above).
			   (if (consp subnode)
			       (let ((wb-hash-shifted (ash (hash-value (wb-map-tree-arb-pair (cdr subnode)))
							   (* (- champ-hash-bits-per-level) (1+ depth))))
				     (hash-shifted (ash hash-shifted (- champ-hash-bits-per-level)))
				     (size (1+ (wb-map-tree-size (cdr subnode)))))
				 (declare (fixnum wb-hash-shifted hash-shifted size))
				 (if (= hash-shifted wb-hash-shifted)
				     ;; Update the collision node.
				     (let ((new-wb-tree (wb-map-tree-with (cdr subnode) key value)))
				       (cond ((eq new-wb-tree (cdr subnode))
					      subnode)
					     ((= (wb-map-tree-size new-wb-tree) (wb-map-tree-size (cdr subnode)))
					      ;; The key was already present, but the value was updated.
					      (let ((ig old-value (wb-map-tree-lookup (cdr subnode) key)))
						(declare (ignore ig))
						(cons (logxor (the fixnum (car subnode))
							      (hash-value old-value) (hash-value value))
						      new-wb-tree)))
					     (t ; new entry in collision node
					      (cons (logxor (the fixnum (car subnode)) key-hash (hash-value value))
						    new-wb-tree))))
				   ;; Oh, this is fun.  We have to add enough levels to get to where the hashes differ.
				   (rlabels (build hash-shifted wb-hash-shifted)
				     (build (hash-shifted wb-hash-shifted)
				       (declare (fixnum hash-shifted wb-hash-shifted))
				       (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
					     (wb-hash-bits (logand wb-hash-shifted champ-hash-level-mask))
					     (content-hash (logxor (the fixnum (car subnode)) key-hash
								   (hash-value value))))
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
		  (logxorf (ch-map-node-hash-value n) key-hash (hash-value value))
		  n)))))))))

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

(defun ch-map-tree-less (tree key key-hash)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum key-hash))
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
			  (values nil (logxor key-hash (hash-value (svref node (1+ entry-idx))))))
			 (t
			  (let ((n (vector-remove-2-at node entry-idx))
				(ex-value-hash (hash-value (svref node (1+ entry-idx)))))
			    (logandc2f (ch-map-node-entry-mask n) (ash 1 hash-bits))
			    (decf (ch-map-node-size n))
			    (logxorf (ch-map-node-hash-value n) key-hash ex-value-hash)
			    (values n (logxor key-hash ex-value-hash))))))
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
				   ((ex-value-hash (hash-value ex-value))))
			       (declare (ignore ig))
			       (if (= 1 (wb-map-tree-size new-wb-tree))
				   ;; Removing next-to-last pair of collision node
				   (let ((rem-key rem-val (wb-map-tree-arb-pair new-wb-tree))
					 ;; `entry-idx' is still valid from above; the hash bits must be the same
					 ((n (vector-ins-2-rem-1 node entry-idx rem-key rem-val subnode-idx))))
				     (logiorf (ch-map-node-entry-mask n) (ash 1 hash-bits))
				     (logandc2f (ch-map-node-subnode-mask n) (ash 1 hash-bits))
				     (decf (ch-map-node-size n))
				     (logxorf (ch-map-node-hash-value n) key-hash ex-value-hash)
				     (values n (logxor key-hash ex-value-hash)))
				 (let ((n (vector-update node subnode-idx
							 (cons (logxor (the fixnum (car subnode)) key-hash
								       ex-value-hash)
							       new-wb-tree))))
				   (decf (ch-map-node-size n))
				   (logxorf (ch-map-node-hash-value n) ex-value-hash)
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
						 ((n
						    (vector-ins-2-rem-1 node new-entry-idx
									(svref new-subnode ch-map-node-header-size)
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
			     (values n content-hash-delta))))))))))))))

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

(defun ch-map-tree-lookup (tree key key-hash)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum key-hash))
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
			  (rec subnode key (ash hash-shifted (- champ-hash-bits-per-level)))))))))))))

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
					  ((key-hash (hash-value key)))
					  (value (svref node (+ ch-map-node-header-size (1+ (* 2 key-idx))))))
				      (setq content-hash (logxor content-hash key-hash (hash-value value)))
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
					      (setf chash (logxor chash (hash-value k) (hash-value v))))
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


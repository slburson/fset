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


;;; ================================================================================
;;; Sets

(defstruct (ch-set
	     (:include set)
	     (:constructor make-ch-set (contents))
	     (:predicate ch-set?)
	     (:print-function print-ch-set)
	     (:copier nil))
  contents)

(defparameter *empty-ch-set* (make-ch-set nil))


;;; ================================================================================
;;; Maps

(defstruct (ch-map
	     (:include map)
	     (:constructor make-ch-map (contents &optional default))
	     (:predicate ch-map?)
	     (:print-function print-ch-map)
	     (:copier nil))
  contents)

(defparameter *empty-ch-map* (make-ch-map nil))

(declaim (inline empty-ch-map))
(defun empty-ch-map (&optional default)
  (if default (make-ch-map nil default)
    *empty-ch-map*))

(defmethod empty-map-instance-form ((type-name (eql 'ch-map)) default)
  `(empty-ch-map ,default))

(defmacro ch-map (&rest args)
  (expand-map-constructor-form 'ch-map args))

(defmethod with-default ((m ch-map) new-default)
  (make-ch-map (ch-map-contents m) new-default))

(defmethod empty? ((m ch-map))
  (null (ch-map-contents m)))

(defmethod with ((m ch-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'ch-map)
  (make-ch-map (ch-map-tree-with (ch-map-contents m) key (hash-value key) value)
	       (map-default m)))

(defmethod less ((m ch-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'ch-map)
  (make-ch-map (ch-map-tree-less (ch-map-contents m) key (hash-value key))
	       (map-default m)))

(defmethod lookup ((m ch-map) key)
  (let ((val? val (ch-map-tree-lookup (ch-map-contents m) key (hash-value key))))
    ;; Our internal convention is the reverse of the external one.
    (values (if val? val (map-default m)) val?)))

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

#||
This differs slightly from the published CHAMP algorithm.  It is agnostic as to the number
of bits in the hash values; it will use as many as are provided.  A collision can be
detected at any level, when two keys hash to the same value but `equal?' is false on them.
When that happens, we fall back to using a WB-tree for those entries.  The resulting
collision node will be as high in the tree as possible given that it can only contain keys
with that hash value; this can require pushing it down an arbitrary number of levels when
adding a new key.
||#
(defun ch-map-tree-with (tree key key-hash value)
  (rlabels (rec tree key-hash 0)
    (rec (node hash-shifted depth)
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
		(let (((ex-key (svref node entry-idx))
		       (ex-val (svref node (1+ entry-idx)))))
		  (if (equal? key ex-key)
		      (if (equal? value ex-val)
			  node ; Key found, value equal: nothing to do
			;; Key found, value differs: just update value
			(let ((n (vector-update node (+ entry-idx 1 ch-map-node-header-size) value)))
			  (setf (ch-map-node-hash-value n)
				(logxor (ch-map-node-hash-value node)
					(hash-value ex-val) (hash-value value)))
			  (setf (svref n (1+ entry-idx)) value)
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
				       hash-shifted (1+ depth))))))
			  (n (make-array (1- (length node)))))
		      (setf (ch-map-node-entry-mask n)
			    (logand (ch-map-node-entry-mask node) (lognot (ash 1 hash-bits))))
		      (assert (not (logbitp hash-bits (ch-map-node-subnode-mask node))))
		      (setf (ch-map-node-subnode-mask n) (logior (ch-map-node-subnode-mask node) (ash 1 hash-bits)))
		      (setf (ch-map-node-size n) (1+ (ch-map-node-size node)))
		      (setf (ch-map-node-hash-value n)
			    (logxor (ch-map-node-hash-value node) key-hash (hash-value value)))
		      ;; Copy up to entry being deleted
		      (dotimes (i (* 2 entry-raw-idx))
			(setf (svref n (+ i ch-map-node-header-size)) (svref node (+ i ch-map-node-header-size))))
		      ;; Copy from next entry up to subnode being inserted
		      (dotimes (i (- subnode-idx entry-idx 1))
			(setf (svref n (+ entry-idx i))
			      (svref node (+ entry-idx i 2))))
		      ;; Insert subnode (`subnode-idx' is where the subnode would have been in `node', had it
		      ;; been present; we subtract 2 for the deleted entry, but add 1 back because it's a new subnode)
		      (setf (svref n (1- subnode-idx)) n2)
		      ;; Copy remaining subnodes
		      (dotimes (i subnode-raw-idx)
			(setf (svref n (+ subnode-idx i))
			      (svref node (+ subnode-idx i 1))))
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
				 (if (= hash-shifted wb-hash-shifted)
				     ;; Update the collision node.
				     (let ((new-wb-tree (wb-map-tree-with (cdr subnode) key value)))
				       (cond ((eq new-wb-tree (cdr subnode))
					      subnode)
					     ((= (wb-map-tree-size new-wb-tree) (wb-map-tree-size (cdr subnode)))
					      ;; The key was already present, but the value was updated.
					      (let ((ig old-value (wb-map-tree-lookup (cdr subnode) key)))
						(declare (ignore ig))
						(cons (logxor (car subnode) (hash-value old-value) (hash-value value))
						      new-wb-tree)))
					     (t ; new entry in collision node
					      (cons (logxor (car subnode) key-hash (hash-value value)) new-wb-tree))))
				   ;; Oh, this is fun.  We have to add enough levels to get to where the hashes differ.
				   (rlabels (build hash-shifted wb-hash-shifted)
				     (build (hash-shifted wb-hash-shifted)
				       (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
					     (wb-hash-bits (logand wb-hash-shifted champ-hash-level-mask))
					     (content-hash (logxor (car subnode) key-hash (hash-value value))))
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
			(incf (ch-map-node-size n) (- (ch-map-node-size new-subnode) (ch-map-node-size subnode)))
			(setf (ch-map-node-hash-value n)
			      (logxor (ch-map-node-hash-value node) (ch-map-node-hash-value subnode)
				      (ch-map-node-hash-value new-subnode)))
			n)))
		;; Neither entry nor subnode found: make new entry
		(let ((n (make-array (+ 2 (length node)))))
		  (dotimes (i entry-idx) ; includes header
		    (setf (svref n i) (svref node i)))
		  (setf (ch-map-node-entry-mask n) (logior (ch-map-node-entry-mask node) (ash 1 hash-bits)))
		  (incf (ch-map-node-size n))
		  (setf (ch-map-node-hash-value n)
			(logxor (ch-map-node-hash-value node) key-hash (hash-value value)))
		  (setf (svref n entry-idx) key)
		  (setf (svref n (1+ entry-idx)) value)
		  (dotimes (i (- (length node) entry-idx))
		    (setf (svref n (+ entry-idx i 2)) (svref node (+ entry-idx i))))
		  n)))))))))

(defun ch-map-tree-less (tree key key-hash)
  (rlabels (rec tree key key-hash)
    (rec (node key hash-shifted)
      (and node
	   (let ((hash-bits (logand hash-shifted champ-hash-level-mask))
		 (entry-mask (ch-map-node-entry-mask node))
		 ((entry-raw-idx (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))
		  ((entry-idx (+ ch-map-node-header-size (* 2 entry-raw-idx)))))
		 (subnode-mask (ch-map-node-subnode-mask node)))
	     (if (logbitp hash-bits entry-mask)
		 (let ((ex-key (svref node entry-idx)))
		   (cond ((not (equal? ex-key key))
			  node)
			 ((and (= (logcount entry-mask) 1) (= 0 subnode-mask))
			  nil)
			 (t
			  (let ((n (make-array (- (length node) 2))))
			    (dotimes (i entry-idx) ; includes header
			      (setf (svref n i) (svref node i)))
			    (dotimes (i (- (length node) entry-idx 2))
			      (setf (svref n (+ i entry-idx))
				    (svref node (+ i entry-idx 2))))
			    (setf (ch-map-node-entry-mask n)
				  (logand (ch-map-node-entry-mask n) (lognot (ash 1 hash-bits))))
			    (decf (ch-map-node-size n))
			    (setf (ch-map-node-hash-value n)
				  (logxor (ch-map-node-hash-value n) key-hash (hash-value (svref node (1+ entry-idx)))))
			    n))))
	       (let ((subnode-raw-idx (logcount (logand (1- (ash 1 hash-bits)) subnode-mask)))
		     ((subnode-idx (- (length node) 1 subnode-raw-idx))))
		 (if (not (logbitp hash-bits subnode-mask))
		     node
		   (let ((subnode (svref node subnode-idx)))
		     (if (consp subnode)
			 (let ((new-wb-tree (wb-map-tree-less (cdr subnode) key)))
			   (if (eq new-wb-tree (cdr subnode))
			       subnode
			     (let ((ig old-value (wb-map-tree-lookup (cdr subnode) key)))
			       (declare (ignore ig))
			       (cons (logxor (car subnode) key-hash (hash-value old-value)) new-wb-tree))))
		       (let ((new-subnode (rec subnode key (ash hash-shifted (- champ-hash-bits-per-level)))))
			 (cond ((eq new-subnode subnode)
				node)
			       ((= (length new-subnode) (+ ch-map-node-header-size 2))
				;; New subnode contains only a single entry: pull it up into this node
				(let ((new-entry-idx (+ ch-map-node-header-size
							(* 2 (logcount (logand (1- (ash 1 hash-bits)) entry-mask)))))
				      ((n (vector-ins-2-rem-1 node new-entry-idx
							      (svref new-subnode ch-map-node-header-size)
							      (svref new-subnode (1+ ch-map-node-header-size))
							      subnode-idx))))
				  (setf (ch-map-node-entry-mask n)
					(logior (ch-map-node-entry-mask n) (ash 1 hash-bits)))
				  (setf (ch-map-node-subnode-mask n)
					(logand (ch-map-node-subnode-mask n) (lognot (ash 1 hash-bits))))
				  n))
			       ((and (= (length new-subnode) (1+ ch-map-node-header-size))
				     (consp (svref new-subnode ch-map-node-header-size)))
				;; New subnode contains only a single collision node: pull it up into this node
				(vector-update node subnode-idx (svref new-subnode ch-map-node-header-size)))
			       (t
				(vector-update node subnode-idx new-subnode))))))))))))))

(defun ch-map-tree-lookup (tree key key-hash)
  (rlabels (rec tree key key-hash)
    (rec (node key hash-shifted)
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
		      (let ((subnode-idx (- (length node) 1))
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

(defmethod internal-do-map ((m ch-map) elt-fn &optional (value-fn (lambda () nil)))
  (declare ;(optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-ch-map-tree-pairs (x y (ch-map-contents m) (funcall value-fn))
    (funcall elt-fn x y)))

(defun print-ch-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "##{|")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))
    (format stream " |}~:[~;/~:*~S~]" (map-default map))))

(defun ch-map-tree-verify (tree)
  (or (null tree)
      (rlabels (rec tree 0 0)
	(rec (node depth partial-hash)
	  (let ((entry-mask (ch-map-node-entry-mask node))
		(subnode-mask (ch-map-node-subnode-mask node))
		((entry-bits (bit-indices entry-mask))
		 (subnode-bits (bit-indices subnode-mask))))
	    (and (= 0 (logand entry-mask subnode-mask))
		 (= 0 (ash entry-mask (- champ-node-radix)))
		 (= 0 (ash subnode-mask (- champ-node-radix)))
		 (= (length node) (+ ch-map-node-header-size (* 2 (logcount entry-mask)) (logcount subnode-mask)))
		 ;; Check that unless root, this node does not contain only an entry ...
		 (not (and (> depth 0) (= 1 (logcount entry-mask)) (= 0 subnode-mask)))
		 ;; ... or only a collision subnode
		 (not (and (> depth 0) (= 0 entry-mask) (= 1 (logcount subnode-mask))
			   (consp (svref node (1- (length node))))))
		 (let ((size (logcount entry-mask))
		       (content-hash 0))
		   (and
		     ;; Check entry key hashes
		     (gmap :and (fn (key-idx hash-bits)
				  (let ((key (svref node (+ ch-map-node-header-size (* 2 key-idx))))
					((key-hash (hash-value key)))
					(value (svref node (+ ch-map-node-header-size (1+ (* 2 key-idx))))))
				    (setq content-hash (logxor content-hash key-hash (hash-value value)))
				    (= (ldb (byte (* champ-hash-bits-per-level (1+ depth)) 0)
					    key-hash)
				       (new-partial-hash hash-bits depth partial-hash))))
			   (:arg index 0)
			   (:arg list entry-bits))
		     ;; Verify subnodes
		     (gmap :and (fn (subnode-idx hash-bits)
				  (let ((subnode (svref node (- (length node) 1 subnode-idx))))
				    (if (consp subnode)
					(let ((chash 0))
					  (do-wb-map-tree-pairs (k v (cdr subnode))
					    (setf chash (logxor content-hash (hash-value k) (hash-value v))))
					  (setf content-hash (logxor content-hash chash))
					  (incf size (wb-map-tree-size (cdr subnode)))
					  (= (car subnode) chash))
				      (and (rec subnode (1+ depth) (new-partial-hash hash-bits depth partial-hash))
					   (progn
					     (incf size (ch-map-node-size subnode))
					     (setf content-hash (logxor content-hash (ch-map-node-hash-value subnode)))
					     t)))))
			   (:arg index 0)
			   (:arg list subnode-bits))
		     ;; Finally, check size and hash
		     (= (ch-map-node-size node) size)
		     (= (ch-map-node-hash-value node) content-hash))))))
	(new-partial-hash (hash-bits depth partial-hash)
	  (dpb hash-bits (byte champ-hash-bits-per-level (* champ-hash-bits-per-level depth))
	       partial-hash)))))

(defun vector-ins-2-rem-1 (vec ins-idx ins-0 ins-1 rem-idx)
  (assert (<= ins-idx rem-idx))
  (let ((v (make-array (1+ (length vec)))))
    (dotimes (i ins-idx)
      (setf (svref v i) (svref vec i)))
    (setf (svref v ins-idx) ins-0)
    (setf (svref v (1+ ins-idx)) ins-1)
    (dotimes (i (- rem-idx ins-idx))
      (setf (svref v (+ ins-idx i 2)) (svref vec (+ ins-idx i))))
    (dotimes (i (- (length vec) rem-idx 1))
      (setf (svref v (+ rem-idx i 2)) (svref vec (+ rem-idx i 1))))
    v))

(defmethod convert ((to-type (eql 'wb-map)) (m ch-map) &key)
  (let ((wb-m (empty-wb-map (map-default m))))
    (do-ch-map-tree-pairs (x y (ch-map-contents m))
      (setf (lookup wb-m x) y))
    wb-m))


;;; ================================================================================
;;; Testing

(defmethod hash-value ((x my-integer))
  (logand (my-integer-value x) 127))

(defun test-champ-maps (n)
  (let ((cm )))
  (dotimes (i n)
    (let (()))))


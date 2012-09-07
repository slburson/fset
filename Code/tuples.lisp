;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: tuples.lisp
;;; Contents: Dynamic tuples implementation.
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


;;; Dynamic Tuples

;;; Tuples are abstractly similar to maps, at least in an untyped language like
;;; Lisp, but have a very different usage profile.  The big difference is that
;;; the key set is bounded, and indeed normally fixed at compile time.  Tuple
;;; keys are analogous to slots of classes; they're not arbitrary values created
;;; at runtime.  In Lisp it is natural to associate them with symbols.  Also,
;;; tuple key comparisons are always done with `eq', not `equal?' as for maps.
;;; So they're really more like structs than maps, except that (a) update is
;;; functional (of course) and (b) there's only one tuple type; any key can be
;;; added to any tuple, yet the representation is sparse: any given tuple takes
;;; only space proportional to the number of pairs it actually contains.

;;; Here is an example of a situation in which the use of tuples is appropriate
;;; -- indeed, highly recommended.  Suppose you have instances of a mutable
;;; class, and you want to create a map mapping those instances to some range
;;; type, but rather than the mapping being by identity, you want it to be by
;;; some part of the content of the instances.  You might be inclined to define
;;; a `compare' method on this class that compares the instances by comparing
;;; an appropriate subset of their attributes.  But there's a danger here.  If
;;; any of the values of those attributes ever changes after the instance has
;;; been used as a map key, the instance will, in general, no longer compare the
;;; same way against the other instances in the map, thus blowing one of the key
;;; invariants of the map's internal structure and rendering it more or less
;;; useless.

;;; What you should do instead is, for each instance used as a key in this map,
;;; make a tuple containing the values of the attributes in question, and then
;;; use the tuple as the map key.  If you attach the tuple to the instance by a
;;; new attribute, then the object can always know what tuple it has been
;;; indexed by in the map; and if the values in that tuple differ, at some 
;;; point, from those in the instance, then you know that the instance values
;;; have changed since the instance was last indexed in the map, and you have
;;; all the information handy to re-index the instance (removing the old
;;; mapping, constructing the new tuple, and then adding the new mapping).

;;; It's true, you could use a list or short vector instead, and for very small
;;; tuples you may as well do so.  But for larger tuples and particularly those
;;; with sparse slots (at which most of the tuples created have no assigned
;;; value), you may find the additional functionality of these tuples useful.

;;; Keys can be defined with `define-tuple-key', or obtained at runtime with
;;; `get-tuple-key'.

;;; The implementation gets its speed by arranging for lookup to be done by
;;; scanning consecutive memory locations (thus taking advantage of cache
;;; effects) rather than by ordered trees.  Thus the performance characteristics
;;; of tuples are quite different from those of the other FSet classes; where the
;;; other FSet classes do lookups in log time but with a significant constant
;;; factor, tuple lookups use linear search but with a very small constant factor
;;; -- making them faster than even a well-compiled `assoc'.  This is partly
;;; because of better use of the cache, and partly because of a clever scheme for
;;; thread-safe dynamic reordering that makes frequently used slots cheaper to
;;; access.

;;; A tuple is implemented as a struct of (a) a descriptor and (b) a vector of
;;; bounded-length "chunk" vectors, the latter containing the values.  Each
;;; descriptor is in general shared by many tuple instances with the same key set;
;;; it has a key vector, which contains two copies of a sequence of pairs
;;; represented as fixnums with two fields; one member of the pair is the key
;;; number, the other is the index of the value.  Lookup scans the sequence to
;;; find the given key number, then uses the index as an address within the chain
;;; of value vectors.  Notice that the index within the key vector at which a
;;; given pair is found is immaterial.

;;; The reason the key vectors contain two copies of the sequence is to give us
;;; thread-safe dynamic reordering without locking lookups.  If one thread is in
;;; the middle of a reordering and another simultaneously does a lookup on a tuple
;;; that uses the same key vector, the worst that can happen is that it will miss
;;; the entry in the first copy of the sequence (which is the one that gets
;;; reordered) and find it in the second copy.

;;; So as not to reorder on every lookup we define a "window" -- the first few
;;; slots of the key vector; the exact size depends on the number of keys -- and
;;; arrange that if a key is found within the window, no reordering takes place.
;;; If a lookup finds the key entry outside the window, however, it exchanges its
;;; position with the previous entry (unless the previous entry is in the window,
;;; in which case a slot in the window is selected pseudorandomly for eviction).

;;; The reason the values are broken up into chunks is to keep update of large
;;; tuples from getting too slow.  I don't really expect there to be many cases
;;; where tuples get larger than a chunk or two, but they could occasionally.

;;; Note that there is the potential for exponential size blowup if a large
;;; number of keys are used in many different orders, since every set of keys
;;; ever used is represented explicitly, and in the worst case this would be
;;; the powerset of the set of keys.  I don't think real code behaves this way,
;;; but it's something to keep in mind.


(defstruct (dyn-tuple
	    (:include tuple)
	    (:constructor make-dyn-tuple (descriptor contents))
	    (:predicate dyn-tuple?)
	    (:print-function print-dyn-tuple)
	    (:copier nil))
  "A class of functional tuples represented as vectors with dynamically-
reordered key vectors.  This is the default implementation of tuples in FSet."
  ;; A `Tuple-Desc'.
  descriptor
  ;; A vector of value chunks (vectors) (all these vectors being simple).
  contents)


(defstruct (tuple-key
	    (:constructor make-tuple-key (name default-fn number))
	    (:predicate tuple-key?)
	    (:print-function print-tuple-key))
  name		; a symbol
  default-fn	; default function for tuples with no explicit entry for this key
		;   (called with one argument, the tuple), or nil
  number)	; used for lookup and sorting

(deflex +Tuple-Key-Name-Map+ (empty-map))

(deflex +Tuple-Key-Seq+ (empty-seq))

(deflex +Tuple-Key-Lock+ (make-lock "Tuple Key Lock"))

(defun get-tuple-key (name &optional default-fn)
  "Finds or creates a tuple key named `name'.  If the key did not already exist,
and `default-fn' is supplied, it is used to compute a value for lookups where
the tuple has no explicit pair with this key; it is called with one argument,
the tuple."
  (assert (or (null default-fn) (typep default-fn 'function)))
  (with-lock (+Tuple-Key-Lock+)
    (let ((key (lookup +Tuple-Key-Name-Map+ name))
	  (key-idx (size +Tuple-Key-Seq+)))
      (or key
	  (if (<= key-idx Tuple-Key-Number-Mask)
	      (let ((key (make-tuple-key name default-fn key-idx)))
		(setf (lookup +Tuple-Key-Name-Map+ name) key)
		(push-last +Tuple-Key-Seq+ key)
		key)
	    (error "Tuple key space exhausted"))))))

(defmacro def-tuple-key (name &optional default-fn)
  "Deprecated; use `define-tuple-key'."
  ;; What this should have been called to begin with.
  `(define-tuple-key ,name ,default-fn))

(defmacro define-tuple-key (name &optional default-fn)
  "Defines a tuple key named `name' as a global lexical variable (see `deflex').
If `default-fn' is supplied, it is used to compute a value for lookups where
the tuple has no explicit pair with this key; it is called with one argument,
the tuple."
  (assert (symbolp name))
  `(deflex ,name (get-tuple-key ',name ,default-fn)))

(defun print-tuple-key (key stream level)
  (declare (ignore level))
  (format stream "#<Key ~A>" (tuple-key-name key)))

(defmethod compare ((key1 tuple-key) (key2 tuple-key))
  ;(compare (tuple-key-number key1) (tuple-key-number key2))  inlined...
  (let ((n1 (tuple-key-number key1))
	(n2 (tuple-key-number key2)))
    (cond ((< n1 n2) ':less)
	  ((> n1 n2) ':greater)
	  (t ':equal))))


(defconstant Tuple-Value-Chunk-Bits 3)
;;; Each value chunk (vector) contains no more than this many elements.
(defconstant Tuple-Value-Chunk-Size (ash 1 Tuple-Value-Chunk-Bits))

(defstruct (Tuple-Desc
	     (:constructor Make-Tuple-Desc-Internal (Key-Set Pairs Lock Serial-Number)))
  ;; The set (as an FSet set) of `tuple-key's for which tuples using this index
  ;; contain values.
  Key-Set
  ;; The pair vector, which contains two copies of the pair sequence, each pair
  ;; represented as a fixnum: the key number is in the low-order `Tuple-Key-Number-
  ;; Size' bits, and the value index is in the next `Tuple-Value-Index-Size' bits.
  Pairs
  ;; The reorder lock.
  Lock
  ;; The reorder map map: maps other tuple-descs to reorder maps for them.  Each
  ;; reorder map is a list with one element for each chunk in the target; that
  ;; element is `nil' if the chunk is unchanged, else it is a vector of indices
  ;; into the source tuple (where an index of `nil' indicates an insertion).
  (Reorder-Map-Map (empty-map))
  ;; Cache used by `Tuple-With': for each key not in this descriptor but that has been
  ;; added to tuples with this descriptor, the descriptor for the new tuple.
  (Next-Desc-Map (empty-map))
  ;; Serial number (used for `Reorder-Map-Map').
  Serial-Number)

(deflex +Tuple-Desc-Next-Serial-Number+ 0)

(deflex +Tuple-Desc-Next-Serial-Number-Lock+ (make-lock))

(defun Make-Tuple-Desc (key-set pairs)
  (Make-Tuple-Desc-Internal key-set pairs (make-lock)
			    (prog1 +Tuple-Desc-Next-Serial-Number+
			      (with-lock (+Tuple-Desc-Next-Serial-Number-Lock+)
				(incf +Tuple-Desc-Next-Serial-Number+)))))

(deflex +Tuple-Descriptor-Map+ (empty-map))

(defmethod compare ((x Tuple-Desc) (y Tuple-Desc))
  (let ((xser (Tuple-Desc-Serial-Number x))
	(yser (Tuple-Desc-Serial-Number y)))
    (cond ((< xser yser) ':less)
	  ((> xser yser) ':greater)
	  (t ':equal))))

;;; Urgh, Allegro 6 doesn't obey inline declarations, so we use a macro for this.
;;; This takes its argument doubled for the convenience of the common case (lookup).
;;; &&& These numbers are SWAGs and may be too small.  See `Tuple-Reorder-Keys'.
(defmacro Tuple-Window-Size (nkeys*2)
  (let ((nkeys*2-var (gensym)))
    `(let ((,nkeys*2-var ,nkeys*2))
       (cond ((< ,nkeys*2-var 16) 3)
	     ((< ,nkeys*2-var 32) 4)
	     ((< ,nkeys*2-var 48) 5)
	     (t 6)))))

(defmethod domain ((tup dyn-tuple))
  (Tuple-Desc-Key-Set (dyn-tuple-descriptor tup)))

(defparameter Tuple-Reorder-Score-Threshold 15		; SWAG
  "The reorder score that triggers a major reordering.")


(defun empty-tuple ()
  "Returns an empty tuple of the default implementation."
  (empty-dyn-tuple))

(defun empty-dyn-tuple ()
  "Returns an empty dyn-tuple."
  (let ((desc (lookup +Tuple-Descriptor-Map+ (empty-map))))
    (unless desc
      (setq desc (Make-Tuple-Desc (empty-set) (vector)))
      (setf (lookup +Tuple-Descriptor-Map+ (empty-map)) desc))
  (make-dyn-tuple desc (vector))))

(deflex +Tuple-Random-Value+ 0
  "State for an extremely fast, low-quality generator of small numbers of
pseudorandom bits.  Yep, this is about as quick-and-dirty as it gets --
we just increment this value by some small prime like 5 each time.  We
don't worry about locking it, either.")

(declaim (inline Tuple-Random-Value))
(defun Tuple-Random-Value ()
  (the fixnum
    (setf +Tuple-Random-Value+
	  (logand (+ (the fixnum +Tuple-Random-Value+) 5)
		  most-positive-fixnum))))

(defconstant Tuple-Reorder-Frequency 31
  "Controls how often we do tuple reordering.  Must be 2^n - 1 for some n.")

(defun Tuple-Lookup (tuple key &optional no-reorder?)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((desc (dyn-tuple-descriptor tuple))
	((pairs (Tuple-Desc-Pairs desc))
	 ((nkeys*2 (length pairs))))
	(key-num (if (typep key 'fixnum) key	; for internal use only
		   (tuple-key-number key))))
    (declare (fixnum nkeys*2 key-num))
    ;; As explained above, we don't have to lock here because there are two copies
    ;; of the pair sequence, only the first of which gets reordered.  So if we
    ;; happen to miss the pair we're looking for in the first copy, because it is
    ;; momentarily not there, we'll find it in the second copy.
    (dotimes (i nkeys*2)
      (declare (fixnum i))
      (let ((pr (svref pairs i)))
	(declare (fixnum pr))
	(when (= (logand pr Tuple-Key-Number-Mask)
		 key-num)
	  (let ((chunks (dyn-tuple-contents tuple))
		(val-idx (the fixnum (ash pr (- Tuple-Key-Number-Size)))))
	    (let ((val (svref (svref chunks (ash val-idx (- Tuple-Value-Chunk-Bits)))
			      (logand val-idx (1- Tuple-Value-Chunk-Size)))))
	      (unless (or no-reorder? (< i (Tuple-Window-Size nkeys*2))
			  ;;; Reorder only on some fraction of the occasions
			  ;;; we come through here.
			  (not (= 0 (logand (Tuple-Random-Value)
					    Tuple-Reorder-Frequency))))
		(Tuple-Reorder-Keys tuple i))
	      (return-from Tuple-Lookup (values t val)))))))
    (values nil nil)))

(defun Tuple-Reorder-Keys (tuple idx)
  ;(declare (optimize (speed 3) (safety 0)))
  (declare (fixnum idx))
  (let ((desc (dyn-tuple-descriptor tuple))
	((pairs (Tuple-Desc-Pairs desc))))
    ;; Some implementations can't do `:wait? nil', but that's okay -- we'll just
    ;; do a little redundant work.
    (with-lock ((Tuple-Desc-Lock desc) :wait? nil)
      (let ((nkeys*2 (length pairs))
	    ((window-size (Tuple-Window-Size nkeys*2))))
	(declare (fixnum nkeys*2))
	(let ((to-idx (if (>= (1- idx) window-size)
			  (1- idx)
			(mod (Tuple-Random-Value) window-size)))
	      ((to-key-pr (svref pairs to-idx)))
	      (key-pr (svref pairs idx)))
	  (declare (fixnum key-pr to-key-pr))
	  (setf (svref pairs idx) to-key-pr)
	  (setf (svref pairs to-idx) key-pr))))))


;;; Someday: multiple key/value pair update.
(defun Tuple-With (tuple key val)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((old-val? old-val (Tuple-Lookup tuple key)))
    (if old-val?
	(if (equal? val old-val)
	    tuple
	  ;; Present in tuple already -- key set doesn't change.
	  ;; The lookup may have reordered the tuple.
	  (let ((key-num (tuple-key-number key))
		(contents (dyn-tuple-contents tuple))
		((desc (dyn-tuple-descriptor tuple))
		 ((pairs (Tuple-Desc-Pairs desc))
		  ((nkeys*2 (length pairs))
		   ((pr (dotimes (i nkeys*2 (assert nil))
			  (let ((pr (svref pairs i)))
			    (when (= key-num (logand pr Tuple-Key-Number-Mask))
			      (return pr)))))
		    ((val-idx (ash pr (- Tuple-Key-Number-Size)))
		     ((ichunk (ash val-idx (- Tuple-Value-Chunk-Bits)))
		      (val-idx (logand val-idx (1- Tuple-Value-Chunk-Size)))
		      ((chunk (svref contents ichunk))
		       ((new-chunk (make-array (length chunk))))))))))
		 (new-contents (make-array (length contents)))))
	    (dotimes (i (length chunk))
	      (setf (svref new-chunk i) (svref chunk i)))
	    (setf (svref new-chunk val-idx) val)
	    (dotimes (i (length contents))
	      (setf (svref new-contents i) (svref contents i)))
	    (setf (svref new-contents ichunk) new-chunk)
	    (make-dyn-tuple desc new-contents)))
      (let ((old-desc (dyn-tuple-descriptor tuple)))
	(unless (< (size (Tuple-Desc-Key-Set old-desc))
		   (1- (ash 1 Tuple-Value-Index-Size)))
	  (error "Tuple too long (limit ~D pairs in this implementation)."
		 (ash 1 Tuple-Value-Index-Size)))
	(let ((new-desc new-key-set
		(let ((nd (lookup (Tuple-Desc-Next-Desc-Map old-desc) key)))
		  (if nd (values nd (Tuple-Desc-Key-Set nd))
		    (let ((nks (with (Tuple-Desc-Key-Set old-desc) key))
			  ((nd (progn
				 (read-memory-barrier)
				 (lookup +Tuple-Descriptor-Map+ nks)))))
		      (when nd
			(setf (lookup (Tuple-Desc-Next-Desc-Map old-desc) key) nd))
		      (values nd nks)))))
	      ((nkeys (size new-key-set))
	       ((window-size (Tuple-Window-Size (* nkeys 2)))))
	      (old-pairs (Tuple-Desc-Pairs old-desc)))
	  (unless new-desc
	    ;; Lock out reorderings while we do this.  One might think we also need a
	    ;; lock to protect `+Tuple-Descriptor-Map+', but actually it doesn't hurt
	    ;; anything if we lose an occasional entry -- some tuples will use a
	    ;; descriptor not in the map, but nothing goes wrong as a consequence.
	    (with-lock ((Tuple-Desc-Lock old-desc))
	      (let ((new-pairs (make-array (* 2 nkeys)))
		    (key-num (tuple-key-number key))
		    ;; The new value goes at the end.
		    ((new-pr (logior key-num (ash (1- nkeys) Tuple-Key-Number-Size)))))
		(flet ((add-pair (i pr)
			 (setf (svref new-pairs i) pr)
			 (setf (svref new-pairs (+ i nkeys)) pr)))
		  (setq new-desc (Make-Tuple-Desc new-key-set new-pairs))
		  ;; The new pair goes just outside the window, if the window is full.
		  (dotimes (i (min (1- nkeys) window-size))
		    (add-pair i (svref old-pairs i)))
		  (if (<= nkeys window-size)
		      (add-pair (1- nkeys) new-pr)
		    (progn
		      (add-pair window-size new-pr)
		      (dotimes (i (- nkeys window-size 1))
			(add-pair (+ i window-size 1)
				  (svref old-pairs (+ i window-size)))))))))
	    ;(setf (lookup +Tuple-Descriptor-Map+ new-key-set) new-desc)
	    ;; Technically, we need a memory barrier to make sure the new map value
	    ;; is fully constructed before being made available to other threads.
	    (setq +Tuple-Descriptor-Map+
		  (prog1
		      (with +Tuple-Descriptor-Map+ new-key-set new-desc)
		    (write-memory-barrier)))
	    (setf (lookup (Tuple-Desc-Next-Desc-Map old-desc) key) new-desc))
	  (let ((reorder-map (Tuple-Get-Reorder-Map old-desc new-desc))
		(old-chunks (dyn-tuple-contents tuple))
		(new-chunks (make-array (ceiling nkeys Tuple-Value-Chunk-Size))))
	    (do ((i 0 (1+ i))
		 (n nkeys (- n Tuple-Value-Chunk-Size))
		 (reorder-map reorder-map (cdr reorder-map)))
		((= i (length new-chunks))
		 (make-dyn-tuple new-desc new-chunks))
	      (if (null (car reorder-map))
		  (setf (svref new-chunks i) (svref old-chunks i))
		(let ((chunk-len (min n Tuple-Value-Chunk-Size))
		      ((new-chunk (make-array chunk-len))))
		  (dotimes (i chunk-len)
		    (let ((old-idx (svref (car reorder-map) i)))
		      (setf (svref new-chunk i)
			    (if old-idx
				(svref (svref old-chunks (ash old-idx
							      (- Tuple-Value-Chunk-Bits)))
				       (logand old-idx (1- Tuple-Value-Chunk-Size)))
			      val))))
		  (setf (svref new-chunks i) new-chunk))))))))))

(defun Tuple-Get-Reorder-Map (old-desc new-desc)
  ;; Again, we don't bother with locking -- it doesn't hurt if we occasionally
  ;; lose a reorder map we've just built.
  (or (lookup (Tuple-Desc-Reorder-Map-Map old-desc) new-desc)
      (setf (lookup (Tuple-Desc-Reorder-Map-Map old-desc) new-desc)
	    (Tuple-Make-Reorder-Map old-desc new-desc))))

(defun Tuple-Make-Reorder-Map (old-desc new-desc)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((old-pairs (Tuple-Desc-Pairs old-desc))
	(new-size (size (Tuple-Desc-Key-Set new-desc)))
	(new-pairs (Tuple-Desc-Pairs new-desc))
	((new-nchunks (ceiling new-size Tuple-Value-Chunk-Size)))
	(result nil))
    (dotimes (ichunk new-nchunks)
      (let ((chunk (make-array (min Tuple-Value-Chunk-Size
				    (- new-size (* ichunk Tuple-Value-Chunk-Size)))))
	    (changed? nil))
	(dotimes (i (length chunk))
	  (let ((new-idx (+ (* ichunk Tuple-Value-Chunk-Size) i))
		((new-pr (cl:find new-idx new-pairs
				  :key #'(lambda (pr)
					   (ash pr (- Tuple-Key-Number-Size)))))
		 ((old-pr (cl:find (logand new-pr Tuple-Key-Number-Mask)
				   old-pairs
				   :key #'(lambda (pr)
					    (logand pr Tuple-Key-Number-Mask))))
		  ((old-idx (and old-pr (ash old-pr (- Tuple-Key-Number-Size))))))))
	    (unless (eql old-idx new-idx)
	      (setq changed? t))
	    (setf (svref chunk i) old-idx)))
	(push (and changed? chunk) result)))
    (nreverse result)))


(defgeneric internal-do-tuple (tuple elt-fn value-fn)
  (:documentation
    "Calls `elt-fn' on successive pairs of the tuple (as two arguments); when done,
calls `value-fn' on no arguments and returns the result(s).  This is called by
`do-tuple' to provide for the possibility of different tuple implementations;
it is not for public use.  `elt-fn' and `value-fn' must be function objects,
not symbols."))

(defmacro do-tuple ((key-var value-var tuple &optional value) &body body)
  "For each pair of `tuple', binds `key-var' and `value-var' and executes `body'.
When done, returns `value'."
  `(block nil
     (internal-do-tuple ,tuple
			#'(lambda (,key-var ,value-var) . ,body)
			#'(lambda () ,value))))

(defmacro Do-Tuple-Internal ((key-var value-var tuple-form &optional value-form)
			     &body body)
  (let ((tuple-var (gensym "TUPLE-"))
	(desc-var (gensym "DESC-"))
	(contents-var (gensym "CONTENTS-"))
	(pairs-var (gensym "PAIRS-"))
	(idx-var (gensym "IDX-"))
	(pr-var (gensym "PR-"))
	(val-idx-var (gensym "VAL-IDX-")))
    `(let ((,tuple-var ,tuple-form))
       (let ((,contents-var (dyn-tuple-contents ,tuple-var))
	     (,desc-var (dyn-tuple-descriptor ,tuple-var))
	     ((,pairs-var (Tuple-Desc-Pairs ,desc-var))))
	 (dotimes (,idx-var (the fixnum (size (Tuple-Desc-Key-Set ,desc-var))))
	   (declare (fixnum ,idx-var))
	   (let ((,pr-var (the fixnum (svref ,pairs-var ,idx-var)))
		 ((,val-idx-var (ash ,pr-var (- Tuple-Key-Number-Size)))))
	     (let ((,key-var (lookup +Tuple-Key-Seq+
				     (logand ,pr-var Tuple-Key-Number-Mask)))
		   (,value-var (svref (svref ,contents-var
					     (ash ,val-idx-var
						  (- Tuple-Value-Chunk-Bits)))
				      (logand ,val-idx-var
					      (1- Tuple-Value-Chunk-Size)))))
	       . ,body)))
	 ,value-form))))

(defmethod internal-do-tuple ((tup tuple) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (Do-Tuple-Internal (x y tup (funcall value-fn))
    (funcall elt-fn x y)))

(defun print-dyn-tuple (tuple stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#~<")
    (do-tuple (key val tuple)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write (list (tuple-key-name key) val) :stream stream))
    (format stream ">")))

(defmethod compare ((tup1 tuple) (tup2 tuple))
  (let ((key-set-1 (Tuple-Desc-Key-Set (dyn-tuple-descriptor tup1)))
	(key-set-2 (Tuple-Desc-Key-Set (dyn-tuple-descriptor tup2)))
	((res (compare key-set-1 key-set-2)))
	(default ':equal))
    (if (not (eq res ':equal))
	res
      (do-set (key key-set-1 default)
	(let ((val1? val1 (Tuple-Lookup tup1 key t))
	      (val2? val2 (Tuple-Lookup tup2 key t))
	      ((res (compare val1 val2))))
	  (declare (ignore val1? val2?))
	  (when (or (eq res ':less) (eq res ':greater))
	    (return res))
	  (when (eq res ':unequal)
	    (setq default ':unequal)))))))


(defmethod with ((tuple tuple) (key tuple-key) &optional (value nil value?))
  (check-three-arguments value? 'with 'tuple)
  (Tuple-With tuple key value))

(defmethod lookup ((tuple tuple) (key tuple-key))
  (let ((val? val (Tuple-Lookup tuple key)))
    (if val? (values val t)
      (let ((default-fn (tuple-key-default-fn key)))
	(values (and default-fn (funcall default-fn tuple)) nil)))))

(defgeneric tuple-merge (tuple1 tuple2 &optional val-fn)
  (:documentation "Returns a new tuple containing all the keys of `tuple1' and `tuple2',
where the value for each key contained in only one tuple is the value from
that tuple, and the value for each key contained in both tuples is the result
of calling `val-fn' on the value from `tuple1' and the value from `tuple2'.
`val-fn' defaults to simply returning its third argument, so the entries in
`tuple2' simply shadow those in `tuple1'."))

(defmethod tuple-merge ((tup1 tuple) (tup2 tuple)
			&optional (val-fn #'(lambda (v1 v2)
					      (declare (ignore v1))
					      v2)))
  ;;; Someday: better implementation.
  (let ((result tup1)
	(val-fn (coerce val-fn 'function)))
    (do-tuple (k v2 tup2)
      (let ((v1? v1 (Tuple-Lookup tup1 k)))
	(setq result (with result k (if v1? (funcall val-fn v1 v2) v2)))))
    result))

(defmethod convert ((to-type (eql 'map)) (tup tuple) &key)
  (let ((m (empty-map)))
    (do-tuple (k v tup)
      (setq m (with m k v)))
    m))

(defmethod convert ((to-type (eql 'list)) (tup tuple) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-tuple (k v tup)
      (push (funcall pair-fn k v) result))
    (nreverse result)))


;;; ================================================================================

(defmethod image ((key tuple-key) (s set))
  (set-image #'(lambda (x) (lookup x key)) s))

(defmethod image ((key tuple-key) (s seq))
  (seq-image #'(lambda (x) (lookup x key)) s))

;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: tuples.lisp
;;; Contents: Dynamic tuples implementation.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
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

(in-package :fset)


(defstruct (dyn-tuple
	    (:include tuple)
	    (:constructor make-dyn-tuple (descriptor contents hash-value))
	    (:predicate dyn-tuple?)
	    (:print-function print-dyn-tuple)
	    (:copier nil))
  "A class of functional tuples represented as vectors with dynamically-
reordered key vectors.  This is the default implementation of tuples in FSet."
  ;; A `Tuple-Desc'.
  (descriptor nil :read-only t)
  ;; A vector of value chunks (vectors) (all these vectors being simple).
  (contents nil :read-only t)
  ;; If nonnull, the cumulative `hash-value' of the keys and values.
  (hash-value nil :type (or null fixnum)))

(define-cross-type-compare-methods dyn-tuple)


(defstruct (tuple-key
	    (:constructor make-tuple-key (name default number &optional type type-check-fn))
	    (:predicate tuple-key?)
	    (:print-function print-tuple-key))
  (name nil :read-only t)	; a symbol (normally, but actually can be any type known to FSet)
  (default 'no-default)		; default, if any, for tuples with no explicit entry for this key
				;   (in FSet1, this was a function called on the tuple)
  (number nil :type fixnum
	      :read-only t)	; used for lookup and sorting
  (type 't)
  (type-check-fn nil :type (or null function)))

(deflex +tuple-key-name-map+ (empty-ch-map))

(deflex +tuple-key-seq+ (empty-seq))

(deflex +tuple-key-lock+ (make-lock "Tuple Key Lock"))

(define-condition tuple-default-type-error (type-error)
    ((key-name :initarg :key-name :reader tuple-default-type-error-key-name))
  (:report (lambda (err stream)
	     (format stream "The value~%  ~S~%is not of type~%  ~S~%when setting default for tuple key ~S"
		     (type-error-datum err) (type-error-expected-type err)
		     (tuple-default-type-error-key-name err)))))

(defun get-tuple-key (name &optional default)
  "Finds or creates a tuple key named `name'.  If `default' is supplied and
nonnull, it will be returned from `lookup' when the tuple has no explicit pair
with this key.  EXCEPTION: for backward compatibility, if `default' is a
function, then instead of being returned itself, it will be called on the tuple
to get the value returned.  So, if you want the default value to be a function,
you have to wrap it in an extra lambda."
  (let ((default-fn (and default
			 (if (functionp default) default
			   (lambda (tup)
			     (declare (ignore tup))
			     default)))))
    (with-lock (+tuple-key-lock+)
      (let ((key (lookup +tuple-key-name-map+ name))
	    (key-idx (size +tuple-key-seq+)))
	(or key
	    (if (<= key-idx tuple-key-number-mask)
		(let ((key (make-tuple-key name default-fn key-idx)))
		  (setf (lookup +tuple-key-name-map+ name) key)
		  (push-last +tuple-key-seq+ key)
		  key)
	      (error "Tuple key space exhausted")))))))
(defun fset2:get-tuple-key (name &key (type 't) (default nil default?) no-default?)
  "Finds or creates a tuple key named `name'.  If `default' is supplied, sets
the key's default to it; this value will be returned from `lookup' when the
tuple has no explicit pair with this key.  If `no-default?' is true, clears the
key's default, so that `lookup' will signal an error in this case.  If neither
is specified, then if the key is being freshly created, it will have a default
of `nil'; if it already existed, its default will be unchanged."
  (when (and default? no-default?)
    (error "Both a default and `no-default?' specified"))
  (unless (or no-default? (eq type 't) (typep default type))
    (error 'tuple-default-type-error :datum default :expected-type type :key-name name))
  (with-lock (+tuple-key-lock+)
    (let ((key (lookup +tuple-key-name-map+ name))
	  (key-idx (size +tuple-key-seq+))
	  (type-check-fn (and (not (eq type 't))
			      (compile nil `(lambda (x) (typep x ',type))))))
      (cond (key
	     (cond (default?
		    (setf (tuple-key-default key) default))
		   (no-default?
		    (setf (tuple-key-default key) 'no-default)))
	     (setf (tuple-key-type key) type)
	     (setf (tuple-key-type-check-fn key) type-check-fn)
	     key)
	    ((<= key-idx tuple-key-number-mask)
	     (let ((key (make-tuple-key name (if default? default (and no-default? 'no-default))
					key-idx type type-check-fn)))
	       (setf (lookup +tuple-key-name-map+ name) key)
	       (push-last +tuple-key-seq+ key)
	       key))
	    (t
	     (error "Tuple key space exhausted"))))))

(defmacro def-tuple-key (name &optional default)
  "Deprecated; use `define-tuple-key'."
  ;; What this should have been called to begin with.
  `(define-tuple-key ,name ,default))

(defmacro define-tuple-key (name &optional default doc-string)
  "Defines a tuple key named `name' as a global lexical variable (see
`deflex').  If `default' is supplied and nonnull, it will be returned from
`lookup' when the tuple has no explicit pair with this key.  EXCEPTION: for
backward compatibility, if `default' is a function, then instead of being
returned itself, it will be called on the tuple to get the value returned.
So, if you want the default value to be a function, you have to wrap it in
an extra lambda.  To supply a doc string without a default, supply `nil'
for `default'."
  (assert (symbolp name))
  `(deflex ,name (get-tuple-key ',name ,default)
     . ,(and doc-string `(,doc-string))))
(defmacro fset2:define-tuple-key (name &key (type 't) (default nil default?) no-default? documentation)
  "Defines a tuple key named `name' as a global lexical variable (see
`deflex').  If `default' is supplied, it will be returned from `lookup',
instead of `nil', when the tuple has no explicit pair with this key.
Alternatively, if `no-default?' is true, `lookup` will signal an error
in that case.  If `type' is supplied, `with' operations using this key
will check that the supplied value is of that type; `default' is also
checked."
  (assert (symbolp name))
  (when (and default? no-default?)
    (error "Both a default and `no-default?' specified"))
  `(deflex-reinit ,name (fset2:get-tuple-key ',name :type ',type
					     ,@(if default? `(:default ,default)
						 (and no-default? '(:no-default? t))))
     . ,(and documentation `(,documentation))))

(defun print-tuple-key (key stream level)
  (declare (ignore level))
  (format stream "#<Key ~S>" (tuple-key-name key)))

(declaim (inline key-compare))
(defun key-compare (key1 key2)
  (let ((n1 (tuple-key-number key1))
	(n2 (tuple-key-number key2)))
    (cond ((< n1 n2) ':less)
	  ((> n1 n2) ':greater)
	  (t ':equal))))
(defmethod compare ((key1 tuple-key) (key2 tuple-key))
  (key-compare key1 key2))

(declaim (inline key-hash-value))
(defun key-hash-value (key)
  (1+ (tuple-key-number key)))

(defmethod hash-value ((key tuple-key))
  (key-hash-value key))

(define-hash-function key-compare key-hash-value)


(defconstant tuple-value-chunk-bits 4)
;;; Each value chunk (vector) contains no more than this many elements.
(defconstant tuple-value-chunk-size (ash 1 tuple-value-chunk-bits))

(defstruct (tuple-desc
	     (:constructor make-tuple-desc-internal (key-set pairs lock serial-number)))
  ;; The set (as an FSet ch-set) of `tuple-key's for which tuples using this index
  ;; contain values.
  (key-set nil :read-only t)
  ;; The pair vector, which contains two copies of the pair sequence, each pair
  ;; represented as a fixnum: the key number is in the low-order `Tuple-Key-Number-
  ;; Size' bits, and the value index is in the next `Tuple-Value-Index-Size' bits.
  (pairs (vector) :type simple-vector :read-only t)
  ;; The reorder lock.
  (lock nil :read-only t)
  ;; The reorder map map: maps other tuple-descs to reorder maps for them.  Each
  ;; reorder map is a list with one element for each chunk in the target; that
  ;; element is `nil' if the chunk is unchanged, else it is a vector of indices
  ;; into the source tuple (where an index of `nil' indicates an insertion).  (mutable)
  (reorder-map-map (empty-ch-map nil 'tuple-desc-compare 'eql-compare))
  ;; Cache used by `Tuple-With': for each key not in this descriptor but that has been
  ;; added to tuples with this descriptor, the descriptor for the new tuple.  (mutable)
  (next-desc-map (empty-ch-map nil 'key-compare 'eql-compare))
  ;; Serial number (used for `Reorder-Map-Map').
  (serial-number 0 :type fixnum :read-only t))

(deflex +tuple-desc-next-serial-number+ 0)

(deflex +tuple-desc-next-serial-number-lock+ (make-lock))

(defun make-tuple-desc (key-set pairs)
  (make-tuple-desc-internal key-set pairs (make-lock)
			    (with-lock (+tuple-desc-next-serial-number-lock+)
			      (prog1 +tuple-desc-next-serial-number+
				(incf +tuple-desc-next-serial-number+)))))

(deflex +tuple-descriptor-map+ (empty-ch-map))

(declaim (inline tuple-desc-compare))
(defun tuple-desc-compare (x y)
  (if (eq x y) ':equal
    (let ((xser (tuple-desc-serial-number x))
	  (yser (tuple-desc-serial-number y)))
      (cond ((< xser yser) ':less)
	    ((> xser yser) ':greater)
	    (t ':equal)))))
(defmethod compare ((x tuple-desc) (y tuple-desc))
  (tuple-desc-compare x y))

(declaim (inline tuple-desc-hash-value))
(defun tuple-desc-hash-value (td)
  (tuple-desc-serial-number td))
(defmethod hash-value ((td tuple-desc))
  (tuple-desc-hash-value td))

(define-hash-function tuple-desc-compare tuple-desc-hash-value)

;;; Urgh, Allegro doesn't obey inline declarations, so we use a macro for this.
;;; This takes its argument doubled for the convenience of the common case (lookup).
;;; &&& These numbers are SWAGs and may be too small.  See `Tuple-Reorder-Keys'.
(defmacro tuple-window-size (nkeys*2)
  (let ((nkeys*2-var (gensymx #:nkeys*2-)))
    `(let ((,nkeys*2-var ,nkeys*2))
       (cond ((< ,nkeys*2-var 16) 3)
	     ((< ,nkeys*2-var 32) 4)
	     ((< ,nkeys*2-var 48) 5)
	     (t 6)))))

(defmethod domain ((tup dyn-tuple))
  (tuple-desc-key-set (dyn-tuple-descriptor tup)))

(defparameter tuple-reorder-score-threshold 15		; SWAG
  "The reorder score that triggers a major reordering.")


(defun empty-tuple ()
  "Returns an empty tuple of the default implementation."
  (empty-dyn-tuple))

(defun empty-dyn-tuple ()
  "Returns an empty dyn-tuple."
  (let ((desc (lookup +tuple-descriptor-map+ (empty-ch-set))))
    (unless desc
      (setq desc (make-tuple-desc (empty-ch-set) (vector)))
      (setf (lookup +tuple-descriptor-map+ (empty-ch-set)) desc))
  (make-dyn-tuple desc (vector) nil)))

(deflex +tuple-random-value+ 0
  "State for an extremely fast, low-quality generator of small numbers of
pseudorandom bits.  Yep, this is about as quick-and-dirty as it gets --
we just increment this value by some small prime like 5 each time.  We
don't worry about locking it, either.")

(declaim (inline tuple-random-value))
(defun tuple-random-value ()
  (the fixnum
    (setf +tuple-random-value+
	  (logand (+ (the fixnum +tuple-random-value+) 5)
		  most-positive-fixnum))))

(defconstant tuple-reorder-frequency 31
  "Controls how often we do tuple reordering.  Must be 2^n - 1 for some n.")

(defmacro tuple-contents-ref (size contents idx)
  (let ((contents-var (gensymx #:contents-))
	(idx-var (gensymx #:idx-)))
    `(let ((,contents-var ,contents)
	   (,idx-var ,idx))
       (declare (fixnum ,idx-var))
       (if (<= ,size tuple-value-chunk-size)
	   (svref ,contents-var ,idx-var)
	 (svref (svref ,contents-var (ash ,idx-var (- tuple-value-chunk-bits)))
		(logand ,idx-var (1- tuple-value-chunk-size)))))))

(defun tuple-lookup (tuple key &optional no-reorder?)
  (declare (optimize (speed 3) (safety 0)))
  (let ((desc (dyn-tuple-descriptor tuple))
	((pairs (tuple-desc-pairs desc))
	 ((nkeys*2 (length pairs))))
	(key-num (if (typep key 'fixnum) key	; for internal use only
		   (tuple-key-number key))))
    (declare (fixnum nkeys*2 key-num)
	     (simple-vector pairs))
    ;; As explained above, we don't have to lock here because there are two copies
    ;; of the pair sequence, only the first of which gets reordered.  So if we
    ;; happen to miss the pair we're looking for in the first copy, because it is
    ;; momentarily not there, we'll find it in the second copy.
    (dotimes (i nkeys*2)
      (declare (fixnum i))
      (let ((pr (svref pairs i)))
	(declare (fixnum pr))
	(when (= (logand pr tuple-key-number-mask)
		 key-num)
	  (let ((chunks (dyn-tuple-contents tuple))
		(val-idx (ash pr (- tuple-key-number-size))))
	    (let ((val (tuple-contents-ref (ash nkeys*2 -1) chunks val-idx)))
	      (unless (or no-reorder? (< i (tuple-window-size nkeys*2))
			  ;;; Reorder only on some fraction of the occasions
			  ;;; we come through here.
			  (not (= 0 (logand (tuple-random-value) tuple-reorder-frequency))))
		(tuple-reorder-keys tuple i))
	      (return-from tuple-lookup (values t val)))))))
    (values nil nil)))

(defun tuple-reorder-keys (tuple idx)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum idx))
  (let ((desc (dyn-tuple-descriptor tuple))
	((pairs (tuple-desc-pairs desc))))
    (declare (simple-vector pairs))
    ;; Some implementations can't do `:wait? nil', but that's okay -- we'll just
    ;; do a little redundant work.
    (with-lock ((tuple-desc-lock desc) :wait? nil)
      (let ((nkeys*2 (length pairs))
	    ((window-size (tuple-window-size nkeys*2))))
	(declare (fixnum nkeys*2))
	;; If it's at least 1 slot past the end of the window, just move it down 1.
	;; If it's in the slot just after the window, swap it with a (pseudo-)randomly-selected
	;; occupant of the window.
	(let ((to-idx (if (>= (1- idx) window-size)
			  (1- idx)
			(mod (tuple-random-value) window-size)))
	      ((to-key-pr (svref pairs to-idx)))
	      (key-pr (svref pairs idx)))
	  (declare (fixnum key-pr to-key-pr))
	  (setf (svref pairs idx) to-key-pr)
	  (setf (svref pairs to-idx) key-pr))))))

(define-condition tuple-value-type-error (type-error)
    ((key :initarg :key :reader tuple-value-type-error-key))
  (:report (lambda (err stream)
	     (format stream "The value~%  ~S~%is not of type~%  ~S~%when assigning tuple key ~S"
		     (type-error-datum err) (type-error-expected-type err)
		     (tuple-value-type-error-key err)))))

;;; Someday: multiple key/value pair update.
(defun tuple-with (tuple key val)
  (let ((type-check-fn (tuple-key-type-check-fn key)))
    (unless (or (null type-check-fn) (funcall type-check-fn val))
      (error 'tuple-value-type-error :datum val :expected-type (tuple-key-type key) :key key)))
  (let ((old-val? old-val (tuple-lookup tuple key)))
    (declare (optimize (speed 3) (safety 0)))  ; moved here to quiet note on `(typep val type)'
    (if old-val?
	(if (equal? val old-val)
	    tuple
	  ;; Present in tuple already -- key set doesn't change.
	  ;; The lookup may have reordered the tuple.
	  (let ((key-num (tuple-key-number key))
		(contents (dyn-tuple-contents tuple))
		((desc (dyn-tuple-descriptor tuple))
		 ((pairs (tuple-desc-pairs desc))
		  ((nkeys*2 (length pairs))
		   ((single-chunk? (<= (ash nkeys*2 -1) tuple-value-chunk-size))
		    (pr (dotimes (i nkeys*2 (assert nil))
			  (let ((pr (svref pairs i)))
			    (declare (fixnum pr))
			    (when (= key-num (logand pr tuple-key-number-mask))
			      (return pr)))))
		    ((val-idx (ash pr (- tuple-key-number-size)))
		     ((ichunk (ash val-idx (- tuple-value-chunk-bits)))
		      (val-idx (logand val-idx (1- tuple-value-chunk-size)))
		      ((chunk (if single-chunk? contents
				(svref contents ichunk)))
		       ((new-chunk (make-array (length chunk))))))))))
		 (new-contents (make-array (length contents)))))
	    (declare (fixnum key-num)
		     (simple-vector pairs contents chunk))
	    (dotimes (i (length chunk))
	      (setf (svref new-chunk i) (svref chunk i)))
	    (let ((prev-hash (dyn-tuple-hash-value tuple))
		  ((new-hash (and prev-hash
				  (let ((prev-val (svref new-chunk val-idx)))
				    (hash-mix prev-hash
					      (hash-unmix (hash-value-fixnum val)
							  (hash-value-fixnum prev-val))))))))
	      (setf (svref new-chunk val-idx) val)
	      (if single-chunk? (make-dyn-tuple desc new-chunk new-hash)
		(progn
		  (dotimes (i (length contents))
		    (setf (svref new-contents i) (svref contents i)))
		  (setf (svref new-contents ichunk) new-chunk)
		  (make-dyn-tuple desc new-contents new-hash))))))
      (let ((old-desc (dyn-tuple-descriptor tuple)))
	(unless (< (the fixnum (size (tuple-desc-key-set old-desc)))
		   (1- (ash 1 tuple-value-index-size)))
	  (error "Tuple too long (limit ~D pairs in this implementation)."
		 (ash 1 tuple-value-index-size)))
	(let ((new-desc new-key-set
		(let ((nd (lookup (tuple-desc-next-desc-map old-desc) key)))
		  (if nd (values nd (tuple-desc-key-set nd))
		    (let ((nks (with (tuple-desc-key-set old-desc) key))
			  ;; The read memory barrier pairs with the write memory barrier below;
			  ;; it ensures that the reads done by the `lookup' see any relevant writes.
			  ((nd (lookup (prog1 +tuple-descriptor-map+
					 (read-memory-barrier))
				       nks))))
		      (when nd
			(setf (lookup (tuple-desc-next-desc-map old-desc) key) nd))
		      (values nd nks)))))
	      ((nkeys (size new-key-set))
	       ((window-size (tuple-window-size (* nkeys 2)))))
	      (old-pairs (tuple-desc-pairs old-desc)))
	  (declare (type (unsigned-byte #.tuple-value-index-size) nkeys))
	  (unless new-desc
	    ;; Lock out reorderings while we do this.  One might think we also need a
	    ;; lock to protect `+Tuple-Descriptor-Map+', but actually it doesn't hurt
	    ;; anything if we lose an occasional entry -- some tuples will use a
	    ;; descriptor not in the map, but nothing goes wrong as a consequence.
	    (with-lock ((tuple-desc-lock old-desc))
	      (let ((new-pairs (make-array (* 2 nkeys)))
		    (key-num (tuple-key-number key))
		    ;; The new value goes at the end.
		    ((new-pr (logior key-num (ash (1- nkeys) tuple-key-number-size)))))
		(declare (fixnum key-num))
		(setq new-desc (make-tuple-desc new-key-set new-pairs))
		;; The new pair goes just outside the window, if the window is full.
		(dotimes (i (min (1- nkeys) window-size))
		  (setf (svref new-pairs i) (svref old-pairs i)))
		(if (<= nkeys window-size)
		    (setf (svref new-pairs (1- nkeys)) new-pr)
		  (progn
		    (setf (svref new-pairs window-size) new-pr)
		    (dotimes (i (- nkeys window-size 1))
		      (setf (svref new-pairs (+ i window-size 1))
			    (svref old-pairs (+ i window-size))))))
		;; We go to some trouble here to keep the second copy sorted in key order.
		;; This makes tuple comparison much faster.
		(do* ((i (1- nkeys) (1+ i)) ; index into `old-pairs'
		      (pr 0)
		      (lim (* 2 (1- nkeys))))
		     ((or (= i lim)
			  (progn
			    (setq pr (svref old-pairs i))
			    (> (logand pr tuple-key-number-mask) key-num)))
		      (setf (svref new-pairs (1+ i)) new-pr)
		      (do ((j i (1+ j)))
			  ((= j lim))
			(declare (fixnum j))
			(setf (svref new-pairs (+ j 2)) (svref old-pairs j))))
		  (declare (fixnum i pr lim))
		  (setf (svref new-pairs (1+ i)) pr))))
	    ;; Technically, we need a memory barrier to make sure the new map value
	    ;; is fully constructed before being made available to other threads.
	    (setq +tuple-descriptor-map+
		  (prog1
		      (with +tuple-descriptor-map+ new-key-set new-desc)
		    (write-memory-barrier)))
	    (setf (lookup (tuple-desc-next-desc-map old-desc) key) new-desc))
	  (let ((reorder-map (tuple-get-reorder-map old-desc new-desc))
		(old-chunks (dyn-tuple-contents tuple)))
	    (do ((i 0 (1+ i))
		 (n nkeys (- n tuple-value-chunk-size))
		 (reorder-map reorder-map (cdr reorder-map))
		 (new-chunks nil))
		((<= n 0)
		 (let ((prev-hash (dyn-tuple-hash-value tuple))
		       ((new-hash (and prev-hash (hash-mix prev-hash (key-hash-value key)
							   (hash-value-fixnum val))))))
		   (if (cdr new-chunks)
		       (make-dyn-tuple new-desc (coerce (nreverse new-chunks) 'vector) new-hash)
		     (make-dyn-tuple new-desc (car new-chunks) new-hash))))
	      (declare (fixnum i n))
	      (if (null (car reorder-map))
		  (push (if (<= (1- nkeys) tuple-value-chunk-size) old-chunks
			  (svref old-chunks i))
			new-chunks)
		(let ((chunk-len (min n tuple-value-chunk-size))
		      ((new-chunk (make-array chunk-len))))
		  (dotimes (i chunk-len)
		    (let ((old-idx (svref (car reorder-map) i)))
		      (setf (svref new-chunk i)
			    (if old-idx (tuple-contents-ref (1- nkeys) old-chunks old-idx)
			      val))))
		  (push new-chunk new-chunks))))))))))

(defun tuple-get-reorder-map (old-desc new-desc)
  ;; Again, we don't bother with locking -- it doesn't hurt if we occasionally
  ;; lose a reorder map we've just built.
  (or (lookup (tuple-desc-reorder-map-map old-desc) new-desc)
      (setf (lookup (tuple-desc-reorder-map-map old-desc) new-desc)
	    (tuple-make-reorder-map old-desc new-desc))))

(defun tuple-make-reorder-map (old-desc new-desc)
  ;(declare (optimize (speed 3) (safety 0)))
  (let ((old-pairs (tuple-desc-pairs old-desc))
	(new-size (size (tuple-desc-key-set new-desc)))
	(new-pairs (tuple-desc-pairs new-desc))
	((new-nchunks (ceiling new-size tuple-value-chunk-size)))
	(result nil))
    (dotimes (ichunk new-nchunks)
      (let ((chunk (make-array (min tuple-value-chunk-size
				    (- new-size (* ichunk tuple-value-chunk-size)))))
	    (changed? nil))
	(dotimes (i (length chunk))
	  (let ((new-idx (+ (* ichunk tuple-value-chunk-size) i))
		((new-pr (cl:find new-idx new-pairs
				  :key #'(lambda (pr)
					   (ash pr (- tuple-key-number-size)))))
		 ((old-pr (cl:find (logand new-pr tuple-key-number-mask)
				   old-pairs
				   :key #'(lambda (pr)
					    (logand pr tuple-key-number-mask))))
		  ((old-idx (and old-pr (ash old-pr (- tuple-key-number-size))))))))
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

(defmacro do-dyn-tuple-pairs ((key-var value-var tuple-form &optional value-form)
			      &body body)
  (let ((tuple-var (gensymx #:tuple-))
	(desc-var (gensymx #:desc-))
	(contents-var (gensymx #:contents-))
	(size-var (gensymx #:size-))
	(pairs-var (gensymx #:pairs-))
	(idx-var (gensymx #:idx-))
	(pr-var (gensymx #:pr-))
	(val-idx-var (gensymx #:val-idx-)))
    `(let ((,tuple-var ,tuple-form))
       (let ((,contents-var (dyn-tuple-contents ,tuple-var))
	     (,desc-var (dyn-tuple-descriptor ,tuple-var))
	     ((,size-var (size (tuple-desc-key-set ,desc-var)))
	      (,pairs-var (tuple-desc-pairs ,desc-var))))
	 (declare (fixnum ,size-var))
	 (dotimes (,idx-var ,size-var)
	   (declare (fixnum ,idx-var))
	   ;; We use the second copy of the pairs, which won't change underneath us.
	   (let ((,pr-var (the fixnum (svref ,pairs-var (+ ,idx-var ,size-var))))
		 ((,val-idx-var (ash ,pr-var (- tuple-key-number-size)))
		  ((,key-var (lookup +tuple-key-seq+
				     (logand ,pr-var tuple-key-number-mask)))
		   (,value-var (tuple-contents-ref ,size-var ,contents-var ,val-idx-var)))))
	     . ,body))
	 ,value-form))))

(defmethod internal-do-tuple ((tup dyn-tuple) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-dyn-tuple-pairs (x y tup (funcall value-fn))
    (funcall elt-fn x y)))

(defun print-dyn-tuple (tuple stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#~<")
    (do-tuple (key val tuple)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline ':fill stream)
      (write (list (tuple-key-name key) val) :stream stream))
    (format stream " >")))

(defmethod compare ((tup1 dyn-tuple) (tup2 dyn-tuple))
  ;; The ordering this imposes is not easily described, but is stable within a session.
  ;; If the key sets are equal, the values are compared in key order, which is the order
  ;; in which the keys were created in the session.  If the key sets aren't equal, we just
  ;; use the result of comparing them.
  (let ((desc1 (dyn-tuple-descriptor tup1))
	(desc2 (dyn-tuple-descriptor tup2))
	((key-set-1 (tuple-desc-key-set desc1))
	 (key-set-2 (tuple-desc-key-set desc2))
	 ((res (if (eq key-set-1 key-set-2) ':equal
		 (compare key-set-1 key-set-2))))))
    (if (or (eq res ':less) (eq res ':greater))
	res
      (let ((pairs (tuple-desc-pairs desc1))
	    (contents1 (dyn-tuple-contents tup1))
	    (contents2 (dyn-tuple-contents tup2))
	    (nkeys (size key-set-1))
	    (default res)) ; note case where res is ':unequal
	(declare (fixnum nkeys))
	(dotimes (i nkeys default)
	  ;; We use the second, sorted copy of the key-index pairs.
	  (let ((idx (ash (svref pairs (+ i nkeys)) (- tuple-key-number-size)))
		((val1 (tuple-contents-ref nkeys contents1 idx))
		 (val2 (tuple-contents-ref nkeys contents2 idx))
		 ((res (compare val1 val2)))))
	    (when (or (eq res ':less) (eq res ':greater))
	      (return res))
	    (when (eq res ':unequal)
	      (setq default ':unequal))))))))

(defmethod hash-value ((tup dyn-tuple))
  (or (dyn-tuple-hash-value tup)
      (let ((hash 0)
	    (mult 1))
	(do-dyn-tuple-pairs (k v tup)
	  (hash-mixf hash (hash-multiply mult (hash-mix (key-hash-value k) (hash-value-fixnum v))))
	  (setf mult (hash-multiply mult 13)))
	(setf (dyn-tuple-hash-value tup) hash))))


(defmethod with ((tuple dyn-tuple) key &optional (value nil value?))
  (check-three-arguments value? 'with 'tuple)
  (check-type key tuple-key)
  (tuple-with tuple key value))

(define-condition fset2:tuple-key-unbound-error (fset2:lookup-error)
    ((tuple :initarg :tuple :reader fset2:tuple-key-unbound-error-tuple)
     (key :initarg :key :reader fset2:tuple-key-unbound-error-key))
  (:report (lambda (tkue stream)
	     (format stream "Key ~S, which has no default, unbound in tuple ~A"
		     (tuple-key-name (fset2:tuple-key-unbound-error-key tkue))
		     (fset2:tuple-key-unbound-error-tuple tkue)))))

(defmethod lookup ((tuple dyn-tuple) (key tuple-key))
  (let ((val? val (tuple-lookup tuple key)))
    (if val? (values val t)
      (let ((default-fn (tuple-key-default key)))
	;; The key might have been declared in FSet 2 code, even though we're accesing it in FSet 1 code.
	(if (eq default-fn 'no-default)
	    (error 'fset2:tuple-key-unbound-error :tuple tuple :key key)
	  (values (and default-fn (funcall default-fn tuple)) nil))))))
(defmethod fset2:lookup ((tuple dyn-tuple) (key tuple-key))
  (let ((val? val (tuple-lookup tuple key)))
    (if val? (values val t)
      (let ((dflt (tuple-key-default key)))
	(if (eq dflt 'no-default)
	    (error 'fset2:tuple-key-unbound-error :tuple tuple :key key)
	  (values dflt nil))))))

(defmethod size ((tuple dyn-tuple))
  (size (tuple-desc-key-set (dyn-tuple-descriptor tuple))))

(defmethod at-rank ((tup dyn-tuple) rank)
  (let ((desc (dyn-tuple-descriptor tup))
	((size (size (tuple-desc-key-set desc)))
	 (pairs (tuple-desc-pairs desc)))
	(contents (dyn-tuple-contents tup)))
    (unless (and (>= rank 0) (< rank size))
      (error 'simple-type-error :datum rank :expected-type `(integer 0 (,size))
	     :format-control "Rank ~D out of bounds on ~A"
	     :format-arguments (list rank tup)))
    ;; We use the second copy of the pairs, which won't change underneath us.
    (let ((pr (svref pairs (+ rank size))))
      (values (lookup +tuple-key-seq+ (logand pr tuple-key-number-mask))
	      (tuple-contents-ref size contents (ash pr (- tuple-key-number-size)))))))

(defgeneric tuple-merge (tuple1 tuple2 &optional val-fn)
  (:documentation "Returns a new tuple containing all the keys of `tuple1' and `tuple2',
where the value for each key contained in only one tuple is the value from
that tuple, and the value for each key contained in both tuples is the result
of calling `val-fn' on the value from `tuple1' and the value from `tuple2'.
`val-fn' defaults to simply returning its second argument, so the entries in
`tuple2' simply shadow those in `tuple1'."))

(defmethod tuple-merge ((tup1 dyn-tuple) (tup2 dyn-tuple)
			&optional (val-fn (fn (_v1 v2) v2)))
  ;;; Someday: better implementation.
  (let ((result tup1)
	(val-fn (coerce val-fn 'function)))
    (do-tuple (k v2 tup2)
      (let ((v1? v1 (tuple-lookup tup1 k)))
	(setq result (with result k (if v1? (funcall val-fn v1 v2) v2)))))
    result))

(defmethod convert ((to-type (eql 'tuple)) (tup tuple) &key)
  tup)
(defmethod convert ((to-type (eql 'dyn-tuple)) (tup dyn-tuple) &key)
  tup)

(defmethod convert ((to-type (eql 'map)) (tup tuple) &key)
  (wb-map-from-tuple tup))

(defmethod convert ((to-type (eql 'wb-map)) (tup tuple) &key key-compare-fn-name val-compare-fn-name)
  (wb-map-from-tuple tup key-compare-fn-name val-compare-fn-name))
(defmethod convert ((to-type (eql 'fset2:wb-map)) (tup tuple) &key key-compare-fn-name val-compare-fn-name)
  (wb-map-from-tuple tup key-compare-fn-name val-compare-fn-name))

(defun wb-map-from-tuple (tup &optional key-compare-fn-name val-compare-fn-name)
  (let ((tree nil)
	(tmorg (wb-map-org (empty-wb-map nil key-compare-fn-name val-compare-fn-name))))
    (do-tuple (k v tup)
      (setq tree (wb-map-tree-with tree k v (tree-map-org-key-compare-fn tmorg) (tree-map-org-val-compare-fn tmorg))))
    (make-wb-map tree tmorg nil)))

(defmethod convert ((to-type (eql 'fset2:map)) (tup tuple) &key)
  (ch-map-from-tuple tup))

(defmethod convert ((to-type (eql 'ch-map)) (tup tuple) &key key-compare-fn-name val-compare-fn-name)
  (ch-map-from-tuple tup key-compare-fn-name val-compare-fn-name))
(defmethod convert ((to-type (eql 'fset2:ch-map)) (tup tuple) &key key-compare-fn-name val-compare-fn-name)
  (ch-map-from-tuple tup key-compare-fn-name val-compare-fn-name))

(defun ch-map-from-tuple (tup &optional key-compare-fn-name val-compare-fn-name)
  (let ((tree nil)
	(hmorg (ch-map-org (empty-ch-map nil key-compare-fn-name val-compare-fn-name)))
	(transient-id (get-next-transient-id)))
    (do-tuple (k v tup)
      (setq tree (ch-map-tree-with tree k v (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
				   (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg) transient-id)))
    (make-ch-map (ch-map-compact-tree tree) hmorg nil)))

(defmethod convert ((to-type (eql 'tuple)) (m map) &key autokey?)
  (dyn-tuple-from-map m autokey?))

(defmethod convert ((to-type (eql 'dyn-tuple)) (m map) &key autokey?)
  (dyn-tuple-from-map m autokey?))

(defun dyn-tuple-from-map (m autokey?)
  (let ((result (empty-dyn-tuple)))
    (do-map (k v m)
      (setq result (tuple-with result (if (and autokey? (not (tuple-key? k)))
					  (fset2:get-tuple-key k)
					k)
			       v)))
    result))

(defmethod convert ((to-type (eql 'list)) (tup tuple) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-tuple (k v tup)
      (push (funcall pair-fn k v) result))
    (nreverse result)))

(defmethod convert ((to-type (eql 'tuple)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (dyn-tuple-from-list list key-fn value-fn))

(defmethod convert ((to-type (eql 'dyn-tuple)) (list list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (dyn-tuple-from-list list key-fn value-fn))

(defun dyn-tuple-from-list (list key-fn value-fn)
  (let ((tup (empty-dyn-tuple)))
    (dolist (x list)
      (setq tup (tuple-with tup (funcall key-fn x) (funcall value-fn x))))
    tup))

(defmethod make-load-form ((tup dyn-tuple) &optional environment)
  (declare (ignore environment))
  `(convert 'dyn-tuple ',(convert 'list tup)))

(defmethod make-load-form ((key tuple-key) &optional environment)
  (declare (ignore environment))
  `(get-tuple-key ',(tuple-key-name key) ',(tuple-key-default key)))


;;; ================================================================================

(define-wb-set-methods (image fset2:image) ((key tuple-key) (s wb-set) &key compare-fn-name)
  (wb-set-image #'(lambda (x) (lookup x key)) (contents s)
		(or compare-fn-name (compare-fn-name s))))

(defmethod image ((key tuple-key) (s seq) &key)
  (seq-image #'(lambda (x) (lookup x key)) s nil))
(defmethod fset2:image ((key tuple-key) (s seq) &key)
  (seq-image #'(lambda (x) (fset2:lookup x key)) s 'no-default))

(defmethod restrict ((tup dyn-tuple) (s set))
  (let ((result (empty-dyn-tuple)))
    (do-tuple (k v tup)
      (when (contains? s k)
	(setf (@ result k) v)))
    result))

(defmethod restrict-not ((tup dyn-tuple) (s set))
  (let ((result (empty-dyn-tuple)))
    (do-tuple (k v tup)
      (unless (contains? s k)
	(setf (@ result k) v)))
    result))



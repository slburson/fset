(in-package :fset)

;;; &&& Naming: should we treat "transient" as a substantive, giving `transient', `set-transient',
;;; and `ch-set-transient'?

(defstruct (transient-collection
	     (:constructor nil)
	     (:predicate transient-collection?)
	     (:copier nil)
	     (:conc-name #:transient-)
	     (:print-function print-transient-collection))
  "The root class of the FSet transient-collection hierarchy.  It is a
structure class.

Transient collections are mutable objects, and are compared by identity,
not content."
  ;; These two values start out equal, but `id' gets updated by reuse following `make-persistent'.
  ;; `serial-no' is used only by `compare' (transients are compared by identity).
  (serial-no nil :type fixnum :read-only t)
  (id nil :type (or fixnum null))
  (lock nil :read-only t))

(define-atomic-series transient-collection-next-id)

(defun get-next-transient-id ()
  (increment-atomic-series transient-collection-next-id))

(declaim (inline allocate-transient-id-if-needed))
(defun allocate-transient-id-if-needed (tcoll)
  (declare (type transient-collection tcoll))
  (unless (transient-id tcoll)
    (setf (transient-id tcoll) (get-next-transient-id))))

(define-equality-slots transient-collection
  #'transient-serial-no)

(defmethod with ((tcoll transient-collection) value1 &optional value2)
  (declare (ignore value1 value2))
  (error "`with' is not supported on a transient collection.  You may be seeing this~@
	  because you called `with' directly, or because you used the `includef'~@
	  modify macro, or because, on a map, you did \(setf \(@ map ...\) ...\).~@
	  On a transient set or relation, use `include!'; on a transient map, use~@
	  `include!' or \(setf \(lookup! map ...\) ...\)."))

(defmethod less ((tcoll transient-collection) value1 &optional value2)
  (declare (ignore value1 value2))
  (error "`less' is not supported on a transient collection.  You may be seeing this~@
	  because you called `less' directly, or because you used the `excludef'~@
	  modify macro.  On a transient collection, use `exclude!'."))

(defun print-transient-collection (coll stream level)
  (declare (ignore level))
  (print-unreadable-object (coll stream :type t)
    (format stream "~D" (transient-serial-no coll))))

(defstruct (transient-set
	     (:include transient-collection)
	     (:constructor nil)
	     (:predicate transient-set?)
	     (:copier nil))
  "The abstract class for FSet transient sets.  It is a structure class.")

(defstruct (transient-map
	     (:include transient-collection)
	     (:constructor nil)
	     (:predicate transient-map?)
	     (:copier nil))
  "The abstract class for FSet transient maps.  It is a structure class."
  (default nil))

(defmethod default! ((tm transient-map))
  (let ((dflt (transient-map-default tm)))
    (if (eq dflt 'no-default) (values nil nil)
      (values dflt t))))

(defmethod (setf default!) (new-default (tm transient-map))
  (setf (transient-map-default tm) new-default))

(defmethod clear-default! ((tm transient-map))
  (setf (transient-map-default tm) 'no-default))

(defstruct (transient-relation
	    (:include transient-collection)
	    (:constructor nil)
	    (:predicate transient-relation?)
	    (:copier nil))
  "The abstract class for FSet transient relations.  It is a structure class.")

(defstruct (transient-2-relation
	    (:include transient-relation)
	    (:constructor nil)
	    (:predicate transient-2-relation?)
	    (:copier nil))
  "The abstract class for FSet transient binary relations.  It is a structure class.")

(defstruct (transient-replay-set
	     (:include transient-set)
	     (:constructor nil)
	     (:predicate transient-replay-set?)
	     (:copier nil))
  "The abstract class for FSet transient replay sets.  It is a structure class."
  (ordering nil))

(defstruct (transient-replay-map
	     (:include transient-map)
	     (:constructor nil)
	     (:predicate transient-replay-map?)
	     (:copier nil))
  "The abstract class for FSet transient replay maps.  It is a structure class."
  (ordering nil))

;;; ================

(defgeneric make-transient (coll &key synchronized?)
  (:documentation
    "Makes a transient collection initialized to the value of `coll'.
Not all FSet collections support transients.  If `synchronized?' is true,
all operations on the transient will be lock-synchronized; otherwise, the
transient is not thread-safe."))

(defgeneric make-persistent (transient-collection &key copy?)
  (:documentation
    "Returns a persistent FSet collection \(the usual kind, with functional semantics\)
with the contents of the transient collection.  Subsequent modifications to the
transient are permitted, and of course will not affect the persistent one.  This
operation has two modes, depending on `copy?'.  If `copy?' is false \(the
default\), it takes O\(1\) time; but subsequent modifications to the transient
will behave performance-wise as if a new transient had been created.  If `copy?'
is true, this takes O\(n\) time, but subsequent modifications to the transient
will run at full speed.  Copying the collection also compacts it."))

(defgeneric clear! (transient-collection)
  (:documentation
    "Discards the existing contents of the transient collection, leaving it empty."))

(defgeneric include! (transient-collection value &optional arg2)
  (:documentation
    "Adds an element to a transient set, or a pair to a transient map or binary
relation.  Returns the transient collection."))

(defgeneric exclude! (transient-collection value &optional arg2)
  (:documentation
    "Removes an element from a transient set, a key from a transient map, or a
pair from a transient binary relation.  Has no effect if the element/key/pair
was not present."))

(defgeneric lookup! (transient-map key)
  (:documentation
    "When called as a function, this is the same as `lookup'.  When used as a
`setf' place, however, it behaves quite differently.  The collection argument
must be a transient map; it is modified to associate the key with the value
passed to `setf'."))

(defgeneric (setf lookup!) (value transient-map key)
  (:documentation
    "The collection argument must be a transient map; it is modified to
associate the key with the value passed to `setf'."))


;;; ================
;;; CH-Set Transients

(defstruct (transient-ch-set
	     (:include transient-set)
	     (:constructor make-transient-ch-set (serial-no id contents org &optional lock))
	     (:predicate transient-ch-set?)
	     (:copier raw-copy-transient-ch-set))
  (contents nil)
  (org nil :type hash-set-org :read-only t))

(defmethod make-transient ((s ch-set) &key synchronized?)
  (let ((serial-no (get-next-transient-id)))
    (make-transient-ch-set serial-no serial-no (ch-set-contents s) (ch-set-org s)
			   (and synchronized? (make-lock "transient lock")))))

(defmethod make-persistent ((ts transient-ch-set) &key copy?)
  (with-lock-maybe ((transient-lock ts))
    (if copy?
	(make-ch-set (ch-set-compact-tree (transient-ch-set-contents ts)) (transient-ch-set-org ts))
      (progn
	(setf (transient-id ts) nil)
	(make-ch-set (transient-ch-set-contents ts) (transient-ch-set-org ts))))))

(defmethod empty? ((ts transient-ch-set))
  (with-lock-maybe ((transient-lock ts))
    (null (transient-ch-set-contents ts))))

(defmethod size ((ts transient-ch-set))
  (with-lock-maybe ((transient-lock ts))
    (ch-set-tree-size (transient-ch-set-contents ts))))

(defmethod arb ((ts transient-ch-set))
  (with-lock-maybe ((transient-lock ts))
    (let ((tree (transient-ch-set-contents ts)))
      (if tree (values (ch-set-tree-arb tree) t)
	(values nil nil)))))

(defmethod contains? ((ts transient-ch-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'contains? 'transient-ch-set)
  (with-lock-maybe ((transient-lock ts))
    (let ((hsorg (transient-ch-set-org ts)))
      (ch-set-tree-contains? (transient-ch-set-contents ts) value
			     (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)))))

(defmethod clear! ((ts transient-ch-set))
  (with-lock-maybe ((transient-lock ts))
    (setf (transient-ch-set-contents ts) nil)))

(defmethod include! ((ts transient-ch-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'include! 'transient-ch-set)
  (with-lock-maybe ((transient-lock ts))
    (allocate-transient-id-if-needed ts)
    (let ((hsorg (transient-ch-set-org ts)))
      (setf (transient-ch-set-contents ts)
	    (ch-set-tree-with (transient-ch-set-contents ts) value
			      (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)
			      (transient-id ts)))))
  ts)

(defmethod exclude! ((ts transient-ch-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'exclude! 'transient-ch-set)
  (with-lock-maybe ((transient-lock ts))
    (allocate-transient-id-if-needed ts)
    (let ((hsorg (transient-ch-set-org ts)))
      (setf (transient-ch-set-contents ts)
	    (ch-set-tree-less (transient-ch-set-contents ts) value
			      (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)
			      (transient-id ts)))))
  ts)


;;; ================
;;; Ch-Map Transients

(defstruct (transient-ch-map
	     (:include transient-map)
	     (:constructor make-transient-ch-map (serial-no id contents org default &optional lock))
	     (:predicate transient-ch-map?)
	     (:copier raw-copy-transient-ch-map))
  (contents nil)
  (org nil :type hash-map-org :read-only t))

(defmethod make-transient ((m ch-map) &key synchronized?)
  (let ((serial-no (get-next-transient-id)))
    (make-transient-ch-map serial-no serial-no (ch-map-contents m) (ch-map-org m) (map-default m)
			   (and synchronized? (make-lock "transient lock")))))

(defmethod make-persistent ((tm transient-ch-map) &key copy?)
  (with-lock-maybe ((transient-lock tm))
    (if copy?
	(make-ch-map (ch-map-compact-tree (transient-ch-map-contents tm)) (transient-ch-map-org tm)
		     (transient-map-default tm))
      (progn
	(setf (transient-id tm) nil)
	(make-ch-map (transient-ch-map-contents tm) (transient-ch-map-org tm) (transient-map-default tm))))))

(defmethod empty? ((tm transient-ch-map))
  (with-lock-maybe ((transient-lock tm))
    (null (transient-ch-map-contents tm))))

(defmethod size ((tm transient-ch-map))
  (with-lock-maybe ((transient-lock tm))
    (ch-map-tree-size (transient-ch-map-contents tm))))

(defmethod arb ((tm transient-ch-map))
  (with-lock-maybe ((transient-lock tm))
    (let ((tree (transient-ch-map-contents tm)))
      (if tree
	  (let ((key val (ch-map-tree-arb-pair tree)))
	    (values key val t))
	(values nil nil nil)))))

(defmethod contains? ((tm transient-ch-map) x &optional (y nil y?))
  (with-lock-maybe ((transient-lock tm))
    (let ((hmorg (transient-ch-map-org tm))
	  ((val? val
	     (ch-map-tree-lookup (transient-ch-map-contents tm) x
				 (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
      (if y? (and val? (equal?-cmp val y (hash-map-org-val-compare-fn hmorg)))
	val?))))

(defmethod clear! ((tm transient-ch-map))
  (with-lock-maybe ((transient-lock tm))
    (setf (transient-ch-map-contents tm) nil)))

(defmethod include! ((tm transient-ch-map) key &optional (value nil value?))
  (check-three-arguments value? 'include! 'transient-ch-map)
  (with-lock-maybe ((transient-lock tm))
    (allocate-transient-id-if-needed tm)
    (let ((hmorg (transient-ch-map-org tm)))
      (setf (transient-ch-map-contents tm)
	    (ch-map-tree-with (transient-ch-map-contents tm) key value
			      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
			      (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg)
			      (transient-id tm)))))
  tm)

(defmethod exclude! ((tm transient-ch-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'exclude! 'transient-ch-map)
  (with-lock-maybe ((transient-lock tm))
    (allocate-transient-id-if-needed tm)
    (let ((hmorg (transient-ch-map-org tm)))
      (setf (transient-ch-map-contents tm)
	    (ch-map-tree-less (transient-ch-map-contents tm) key
			      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
			      (hash-map-org-val-hash-fn hmorg) (transient-id tm)))))
  tm)

(define-methods (lookup fset2:lookup lookup!) ((tm transient-ch-map) key)
  (with-lock-maybe ((transient-lock tm))
    (let ((hmorg (transient-ch-map-org tm))
	  ((val? val mkey (ch-map-tree-lookup (transient-ch-map-contents tm) key
					      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
      (values (if val? val
		(let ((dflt (transient-map-default tm)))
		  (if (eq dflt 'no-default)
		      (error 'fset2:map-domain-error :map tm :key key)
		    dflt)))
	      val? mkey))))

(defmethod (setf lookup!) (value (tm transient-ch-map) key)
  (include! tm key value)
  value)


;;; ================
;;; CH-2-Relation transients

(defstruct (transient-ch-2-relation
	     (:include transient-2-relation)
	     (:constructor make-transient-ch-2-relation (serial-no id map0 map1 org &optional lock))
	     (:predicate transient-ch-2-relation?)
	     (:copier raw-copy-transient-ch-2-relation))
  (size 0 :type integer)
  (map0 nil)
  (map1 nil)
  (org nil :type hash-map-org :read-only t))

(defmethod make-transient ((rel ch-2-relation) &key synchronized?)
  (let ((serial-no (get-next-transient-id)))
    (make-transient-ch-2-relation serial-no serial-no (ch-2-relation-map0 rel) (ch-2-relation-map1 rel)
				  (ch-2-relation-org rel) (and synchronized? (make-lock "transient lock")))))

(defmethod make-persistent ((trel transient-ch-2-relation) &key copy?)
  (with-lock-maybe ((transient-lock trel))
    (if copy?
	(make-ch-2-relation (transient-ch-2-relation-size trel)
			    (ch-map-compact-tree (transient-ch-2-relation-map0 trel)
						 :compact-value-fn #'ch-set-compact-tree)
			    (ch-map-compact-tree (transient-ch-2-relation-map1 trel)
						 :compact-value-fn #'ch-set-compact-tree)
			    (transient-ch-2-relation-org trel))
      (progn
	(setf (transient-id trel) nil)
	(make-ch-2-relation (transient-ch-2-relation-size trel) (transient-ch-2-relation-map0 trel)
			    (transient-ch-2-relation-map1 trel) (transient-ch-2-relation-org trel))))))

(defmethod arity ((trel transient-2-relation))
  2)

(defmethod empty? ((trel transient-ch-2-relation))
  (with-lock-maybe ((transient-lock trel))
    (zerop (transient-ch-2-relation-size trel))))

(defmethod size ((trel transient-ch-2-relation))
  (with-lock-maybe ((transient-lock trel))
    (transient-ch-2-relation-size trel)))

(defmethod arb ((trel transient-ch-2-relation))
  (with-lock-maybe ((transient-lock trel))
    (let ((tree (transient-ch-2-relation-map0 trel)))
      (if tree
	  (let ((key val (ch-map-tree-arb-pair tree)))
	    (values key (ch-set-tree-arb val) t))
	(values nil nil nil)))))

(defmethod contains? ((trel transient-ch-2-relation) x &optional (y nil y?))
  (check-three-arguments y? 'contains? 'transient-ch-2-relation)
  (with-lock-maybe ((transient-lock trel))
    (let ((org (transient-ch-2-relation-org trel))
	  ((found? set-tree (ch-map-tree-lookup (transient-ch-2-relation-map0 trel) x
						(hash-map-org-key-hash-fn org) (hash-map-org-key-compare-fn org)))))
      (and found? (ch-set-tree-contains? set-tree y
					 (hash-map-org-val-hash-fn org) (hash-map-org-val-compare-fn org))))))

(define-methods (lookup fset2:lookup) ((trel transient-ch-2-relation) x)
  "Returns the set of range values that the relation currently pairs `x' with.
Note that this requires making an O(n) copy of the set."
  (with-lock-maybe ((transient-lock trel))
    (let ((org (transient-ch-2-relation-org trel))
	  ((found? set-tree (ch-map-tree-lookup (transient-ch-2-relation-map0 trel) x
						(hash-map-org-key-hash-fn org) (hash-map-org-key-compare-fn org)))))
      (declare (ignore found?))
      (make-ch-set (ch-set-compact-tree set-tree) (ch-2-relation-org-domain-set-org org)))))

(defmethod lookup-inv ((trel transient-ch-2-relation) y)
  "Returns the set of domain values that the relation currently pairs with `y'.
Note that this requires making an O(n) copy of the set."
  (with-lock-maybe ((transient-lock trel))
    (let ((map1 (transient-ch-2-relation-get-inverse trel))
	  (org (transient-ch-2-relation-org trel))
	  ((ignore set-tree (ch-map-tree-lookup map1 y (hash-map-org-val-hash-fn org)
						(hash-map-org-val-compare-fn org)))))
      (declare (ignore ignore))
      (make-ch-set (ch-set-compact-tree set-tree) (ch-2-relation-org-range-set-org org)))))

(defun transient-ch-2-relation-get-inverse (trel)
  "Call this only inside `with-lock-maybe'."
  (allocate-transient-id-if-needed trel)
  (or (transient-ch-2-relation-map1 trel)
      (setf (transient-ch-2-relation-map1 trel)
	    (ch-2-relation-compute-inverse (transient-ch-2-relation-map0 trel) (transient-ch-2-relation-org trel)
					   (transient-id trel)))))

(defmethod clear! ((trel transient-ch-2-relation))
  (with-lock-maybe ((transient-lock trel))
    (setf (transient-ch-2-relation-size trel) 0)
    (setf (transient-ch-2-relation-map0 trel) nil)
    (setf (transient-ch-2-relation-map1 trel) nil)))

(defmethod include! ((trel transient-ch-2-relation) x &optional (y nil y?))
  (let ((x y (if y? (values x y)
	       (values (car x) (cdr x)))))
    (with-lock-maybe ((transient-lock trel))
      (allocate-transient-id-if-needed trel)
      (let ((org (transient-ch-2-relation-org trel))
	    ((map0-hash-fn (hash-map-org-key-hash-fn org))
	     (map0-cmp-fn (hash-map-org-key-compare-fn org))
	     (map1-hash-fn (hash-map-org-val-hash-fn org))
	     (map1-cmp-fn (hash-map-org-val-compare-fn org))
	     ((ignore set-tree (ch-map-tree-lookup (transient-ch-2-relation-map0 trel) x map0-hash-fn map0-cmp-fn))))
	    (map1 (transient-ch-2-relation-map1 trel)))
	(declare (ignore ignore))
	(let ((new-set-tree changed?
		(ch-set-tree-with set-tree y map1-hash-fn map1-cmp-fn (transient-id trel))))
	  (when changed?
	    (incf (transient-ch-2-relation-size trel))
	    (setf (transient-ch-2-relation-map0 trel)
		  (ch-map-tree-with (transient-ch-2-relation-map0 trel) x new-set-tree map0-hash-fn map0-cmp-fn
				    #'ch-set-tree-hash-value #'eql-compare (transient-id trel)))
	    (when map1
	      (let ((ignore set-tree-1 (ch-map-tree-lookup map1 y map1-hash-fn map1-cmp-fn)))
		(declare (ignore ignore))
		(setf (transient-ch-2-relation-map1 trel)
		      (ch-map-tree-with map1 y (ch-set-tree-with set-tree-1 x map0-hash-fn map0-cmp-fn
								 (transient-id trel))
					map1-hash-fn map1-cmp-fn #'ch-set-tree-hash-value #'eql-compare
					(transient-id trel))))))))))
  trel)

(defmethod exclude! ((trel transient-ch-2-relation) x &optional (y nil y?))
  (let ((x y (if y? (values x y)
	       (values (car x) (cdr x)))))
    (with-lock-maybe ((transient-lock trel))
      (allocate-transient-id-if-needed trel)
      (let ((org (transient-ch-2-relation-org trel))
	    ((map0-hash-fn (hash-map-org-key-hash-fn org))
	     (map0-cmp-fn (hash-map-org-key-compare-fn org))
	     (map1-hash-fn (hash-map-org-val-hash-fn org))
	     (map1-cmp-fn (hash-map-org-val-compare-fn org))
	     ((ignore set-tree (ch-map-tree-lookup (transient-ch-2-relation-map0 trel) x map0-hash-fn map0-cmp-fn))))
	    (map0 (transient-ch-2-relation-map0 trel))
	    (map1 (transient-ch-2-relation-map1 trel)))
	(declare (ignore ignore))
	(let ((new-set-tree changed?
		(ch-set-tree-less set-tree y map1-hash-fn map1-cmp-fn (transient-id trel))))
	  (when changed?
	    (decf (transient-ch-2-relation-size trel))
	    (setf (transient-ch-2-relation-map0 trel)
		  (if new-set-tree
		      (ch-map-tree-with map0 x new-set-tree map0-hash-fn map0-cmp-fn
					#'ch-set-tree-hash-value #'eql-compare (transient-id trel))
		    (ch-map-tree-less map0 x map0-hash-fn map0-cmp-fn #'ch-set-tree-hash-value (transient-id trel))))
	    (when map1
	      (let ((ignore set-tree-1 (ch-map-tree-lookup map1 y map1-hash-fn map1-cmp-fn))
		    ((new-set-tree (ch-set-tree-less set-tree-1 x map0-hash-fn map0-cmp-fn (transient-id trel)))))
		(declare (ignore ignore))
		(setf (transient-ch-2-relation-map1 trel)
		      (if new-set-tree
			  (ch-map-tree-with map1 y new-set-tree map1-hash-fn map1-cmp-fn
					    #'ch-set-tree-hash-value #'eql-compare (transient-id trel))
			(ch-map-tree-less map1 y map1-hash-fn map1-cmp-fn #'ch-set-tree-hash-value
					  (transient-id trel)))))))))))
  trel)


;;; ================
;;; CH-Replay-Set transients

(defstruct (transient-ch-replay-set
	     (:include transient-replay-set)
	     (:constructor make-transient-ch-replay-set (serial-no id contents ordering org &optional lock))
	     (:predicate transient-ch-replay-set?)
	     (:copier nil))
  (contents nil)
  (org nil :type hash-set-org :read-only t))

(defmethod make-transient ((rs ch-replay-set) &key synchronized?)
  (let ((serial-no (get-next-transient-id)))
    (make-transient-ch-replay-set serial-no serial-no (ch-replay-set-contents rs) (replay-set-ordering rs)
				  (ch-replay-set-org rs) (and synchronized? (make-lock "transient lock")))))

(defmethod make-persistent ((trs transient-ch-replay-set) &key copy?)
  (with-lock-maybe ((transient-lock trs))
    (if copy?
	(make-ch-replay-set (ch-set-compact-tree (transient-ch-replay-set-contents trs))
			    (transient-ch-replay-set-ordering trs) (transient-ch-replay-set-org trs))
      (progn
	(setf (transient-id trs) nil)
	(make-ch-replay-set (transient-ch-replay-set-contents trs) (transient-ch-replay-set-ordering trs)
			    (transient-ch-replay-set-org trs))))))

(defmethod empty? ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (null (transient-ch-replay-set-contents trs))))

(defmethod size ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (ch-set-tree-size (transient-ch-replay-set-contents trs))))

(defmethod arb ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (let ((tree (transient-ch-replay-set-contents trs)))
      (if tree (values (ch-set-tree-arb tree) t)
	(values nil nil)))))

(defmethod first ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (let ((val? val (wb-ht?-seq-tree-subscript (transient-replay-set-ordering trs) 0)))
      (values val val?))))

(defmethod last ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (let ((tree (transient-replay-set-ordering trs))
	  ((val? val (wb-ht?-seq-tree-subscript tree (1- (wb-ht?-seq-tree-size tree))))))
      (values val val?))))

(defmethod index ((trs transient-ch-replay-set) x)
  "WARNING: linear-time operation!"
  (with-lock-maybe ((transient-lock trs))
    (let ((idx 0)
	  (compare-fn (hash-set-org-compare-fn (transient-ch-replay-set-org trs))))
      (do-wb-seq-tree-members (e (transient-replay-set-ordering trs))
	(when (equal?-cmp e x compare-fn)
	  (return idx))
	(incf idx)))))

(defmethod at-index ((trs transient-ch-replay-set) index)
  (with-lock-maybe ((transient-lock trs))
    (let ((ordering (transient-replay-set-ordering trs))
	  ((size (wb-ht?-seq-tree-size ordering))))
      (unless (and (>= index 0) (< index size))
	(error 'simple-type-error :datum index :expected-type `(integer 0 (,size))
				  :format-control "Index ~D out of bounds on ~A"
				  :format-arguments (list index trs)))
      (let ((ignore val (wb-ht?-seq-tree-subscript ordering index)))
	(declare (ignore ignore))
	val))))

(defmethod contains? ((trs transient-ch-replay-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'transient-ch-replay-set)
  (with-lock-maybe ((transient-lock trs))
    (let ((hsorg (transient-ch-replay-set-org trs)))
      (ch-set-tree-contains? (transient-ch-replay-set-contents trs) x
			     (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)))))

(defmethod clear! ((trs transient-ch-replay-set))
  (with-lock-maybe ((transient-lock trs))
    (setf (transient-ch-replay-set-contents trs) nil)
    (setf (transient-replay-set-ordering trs) nil)))

(defmethod include! ((trs transient-ch-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'include! 'transient-ch-replay-set)
  (with-lock-maybe ((transient-lock trs))
    (let ((contents (transient-ch-replay-set-contents trs))
	  (hsorg (transient-ch-replay-set-org trs))
	  ((new-contents changed?
	     (ch-set-tree-with contents value (hash-set-org-hash-fn hsorg) (hash-set-org-compare-fn hsorg)))))
      (when changed?
	(setf (transient-ch-replay-set-contents trs) new-contents)
	(setf (transient-ch-replay-set-ordering trs)
	      (wb-ht?-seq-tree-append (transient-ch-replay-set-ordering trs) value)))))
  trs)

(defmethod exclude! ((trs transient-ch-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'exclude! 'transient-ch-replay-set)
  (with-lock-maybe ((transient-lock trs))
    (let ((contents (transient-ch-replay-set-contents trs))
	  (hsorg (transient-ch-replay-set-org trs))
	  ((compare-fn (hash-set-org-compare-fn hsorg))
	   ((new-contents changed?
	      (ch-set-tree-less contents value (hash-set-org-hash-fn hsorg) compare-fn)))))
      (when changed?
	(let ((tree (transient-replay-set-ordering trs))
	      ((pos (let ((pos 0))
		      (do-wb-seq-tree-members (x tree (error "Bug in `less' on `ch-replay-set'"))
			(when (equal?-cmp x value compare-fn)
			  (return pos))
			(incf pos))))
	       ((new-ordering (wb-ht?-seq-tree-remove tree pos)))))
	  (setf (transient-ch-replay-set-contents trs) new-contents)
	  (setf (transient-ch-replay-set-ordering trs) new-ordering)))))
  trs)


;;; ================
;;; CH-Replay-Map transients

(defstruct (transient-ch-replay-map
	     (:include transient-replay-map)
	     (:constructor make-transient-ch-replay-map (serial-no id contents ordering org &optional lock))
	     (:predicate transient-ch-replay-map?)
	     (:copier nil))
  (contents nil)
  (org nil :type hash-map-org :read-only t))

(defmethod make-transient ((rm ch-replay-map) &key synchronized?)
  (let ((serial-no (get-next-transient-id)))
    (make-transient-ch-replay-map serial-no serial-no (ch-replay-map-contents rm) (replay-map-ordering rm)
				  (ch-replay-map-org rm) (and synchronized? (make-lock "transient lock")))))

(defmethod make-persistent ((trm transient-ch-replay-map) &key copy?)
  (with-lock-maybe ((transient-lock trm))
    (if copy?
	(make-ch-replay-map (ch-map-compact-tree (transient-ch-replay-map-contents trm))
			    (transient-ch-replay-map-ordering trm) (transient-ch-replay-map-org trm)
			    (transient-map-default trm))
      (progn
	(setf (transient-id trm) nil)
	(make-ch-replay-map (transient-ch-replay-map-contents trm) (transient-ch-replay-map-ordering trm)
			    (transient-ch-replay-map-org trm) (transient-map-default trm))))))

(defmethod empty? ((trm transient-ch-replay-map))
  (with-lock-maybe ((transient-lock trm))
    (null (transient-ch-replay-map-contents trm))))

(defmethod size ((trm transient-ch-replay-map))
  (with-lock-maybe ((transient-lock trm))
    (ch-map-tree-size (transient-ch-replay-map-contents trm))))

(defmethod arb ((trm transient-ch-replay-map))
  (with-lock-maybe ((transient-lock trm))
    (let ((tree (transient-ch-replay-map-contents trm)))
      (if tree
	  (let ((key val (ch-map-tree-arb-pair tree)))
	    (values key val t))
	(values nil nil nil)))))

(defmethod first ((trm transient-ch-replay-map))
  (at-index trm 0))

(defmethod last ((trm transient-ch-replay-map))
  (at-index trm (1- (size trm))))

(defmethod index ((trm transient-ch-replay-map) key)
  "WARNING: linear-time operation!"
  (with-lock-maybe ((transient-lock trm))
    (let ((idx 0)
	  (key-cmp-fn (hash-map-org-key-compare-fn (transient-ch-replay-map-org trm))))
      (do-wb-seq-tree-members (k (transient-replay-map-ordering trm))
	(when (equal?-cmp k key key-cmp-fn)
	  (return idx))
	(incf idx)))))

(defmethod at-index ((trm transient-ch-replay-map) index)
  (with-lock-maybe ((transient-lock trm))
    (let ((ordering (transient-replay-map-ordering trm))
	  ((size (wb-ht?-seq-tree-size ordering))))
      (unless (and (>= index 0) (< index size))
	(error 'simple-type-error :datum index :expected-type `(integer 0 (,(1- size)))
				  :format-control "Index ~D out of bounds on ~A"
				  :format-arguments (list index trm)))
      (let ((ignore key (wb-ht?-seq-tree-subscript ordering index))
	    (hmorg (transient-ch-replay-map-org trm))
	    ((val? val (ch-map-tree-lookup (transient-ch-replay-map-contents trm) key
					   (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
	(declare (ignore ignore))
	(unless val?
	  (error "Bug in transient-ch-replay-map"))
	(values key val)))))

(defmethod contains? ((trm transient-ch-replay-map) x &optional (y nil y?))
  (with-lock-maybe ((transient-lock trm))
    (let ((hmorg (transient-ch-replay-map-org trm))
	  ((val? val
	     (ch-map-tree-lookup (transient-ch-replay-map-contents trm) x
				 (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
      (if y? (and val? (equal?-cmp val y (hash-map-org-val-compare-fn hmorg)))
	val?))))

(defmethod clear! ((trm transient-ch-replay-map))
  (with-lock-maybe ((transient-lock trm))
    (setf (transient-ch-replay-map-contents trm) nil)
    (setf (transient-replay-map-ordering trm) nil)))

(defmethod include! ((trm transient-ch-replay-map) key &optional (value nil value?))
  (check-three-arguments value? 'include! 'transient-ch-replay-map)
  (with-lock-maybe ((transient-lock trm))
    (allocate-transient-id-if-needed trm)
    (let ((contents (transient-ch-replay-map-contents trm))
	  (hmorg (transient-ch-replay-map-org trm))
	  ((new-contents changed?
	     (ch-map-tree-with contents key value
			       (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)
			       (hash-map-org-val-hash-fn hmorg) (hash-map-org-val-compare-fn hmorg)
			       (transient-id trm)))))
      (when changed?
	(setf (transient-ch-replay-map-contents trm) new-contents)
	(setf (transient-ch-replay-map-ordering trm)
	      (wb-ht?-seq-tree-append (transient-ch-replay-map-ordering trm) key)))))
  trm)

(defmethod exclude! ((trm transient-ch-replay-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'exclude! 'transient-ch-replay-map)
  (with-lock-maybe ((transient-lock trm))
    (allocate-transient-id-if-needed trm)
    (let ((contents (transient-ch-replay-map-contents trm))
	  (hmorg (transient-ch-replay-map-org trm))
	  ((key-cmp-fn (hash-map-org-key-compare-fn hmorg))
	   ((new-contents ignore changed?
	      (ch-map-tree-less contents key (hash-map-org-key-hash-fn hmorg) key-cmp-fn
				(hash-map-org-val-hash-fn hmorg) (transient-id trm))))))
      (declare (ignore ignore))
      (when changed?
	(let ((tree (transient-replay-map-ordering trm))
	      ((pos (let ((pos 0))
		      (do-wb-seq-tree-members (x tree (error "Bug in `less' on `ch-replay-map'"))
			(when (equal?-cmp x key key-cmp-fn)
			  (return pos))
			(incf pos))))
	       ((new-ordering (wb-ht?-seq-tree-remove tree pos)))))
	  (setf (transient-ch-replay-map-contents trm) new-contents)
	  (setf (transient-ch-replay-map-ordering trm) new-ordering)))))
  trm)

(define-methods (lookup fset2:lookup lookup!) ((trm transient-ch-replay-map) key)
  (with-lock-maybe ((transient-lock trm))
    (let ((hmorg (transient-ch-replay-map-org trm))
	  ((val? val mkey (ch-map-tree-lookup (transient-ch-replay-map-contents trm) key
					      (hash-map-org-key-hash-fn hmorg) (hash-map-org-key-compare-fn hmorg)))))
      (values (if val? val
		(let ((dflt (transient-map-default trm)))
		  (if (eq dflt 'no-default)
		      (error 'fset2:map-domain-error :map trm :key key)
		    dflt)))
	      val? mkey))))

(defmethod (setf lookup!) (value (trm transient-ch-replay-map) key)
  (include! trm key value)
  value)

;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)


(defstruct (complement-set
	     (:include set)
	     (:constructor make-complement-set (complement))
	     (:predicate complement-set?)
	     (:print-function print-complement-set)
	     (:copier nil))
  "A \"complement set\" is the complement of an ordinary set.  It's infinite, so
it can't be enumerated as is.  But its complement is ordinary, of course, as is
its intersection with an ordinary set, and the difference of it and another
complement set."
  complement)

(defgeneric complement (set)
  (:documentation
    "Returns the complement of the set."))

;;; Compatibility method.
(defmethod complement ((x function))
  (cl:complement x))

(defmethod complement ((s set))
  (make-complement-set s))

(defmethod complement ((cs complement-set))
  (complement-set-complement cs))

(defmethod contains? ((cs complement-set) x)
  (not (contains? (complement-set-complement cs) x)))

(defmethod arb ((cs complement-set))
  ;; Well... I _could_ return some newly consed object... but I think this
  ;; makes more sense :-)
  (error "Can't take `arb' of a complement-set"))

(defmethod size ((cs complement-set))
  ;; Not sure this really makes sense... but what the hell...
  (- (size (complement-set-complement cs))))

(defmethod with ((cs complement-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'complement-set)
  (let ((comp (complement-set-complement cs))
	((new (less comp x))))
    (if (eq new comp) cs
      (make-complement-set new))))

(defmethod less ((cs complement-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'complement-set)
  (let ((comp (complement-set-complement cs))
	((new (with comp x))))
    (if (eq new comp) cs
      (make-complement-set new))))

(defmethod union ((cs1 complement-set) (cs2 complement-set) &key)
  (make-complement-set (intersection (complement-set-complement cs1)
				     (complement-set-complement cs2))))

(defmethod union ((cs complement-set) (s set) &key)
  (make-complement-set (set-difference (complement-set-complement cs) s)))

(defmethod union ((s set) (cs complement-set) &key)
  (make-complement-set (set-difference (complement-set-complement cs) s)))

(defmethod intersection ((cs1 complement-set) (cs2 complement-set) &key)
  (make-complement-set (union (complement-set-complement cs1)
			      (complement-set-complement cs2))))

(defmethod intersection ((cs complement-set) (s set) &key)
  (set-difference s (complement-set-complement cs)))

(defmethod intersection ((s set) (cs complement-set) &key)
  (set-difference s (complement-set-complement cs)))

(defmethod set-difference ((cs1 complement-set) (cs2 complement-set) &key)
  ;; The Venn diagram is very helpful for understanding this.
  (set-difference (complement-set-complement cs2) (complement-set-complement cs1)))

(defmethod set-difference ((cs complement-set) (s set) &key)
  (make-complement-set (union (complement-set-complement cs) s)))

(defmethod set-difference ((s set) (cs complement-set) &key)
  (intersection s (complement-set-complement cs)))

(defmethod subset? ((cs1 complement-set) (cs2 complement-set))
  (subset? (complement-set-complement cs2) (complement-set-complement cs1)))

(defmethod subset? ((cs complement-set) (s set))
  nil)

(defmethod subset? ((s set) (cs complement-set))
  (disjoint? s (complement-set-complement cs)))

(defmethod disjoint? ((cs1 complement-set) (cs2 complement-set))
  nil)

(defmethod disjoint? ((cs complement-set) (s set))
  (subset? s (complement-set-complement cs)))

(defmethod disjoint? ((s set) (cs complement-set))
  (subset? s (complement-set-complement cs)))

(defmethod internal-do-set ((cs complement-set) elt-fn value-fn)
  (declare (ignore elt-fn value-fn))
  (error "Can't enumerate a complement-set"))

(defun print-complement-set (cs stream level)
  (declare (ignore level))
  (format stream "~~")			; to distinguish from bounded-sets
  (write (complement-set-complement cs) :stream stream))

(defmethod compare ((cs1 complement-set) (cs2 complement-set))
  (compare (complement-set-complement cs2) (complement-set-complement cs1)))

(defmethod compare ((cs complement-set) (s set))
  ':greater)

(defmethod compare ((s set) (cs complement-set))
  ':less)


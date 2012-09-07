;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)


;;; "Bounded" is certainly not an ideal term, but I couldn't find anything better
;;; in Wikipedia's pages on topology.  "Set-in-discrete-topology" is just too long.
(defstruct (bounded-set
	     (:include set)
	     (:constructor make-bounded-set-internal (universe set complement?))
	     (:predicate bounded-set?)
	     (:print-function print-bounded-set)
	     (:copier nil))
  "A \"bounded set\" is a subset (not necessarily proper) of a specified set,
called the \"universe\".  (Topologically, it is a set in the discrete topology
on the universe.)"
  universe
  set
  ;; We go to some trouble to make sure that the `set' never contains more than
  ;; half the `universe'.  This doesn't help asymptotic complexity, but does help
  ;; with the constant factor.
  complement?)

(defun make-bounded-set (universe set &optional complement?)
  (unless (subset? set universe)
    (error "Attempt to create a bounded-set whose set is not a subset of its universe"))
  ;; Ensure that if the set is exactly half the size of the universe, we use the
  ;; positive representation.
  (if complement?
      (if (<= (size universe) (* 2 (size set)))
	  (make-bounded-set-internal universe (set-difference universe set) nil)
	(make-bounded-set-internal universe set t))
    (if (< (size universe) (* 2 (size set)))
	(make-bounded-set-internal universe (set-difference universe set) t)
      (make-bounded-set-internal universe set nil))))

(defun bounded-set-contents (bs)
  (if (bounded-set-complement? bs)
      (set-difference (bounded-set-universe bs) (bounded-set-set bs))
    (bounded-set-set bs)))

(defmethod complement ((bs bounded-set))
  (make-bounded-set-internal (bounded-set-universe bs) (bounded-set-set bs)
			     (not (bounded-set-complement? bs))))

(defmethod empty? ((bs bounded-set))
  (and (not (bounded-set-complement? bs))
       (empty? (bounded-set-set bs))))

(defmethod contains? ((bs bounded-set) x)
  (if (bounded-set-complement? bs)
      (not (contains? (bounded-set-set bs) x))
    (contains? (bounded-set-set bs) x)))

(defmethod arb ((bs bounded-set))
  (if (bounded-set-complement? bs)
      ;; Ugh
      (do-set (x (bounded-set-universe bs))
	(unless (contains? (bounded-set-set bs) x)
	  (return x)))
    (arb (bounded-set-set bs))))

(defmethod size ((bs bounded-set))
  (if (bounded-set-complement? bs)
      (- (size (bounded-set-universe bs))
	 (size (bounded-set-set bs)))
    (size (bounded-set-set bs))))

(defmethod with ((bs1 bounded-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'bounded-set)
  (unless (contains? (bounded-set-universe bs1) x)
    (error "NIU: You have addressed a planet not ...~@
	    er, I mean, you have tried to add an element to a bounded-set~@
	    that is not in its universe"))
  (if (bounded-set-complement? bs1)
      (make-bounded-set-internal (bounded-set-universe bs1)
				 (less (bounded-set-set bs1) x)
				 t)
    (make-bounded-set (bounded-set-universe bs1) (with (bounded-set-set bs1) x))))

(defmethod less ((bs1 bounded-set) x &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'bounded-set)
  (unless (contains? (bounded-set-universe bs1) x)
    (error "NIU: You have addressed a planet not ...~@
	    er, I mean, you have tried to remove an element from a bounded-set~@
	    that is not in its universe"))
  (if (bounded-set-complement? bs1)
      (make-bounded-set (bounded-set-universe bs1) (with (bounded-set-set bs1) x) t)
    (make-bounded-set-internal (bounded-set-universe bs1)
			       (less (bounded-set-set bs1) x)
			       nil)))

(defmethod union ((bs1 bounded-set) (bs2 bounded-set) &key)
  (unless (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
    (error "Can't take the union of two bounded-sets with different universes"))
  (let ((u (bounded-set-universe bs1))
	(s1 (bounded-set-set bs1))
	(s2 (bounded-set-set bs2)))
    (if (bounded-set-complement? bs1)
	(if (bounded-set-complement? bs2)
	    (make-bounded-set-internal u (intersection s1 s2) t)
	  (make-bounded-set-internal u (set-difference s1 s2) t))
      (if (bounded-set-complement? bs2)
	  (make-bounded-set-internal u (set-difference s2 s1) t)
	(make-bounded-set u (union s1 s2))))))

(defmethod intersection ((bs1 bounded-set) (bs2 bounded-set) &key)
  (unless (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
    (error "Can't take the intersection of two bounded-sets with different universes"))
  (let ((u (bounded-set-universe bs1))
	(s1 (bounded-set-set bs1))
	(s2 (bounded-set-set bs2)))
    (if (bounded-set-complement? bs1)
	(if (bounded-set-complement? bs2)
	    (make-bounded-set u (union s1 s2) t)
	  (make-bounded-set-internal u (set-difference s2 s1) nil))
      (if (bounded-set-complement? bs2)
	  (make-bounded-set-internal u (set-difference s1 s2) nil)
	(make-bounded-set-internal u (intersection s1 s2) nil)))))

(defmethod set-difference ((bs1 bounded-set) (bs2 bounded-set) &key)
  (unless (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
    (error "Can't take the set-difference of two bounded-sets with different universes"))
  (let ((u (bounded-set-universe bs1))
	(s1 (bounded-set-set bs1))
	(s2 (bounded-set-set bs2)))
    (if (bounded-set-complement? bs1)
	(if (bounded-set-complement? bs2)
	    (make-bounded-set-internal u (set-difference s2 s1) nil)
	  (make-bounded-set u (union s1 s2) t))
      (if (bounded-set-complement? bs2)
	  (make-bounded-set-internal u (intersection s1 s2) nil)
	(make-bounded-set-internal u (set-difference s1 s2) nil)))))

(defmethod subset? ((bs1 bounded-set) (bs2 bounded-set))
  (unless (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
    (error "Can't do `subset?' on two bounded-sets with different universes"))
  (let ((s1 (bounded-set-set bs1))
	(s2 (bounded-set-set bs2)))
    (if (bounded-set-complement? bs1)
	(and (bounded-set-complement? bs2)
	     (subset? s2 s1))
      (if (bounded-set-complement? bs2)
	  (disjoint? s1 s2)
	(subset? s1 s2)))))

(defmethod disjoint? ((bs1 bounded-set) (bs2 bounded-set))
  (unless (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
    (error "Can't do `disjoint?' on two bounded-sets with different universes"))
  (let ((s1 (bounded-set-set bs1))
	(s2 (bounded-set-set bs2)))
    (if (bounded-set-complement? bs1)
	;; Note, we've ruled out the case where the two sets are mutual complements,
	;; both in complement form.
	(and (not (bounded-set-complement? bs2))
	     (subset? s2 s1))
      (if (bounded-set-complement? bs2)
	  (subset? s1 s2)
	(disjoint? s1 s2)))))

(defmethod internal-do-set ((bs bounded-set) elt-fn value-fn)
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (if (bounded-set-complement? bs)
      ;; Should we form the complement?  That would cons -- but this is O(n log n).
      (internal-do-set (bounded-set-universe bs)
		       (lambda (x)
			 (unless (contains? (bounded-set-set bs) x)
			   (funcall elt-fn x)))
		       value-fn)
    (internal-do-set (bounded-set-set bs) elt-fn value-fn)))

(defun print-bounded-set (bs stream level)
  (declare (ignore level))
  (format stream "~:[+~;-~]" (bounded-set-complement? bs))
  (write (bounded-set-set bs) :stream stream))

(defmethod compare ((bs1 bounded-set) (bs2 bounded-set))
  ;; We don't constrain the bounded-sets to have the same universes, since the
  ;; FSet way is to let you mix absolutely any objects in sets.  (We feel no
  ;; obligation to make the different-universe case be fast, though.)
  (if (equal? (bounded-set-universe bs1) (bounded-set-universe bs2))
      (let ((s1 (bounded-set-set bs1))
	    (s2 (bounded-set-set bs2)))
	(if (bounded-set-complement? bs1)
	    (if (bounded-set-complement? bs2)
		(compare s2 s1)
	      ':greater)
	  (if (bounded-set-complement? bs2)
	      ':less
	    (compare s1 s2))))
    (compare (bounded-set-contents bs1) (bounded-set-contents bs2))))

(defmethod compare ((bs bounded-set) (s set))
  ;; Potentially slow, but unlikely to be used.
  (compare (bounded-set-contents bs) s))

(defmethod compare ((s set) (bs bounded-set))
  ;; Potentially slow, but unlikely to be used.
  (compare s (bounded-set-contents bs)))

;;; Hmm... we have no way to say "a normal set" except to specify the
;;; implementation.  Seems like we have a missing abstract class,
;;; `enumerated-set' or some such.
(defmethod convert ((to-type (eql 'wb-set)) (bs bounded-set) &key)
  (bounded-set-contents bs))


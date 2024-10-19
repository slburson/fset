;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: replay.lisp
;;; Contents: Replay sets and maps.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(in-package :fset)

;;; ================================================================================
;;; Replay sets

(defstruct (wb-replay-set
	     (:include replay-set)
	     (:constructor make-wb-replay-set (contents ordering))
	     (:predicate wb-replay-set?)
	     (:print-function print-wb-replay-set)
	     (:copier nil))
  "A replay set is like a set, except that its iteration order is the order in which members
were added to it.  It does not support all set operations, but you can convert it to a set.
Note that in the current implementation, `less' on a replay set takes O(n) time.  Replay sets
are printed as \"#{= ... }\"."
  (contents nil :read-only t)
  (ordering nil :read-only t))

(defparameter *empty-wb-replay-set* (make-wb-replay-set nil nil))

(defun empty-replay-set ()
  *empty-wb-replay-set*)

(defun empty-wb-replay-set ()
  *empty-wb-replay-set*)

(defmethod empty-instance-form ((type-name (eql 'wb-replay-set)))
  '(empty-wb-replay-set))

(defmethod empty? ((s wb-replay-set))
  (null (wb-replay-set-contents s)))

(defmethod size ((s wb-replay-set))
  (wb-set-tree-size (wb-replay-set-contents s)))

(defmethod arb ((s wb-replay-set))
  (let ((tree (wb-replay-set-contents s)))
    (if tree (values (wb-set-tree-arb tree) t)
      (values nil nil))))

(defmethod contains? ((s wb-replay-set) x &optional (y nil y?))
  (declare (ignore y))
  (check-two-arguments y? 'contains? 'wb-set)
  (wb-set-tree-member? (wb-replay-set-contents s) x))

(defmethod lookup ((s wb-replay-set) value)
  (wb-set-tree-find-equal (wb-replay-set-contents s) value))

(defmethod convert ((to-type (eql 'seq)) (s wb-replay-set) &key)
  (make-wb-seq (wb-replay-set-ordering s)))
(defmethod convert ((to-type (eql 'wb-seq)) (s wb-replay-set) &key)
  (make-wb-seq (wb-replay-set-ordering s)))

(defmethod convert ((to-type (eql 'set)) (s wb-replay-set) &key)
  (make-wb-set (wb-replay-set-contents s)))
(defmethod convert ((to-type (eql 'wb-set)) (s wb-replay-set) &key)
  (make-wb-set (wb-replay-set-contents s)))

(defmethod convert ((to-type (eql 'replay-set)) (s wb-replay-set) &key)
  s)
(defmethod convert ((to-type (eql 'wb-replay-set)) (s wb-replay-set) &key)
  s)
(defmethod convert ((to-type (eql 'replay-set)) (s wb-set) &key)
  ;; Of course, this uses the ordering of `s'.
  (make-wb-replay-set (wb-set-contents s) (wb-seq-contents (convert 'wb-seq s))))
(defmethod convert ((to-type (eql 'wb-replay-set)) (s wb-set) &key)
  (make-wb-replay-set (wb-set-contents s) (wb-seq-contents (convert 'wb-seq s))))

(defmethod with ((s wb-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	((new-contents (wb-set-tree-with contents value))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents
			  (let ((tree (wb-replay-set-ordering s)))
			    (wb-seq-tree-insert tree (wb-seq-tree-size tree) value))))))

(defmethod union ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: it adds the members
of `s2' to `s1', so the ordering of the result will be that of `s1' with any
new members appended."
  (cond ((empty? s2) s1)
	((empty? s1)
	 (if (wb-replay-set? s2) s2
	   (convert 'wb-replay-set s2)))
	(t
	 (let ((contents (wb-replay-set-contents s1))
	       (ordering (wb-replay-set-ordering s1)))
	   ;; This is O(n log m), rather than the usual O(m + n).
	   (do-set (x s2)
	     (let ((tmp (wb-set-tree-with contents x)))
	       (unless (eq tmp contents)
		 (setq contents tmp)
		 (setq ordering (wb-seq-tree-insert ordering (wb-seq-tree-size ordering) x)))))
	   (make-wb-replay-set contents ordering)))))

(defmethod intersection ((s1 wb-replay-set) (s2 set) &key)
  "As the parameter types suggest, this is not symmetric: the ordering of the
result is that of `s1', filtered by membership in `s2'."
  (cond ((empty? s2) (empty-wb-replay-set))
	((empty? s1) s1)
	(t
	 (make-wb-replay-set (wb-set-contents (intersection (convert 'wb-set s1) (convert 'wb-set s2)))
			     (wb-seq-contents (filter s2 (convert 'wb-seq s1)))))))

;;; WARNING: linear-time operation!
(defmethod less ((s wb-replay-set) value &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-replay-set)
  (let ((contents (wb-replay-set-contents s))
	((new-contents (wb-set-tree-less contents value))))
    (if (eq new-contents contents)
	s
      (make-wb-replay-set new-contents
			  (let ((tree (wb-replay-set-ordering s)))
			    (wb-seq-tree-remove tree (or (position value (make-wb-seq tree))
							 (error "Bug in `less' on `wb-replay-set'"))))))))

(defmethod iterator ((s wb-replay-set) &key)
  (make-wb-seq-tree-iterator (wb-replay-set-ordering s)))

(defmethod internal-do-set ((s wb-replay-set) elt-fn &optional (value-fn (lambda () nil)))
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (wb-replay-set-ordering s) (funcall value-fn))
    (funcall elt-fn x)))

(defmethod compare ((a wb-replay-set) (b wb-replay-set))
  ;; The ordering is considered metadata, not part of the set value.  If you want to compare
  ;; the orderings, convert them to seqs.
  (wb-set-tree-compare (wb-replay-set-contents a) (wb-replay-set-contents b)))

(defun print-wb-replay-set (set stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{=" :suffix " }")
    (do-set (x set)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (write x :stream stream))))

;;; A bit faster than `set', if you know it's a `wb-replay-set'.
(gmap:def-gmap-arg-type wb-replay-set (set)
  "Yields the elements of `set'."
  `((make-wb-seq-tree-iterator-internal (wb-replay-set-ordering ,set))
    #'wb-seq-tree-iterator-done?
    #'wb-seq-tree-iterator-get))


;;; ================================================================================
;;; Replay maps

(defstruct (wb-replay-map
	     (:include replay-map)
	     (:constructor make-wb-replay-map (contents ordering &optional default))
	     (:predicate wb-replay-map?)
	     (:print-function print-wb-replay-map)
	     (:copier nil))
  "A replay map is like a map, except that its iteration order is the order in which keys
were first added to it.  It does not support all map operations, but you can convert it
to a map.  Note that in the current implementation, `less' on a replay map takes O(n) time.
Replay maps are printed as \"#{=| ... |}\"."
  contents
  ordering)

(defparameter *empty-wb-replay-map* (make-wb-replay-map nil nil))

(defun empty-replay-map (&optional default)
  (if default (make-wb-replay-map nil nil default)
    *empty-wb-replay-map*))

(defmethod empty-map-instance-form ((type-name (eql 'replay-map)) default)
  `(empty-replay-map ,default))

(defun empty-wb-replay-map (&optional default)
  (if default (make-wb-replay-map nil nil default)
    *empty-wb-replay-map*))

(defmethod empty-map-instance-form ((type-name (eql 'wb-replay-map)) default)
  `(empty-wb-replay-map ,default))

(defmethod with-default ((m wb-replay-map) new-default)
  (make-wb-replay-map (wb-replay-map-contents m) (wb-replay-map-ordering m) new-default))

(defmethod empty? ((m wb-replay-map))
  (null (wb-replay-map-contents m)))

(defmethod arb ((m wb-replay-map))
  (let ((tree (wb-replay-map-contents m)))
    (if tree
	(let ((key val (wb-map-tree-arb-pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod size ((m wb-replay-map))
  (WB-Map-Tree-Size (wb-replay-map-contents m)))

(defmethod convert ((to-type (eql 'map)) (m wb-replay-map) &key)
  (make-wb-map (wb-replay-map-contents m) (map-default m)))
(defmethod convert ((to-type (eql 'wb-map)) (m wb-replay-map) &key)
  (make-wb-map (wb-replay-map-contents m) (map-default m)))

(defmethod lookup ((m wb-replay-map) key)
  (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) key)))
    (values (if val? val (map-default m)) val?)))

(defmethod domain-contains? ((m wb-replay-map) x)
  (wb-map-tree-lookup (wb-replay-map-contents m) x))

(defmethod with ((m wb-replay-map) key &optional (value nil value?))
  (check-three-arguments value? 'with 'wb-replay-map)
  (let ((contents (wb-replay-map-contents m))
	((new-contents (wb-map-tree-with contents key value))))
    (if (eq new-contents contents)
	m
      (if (= (wb-map-tree-size new-contents) (wb-map-tree-size contents))
	  ;; New value for existing key.
	  (make-wb-replay-map new-contents (wb-replay-map-ordering m) (map-default m))
	(make-wb-replay-map new-contents
			    (let ((tree (wb-replay-map-ordering m)))
			      (wb-seq-tree-insert tree (wb-seq-tree-size tree) key))
			    (map-default m))))))

;;; WARNING: linear-time operation!
(defmethod less ((m wb-replay-map) key &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'wb-replay-map)
  (let ((contents (wb-replay-map-contents m))
	((new-contents (WB-Map-Tree-Less contents key))))
    (if (eq new-contents contents)
	m
      (make-wb-replay-map new-contents
			  (let ((tree (wb-replay-map-ordering m)))
			    (wb-seq-tree-remove tree (or (position key (make-wb-seq tree))
							 (error "Bug in `less' on `wb-replay-map'"))))
			  (map-default m)))))

(defmethod domain ((m wb-replay-map))
  "The domain of a replay map is a replay set."
  (make-wb-replay-set (wb-map-tree-domain (wb-replay-map-contents m)) (wb-replay-map-ordering m)))

(defmethod iterator ((m wb-replay-map) &key)
  (let ((iter (make-wb-seq-tree-iterator-internal (wb-replay-map-ordering m)))
	(contents (wb-replay-map-contents m)))
    (lambda (op)
      (ecase op
	(:get (let ((key key? (wb-seq-tree-iterator-get iter)))
		(if (not key?)
		    (values nil nil nil)
		  (values key (wb-map-tree-lookup contents key) t))))
	(:done? (wb-seq-tree-iterator-done? iter))
	(:more? (not (wb-seq-tree-iterator-done? iter)))))))

(defmethod internal-do-map ((m wb-replay-map) elt-fn &optional (value-fn (lambda () nil)))
  (declare (optimize (speed 3) (safety 0))
	   (type function elt-fn value-fn))
  (do-wb-seq-tree-members (x (wb-replay-map-ordering m) (funcall value-fn))
    (funcall elt-fn x (let ((val? val (wb-map-tree-lookup (wb-replay-map-contents m) x)))
			(assert val? () "Bug in `wb-replay-map' iteration")
			val))))

(defun print-wb-replay-map (map stream level)
  (declare (ignore level))
  (pprint-logical-block (stream nil :prefix "#{=|")
    (do-map (x y map)
      (pprint-pop)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      ;; There might be a map entry for 'quote or 'function...
      (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* nil))
	(write (list x y) :stream stream)))
    (format stream " |}~:[~;/~:*~S~]" (map-default map))))

(gmap:def-gmap-arg-type wb-replay-map (map)
  "Yields each pair of `map', as two values."
  (let ((map-var (gensym "MAP-")))
    `((make-wb-seq-tree-iterator-internal (wb-replay-map-ordering ,map-var))
      #'wb-seq-tree-iterator-done?
      (:values 2 #'(lambda (it) (let ((key key? (wb-seq-tree-iterator-get it)))
				  (if (not key?)
				      (values nil nil)
				    (values key (let ((val? val
							(wb-map-tree-lookup (wb-replay-map-contents ,map-var) key)))
						  (assert val? () "Bug in `wb-replay-map' GMap arg type")
						  val))))))
      nil
      ((,map-var ,map)))))

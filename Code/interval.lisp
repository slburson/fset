;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: interval.lisp
;;; Contents: interval sets
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

;;; Assumption: the items are totally ordered (no unequal-but-equivalent pairs).

(defstruct (interval-set
	    (:include set)
	    (:constructor make-interval-set (contents))
	    (:predicate interval-set?)
	    (:print-function print-interval-set)
	    (:copier nil))
  contents)

(defun print-interval-set (set stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream "#I{")
      (let ((i 0))
	(Do-WB-Set-Tree-Members (iv (interval-set-contents set))
	  (format stream " ")
	  (when (and *print-length* (>= i *print-length*))
	    (format stream "...")
	    (return))
	  (incf i)
	  (let ((*print-level* (and *print-level* (1- *print-level*))))
	    (write iv :stream stream))))
      (format stream " }"))))

(defstruct (interval
	    (:constructor make-raw-interval (lower upper kind))
	    (:predicate interval?)
	    (:print-function print-interval)
	    (:copier nil))
  lower
  upper
  kind)		; closed at: one of ':both, ':lower, ':upper, ':neither

(defun print-interval (iv stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream (if (interval-lower-closed? iv) "[" "("))
      (let ((*print-level* (and *print-level* (1- *print-level*))))
	(write (interval-lower iv) :stream stream)
	(format stream " ")
	(write (interval-upper iv) :stream stream))
      (format stream (if (interval-upper-closed? iv) "]" ")")))))

(defun interval-kind-symbol (lower-closed? upper-closed?)
  (if lower-closed?
      (if upper-closed? ':both ':lower)
    (if upper-closed? ':upper ':neither)))

(defun make-interval (lower upper lower-closed? upper-closed?)
  (let ((comp (compare lower upper)))
    (unless (and (not (eq comp ':greater))
		 (or (eq comp ':less)
		     ;; If the interval is null, it had better be closed.
		     (and lower-closed? upper-closed?)))
      (error "Attempt to create inconsistent interval")))
  (make-raw-interval lower upper (interval-kind-symbol lower-closed? upper-closed?)))

(defun interval-lower-closed? (iv)
  (let ((kind (interval-kind iv)))
    (or (eq kind ':lower) (eq kind ':both))))

(defun interval-upper-closed? (iv)
  (let ((kind (interval-kind iv)))
    (or (eq kind ':upper) (eq kind ':both))))

;;; Says `:equal' if `x' is in `iv'.
(defmethod compare ((x t) (iv interval))
  (cond ((let ((comp (compare x (interval-lower iv))))
	   (or (eq comp ':less)
	       (and (eq comp ':equal) (not (interval-lower-closed? iv)))))
	 ':less)
	((let ((comp (compare x (interval-upper iv))))
	   (or (eq comp ':greater)
	       (and (eq comp ':equal) (not (interval-upper-closed? iv)))))
	 ':greater)
	(t ':equal)))

;;; Says `:equal' if `x' is in `iv'.
(defmethod compare ((iv interval) (x t))
  (cond ((let ((comp (compare (interval-upper iv) x)))
	   (or (eq comp ':less)
	       (and (eq comp ':equal) (not (interval-upper-closed? iv)))))
	 ':less)
	((let ((comp (compare (interval-lower iv) x)))
	   (or (eq comp ':greater)
	       (and (eq comp ':equal) (not (interval-lower-closed? iv)))))
	 ':greater)
	(t ':equal)))

;;; Says `:equal' if the intervals overlap.
(defmethod compare ((iv0 interval) (iv1 interval))
  (values (compare-intervals iv0 iv1)))

(defun compare-intervals (iv0 iv1)
  "Second value is true if the two abut.  `:equal' means they overlap."
  (let ((comp-ul (compare (interval-upper iv0) (interval-lower iv1))))
    (cond ((or (eq comp-ul ':less)
	       (and (eq comp-ul ':equal)
		    (not (interval-upper-closed? iv0))
		    (not (interval-lower-closed? iv1))))
	   (values ':less nil))
	  ((and (eq comp-ul ':equal)
		(not (and (interval-upper-closed? iv0) (interval-lower-closed? iv1))))
	   (values ':less t))
	  (t
	   (let ((comp-lu (compare (interval-lower iv0) (interval-upper iv1))))
	     (cond ((or (eq comp-lu ':greater)
			(and (eq comp-lu ':equal)
			     (not (interval-lower-closed? iv0))
			     (not (interval-upper-closed? iv1))))
		    (values ':greater nil))
		   ((and (eq comp-lu ':equal)
			 (not (and (interval-lower-closed? iv0)
				   (interval-upper-closed? iv1))))
		    (values ':greater t))
		   (t ':equal)))))))

(defun empty-interval-set ()
  (make-interval-set nil))

(defmethod empty? ((s interval-set))
  (null (interval-set-contents s)))

(defmethod size ((s interval-set))
  "The number of intervals in the set."
  (WB-Set-Tree-Size (interval-set-contents s)))

;;; Internal.
(defgeneric with-interval (interval-set lower upper lower-closed? upper-closed?))

(defmethod with-interval ((s interval-set) lower upper lower-closed? upper-closed?)
  (let ((contents (interval-set-contents s)))
    (let ((size (WB-Set-Tree-Size contents))
	  ((raw-lower-rank lower-found? (WB-Set-Tree-Find-Rank contents lower))
	   (raw-upper-rank upper-found? (WB-Set-Tree-Find-Rank contents upper))
	   ((lower-rank (if lower-found? (1+ raw-lower-rank) raw-lower-rank))
	    (upper-rank (if upper-found? (1- raw-upper-rank) raw-upper-rank))
	    ((removed (gmap :set (lambda (i) (WB-Set-Tree-Rank-Element contents i))
			    (:index lower-rank upper-rank))))))
	  (new-lower lower)
	  (new-lower-closed? lower-closed?)
	  (new-upper upper)
	  (new-upper-closed? upper-closed?))
      (declare (fixnum size raw-lower-rank raw-upper-rank lower-rank upper-rank))
      (when (or lower-found? (> lower-rank 0))
	(let ((prev-iv (WB-Set-Tree-Rank-Element contents (1- lower-rank))))
	  (when (or lower-found?
		    (and (equal? (interval-upper prev-iv) lower)
			 (or (interval-upper-closed? prev-iv)
			     lower-closed?)))
	    (adjoinf removed prev-iv)
	    (ecase (compare (interval-lower prev-iv) lower)
	      ((:less)
	       (setq new-lower (interval-lower prev-iv))
	       (setq new-lower-closed? (interval-lower-closed? prev-iv)))
	      ((:equal)
	       (when (interval-lower-closed? prev-iv)
		 (setq new-lower-closed? t)))))))
      (when (or upper-found? (< upper-rank size))
	(let ((next-iv (WB-Set-Tree-Rank-Element contents upper-rank)))
	  (when (or upper-found?
		    (and (equal? (interval-lower next-iv) upper)
			 (or (interval-lower-closed? next-iv)
			     upper-closed?)))
	    (adjoinf removed next-iv)
	    (ecase (compare (interval-upper next-iv) upper)
	      ((:greater)
	       (setq new-upper (interval-upper next-iv))
	       (setq new-upper-closed? (interval-upper-closed? next-iv)))
	      ((:equal)
	       (when (interval-upper-closed? next-iv)
		 (setq new-upper-closed? t)))))))
      (make-interval-set
	(WB-Set-Tree-With (WB-Set-Tree-Diff contents (wb-set-contents removed))
			  (make-interval new-lower new-upper
					 new-lower-closed? new-upper-closed?))))))

(defmethod with ((s interval-set) (iv interval) &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'with 'interval-set)
  (with-interval s (interval-lower iv) (interval-upper iv)
		 (interval-lower-closed? iv) (interval-upper-closed? iv)))


;;; Internal.
(defgeneric less-interval (interval-set lower upper lower-closed? upper-closed?))

(defmethod less-interval ((s interval-set) lower upper lower-closed? upper-closed?)
  (let ((contents (interval-set-contents s)))
    (let ((lower-rank lower-found? (WB-Set-Tree-Find-Rank contents lower))
	  (upper-rank upper-found? (WB-Set-Tree-Find-Rank contents upper))
	  ((removed (gmap :set (lambda (i) (WB-Set-Tree-Rank-Element contents i))
			  (:index lower-rank upper-rank))))
	  (new (set)))
      (declare (fixnum lower-rank upper-rank))
      (when lower-found?
	(let ((lower-iv (WB-Set-Tree-Rank-Element contents lower-rank)))
	  (unless (and (equal? (interval-upper lower-iv) lower)
		       (not (interval-upper-closed? lower-iv))
		       (not lower-closed?))
	    (adjoinf removed lower-iv)
	    (let ((comp (compare (interval-lower lower-iv) lower)))
	      (when (or (eq comp ':less)
			(and (eq comp ':equal)
			     (interval-lower-closed? lower-iv)
			     (not lower-closed?)))
		(adjoinf new (make-interval (interval-lower lower-iv) lower
					    (interval-lower-closed? lower-iv)
					    (not lower-closed?))))))))
      (when upper-found?
	(let ((upper-iv (WB-Set-Tree-Rank-Element contents upper-rank)))
	  (unless (and (equal? (interval-lower upper-iv) upper)
		       (not (interval-lower-closed? upper-iv))
		       (not upper-closed?))
	    (adjoinf removed upper-iv)
	    (let ((comp (compare (interval-upper upper-iv) upper)))
	      (when (or (eq comp ':greater)
			(and (eq comp ':equal)
			     (interval-upper-closed? upper-iv)
			     (not upper-closed?)))
		(adjoinf new (make-interval upper (interval-upper upper-iv)
					    (not upper-closed?)
					    (interval-upper-closed? upper-iv))))))))
      (make-interval-set
	(WB-Set-Tree-Union (WB-Set-Tree-Diff contents (wb-set-contents removed))
			   (wb-set-contents new))))))

(defmethod less ((s interval-set) (iv interval) &optional (arg2 nil arg2?))
  (declare (ignore arg2))
  (check-two-arguments arg2? 'less 'interval-set)
  (less-interval s (interval-lower iv) (interval-upper iv)
		 (interval-lower-closed? iv) (interval-upper-closed? iv)))

(defmethod union ((s0 interval-set) (s1 interval-set) &key)
  ;; Works, but needs to be rewritten to run in linear time and cons less.
  (let ((contents0 (interval-set-contents s0))
	(contents1 (interval-set-contents s1)))
    (let ((iter0 (Make-WB-Set-Tree-Iterator-Internal contents0))
	  (iter1 (Make-WB-Set-Tree-Iterator-Internal contents1))
	  ((cur0 (WB-Set-Tree-Iterator-Get iter0))
	   (cur1 (WB-Set-Tree-Iterator-Get iter1)))
	  (result nil))
      (while (and cur0 cur1)
	(let ((comp abut? (compare-intervals cur0 cur1))
	      ((comp (if abut? ':equal comp))))
	  (ecase comp
	    ((:less)
	     (setq result (WB-Set-Tree-With result cur0))
	     (setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
	    ((:greater)
	     (setq result (WB-Set-Tree-With result cur1))
	     (setq cur1 (WB-Set-Tree-Iterator-Get iter1)))
	    ((:equal)		; they overlap or abut
	     (let ((lcomp (compare (interval-lower cur0) (interval-lower cur1)))
		   (ucomp (compare (interval-upper cur0) (interval-upper cur1))))
	       (if (or (eq lcomp ':less)
		       (and (eq lcomp ':equal) (interval-lower-closed? cur0)))
		   (progn
		     (when (or (eq ucomp ':less)
			       (and (eq ucomp ':equal)
				    (not (interval-upper-closed? cur0))
				    (interval-upper-closed? cur1)))
		       (setq cur0 (make-interval
				    (interval-lower cur0) (interval-upper cur1)
				    (interval-lower-closed? cur0)
				    (interval-upper-closed? cur1))))
		     (setq cur1 (WB-Set-Tree-Iterator-Get iter1)))
		 (progn
		   (when (or (eq ucomp ':greater)
			     (and (eq ucomp ':equal)
				  (not (interval-upper-closed? cur1))
				  (interval-upper-closed? cur0)))
		     (setq cur1 (make-interval
				  (interval-lower cur1) (interval-upper cur0)
				  (interval-lower-closed? cur1)
				  (interval-upper-closed? cur0))))
		   (setq cur0 (WB-Set-Tree-Iterator-Get iter0)))))))))
      (while cur0
	(setq result (WB-Set-Tree-With result cur0))
	(setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
      (while cur1
	(setq result (WB-Set-Tree-With result cur1))
	(setq cur1 (WB-Set-Tree-Iterator-Get iter1)))
      (make-interval-set result))))

(defmethod intersection ((s0 interval-set) (s1 interval-set) &key)
  ;; Works, but needs to be rewritten to run in linear time and cons less.
  (let ((contents0 (interval-set-contents s0))
	(contents1 (interval-set-contents s1)))
    (let ((iter0 (Make-WB-Set-Tree-Iterator-Internal contents0))
	  (iter1 (Make-WB-Set-Tree-Iterator-Internal contents1))
	  ((cur0 (WB-Set-Tree-Iterator-Get iter0))
	   (cur1 (WB-Set-Tree-Iterator-Get iter1)))
	  (result nil))
      (while (and cur0 cur1)
	(let ((comp (compare-intervals cur0 cur1)))
	  (ecase comp
	    ((:less)
	     (setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
	    ((:greater)
	     (setq cur1 (WB-Set-Tree-Iterator-Get iter1)))
	    ((:equal)		; they overlap
	     (let ((lcomp (compare (interval-lower cur0) (interval-lower cur1)))
		   (ucomp (compare (interval-upper cur0) (interval-upper cur1))))
	       (if (or (eq ucomp ':less)
		       (and (eq ucomp ':equal) (interval-upper-closed? cur1)))
		   (progn
		     (when (or (eq lcomp ':less)
			       (and (eq lcomp ':equal)
				    (interval-lower-closed? cur0)
				    (not (interval-lower-closed? cur1))))
		       (setq cur0 (make-interval
				    (interval-lower cur1) (interval-upper cur0)
				    (interval-lower-closed? cur1)
				    (interval-upper-closed? cur0))))
		     (setq result (WB-Set-Tree-With result cur0))
		     (setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
		 (progn
		   (when (or (eq lcomp ':greater)
			     (and (eq lcomp ':equal)
				  (interval-lower-closed? cur1)
				  (not (interval-lower-closed? cur0))))
		     (setq cur1 (make-interval
				  (interval-lower cur0) (interval-upper cur1)
				  (interval-lower-closed? cur0)
				  (interval-upper-closed? cur1))))
		   (setq result (WB-Set-Tree-With result cur1))
		   (setq cur1 (WB-Set-Tree-Iterator-Get iter1)))))))))
      (make-interval-set result))))

(defmethod set-difference ((s0 interval-set) (s1 interval-set) &key)
  ;; Works, but needs to be rewritten to run in linear time and cons less.
  (let ((contents0 (interval-set-contents s0))
	(contents1 (interval-set-contents s1)))
    (let ((iter0 (Make-WB-Set-Tree-Iterator-Internal contents0))
	  (iter1 (Make-WB-Set-Tree-Iterator-Internal contents1))
	  ((cur0 (WB-Set-Tree-Iterator-Get iter0))
	   (cur1 (WB-Set-Tree-Iterator-Get iter1)))
	  (result nil))
      (while (and cur0 cur1)
	(let ((comp (compare-intervals cur0 cur1)))
	  (ecase comp
	    ((:less)
	     (setq result (WB-Set-Tree-With result cur0))
	     (setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
	    ((:greater)
	     (setq cur1 (WB-Set-Tree-Iterator-Get iter1)))
	    ((:equal)		; they overlap
	     (let ((lcomp (compare (interval-lower cur0) (interval-lower cur1)))
		   (ucomp (compare (interval-upper cur0) (interval-upper cur1))))
	       (when (or (eq lcomp ':less)
			 (and (eq lcomp ':equal)
			      (interval-lower-closed? cur0)
			      (not (interval-lower-closed? cur1))))
		 (let ((iv (make-interval (interval-lower cur0) (interval-lower cur1)
					  (interval-lower-closed? cur0)
					  (not (interval-lower-closed? cur1)))))
		   (setq result (WB-Set-Tree-With result iv))))
	       (if (eq ucomp ':greater)
		   (setq cur0 (make-interval (interval-upper cur1) (interval-upper cur0)
					     (not (interval-upper-closed? cur1))
					     (interval-upper-closed? cur0)))
		 (setq cur0 (WB-Set-Tree-Iterator-Get iter0))))))))
      (while cur0
	(setq result (WB-Set-Tree-With result cur0))
	(setq cur0 (WB-Set-Tree-Iterator-Get iter0)))
      (make-interval-set result))))


;;; ================================================================================
;;; Interval set relations

;;; An "interval set relation" is a binary relation whose left domain is encoded as
;;; an interval set.  It does not cache its inverse (it could, but I have no need
;;; for this).  Adam Megacz calls it a "topological bag", but that doesn't seem
;;; right to me (it's certainly not a bag in the sense in which I use the word).

#|| Someday
(defstruct (interval-set-relation
	    (:constructor make-interval-set-relation (contents))
	    (:predicate interval-set-relation?)
	    (:print-function print-interval-set-relation)
	    (:copier nil))
  contents)
||#


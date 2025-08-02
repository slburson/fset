;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: swank.lisp
;;; Contents: Support for inspecting FSet collections in Slime
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)

(defstruct (subrange
	     (:constructor make-subrange (coll lo hi)))
  coll
  lo
  hi) ; exclusive

(defmethod swank-backend::emacs-inspect ((sr subrange))
  (emacs-inspect-partial (subrange-coll sr) (subrange-lo sr) (subrange-hi sr)))

(defmethod swank-backend::emacs-inspect ((s set))
  (emacs-inspect-partial s 0 (size s)))

(defmethod emacs-inspect-partial ((s set) lo hi)
  (append (if (<= hi (+ lo 32))
	      (gmap (:result append)
		    (fn (i) (swank-backend::label-value-line i (at-rank s i)))
		    (:arg index lo hi))
	    ;; It may seem odd to start in the middle, but it lets you do binary search to get to
	    ;; any element; starting at the beginning is much slower in many cases.
	    (let ((mid (floor (+ lo hi) 2))
		  ((lo2 (max 0 (- mid 16)))
		   (hi2 (min (size s) (+ mid 16)))))
	      (append (and (> lo2 lo)
			   (list `(:value ,(make-subrange s lo lo2)
					  ,(format nil "** Show entries ~D - ~D **" lo (1- lo2)))
				 '(:newline)))
		      (emacs-inspect-partial s lo2 hi2)
		      (and (< hi2 hi)
			   (list `(:value ,(make-subrange s hi2 hi)
					  ,(format nil "** Show entries ~D - ~D **" hi2 (1- hi)))
				 '(:newline))))))
	  '((:newline))
	  (let ((cmp-fn-name (compare-fn-name s)))
	    (and (not (eq cmp-fn-name 'compare))
		 (swank-backend::label-value-line "compare-fn" cmp-fn-name)))))

(defmethod swank-backend::emacs-inspect ((s seq))
  (emacs-inspect-partial s 0 (size s)))

(defmethod emacs-inspect-partial ((s seq) lo hi)
  (append (if (<= hi (+ lo 32))
	      (gmap (:result append)
		    (fn (i) (swank-backend::label-value-line i (@ s i)))
		    (:arg index lo hi))
	    (let ((mid (floor (+ lo hi) 2))
		  ((lo2 (max 0 (- mid 16)))
		   (hi2 (min (size s) (+ mid 16)))))
	      (append (and (> lo2 lo)
			   (list `(:value ,(make-subrange s lo lo2)
					  ,(format nil "** Show entries ~D - ~D **" lo (1- lo2)))
				 '(:newline)))
		      (emacs-inspect-partial s lo2 hi2)
		      (and (< hi2 hi)
			   (list `(:value ,(make-subrange s hi2 hi)
					  ,(format nil "** Show entries ~D - ~D **" hi2 (1- hi)))
				 '(:newline))))))
	  '((:newline))
	  (let ((default (default s)))
	    (and default
		 (swank-backend::label-value-line "default" default)))))

(defmethod swank-backend::emacs-inspect ((m map))
  (emacs-inspect-partial m 0 (size m)))

(defmethod emacs-inspect-partial ((m map) lo hi)
  (append (if (<= hi (+ lo 16))
	      (gmap (:result append)
		    (fn (i) (let ((k v (at-rank m i)))
			      (append (swank-backend::label-value-line (format nil "Key   ~D" i) k)
				      (swank-backend::label-value-line (format nil "Value ~D" i) v))))
		    (:arg index lo hi))
	    (let ((mid (floor (+ lo hi) 2))
		  ((lo2 (max 0 (- mid 8)))
		   (hi2 (min (size m) (+ mid 8)))))
	      (append (and (> lo2 lo)
			   (list `(:value ,(make-subrange m lo lo2)
					  ,(format nil "** Show entries ~D - ~D **" lo (1- lo2)))
				 '(:newline)))
		      (emacs-inspect-partial m lo2 hi2)
		      (and (< hi2 hi)
			   (list `(:value ,(make-subrange m hi2 hi)
					  ,(format nil "** Show entries ~D - ~D **" hi2 (1- hi)))
				 '(:newline))))))
	  '((:newline))
	  (let ((key-cmp-fn-name (key-compare-fn-name m)))
	    (and (not (eq key-cmp-fn-name 'compare))
		 (swank-backend::label-value-line "compare-fn" key-cmp-fn-name)))
	  (let ((val-cmp-fn-name (val-compare-fn-name m)))
	    (and (not (eq val-cmp-fn-name 'compare))
		 (swank-backend::label-value-line "compare-fn" val-cmp-fn-name)))
	  (let ((default (default m)))
	    (and default
		 (swank-backend::label-value-line "default" default)))))

(defmethod swank-backend::emacs-inspect ((b bag))
  (emacs-inspect-partial b 0 (set-size b)))

(defmethod emacs-inspect-partial ((b bag) lo hi)
  (append (if (<= hi (+ lo 16))
	      (gmap (:result append)
		    (fn (i) (let ((v m (at-rank b i)))
			      (append (swank-backend::label-value-line (format nil "Value ~D" i) v)
				      (swank-backend::label-value-line (format nil "Count ~D" i) m))))
		    (:arg index lo hi))
	    (let ((mid (floor (+ lo hi) 2))
		  ((lo2 (max 0 (- mid 8)))
		   (hi2 (min (set-size b) (+ mid 8)))))
	      (append (and (> lo2 lo)
			   (list `(:value ,(make-subrange b lo lo2)
					  ,(format nil "** Show entries ~D - ~D **" lo (1- lo2)))
				 '(:newline)))
		      (emacs-inspect-partial b lo2 hi2)
		      (and (< hi2 hi)
			   (list `(:value ,(make-subrange b hi2 hi)
					  ,(format nil "** Show entries ~D - ~D **" hi2 (1- hi)))
				 '(:newline))))))
	  '((:newline))
	  (let ((cmp-fn-name (compare-fn-name b)))
	    (and (not (eq cmp-fn-name 'compare))
		 (swank-backend::label-value-line "compare-fn" cmp-fn-name)))))

(defmethod swank-backend::emacs-inspect ((tup tuple))
  (emacs-inspect-partial tup 0 (size tup)))

(defmethod emacs-inspect-partial ((tup tuple) lo hi)
  (if (<= hi (+ lo 32))
      (gmap (:result append)
	    (fn (i) (let ((k v (at-rank tup i)))
		      (swank-backend::label-value-line (format nil "~A~32T" (tuple-key-name k)) v)))
	    (:arg index lo hi))
    (let ((mid (floor (+ lo hi) 2))
	  ((lo2 (max 0 (- mid 16)))
	   (hi2 (min (size tup) (+ mid 16)))))
      (append (and (> lo2 lo)
		   (list `(:value ,(make-subrange tup lo lo2) ,(format nil "** Show entries ~D - ~D **" lo (1- lo2)))
			 '(:newline)))
	      (emacs-inspect-partial tup lo2 hi2)
	      (and (< hi2 hi)
		   (list `(:value ,(make-subrange tup hi2 hi) ,(format nil "** Show entries ~D - ~D **" hi2 (1- hi)))
			 '(:newline)))))))

(defmethod swank-backend::emacs-inspect ((br 2-relation))
  (emacs-inspect-partial (convert 'map-to-sets br) 0 (size br)))

(defmethod swank-backend::emacs-inspect ((lr list-relation))
  (emacs-inspect-partial (convert 'set lr) 0 (size lr)))

(defmethod swank-backend::emacs-inspect ((cs complement-set))
  ;; &&& Would be nice to add a header reminding the user that they're viewing the elements _not_ in the set.
  (emacs-inspect-partial (complement cs) 0 (size (complement cs))))


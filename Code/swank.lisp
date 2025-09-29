;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: swank.lisp
;;; Contents: Support for inspecting FSet collections in Slime
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)

(defvar *emacs-inspect-internals* nil)

(defstruct (subrange
	     (:constructor make-subrange (coll lo hi)))
  coll
  lo
  hi) ; exclusive

(defmethod swank::emacs-inspect ((sr subrange))
  (append (emacs-inspect-partial (subrange-coll sr) (subrange-lo sr) (subrange-hi sr))
	  (emacs-inspect-footer (subrange-coll sr))))

(defgeneric emacs-inspect-footer (collection)
  (:method-combination append :most-specific-last)
  (:method append ((x t)) nil))

(defmethod swank::emacs-inspect ((s set))
  (append (emacs-inspect-partial s 0 (size s))
	  (emacs-inspect-footer s)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod emacs-inspect-partial ((s set) lo hi)
  (append (if (<= hi (+ lo 32))
	      (gmap (:result append)
		    (fn (i) (swank::label-value-line i (at-index s i)))
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
				 '(:newline))))))))

(defmethod emacs-inspect-footer append ((s set))
  (let ((cmp-fn-name (compare-fn-name s)))
    (and (not (eq cmp-fn-name 'compare))
	 (cons '(:newline)
	       (swank::label-value-line "compare-fn" cmp-fn-name)))))

(defmethod emacs-inspect-footer append ((s wb-set))
  (list '(:newline) '(:newline)
	`(:value ,(wb-set-contents s) "[Show internal tree]")))

(defmethod emacs-inspect-footer append ((s ch-set))
  ;; &&& Also show hash fn?
  (list '(:newline) '(:newline)
	`(:value ,(ch-set-contents s) "[Show internal tree]")))

(defmethod swank::emacs-inspect ((s seq))
  (append (emacs-inspect-partial s 0 (size s))
	  (emacs-inspect-footer s)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod emacs-inspect-partial ((s seq) lo hi)
  (append (if (<= hi (+ lo 32))
	      (gmap (:result append)
		    (fn (i) (swank::label-value-line i (@ s i)))
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
				 '(:newline))))))))

(defmethod emacs-inspect-footer append ((s seq))
  (let ((default (default s)))
    (and default
	 (cons '(:newline)
	       (swank::label-value-line "default" default)))))

(defmethod emacs-inspect-footer append ((s wb-seq))
  (list '(:newline) '(:newline)
	`(:value ,(wb-seq-contents s) "[Show internal tree]")))

(defmethod swank::emacs-inspect ((m map))
  (append (emacs-inspect-partial m 0 (size m))
	  (emacs-inspect-footer m)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod emacs-inspect-partial ((m map) lo hi)
  (append (if (<= hi (+ lo 16))
	      (gmap (:result append)
		    (fn (i) (let ((k v (at-index m i)))
			      (append (swank::label-value-line (format nil "Key   ~D" i) k)
				      (swank::label-value-line (format nil "Value ~D" i) v))))
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
				 '(:newline))))))))

(defmethod emacs-inspect-footer append ((m map))
  (append '((:newline))
	  (let ((key-cmp-fn-name (key-compare-fn-name m)))
	    (and (not (eq key-cmp-fn-name 'compare))
		 (swank::label-value-line "key-compare-fn" key-cmp-fn-name)))
	  (let ((val-cmp-fn-name (val-compare-fn-name m)))
	    (and (not (eq val-cmp-fn-name 'compare))
		 (swank::label-value-line "val-compare-fn" val-cmp-fn-name)))
	  (let ((default (default m)))
	    (and default
		 (swank::label-value-line "default" default)))))

(defmethod emacs-inspect-footer append ((s wb-map))
  (list '(:newline) '(:newline)
	`(:value ,(wb-map-contents s) "[Show internal tree]")))

(defmethod emacs-inspect-footer append ((s ch-map))
  ;; &&& Also show hash fns?
  (list '(:newline) '(:newline)
	`(:value ,(ch-map-contents s) "[Show internal tree]")))

(defmethod swank::emacs-inspect ((b bag))
  (append (emacs-inspect-partial b 0 (set-size b))
	  (emacs-inspect-footer b)))

(defmethod emacs-inspect-partial ((b bag) lo hi)
  (append (if (<= hi (+ lo 16))
	      (gmap (:result append)
		    (fn (i) (let ((v m (at-index b i)))
			      (append (swank::label-value-line (format nil "Value ~D" i) v)
				      (swank::label-value-line (format nil "Count ~D" i) m))))
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
				 '(:newline))))))))

(defmethod emacs-inspect-footer append ((b bag))
  (let ((cmp-fn-name (compare-fn-name b)))
    (and (not (eq cmp-fn-name 'compare))
	 (cons '(:newline)
	       (swank::label-value-line "compare-fn" cmp-fn-name)))))

(defmethod swank::emacs-inspect ((tup tuple))
  (append (emacs-inspect-partial tup 0 (size tup))
	  (emacs-inspect-footer tup)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod emacs-inspect-partial ((tup tuple) lo hi)
  (if (<= hi (+ lo 32))
      (gmap (:result append)
	    (fn (i) (let ((k v (at-index tup i)))
		      (swank::label-value-line (format nil "~A~32T" (tuple-key-name k)) v)))
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

(defmethod swank::emacs-inspect ((br 2-relation))
  (append (emacs-inspect-partial (convert 'map-to-sets br) 0 (size br))
	  (emacs-inspect-footer br)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod swank::emacs-inspect ((lr list-relation))
  (append (emacs-inspect-partial (convert 'set lr) 0 (size lr))
	  (emacs-inspect-footer lr)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod swank::emacs-inspect ((cs complement-set))
  (append (emacs-inspect-partial (complement cs) 0 (size (complement cs)))
	  (emacs-inspect-footer cs)
	  (and *emacs-inspect-internals*
	       (cons '(:newline) (call-next-method)))))

(defmethod emacs-inspect-footer append ((cs complement-set))
  '((:newline)
    "This is a complement set.  The values displayed are the ones NOT in the set."
    (:newline)))


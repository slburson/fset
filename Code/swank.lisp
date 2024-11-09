;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: swank.lisp
;;; Contents: Support for inspecting FSet collections in Slime
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
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
  (if (<= hi (+ lo 32))
      (gmap (:result append)
	    (fn (i) (swank-backend::label-value-line i (at-rank s i)))
	    (:arg index lo hi))
    ;; It may seem odd to start in the middle, but it lets you do binary search to get to
    ;; any element; starting at the beginning is much slower in many cases.
    (let ((mid (floor (+ lo hi) 2))
	  ((lo2 (max 0 (- mid 16)))
	   (hi2 (min (size s) (+ mid 16)))))
      (append (and (> lo2 lo)
		   (list `(:value ,(make-subrange s lo lo2) ,(format nil "Entries ~D - ~D" lo (1- lo2)))
			 '(:newline)))
	      (emacs-inspect-partial s lo2 hi2)
	      (and (< hi2 hi)
		   (list `(:value ,(make-subrange s hi2 hi) ,(format nil "Entries ~D - ~D" hi2 (1- hi)))
			 '(:newline)))))))

(defmethod swank-backend::emacs-inspect ((s seq))
  (emacs-inspect-partial s 0 (size s)))

(defmethod emacs-inspect-partial ((s seq) lo hi)
  (if (<= hi (+ lo 32))
      (gmap (:result append)
	    (fn (i) (swank-backend::label-value-line i (@ s i)))
	    (:arg index lo hi))
    (let ((mid (floor (+ lo hi) 2))
	  ((lo2 (max 0 (- mid 16)))
	   (hi2 (min (size s) (+ mid 16)))))
      (append (and (> lo2 lo)
		   (list `(:value ,(make-subrange s lo lo2) ,(format nil "Entries ~D - ~D" lo (1- lo2)))
			 '(:newline)))
	      (emacs-inspect-partial s lo2 hi2)
	      (and (< hi2 hi)
		   (list `(:value ,(make-subrange s hi2 hi) ,(format nil "Entries ~D - ~D" hi2 (1- hi)))
			 '(:newline)))))))

(defmethod swank-backend::emacs-inspect ((m map))
  (emacs-inspect-partial m 0 (size m)))

(defmethod emacs-inspect-partial ((m map) lo hi)
  (if (<= hi (+ lo 16))
      (gmap (:result append)
	    (fn (i k v) (append (swank-backend::label-value-line (format nil "Key   ~D" i) k)
				(swank-backend::label-value-line (format nil "Value ~D" i) v)))
	    (:arg index 0)
	    (:arg map m))
    (let ((mid (floor (+ lo hi) 2))
	  ((lo2 (max 0 (- mid 8)))
	   (hi2 (min (size m) (+ mid 8)))))
      (append (and (> lo2 lo)
		   (list `(:value ,(make-subrange m lo lo2) ,(format nil "Entries ~D - ~D" lo (1- lo2)))
			 '(:newline)))
	      (emacs-inspect-partial m lo2 hi2)
	      (and (< hi2 hi)
		   (list `(:value ,(make-subrange m hi2 hi) ,(format nil "Entries ~D - ~D" hi2 (1- hi)))
			 '(:newline)))))))

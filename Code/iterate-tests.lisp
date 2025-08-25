;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: iterate-tests.lisp
;;; Contents: Tests for FSet/Iterate
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset/iterate/test)

(defun test-fset/iterate ()
  (macrolet ((test (form)
	       `(unless ,form
		  (error "Test failed: ~S" ',form))))
    ;; Demonstrate the absence of conflict between the two uses of `with'.
    (test (equal? (let ((s (set 3)))
		    (iter (with s2 = (with s 4))
		      (for x in-set s2)
		      (collect x)))
		  '(3 4)))

    (test (equal? (iter (with s = (empty-set))
                        (for x in '(3 4))
                        (setf s (with s x))
                        (finally (return s)))
		  (set 3 4)))

    (test (equal? (iter (for x :in-set (set 1 3 7)) (collect x))
		  '(1 3 7)))
    (test (equal? (iter (for x :in-bag (bag 1 3 3 7)) (collect x))
		  '(1 3 3 7)))
    (test (equal? (iter (for (x n) :in-bag (bag 1 3 3 7)) (collect (cons x n)))
		  '((1 . 1) (3 . 2) (7 . 1))))
    (test (equal? (iter (for (k v) :in-map (map ('a 3) ('b 14) ('c 42))) (collect (cons k v)))
		  '((a . 3) (b . 14) (c . 42))))

    ;; I think these are all the cases needed for `:in-seq'.
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :from 3) (collect x))
		  '(3 e 5 f g 8)))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :upfrom 3 :to 6) (collect x))
		  '(3 e 5 f)))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :upfrom 3 :below 6) (collect x))
		  '(3 e 5)))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :from 7 :downto 2) (collect x))
		  (reverse '(2 3 e 5 f g))))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :downfrom 7 :above 2) (collect x))
		  (reverse '(3 e 5 f g))))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :downfrom 7 :downto 2) (collect x))
		  (reverse '(2 3 e 5 f g))))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :downfrom 5) (collect x))
		  (reverse '(a b 2 3 e 5))))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :to 4) (collect x))
		  '(a b 2 3 e)))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :downto 4) (collect x))
		  (reverse '(e 5 f g 8))))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :above 4) (collect x))
		  '(5 f g 8)))
    (test (equal? (iter (for x :in-seq (seq 'a 'b 2 3 'e 5 'f 'g 8) :below 4) (collect x))
		  (reverse '(a b 2 3))))

    (test (equal? (iter (for x in '(1 2 3 2)) (collect-set x))
		  (set 1 2 3)))
    (test (equal? (iter (for x in '(1 2 3 2)) (collect-set x :initial-value (ch-set)))
		  (ch-set 1 2 3)))

    (test (equal? (iter (for x in '(1 2 3 2)) (collect-bag x))
		  (bag 1 2 2 3)))
    (test (equal? (iter (for x in '(1 2 3 2)) (collect-bag x :initial-value (wb-custom-bag 'fset::erapmoc)))
		  (wb-custom-bag 'fset::erapmoc 1 2 2 3)))
    (test (equal? (iter (for x in '(1 2 3 2)) (for n in '(1 2 1 3)) (collect-bag x :count n))
		  (bag 1 (% 2 5) 3)))

    (test (equal? (iter (for x in '(q w e r t y u i o p)) (collect-seq x))
		  (convert 'seq '(q w e r t y u i o p))))

    (test (equal? (iter (for (k v) in '((a 3) (b 7) (c 4))) (collect-map k -> v))
		  (map ('a 3) ('b 7) ('c 4))))
    (test (equal? (iter (for (k v) in '((a 3) (b 7) (c 4)))
		    (collect-map k -> v :initial-value (wb-custom-map 'fset::erapmoc nil)))
		  (wb-custom-map 'fset::erapmoc nil ('a 3) ('b 7) ('c 4))))
    (test (equal? (iter (for (k v) in '((a 3) (b 7) (c 4) (b 13) (d 22) (a 12) (a 17)))
		    (collect-map-to-sets k -> v))
		  (map ('a (set 3 12 17)) ('b (set 7 13)) ('c (set 4)) ('d (set 22)) :default (set))))

    (test (equal? (iter (for s in (list (set 1 2) (set 2 5))) (unioning s))
		  (set 1 2 5)))
    ;; Bag union combines counts with `max'.  The result is a bag if any member of the series is a bag,
    ;; even if the resulting counts (multiplicities) are all 1.
    (test (equal? (iter (for s in (list (set 2 5) (bag 1 2 2))) (unioning s))
		  (bag 1 2 2 5)))

    (test (equal? (iter (for s in (list (set 1 2) (set 2 5))) (intersecting s))
		  (set 2)))
    ;; Bag intersection combines counts with `min'.
    (test (equal? (iter (for s in (list (bag 1 2 2) (bag 2 5))) (intersecting s))
		  (bag 2)))

    (test (equal? (iter (for s in (list (seq 'now 'is 'the) (seq 'time 'for 'all))) (concating s))
		  (seq 'now 'is 'the 'time 'for 'all)))

    (test (equal? (iter (for m in (list (map ('a 4) ('b 7)) (map ('a 8) ('c 42)))) (map-unioning m))
		  (map ('a 8) ('b 7) ('c 42))))
    (test (equal? (iter (for m in (list (map ('a 4) ('b 7)) (map ('a 8) ('c 42))))
		    (map-unioning m combining-with #'+))
		  (map ('a 12) ('b 7) ('c 42))))
    (test (equal? (iter (for m in (list (map ('a 4) ('b 7)) (map ('a 8) ('c 42)))) (map-intersecting m))
		  (map ('a 8))))
    (test (equal? (iter (for m in (list (map ('a 4) ('b 7)) (map ('a 8) ('c 42))))
		    (map-intersecting m combining-with #'+))
		  (map ('a 12))))))

(in-package :fset)

;;; `adjoinf' / `removef', which don't form a good pair, are now deprecated
;;; in favor of `includef' / `excludef'.
(define-modify-macro adjoinf (&rest item-or-tuple)
  with
  "(adjoinf coll . args) --> (setf coll (with coll . args))")

(define-modify-macro removef (&rest item-or-tuple)
  less
  "(removef coll . args) --> (setf coll (less coll . args))")

(define-modify-macro includef (&rest item-or-tuple)
  with
  "(includef coll . args) --> (setf coll (with coll . args))")

(define-modify-macro excludef (&rest item-or-tuple)
  less
  "(excludef coll . args) --> (setf coll (less coll . args))")

(define-modify-macro unionf (set)
  union)

(define-modify-macro intersectf (set)
  intersection)

(define-modify-macro imagef (fn)
  ximage)

(defun ximage (coll fn)
  (image fn coll))

(define-modify-macro composef (fn)
  compose)

(define-modify-macro push-first (val)
  with-first
  "(push-first seq val) --> (setf seq (with-first seq val))")

(define-modify-macro push-last (val)
  with-last
  "(push-last seq val) --> (setf seq (with-last seq val))")

(defmacro pop-first (seq &environment env)
  "Removes the first element from `seq' and returns it."
  (let ((vars vals new setter getter (get-setf-expansion seq env)))
    (unless (= 1 (length new))
      (error "Nonsensical `~A' form: ~S" 'pop-first `(pop-first ,seq)))
    `(let* (,@(mapcar #'list vars vals)
	    (,(car new) ,getter))
       (prog1
	 (first ,(car new))
	 (setq ,(car new) (less-first ,(car new)))
	 ,setter))))

(defmacro pop-last (seq &environment env)
  "Removes the last element from `seq' and returns it."
  (let ((vars vals new setter getter (get-setf-expansion seq env)))
    (unless (= 1 (length new))
      (error "Nonsensical `~A' form: ~S" 'pop-last `(pop-last ,seq)))
    `(let* (,@(mapcar #'list vars vals)
	    (,(car new) ,getter))
       (prog1
	 (last ,(car new))
	 (setq ,(car new) (less-last ,(car new)))
	 ,setter))))

(define-modify-macro appendf (seq)
  concat)

(define-modify-macro prependf (seq)
  xconcat)

(defun xconcat (seq1 seq2)
  (concat seq2 seq1))


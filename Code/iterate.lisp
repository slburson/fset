;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: iterate.lisp
;;; Contents: FSet definitions for the Iterate macro
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset/iterate)

;;; Both FSet and Iterate export a `with', and we would prefer not to write a package prefix
;;; on either of them.  Fortunately, we can have this particular piece of cake and eat it too,
;;; because Iterate doesn't fdefine `with'; it defines it as a clause. We can force Iterate to
;;; treat `fset:with` as a special form, superseding clause dispatch, and decide based on whether
;;; we are at the top level to expand into a binding or a function call.

(defun walk-with (&rest form)
  (assert (eql (car form) 'fset:with))
  (if iter::*top-level?*
      (iter::walk
       `(iter:with ,@(rest form)))
      (cons (car form)
            (iter::walk-arglist (rest form)))))

(eval-when (:load-toplevel :execute)
  (pushnew
   (cons 'fset:with 'walk-with)
   iter::*special-form-alist*
   :test #'equal))

;;; ================ Drivers/Generators ================

(defclause-driver (for var in-set set)
  "Elements of a set."
  (top-level-check)
  (let ((iter-var (make-var-and-binding 'set-iter `(iterator ,set)))
	((setqs (do-dsetq var `(funcall ,iter-var ':get)))
	 (test `(when (funcall ,iter-var ':done?) (go ,*loop-end*)))))
    (setq *loop-end-used?* t)
    (return-driver-code :next (list test setqs) :variable var)))

(defclause-driver (for var in-iterator x)
  "Elements of any FSet iterator."
  (top-level-check)
  (let ((iter-var (make-var-and-binding 'iter `(iterator ,x)))
	((setqs (do-dsetq var `(funcall ,iter-var ':get)))
	 (test `(when (funcall ,iter-var ':done?) (go ,*loop-end*)))))
    (setq *loop-end-used?* t)
    (return-driver-code :next (list test setqs) :variable var)))

(defclause-driver (for var-or-var*count in-bag bag)
  "Elements or pairs of a bag.  If `var-or-var*count' is a symbol, it
will be bound to successive values from the bag, with repetitions of
elements with multiplicities greater than 1.  If it's a list, the first
element will be bound \(potentially with destructuring\) to successive
unique elements of the bag, with the second, if present, bound to the
corresponding multiplicity."
  (top-level-check)
  (unless (or (symbolp var-or-var*count)
	      (and (listp var-or-var*count) (<= (length var-or-var*count) 2)
		   (symbolp (cadr var-or-var*count))))
    (clause-error "~A should be either a single variable or a list of one or two variables, ~@
		   of which the first can be a pattern"
		  'var-or-var*count))
  (let ((iter-var (make-var-and-binding 'bag-iter `(iterator ,bag :pairs? ,(listp var-or-var*count))))
	(var-spec (if (listp var-or-var*count) `(values . ,var-or-var*count)
		    var-or-var*count))
	((setqs (do-dsetq var-spec `(funcall ,iter-var ':get)))
	 (test `(when (funcall ,iter-var ':done?) (go ,*loop-end*)))))
    (setq *loop-end-used?* t)
    (return-driver-code :next (list test setqs) :variable var-spec)))

(defclause-driver (for key*val in-map map)
  "Pairs of a map.  `key*val' should be a list of one or two variables/patterns."
  (top-level-check)
  (unless (and (listp key*val) (<= (length key*val) 2))
    (clause-error "~A should be a list of one or two variables/patterns" 'key*val))
  (let ((iter-var (make-var-and-binding 'map-iter `(iterator ,map)))
	(var-spec `(values . ,key*val))
	((setqs (do-dsetq var-spec `(funcall ,iter-var ':get)))
	 (test `(when (funcall ,iter-var ':done?) (go ,*loop-end*)))))
    (setq *loop-end-used?* t)
    (return-driver-code :next (list test setqs) :variable var-spec)))

;;; We don't use `defclause-sequence' because it assumes indexing is efficient.
;;; On FSet seqs, indexing is O(log n), but an iterator is O(1) per element.
;;; &&& I've punted on `with-index' for now; it's a PITA since we're not doing indexing.
(defclause-driver (for var in-seq seq
		       &optional from from upfrom upfrom downfrom downfrom
		       to to downto downto above above below below
		       by step)
  "Elements of a seq.  The index range can be specified with `from'/`upfrom'
or `downfrom', and/or `to', `downto', `above', or `below', stepped by `by'."
  (top-level-check)
  (unless (> 2 (size (filter #'identity (seq from upfrom downfrom))))
    (clause-error "Only one of FROM, UPFROM, or DOWNFROM may be specified"))
  (unless (> 2 (size (filter #'identity (seq to downto above below))))
    (clause-error "Only one of TO, DOWNTO, ABOVE, or BELOW may be specified"))
  (let ((start end from-end?
	  (cond ((and (or from upfrom) (or to below))
		 (values (or from upfrom) (or below `(1+ ,to)) nil))
		((and (or from downfrom) (or downto above))
		 (values (or downto `(1+ ,above)) `(1+ ,(or from downfrom)) t))
		((or from upfrom)
		 (values (or from upfrom) nil nil))
		(downfrom
		 (values nil `(1+ ,downfrom) t))
		(to
		 (values nil `(1+ ,to) nil))
		(downto
		 (values downto nil t))
		(above
		 (values `(1+ ,above) nil nil))
		(below
		 (values nil below t))
		(t
		 (values nil nil nil))))
	((iter-var (make-var-and-binding 'seq-iter `(iterator ,seq :start ,start :end ,end :from-end? ,from-end?)))
	 ((setqs (do-dsetq var (if step
				   `(prog1
				      (funcall ,iter-var ':get)
				      ;; Obviously not ideal if `step' is large; at some point we should
				      ;; switch to indexing.  But when depends on the size of the seq.
				      (dotimes (i (1- ,step)) (funcall ,iter-var ':get)))
				 `(funcall ,iter-var ':get))))
	  (test `(when (funcall ,iter-var ':done?) (go ,*loop-end*))))))
    (setq *loop-end-used?* t)
    (return-driver-code :next (list test setqs) :variable var)))


;;; ================ Accumulators ================

(defclause (collect-set expr &optional initial-value init-val into var-spec)
  "Collects values into a set, by default a `wb-set' ordered by `compare'.
Use `initial-value' to construct a different kind of set."
  (simple-fset-collect expr (or init-val '(wb-set)) var-spec 'with))

(defclause (collect-bag expr &optional initial-value init-val into var-spec)
  "Collects values into a bag, by default a `wb-bag' ordered by `compare'.
Use `initial-value' to construct a different kind of bag."
  (simple-fset-collect expr (or init-val '(wb-bag)) var-spec 'with))

(defclause (collect-bag val-expr count count-expr &optional initial-value init-val into var-spec)
  "Collects value/count pairs into a bag, by default a `wb-bag' ordered by
`compare'.  Use `initial-value' to construct a different kind of bag."
  (local-binding-check init-val)
  (let ((var-spec (or var-spec *result-var*))
	((var (extract-var var-spec))
	 ((op-expr `(with ,var ,(walk-expr val-expr) ,(walk-expr count-expr))))))
    (make-accum-var-binding var-spec (or init-val '(wb-bag)) nil :type nil)
    (return-code :body `((setq ,var ,op-expr)))))

;;; There's only one kind of seq, and I don't expect that to change soon.
(defclause (collect-seq expr &optional into var-spec)
  "Collects values into a seq."
  (simple-fset-collect expr '(wb-seq) var-spec 'with-last))

(defun simple-fset-collect (expr init-val var-spec accum-fn)
  (local-binding-check init-val)
  (let ((var-spec (or var-spec *result-var*))
	((var (extract-var var-spec))
	 ((op-expr `(,accum-fn ,var ,(walk-expr expr))))))
    (make-accum-var-binding var-spec init-val nil :type nil)
    (return-code :body `((setq ,var ,op-expr)))))

(defmacro collect-map (key-expr val-expr &rest args)
  "This macro exists for Iterate's code walker to expand it into an
actual clause."
  `(%collect-map ,key-expr -> ,val-expr ,@args))

;;; The arrow (or some keyword, anyway) is required by Iterate.  Be glad I didn't use Unicode `→' :-)
(defclause (%collect-map key-expr -> val-expr &optional initial-value init-val into var-spec)
  "Collects key/value pairs into a map, by default a `wb-map' ordered by
`compare'.  Use `initial-value' to construct a different kind of map."
  (local-binding-check init-val)
  (let ((var-spec (or var-spec *result-var*))
	((var (extract-var var-spec))
	  ((op-expr `(with ,var ,(walk-expr key-expr) ,(walk-expr val-expr))))))
    (make-accum-var-binding var-spec (or init-val '(wb-map)) nil :type nil)
    (return-code :body `((setq ,var ,op-expr)))))

(defmacro collect-map-to-sets (key-expr val-expr &rest args)
  "This macro exists for Iterate's code walker to expand it into an
actual clause."
  `(%collect-map-to-sets ,key-expr -> ,val-expr ,@args))

(defclause (%collect-map-to-sets key-expr -> val-expr &optional initial-value init-val into var-spec)
  "Collects key/value pairs into a map, collecting values for each key into a
set.  By default, the result a `wb-map' ordered by `compare', and the value
sets are `wb-sets' ordered by `compare'.  Use `initial-value' to construct a
different kind of map; the map's default must be a set \(or bag\)."
  (local-binding-check init-val)
  (let ((var-spec (or var-spec *result-var*))
	(key-var (fset::gensymx #:key-))
	((var (extract-var var-spec))
	 ((op-expr `(let ((,key-var ,(walk-expr key-expr)))
		      (with ,var ,key-var (with (lookup ,var ,key-var) ,(walk-expr val-expr))))))))
    (make-accum-var-binding var-spec (or init-val '(wb-map :default (wb-set))) nil :type nil)
    (return-code :body `((setq ,var ,op-expr)))))

;;; Shadows `iter:unioning'.  Since you're using FSet, you probably don't want CL's
;;; quadratic-time list union, although you'll still get that if your sets are lists.
;;; The `at' keyword of `iter:unioning' is not supported, however, and in the list
;;; case, there's no ordering guarantee such as `iter:unioning' makes.
(defclause (unioning expr &optional initial-value init-val into var-spec)
  "Unions sets/bags into a result set or bag.  If `init-val' is provided, the
result will be of that kind \(wrt implementation and ordering\).  Otherwise,
the result will be of the same kind as the first set/bag received, unless the
iteration terminates without executing this clause, in which case the result
will be the empty set of the default kind."
  (fset-reduce expr init-val var-spec 'union '(empty-set)))

(defclause (intersecting expr &optional initial-value init-val into var-spec)
  "Intersects sets/bags into a result set/bag.  If `initial-value' is provided,
the result will be of that kind (implementation and ordering); note that unless
you have a specific initial set you want to compute a subset of, you will
probably want to provide a complement set as the initial value \(see `full-set'
and `complement'\).  Otherwise, the result will be of the same kind as the
first set received, unless the iteration terminates without executing this
clause, in which case the result will be the full set of the default kind;
see `full-set'."
  (fset-reduce expr init-val var-spec 'intersection (full-set)))

;;; I don't see any point in accepting an initial value here, since there's
;;; only one kind of seq.
(defclause (concating expr &optional into var-spec)
  "Concatenates successive seqs into a result seq."
  (fset-reduce expr nil var-spec 'concat '(seq)))

(defclause (map-unioning expr &optional combining-with val-fn initial-value init-val into var-spec)
  "Map-unions maps into a result map (see `fset:map-union').  If `val-fn' is
provided, it is used to combine values corresponding to the same key; the
default is to overwrite the value from the earlier map with the later value.
If `init-val' is provided, the result will be of that kind \(wrt implementation
and ordering\).  Otherwise, the result will be of the same kind as the first map
received, unless the iteration terminates without executing this clause, in
which case the result will be the empty map of the default kind."
  (fset-reduce expr init-val var-spec `(lambda (m1 m2) (map-union m1 m2 . ,(and val-fn (list val-fn)))) '(map)))

;;; In the case of intersecting zero maps, there's nothing reasonable we could
;;; return, since there's no such thing as a "full map".  (There is a full
;;; binary relation, but it's not a function.)  So may as well just return nil.
;;; For the same reason, there's no point in accepting an initial value.
(defclause (map-intersecting expr &optional combining-with val-fn into var-spec)
  "Map-intersects maps into a result map (see `fset:map-intersection').  If
`val-fn' is provided, it is used to combine values corresponding to the same
key; the default is to overwrite the value from the earlier map with the later
value."
  (fset-reduce expr nil var-spec `(lambda (m1 m2) (map-intersection m1 m2 . ,(and val-fn (list val-fn)))) 'nil))

(defun fset-reduce (expr init-val var-spec reduce-fn default)
  ;; Technique borrowed from `reducing' clause in Iterate.  This way, the implementation
  ;; and organization of the result will be that of the first value of `expr', unless
  ;; `init-val' is provided.
  (if init-val
      (progn
	(local-binding-check init-val)
	(return-reduction-code :identity init-val :operation `',reduce-fn :external-op? t
			       :expression expr :test nil :variable var-spec :type nil
			       :accum-kind nil))
    (progn
      (let ((expr (walk-expr expr))
	    (var-spec (or var-spec *result-var*))
	    ((var (extract-var var-spec))
	     (entry (make-accum-var-binding var-spec default nil))
	     ((prev-first-time-var (third entry))
	      ((update-code first-time-var (if-1st-time `((setq ,var ,expr))
							`((setq ,var (,reduce-fn ,var ,expr)))))))))
	(when (null prev-first-time-var)
	  (setf (cddr entry) (list first-time-var)))
	(return-code :body (list update-code))))))

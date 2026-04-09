;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: macros.lisp
;;; Contents: Collected macros, separated out for compilation and coverage purposes
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; ================================================================================
;;; Miscellany

;;; A small notational convenience.  I want to give `gensym' an uninterned symbol (in a
;;; possibly futile effort to write case-mode-generic code), but it's not specified to
;;; call `string' on a symbol argument.
(defmacro gensymx (arg)
  `(gensym (string ',arg)))

(defmacro postincf (place &optional (delta 1))
  ;; I used to do the `get-setf-expansion' thing and save the value in a local, but this
  ;; seems to give better code, at least on SBCL on AMD64.
  `(1- (incf ,place ,delta)))

(defmacro n-values (n expr)
  "Forces the return of exactly `n' values from `expr'.  `n' must be a compile-time
constant."
  (assert (typep n 'fixnum))
  (let ((vars (gmap (:result list) (fn (_i) (gensym))
		    (:index 0 n))))
    `(multiple-value-bind ,vars ,expr
       (values . ,vars))))

(defmacro split-cases (preds &body body)
  "Duplicates `body' in each branch of a `cond' using `preds', a list of forms,
as the corresponding tests, and once more for the case where `preds' are all
false.  Useful when `body' can be optimized differently based on the value of
each predicate."
  `(cond ,@(mapcar (fn (pred)
		     `(,pred . ,body))
		   preds)
	 (t . ,body)))

(defmacro e-split-cases (preds &body body)
  "Duplicates `body' in each branch of a `cond' using `preds', a list of forms,
as the corresponding tests; signals an error the case where `preds' are all
false.  Useful when `body' can be optimized differently based on the value of
each predicate.  (The name should recall `ecase' etc.)"
  `(cond ,@(mapcar (fn (pred)
		     `(,pred . ,body))
		   preds)
	 (t (error "Fell through `e-split-cases'"))))

(defmacro split-string-cases ((var) &body body)
  "Assumes `var' is bound to a string; if feature `FSet-Ext-Strings' is set,
duplicates `body' in one context where `var' holds a simple-base-string, and
one where it doesn't.  If the feature is not set, this is just `progn'."
  (declare (ignorable var))
  #+fset-ext-strings
  `(if (typep ,var 'simple-base-string) (progn . ,body)
     (progn . ,body))
  #-fset-ext-strings
  `(progn . ,body))

(defmacro split-cases-on-var ((var . values) &body body)
  "For each of `values', if `var' is `eql' to it, binds it to that value
around `body'.  The `values' are unevaluated constants.  Useful when `body'
can be optimized differently depending on the value of `var'."
  (if (null (cdr values))
      `(let ((,var ',(car values)))
	 . ,body)
    `(if (eql ,var ',(car values))
	 (let ((,var ',(car values)))
	   . ,body)
       (split-cases-on-var (,var . ,(cdr values)) . ,body))))


;;; ================================================================================
;;; Macros related to order.lisp

(defmacro compare-slots (obj1 obj2 &rest accessors)
  "A handy macro for writing the bodies of `compare' methods and comparison
functions for user classes.  Returns the result of comparing the two objects
by comparing the results of calling each of `accessors', in order, on the
objects.  Despite the name, an accessor can actually be any function on the
class in question; it can also be a symbol, which will be used to access the
slot via `slot-value'.  For example, if class `frob' has accessor `frob-color'
and slot `id':

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots f1 f2 #'frob-color 'id))

At least on SBCL, it will be fastest to use slot names (e.g. 'id) on standard
classes, but accessor functions (e.g. #'frob-color) on structure classes.  If
you're writing a function instead of a method, declaring the type of the
parameters can also help.

By default, the values of a given accessor on the two objects will be compared
with `compare'.  To override this, you have two choices depending on whether
the function you want to call obeys the FSet comparison protocol (returning
`:equal', `:less', `:greater', or `:unequal') or is just a boolean predicate
\(like `cl:<'\).  In the first case, supply the accessor as a list
`\(:compare-fn ,acc ,compare-fn\) where `acc' is the accessor and `compare-fn'
is the name of the function to use; in the second case, use `:less-fn' instead
of `:compare-fn'.  For example:

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots f1 f2 (:compare-fn #'frob-color #'compare-colors)
                         (:less-fn 'id #'<)))

If the symbol `:eql' is supplied as the last accessor, then if the comparisons
by the other supplied accessors all return `:equal' but `obj1' and `obj2' are
not eql, this returns `:unequal'."
  (expand-compare-slots obj1 obj2 accessors t))

(defmacro compare-slots-no-unequal (obj1 obj2 &rest accessors)
  "A handy macro for writing the bodies of `compare' methods for user classes,
in the case when you know the comparison will never need to return `:unequal'
\(a case handled correctly by `compare-slots', but with a slight time cost\).
\[UPDATE: after improvements to `compare-slots', the performance difference is
now negligible, and `compare-slots' now supports `:compare-fn' and `:less-fn',
so this macro is now fairly pointless and is deprecated.  Just use
`compare-slots'.\]

Returns the result of comparing the two objects by comparing the results of
calling each of `accessors', in order, on the objects, using a nested call to
`compare'.  Despite the name, an accessor can actually be any function on the
class in question; it can also be a symbol, which will be used to access the
slot via `slot-value'.  For example, if class `frob' has accessor `frob-foo' and
slot `bar':

At least on SBCL, it will be fastest to use slot names (e.g. 'id) on standard
classes, but accessor functions (e.g. #'frob-color) on structure classes.

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots-no-unequal f1 f2 #'frob-foo 'bar))

By default, the values of a given accessor on the two objects will be compared
with `compare'.  To override this, you have two choices depending on whether
the function you want to call obeys the FSet comparison protocol (returning
`:equal', `:less', `:greater', or `:unequal') or is just a boolean predicate
\(like `cl:<'\).  In the first case, supply the accessor as a list
`\(:compare-fn ,acc ,compare-fn\) where `acc' is the accessor and `compare-fn'
is the name of the function to use; in the second case, use `:less-fn' instead
of `:compare-fn'.  For example:

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots f1 f2 (:compare-fn #'frob-color #'compare-colors)
                         (:less-fn 'id #'<)))

`:compare' (sic; not `:compare-fn') is a deprecated synonym of `:less-fn'."
  (expand-compare-slots obj1 obj2 accessors nil))

(defun expand-compare-slots (obj1 obj2 accessors check-unequal?)
  (let ((default-var (gensymx #:default-))
	(comp-var (gensymx #:comp-))
	(obj1-var (gensymx #:obj1-))
	(obj2-var (gensymx #:obj2-)))
    (labels ((rec (accs)
	       (if (or (null accs)
		       (and check-unequal? (eq (car accs) ':eql)
			    (or (null (cdr accs))
				(error "If ~S is supplied to ~S, it must be ~
					the last argument"
				       ':eql 'compare-slots))))
		   (if check-unequal? default-var '':equal)
		 (let ((acc (car accs)))
		   (if (and (listp acc) (member (car acc) '(:less-fn :compare)))
		       (let ((accval1-var (gensymx #:accval1-))
			     (accval2-var (gensymx #:accval2-)))
			 `(let ((,accval1-var ,(call (second acc) obj1-var))
				(,accval2-var ,(call (second acc) obj2-var)))
			    (if ,(call (third acc) accval1-var accval2-var)
				':less
			      (if ,(call (third acc) accval2-var accval1-var)
				  ':greater
				,(rec (cdr accs))))))
		     (let ((acc comp-fn (if (and (listp acc) (eq (car acc) ':compare-fn))
					    (values (second acc) (third acc))
					  (values acc '#'compare))))
		       `(let ((,comp-var ,(call comp-fn (call acc obj1-var) (call acc obj2-var))))
			  (if (or (eq ,comp-var ':less) (eq ,comp-var ':greater))
			      ,comp-var
			    (progn
			      ,@(and check-unequal?
				 `((when (eq ,comp-var ':unequal)
				     (setq ,default-var ':unequal))))
			      ,(rec (cdr accs))))))))))
	     (call (fn &rest args)
	       (cond ((and (listp fn)
			   (eq (car fn) 'function))
		      `(,(cadr fn) . ,args))
		     ((and (listp fn)
			   (eq (car fn) 'lambda))
		      `(,fn . ,args))
		     ((and (listp fn) (eq (car fn) 'fn))
		      (call (macroexpand fn) args))
		     ((and (listp fn)
			   (eq (car fn) 'quote)
			   (symbolp (cadr fn)))
		      `(slot-value ,(car args) ,fn))
		     (t `(funcall ,fn . ,args)))))
      `(let ((,obj1-var ,obj1)
	     (,obj2-var ,obj2)
	     . ,(and check-unequal? `((,default-var ,(if (member ':eql accessors) '':unequal '':equal)))))
	(if (eql ,obj1-var ,obj2-var) ':equal
	  ,(rec accessors))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deflex +master-type-ordering+ nil
    "Keeps track of the types for which explicit cross-comparison methods have
been generated, and against which subsequent such methods will be generated.
This is a list in reverse order."))

(defmacro define-generics (names parameter-list &body body)
  `(progn . ,(mapcar (fn (name)
		       `(defgeneric ,name ,parameter-list . ,body))
		     names)))

(defmacro define-methods (methods parameter-list &body body)
  `(progn . ,(mapcar (fn (method)
		       `(defmethod ,method ,parameter-list . ,body))
		     methods)))

(defmacro define-convert-methods (to-types parameter-list-rest &body body)
  "Defines a method on `convert' for each of `to-types'.  For each one, if it
is not in the `fset' package, and `parameter-list-rest' contains `default',
replaces `default' with the appropriate FSet 2 parameters, and binds `default'
around the body."
  `(progn . ,(mapcar (fn (to-type)
		       (let ((fset1? (eq (symbol-package to-type) (symbol-package 'collection)))
			     (dflt-pos (cl:position-if (fn (x) (or (eq x 'default)
								   (and (consp x) (eq (car x) 'default))))
						       parameter-list-rest))
			     ((dflt-param (and dflt-pos (nth dflt-pos parameter-list-rest)))
			      (params-rest (if (or fset1? (null dflt-pos))
					       parameter-list-rest
					     (append (cl:subseq parameter-list-rest 0 dflt-pos)
						     '((default nil default?) no-default?)
						     (cl:subseq parameter-list-rest (1+ dflt-pos)))))))
			 `(defmethod convert ,(cons `(to-type (eql ',to-type))
					            params-rest)
			    (let ,(and (not fset1?) dflt-pos
				       `((default (fset2-default default? default no-default?
								 . ,(and (consp dflt-param) `(,(cadr dflt-param)))))))
			      . ,body))))
		     to-types)))

;;; Handy macro to generate the cross-comparison methods.
(defmacro define-cross-type-compare-methods (type)
  "Generates cross-type comparison methods for `type' against the types on
which the macro has previously been invoked.  This macro is intended to be
invoked at the top level of a source file.  You should make sure that calls
to this macro are always compiled in the same order; if you don't, you could
possibly get a \"master type ordering out of sync\" error, at which point you
should delete all your fasls, restart your Lisp session, and recompile.
However, the implementation tries very hard to prevent this."
  (unless (symbolp type)
    (error "Type name required, not ~S" type))
  ;; Have to add it to the list, if it's not there, at both expansion time and
  ;; load time.
  (pushnew type +master-type-ordering+)
  (let ((types (member type +master-type-ordering+))
	((prev-types (cdr types))))
    `(progn
       (let ((mto-len (length +master-type-ordering+)))
	 (unless (if (< mto-len ,(length types))
		     (equal +master-type-ordering+
			    (cl:subseq ',prev-types (- ,(length prev-types) mto-len)))
		   (equal (cl:subseq +master-type-ordering+
				     (- mto-len ,(length types)))
			  ',types))
	   ;; This can happen if calls to this macro are compiled in a different
	   ;; order on different occasions.
	   (error "FSet master type ordering out of sync.~@
		   See fset::define-cross-type-compare-methods.")))
       (unless (member ',type +master-type-ordering+)
	 ;; You might think we would set it to the full expansion-time value,
	 ;; but that would cause problems if FSet is recompiled in a session
	 ;; in which this macro has been invoked on other types -- it would cause
	 ;; this fasl to contain symbols from those packages.
	 (setq +master-type-ordering+ ',types))
       . ,(cl:reduce #'append
		     (mapcar (lambda (type2)
			       `((defmethod compare ((a ,type2) (b ,type))
				   ':less)
				 (defmethod compare ((a ,type) (b ,type2))
				   ':greater)))
			     prev-types)
		     :from-end t))))


;;; ================================================================================
;;; Macros related to hash.lisp

(define-modify-macro logiorf (&rest args)
  logior)

(define-modify-macro logandc2f (arg-2)
  logandc2)

(define-modify-macro logxorf (&rest args)
  logxor)

(defmacro define-equality-slots (class &rest slots/accessors)
  "A handy macro for specifying which slots of the class determine whether
instances are equal.  Generates methods on `compare' and `hash-value'.
These methods call each of `slots/accessors' on the object(s); the results
are compared or hashed, recursively.  Comparison uses the slots or accessors
in the order provided.

For best performance (at least on SBCL), it is recommended to supply slot
names as symbols for standard classes -- these will turn into `slot-value'
forms -- but accessor names as functions for structure classes.  Arbitrary
functions on the class may also be supplied.

If the symbol `:eql' is supplied as the last accessor, then if the comparisons
by the other supplied accessors all return `:equal' but the arguments are not
eql, the generated `compare' method returns `:unequal'.

Examples:

  (defstruct point x y)
  (define-equality-slots point #'x #'y)

  (defclass part () ((part-number ...) ...))
  (define-equality-slots part 'part-number)

If you're using `define-class' from Misc-Extensions, you can just say:

  (define-class part ((part-number :equality ...) ...))"
  `(progn
     (defmethod compare ((a ,class) (b ,class))
       (compare-slots a b . ,slots/accessors))
     (defmethod hash-value ((x ,class))
       (hash-slots x . ,(if (eq (car (cl:last slots/accessors)) ':eql) (butlast slots/accessors)
			  slots/accessors)))))

(defmacro define-comparison-slots (class &rest slots/accessors)
  "Old name of `define-equality-slots'.  Deprecated."
  `(define-equality-slots ,class . ,slots/accessors))

;;; &&& Add `(:cache slot/acc)' syntax for auto-caching
(defmacro hash-slots (obj &rest slots/accessors)
  "A handy macro for writing the bodies of `hash-value' methods for user classes.
Computes a hash value by combining the hash values of the specified slots
and/or accessors applied to the object.  For standard classes, best performance
is gotten by supplying the slot names as \(quoted\) symbols; for structure
classes, it is best to supply accessor functions.  \(Actually, any function on
the object can be used.\)  For example:

  (defstruct point x y)
  (defmethod hash-value ((p point))
    (hash-slots p #'x #'y))

  (defclass part () ((part-number ...) ...))
  (defmethod hash-value ((p part))
    (hash-slots part 'part-number))

In most cases, it is better to use `define-equality-slots' instead of calling
this macro directly, to keep the `hash-value' and `compare' methods consistent."
  (unless slots/accessors
    (error "At least one slot/accessor must be supplied"))
  (let ((x-var (gensymx #:x-)))
    (rlabels `(let ((,x-var ,obj))
		,(rec `(hash-value-fixnum ,(call (car slots/accessors) x-var)) (cdr slots/accessors)))
      (rec (value accs)
	(if (null accs) value
	  (rec `(hash-mix (hash-multiply 17 ,value)
			  (hash-value-fixnum ,(call (car accs) x-var)))
	       (cdr accs))))
      (call (fn arg)
	;; Makes the expansion more readable, if nothing else
	(cond ((and (listp fn)
		    (eq (car fn) 'function))
	       `(,(cadr fn) ,arg))
	      ((and (listp fn)
		    (eq (car fn) 'lambda))
	       `(,fn ,arg))
	      ((and (listp fn) (eq (car fn) 'fn))
	       (call (macroexpand fn) arg))
	      ((and (listp fn)
		    (eq (car fn) 'quote)
		    (symbolp (cadr fn)))
	       `(slot-value ,arg ,fn))
	      (t `(funcall ,fn ,arg)))))))

;;; This incantation lets you use `:equality' as a slot option in `define-class',
;;; to specify the equality slots.
(add-define-class-extension ':equality 'define-class-equality-slots-extension)
(defun define-class-equality-slots-extension (class slots expanded-slot-specs)
  (dolist (slot slots)
    (unless (member ':constant (second (assoc slot expanded-slot-specs)))
      (error "Slot ~S marked as :equality; must also be :constant" slot)))
  `(define-equality-slots ,class . ,(mapcar (fn (x) `',x) slots)))


;;; ================================================================================
;;; Macros related to fset.lisp

;;; See note above "FSet-Use-Package-Locks" in `port.lisp'.
#+fset-use-package-locks
(progn
  (defmacro internal-with (&rest args)
    `(with . ,args))
  (defmacro internal-less (&rest args)
    `(less . ,args))
  (defmacro internal-union (&rest args)
    `(union . ,args))
  (defmacro internal-intersection (&rest args)
    `(intersection . ,args))
  (defmacro internal-set-difference (&rest args)
    `(set-difference . ,args))
  (defmacro internal-with-default (&rest args)
    `(with-default . ,args))
  (defmacro internal-without-default (&rest args)
    `(fset2:without-default . ,args))
  (defmacro internal-map-union (&rest args)
    `(map-union . ,args))
  (defmacro internal-map-intersection (&rest args)
    `(map-intersection . ,args))
  (defmacro internal-compose (&rest args)
    `(compose . ,args))
  (defmacro internal-first (&rest args)
    `(first . ,args))
  (defmacro internal-last (&rest args)
    `(last . ,args))
  (defmacro internal-with-first (&rest args)
    `(with-first . ,args))
  (defmacro internal-with-last (&rest args)
    `(with-last . ,args))
  (defmacro internal-less-first (&rest args)
    `(less-first . ,args))
  (defmacro internal-less-last (&rest args)
    `(less-last . ,args))
  (defmacro internal-concat (&rest args)
    `(concat . ,args))
  (defmacro internal-insert (&rest args)
    `(insert . ,args))
  (defmacro internal-splice (&rest args)
    `(splice . ,args)))

#-fset-use-package-locks
(progn
  (declaim (inline internal-with))
  (defun internal-with (coll x &optional (y nil y?))
    (if y? (with coll x y)
      (with coll x)))
  (declaim (inline internal-less))
  (defun internal-less (coll x &optional (y nil y?))
    (if y? (less coll x y)
      (less coll x)))
  (declaim (inline internal-union))
  (defun internal-union (x y)
    (union x y))
  (declaim (inline internal-intersection))
  (defun internal-intersection (x y)
    (intersection x y))
  (declaim (inline internal-set-difference))
  (defun internal-set-difference (x y)
    (set-difference x y))
  (declaim (inline internal-with-default))
  (defun internal-with-default (map val)
    (with-default map val))
  (declaim (inline internal-without-default))
  (defun internal-without-default (map)
    (fset2:without-default map))
  (declaim (inline internal-map-union))
  (defun internal-map-union (map1 map2 &optional (val-fn nil val-fn?))
    (if val-fn?
	(map-union map1 map2 val-fn)
      (map-union map1 map2)))
  (declaim (inline internal-map-intersection))
  (defun internal-map-intersection (map1 map2 &optional (val-fn nil val-fn?))
    (if val-fn?
	(map-intersection map1 map2 val-fn)
      (map-intersection map1 map2)))
  (declaim (inline internal-compose))
  (defun internal-compose (map1 map2-or-fn)
    (compose map1 map2-or-fn))
  (declaim (inline internal-first))
  (defun internal-first (seq)
    (first seq))
  (declaim (inline internal-last))
  (defun internal-last (seq)
    (last seq))
  (declaim (inline internal-with-first))
  (defun internal-with-first (seq val)
    (with-first seq val))
  (declaim (inline internal-with-last))
  (defun internal-with-last (seq val)
    (with-last seq val))
  (declaim (inline internal-less-first))
  (defun internal-less-first (seq)
    (less-first seq))
  (declaim (inline internal-less-last))
  (defun internal-less-last (seq)
    (less-last seq))
  (declaim (inline internal-concat))
  (defun internal-concat (seq1 &rest seqs)
    (apply #'concat seq1 seqs))
  (declaim (inline internal-insert))
  (defun internal-insert (seq idx val)
    (insert seq idx val))
  (declaim (inline internal-splice))
  (defun internal-splice (seq idx val)
    (splice seq idx val)))

;;; --------------------------------
;;; Modify macros

;;; The `internal-*' names are unexported versions, which we use here to prevent any
;;; possibility of unintentional capture by a `labels', `flet', or `macrolet' form
;;; in client code.

;;; `adjoinf' / `removef', which don't form a good pair, are now deprecated
;;; in favor of `includef' / `excludef'.
(define-modify-macro adjoinf (&rest item-or-tuple)
  internal-with
  "(adjoinf coll . args) --> (setf coll (with coll . args))")

(define-modify-macro removef (&rest item-or-tuple)
  internal-less
  "(removef coll . args) --> (setf coll (less coll . args))")

(define-modify-macro includef (&rest item-or-tuple)
  internal-with
  "(includef coll . args) --> (setf coll (with coll . args))")

(define-modify-macro excludef (&rest item-or-tuple)
  internal-less
  "(excludef coll . args) --> (setf coll (less coll . args))")

(define-modify-macro unionf (set)
  internal-union)

(define-modify-macro intersectf (set)
  internal-intersection)

;;; Not completely thrilled with these names, but nothing better occurs to me.
(define-modify-macro set-differencef (set)
  internal-set-difference)

(define-modify-macro fset2:clear-default ()
  internal-without-default)

;;; It might seem more natural to use `&optional' than `&rest', but then this macro would have to
;;; know the correct default value for `val-fn'; otherwise `define-modify-macro' would fill in `nil'.
(define-modify-macro map-unionf (map &rest stuff)
  internal-map-union)

;;; Ditto.
(define-modify-macro map-intersectf (map &rest stuff)
  internal-map-intersection)

(define-modify-macro imagef (fn)
  ximage)

(declaim (inline ximage))
(defun ximage (coll fn)
  (image fn coll))

(define-modify-macro map-imagef (fn)
  map-ximage)

(declaim (inline map-ximage))
(defun map-ximage (coll fn)
  (map-image fn coll))

(define-modify-macro updatef (fn &rest keys)
  xupdate)

(declaim (inline xupdate))
(defun xupdate (coll fn &rest keys)
  (apply #'update fn coll keys))

(define-modify-macro composef (fn)
  internal-compose)

(define-modify-macro push-first (val)
  internal-with-first
  "(push-first seq val) --> (setf seq (with-first seq val))")

(define-modify-macro push-last (val)
  internal-with-last
  "(push-last seq val) --> (setf seq (with-last seq val))")

(defmacro pop-first (seq &environment env)
  "Removes the first element from `seq' and returns it."
  (let ((vars vals new setter getter (get-setf-expansion seq env)))
    (unless (= 1 (length new))
      (error "Nonsensical `~A' form: ~S" 'pop-first `(pop-first ,seq)))
    `(let* (,@(mapcar #'list vars vals)
	    (,(car new) ,getter))
       (prog1
	 (internal-first ,(car new))
	 (setq ,(car new) (internal-less-first ,(car new)))
	 ,setter))))

(defmacro pop-last (seq &environment env)
  "Removes the last element from `seq' and returns it."
  (let ((vars vals new setter getter (get-setf-expansion seq env)))
    (unless (= 1 (length new))
      (error "Nonsensical `~A' form: ~S" 'pop-last `(pop-last ,seq)))
    `(let* (,@(mapcar #'list vars vals)
	    (,(car new) ,getter))
       (prog1
	 (internal-last ,(car new))
	 (setq ,(car new) (internal-less-last ,(car new)))
	 ,setter))))

(define-modify-macro appendf (seq)
  internal-concat)

(define-modify-macro prependf (seq)
  xconcat)

(define-modify-macro insertf (idx value)
  internal-insert)

(define-modify-macro splicef (idx subseq)
  internal-splice)

(declaim (inline xconcat))
(defun xconcat (seq1 seq2)
  (concat seq2 seq1))


;;; --------------------------------
;;; SETF expanders and `@'

(define-setf-expander lookup (collection key/index &environment env)
  "Functionally updates `collection' (a map or seq) to associate the value
being stored with the key (for a map) or index (for a seq).  That is, assigns
to the place holding `collection' a new collection which is the same except
for the value at `key/index'; does not modify the existing collection.
The `collection' subform must be `setf'able."
  (expand-setf-of-lookup 'lookup collection key/index env))
(define-setf-expander fset2:lookup (collection key/index &environment env)
  "Functionally updates `collection' (a map or seq) to associate the value
being stored with the key (for a map) or index (for a seq).  That is, assigns
to the place holding `collection' a new collection which is the same except
for the value at `key/index'; does not modify the existing collection.
The `collection' subform must be `setf'able."
  (expand-setf-of-lookup 'fset2:lookup collection key/index env))

(defun expand-setf-of-lookup (name collection key env)
  (let ((temps vals stores store-form access-form
	  (get-setf-expansion collection env))
	(key-temp (gensymx #:key-))
	(val-temp (gensymx #:val-))
	((coll-temp (car stores)))
	(lookup-fn (intern (string 'internal-lookup) (symbol-package name))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `~A'" name))
    (values (cons key-temp temps)
	    (cons key vals)
	    (list val-temp)
	    `(let ((,coll-temp (internal-with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(,lookup-fn ,access-form ,key-temp))))

(define-setf-expander default (collection &environment env)
  "Functionally updates the default of `collection' (a map or seq).  That is,
assigns to the place holding `collection' a new collection with the specified
default; does not modify the existing collection.  The `collection' subform
must be `setf'able."
  (let ((temps vals stores store-form access-form (get-setf-expansion collection env))
	(val-temp (gensymx #:val-))
	((coll-temp (car stores))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `default'"))
    (values temps vals (list val-temp)
	    `(let ((,coll-temp (internal-with-default ,access-form ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(default ,access-form))))

(defmacro @ (fn-or-collection &rest args)
  "A little hack with two purposes: (1) to make it easy to make FSet maps
behave like Lisp functions in certain contexts; and (2) to somewhat lessen the
pain of writing higher-order code in a two-namespace Lisp like Common Lisp.
The idea is that you can write `(@ fn arg)', and if `fn' is a Lisp function,
it will be funcalled on the argument; otherwise `lookup' (q.v.) will be called
on `fn' and `arg'.  To allow for `@' to be used in more contexts, it actually
can take any number of `args', though `lookup' always takes exactly two.  Thus
you can write `(@ fn arg1 arg2 ...)' when you just want a shorter name for
`funcall'.  As a matter of style, it is suggested that `@' be used only for
side-effect-free functions.  Also, though this doc string has spoken only of
FSet maps, `@' can be used with any type that `lookup' works on.  Can be used
with `setf', but only on collections, not functions, of course."
  (expand-@ '@ fn-or-collection args))
(defmacro fset2:@ (fn-or-collection &rest args)
  "A little hack with two purposes: (1) to make it easy to make FSet maps
behave like Lisp functions in certain contexts; and (2) to somewhat lessen the
pain of writing higher-order code in a two-namespace Lisp like Common Lisp.
The idea is that you can write `(@ fn arg)', and if `fn' is a Lisp function,
it will be funcalled on the argument; otherwise `lookup' (q.v.) will be called
on `fn' and `arg'.  To allow for `@' to be used in more contexts, it actually
can take any number of `args', though `lookup' always takes exactly two.  Thus
you can write `(@ fn arg1 arg2 ...)' when you just want a shorter name for
`funcall'.  As a matter of style, it is suggested that `@' be used only for
side-effect-free functions.  Also, though this doc string has spoken only of
FSet maps, `@' can be used with any type that `lookup' works on.  Can be used
with `setf', but only on collections, not functions, of course."
  (expand-@ 'fset2:@ fn-or-collection args))

(defun expand-@ (name fn-or-collection args)
  (if (= (length args) 1)
      (let ((fn-var (gensymx #:fn-))
	    (arg-var (gensymx #:arg-))
	    (lookup-fn (intern (string 'internal-lookup) (symbol-package name))))
	`(let ((,fn-var ,fn-or-collection)
	       (,arg-var ,(car args)))
	   (if (functionp ,fn-var)
	       (funcall ,fn-var ,arg-var)
	     (,lookup-fn ,fn-var ,arg-var))))
    `(funcall ,fn-or-collection . ,args)))

;;; Have to do the same thing for `@', since `setf' would not know what to
;;; do with its normal expansion.
(define-setf-expander @ (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (expand-setf-of-lookup '@ collection key env))
(define-setf-expander fset2:@ (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (expand-setf-of-lookup 'fset2:@ collection key env))

(declaim (inline internal-lookup))
(defun internal-lookup (&rest args)
  (apply #'lookup args))
(declaim (inline fset2::internal-lookup))
(defun fset2::internal-lookup (&rest args)
  (apply #'fset2:lookup args))


;;; --------------------------------
;;; Defining hash functions for comparison functions

(defmacro define-hash-function (compare-fn-name hash-fn-name)
  "Specifies the hash function that hash-based collections are to use
for a given comparison function."
  (check-type compare-fn-name symbol)
  (check-type hash-fn-name symbol)
  `(setf (get ',compare-fn-name 'hash-function) ',hash-fn-name))


;;; --------------------------------
;;; Iteration

(defmacro do-set ((var set &optional value) &body body)
  "For each element of `set', binds `var' to it and executes `body'.  When done,
returns `value'."
  `(block nil		; in case `body' contains `(return ...)'
     (let ((elt-fn #'(lambda (,var) . ,body))
	   (value-fn #'(lambda () ,value)))
       ;; SBCL evidently figures this out without our help, but other implementations may benefit.
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-set ,set elt-fn value-fn))))


(defmacro do-bag-pairs ((value-var mult-var bag &optional value)
			&body body)
  "For each element of `bag', binds `value-var' and `mult-var' to the element and
its multiplicity respectively, and executes `body'.  When done, returns `value'."
  `(block nil
     (let ((elt-fn #'(lambda (,value-var ,mult-var) . ,body))
	   (value-fn #'(lambda () ,value)))
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-bag-pairs ,bag elt-fn value-fn))))

(defmacro do-bag ((value-var bag &optional value)
		  &body body)
  "For each element of `bag', binds `value-var' to it and and executes `body' a
number of times equal to the element's multiplicity.  When done, returns `value'."
  (let ((mult-var (gensymx #:mult-))
	(idx-var (gensymx #:idx-)))
    `(block nil
       (let ((elt-fn #'(lambda (,value-var ,mult-var)
			  ;; Seems safe to assume it's a fixnum here.
			  (declare (type fixnum ,mult-var))
			  (dotimes (,idx-var ,mult-var)
			    (declare (type fixnum ,idx-var))
			    . ,body)))
	     (value-fn #'(lambda () ,value)))
	 (declare (dynamic-extent elt-fn value-fn))
	 (internal-do-bag-pairs ,bag elt-fn value-fn)))))

(defmacro do-map ((key-var value-var map &optional value) &body body)
  "For each pair of `map', binds `key-var' and `value-var' and executes `body'.
When done, returns `value'."
  `(block nil
     (let ((elt-fn #'(lambda (,key-var ,value-var) . ,body))
	   (value-fn #'(lambda () ,value)))
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-map ,map elt-fn value-fn))))

(defmacro do-map-domain ((key-var map &optional value) &body body)
  "For each pair of `map', binds `key-var' and executes `body'.  When done,
returns `value'."
  (let ((value-var (gensymx #:val-)))
    `(block nil
       (let ((elt-fn #'(lambda (,key-var ,value-var)
			  (declare (ignore ,value-var))
			  . ,body))
	     (value-fn #'(lambda () ,value)))
	 (declare (dynamic-extent elt-fn value-fn))
	 (internal-do-map ,map elt-fn value-fn)))))

(defmacro do-seq ((var seq
		   &key (start nil start?) (end nil end?) (from-end? nil from-end??)
		   (index nil index?) (value nil))
		  &body body)
  "For each element of `seq', possibly restricted by `start' and `end', and in
reverse order if `from-end?' is true, binds `var' to it and executes `body'.
If `index' is supplied, it names a variable that will be bound at each
iteration to the index of the current element of `seq'.  When done, returns
`value'."
  ;; Because we have only one implementation, we don't need to go through `internal-do-seq'
  ;; unless they're using the `index' feature.  Skipping `internal-do-seq' saves us about 20%
  ;; on one micro-benchmark.
  (if index?
      `(block nil
	 (let ((elt-fn #'(lambda (,var . ,(and index? `(,index)))
			   ,@(and index? `((declare (fixnum ,index))))
			   . ,body))
	       (value-fn #'(lambda () ,value)))
	   (declare (dynamic-extent elt-fn value-fn))
	   (internal-do-seq ,seq elt-fn value-fn ,index?
			    ,@(and start? `(:start ,start))
			    ,@(and end? `(:end ,end))
			    ,@(and from-end?? `(:from-end? ,from-end?)))))
    `(do-wb-seq-tree-members-gen (,var (wb-seq-contents ,seq) ,(if start? start 0) ,(if end? end `(size ,seq))
				       ,(and from-end?? from-end?) ,value)
       . ,body)))

(defmacro do-seq-chunks ((var seq &optional value) &body body)
  "Internally, a seq is represented as a tree whose leaves are vectors, each
either a `simple-vector' or a `simple-string'.  For each such vector in left-
to-right order, binds `var' to it and executes `body'."
  `(block nil
     (let ((vec-fn (fn (,var) . ,body))
	   (value-fn (fn () ,value)))
       (declare (dynamic-extent vec-fn value-fn))
       (internal-do-seq-chunks ,seq vec-fn value-fn))))

(defmacro do-2-relation ((key val br &optional value) &body body)
  "Enumerates all pairs of the relation `br', binding them successively to `key' and `val'
and executing `body'."
  `(block nil
     (internal-do-2-relation ,br (lambda (,key ,val) . ,body)
			     (lambda () ,value))))

(defmacro do-list-relation ((tuple rel &optional value) &body body)
  `(block nil
     (internal-do-list-relation ,rel (lambda (,tuple) . ,body)
				(lambda () ,value))))

(defmacro do-elements ((var iterable &optional value) &body body)
  "Here `iterable' is any object for which there is a method on `iterator', q.v.
Calls `iterator' on `iterable', passing no keyword arguments, to obtain an
iterator.  Binds `var' to the successive values returned by the iterator and
executes `body'.  When done, returns `value'.

\(To pass keyword arguments to the `iterator' call, use `do-iterator'.\)"
  (let ((it-var (gensymx #:it-)))
    `(do ((,it-var (iterator ,iterable)))
	 ((funcall ,it-var ':done?)
	  ,value)
       (let ((,var (funcall ,it-var :get)))
	 . ,body))))

(defmacro do-iterator ((var iter &optional value) &body body)
  "Here `iter' is any object that conforms to the FSet stateful iterator
protocol; these are normally constructed by calling `iterator', q.v.
Binds `var' to the successive values returned by the iterator and executes
`body'.  When done, returns `value'."
  (let ((it-var (gensymx #:it-)))
    `(do ((,it-var ,iter))
	 ((funcall ,it-var ':done?)
	  ,value)
       (let ((,var (funcall ,it-var :get)))
	 . ,body))))

(defmacro do-pair-iterator ((var-a var-b iter &optional value) &body body)
  "Here `iter' is any object that conforms to the FSet stateful iterator
protocol; these are normally constructed by calling `iterator', q.v.
Binds `var-a' and `var-b' to the successive pairs returned by the iterator
as two values, and executes `body'.  When done, returns `value'."
  (let ((it-var (gensymx #:it-)))
    `(do ((,it-var ,iter))
	 ((funcall ,it-var ':done?)
	  ,value)
       (let ((,var-a ,var-b (funcall ,it-var :get)))
	 . ,body))))


;;; --------------------------------
;;; GMap argument and result type definitions

;;; I've moved these here so I can use them in more of the code.

;;; The "old syntax" calls `gmap:def-gmap-arg-type' and `gmap:def-gmap-res-type' define
;;; both the name as supplied and the same name in the keyword package.  I have kept these
;;; in cases where I had already published FSet releases that used them, so as not to
;;; break people's code.  However, they are deprecated; all new definitions should use
;;; `gmap:def-arg-type' and `gmap:def-result-type', which do not define the keyword names.

;;; NOTE: these must never invoke constructor macros (`set', `ch-set', etc.), because
;;; those might not be defined yet while compiling FSet itself.

;;; ----------------
;;; Generic

(gmap:def-gmap-arg-type sequence (seq)
  "Yields the elements of `seq', which can be of any CL sequence type as well
as an FSet seq, or a set or bag as well."
  `((the function (iterator ,seq))
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

(gmap:def-arg-type iterator (it)
  `(,it
    #'(lambda (it) (declare (type function it)) (funcall it ':done?))
    #'(lambda (it) (declare (type function it)) (funcall it ':get))))

;;; The new (for 1.4.7) functional iterators all take a `:from-end?' option.
;;; Other than that, there's no reason to use them with `gmap'; they're slower
;;; (though not as much slower as you might expect; less than 2x) and generate
;;; lots of garbage.  They were fun to write, though :-)
(gmap:def-arg-type fun-sequence (fun-iterable &key from-end?)
  "Yields the elements of `fun-iterable', which can be an FSet seq, set, or bag.
If `:from-end?' is true, iterates in reverse order."
  `((the function (fun-iterator ,fun-iterable :from-end? ,from-end?))
    #'(lambda (it) (funcall it ':empty?))
    #'(lambda (it) (funcall it ':first))
    #'(lambda (it) (funcall it ':rest))))

;;; ----------------
;;; Sets

(gmap:def-gmap-arg-type set (set)
  "Yields the elements of `set'."
  `((the function (iterator ,set))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))
    nil nil nil
    (do-set ,set)))

(gmap:def-arg-type-synonym fset2:set set)

(gmap:def-arg-type-synonym fun-set fun-sequence)

;;; The only reason to use the functional iterators in `gmap' forms is that you want
;;; to iterate in reverse order for some reason.  But I do want to test them.
(gmap:def-arg-type fun-set (set &key from-end?)
  `((fun-iterator ,set :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    #'(lambda (it) (funcall it ':first))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-result-type set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (s x) (wb-set-tree-with s x #'compare)) #'make-wb-set ,filterp))

;;; Faster than `set', if you know it's a `wb-set'.
(gmap:def-gmap-arg-type wb-set (set)
  "Yields the elements of `set'."
  `((make-wb-set-tree-iterator-internal (wb-set-contents ,set))
    #'wb-set-tree-iterator-done?
    #'wb-set-tree-iterator-get
    nil nil nil
    (do-wb-set-tree-members (wb-set-contents ,set))))

(gmap:def-gmap-res-type wb-set (&key filterp compare-fn-name)
  "Returns a set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is supplied, it specifies a custom ordering."
  (let ((org-var (gensymx #:org-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (wb-set-tree-with s x ,cf-var))
	  #'(lambda (s)
	      (if (eq ,cf-var #'compare)
		  (make-wb-set s)
		(make-wb-custom-set s ,org-var)))
	  ,filterp
	  ((,org-var (wb-set-org (empty-wb-set ,compare-fn-name)))
	   ((,cf-var (tree-set-org-compare-fn ,org-var)))))))

;;; Faster than `set', if you know it's a `ch-set'.
(gmap:def-arg-type ch-set (set)
  `((make-ch-set-tree-iterator-internal (ch-set-contents ,set))
    #'ch-set-tree-iterator-done?
    #'ch-set-tree-iterator-get
    nil nil nil
    (do-ch-set-tree-members (ch-set-contents ,set))))

(gmap:def-result-type fset2:set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `((make-transient (fset2:empty-set))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-result-type ch-set (&key filterp compare-fn-name)
  "Returns a set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is supplied, it specifies a custom ordering."
  `((make-transient (empty-ch-set ,compare-fn-name))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-gmap-res-type union (&key filterp)
  "Returns the union of the sets, optionally filtered by `filterp'."
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (s x) (if (null s) x (union s x)))
	#'(lambda (s) (or s (ch-set)))
	,filterp))

(gmap:def-gmap-res-type intersection (&key filterp)
  "Returns the intersection of the sets, optionally filtered by `filterp'.
\(If zero sets are supplied, returns the full set; see `full-set'.\)"
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (s x) (if (null s) x (intersection s x)))
	#'(lambda (s) (or s (full-set)))
	,filterp))

;;; ----------------
;;; Bags

(gmap:def-gmap-arg-type bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((the function (iterator ,bag))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))
    nil nil nil
    (do-bag ,bag)))

(gmap:def-arg-type-synonym fset2:bag bag)

(gmap:def-gmap-arg-type bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((the function (iterator ,bag :pairs? t))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))
    nil nil nil
    (do-bag-pairs ,bag)))

(gmap:def-arg-type-synonym fset2:bag-pairs bag-pairs)

(gmap:def-gmap-arg-type wb-bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((make-wb-bag-tree-iterator-internal (wb-bag-contents ,bag))
    #'wb-bag-tree-iterator-done?
    #'wb-bag-tree-iterator-get))  ; no `Do-WB-Bag-Tree-Elements'

(gmap:def-gmap-arg-type wb-bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((make-wb-bag-tree-pair-iterator-internal (wb-bag-contents ,bag))
    #'wb-bag-tree-pair-iterator-done?
    (:values 2 #'wb-bag-tree-pair-iterator-get)
    nil nil nil
    (do-wb-bag-tree-pairs (wb-bag-contents ,bag))))

(gmap:def-arg-type-synonym fun-bag fun-sequence)

(gmap:def-arg-type fun-bag-pairs (bag &key from-end?)
  `((fun-iterator ,bag :pairs? t :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-gmap-res-type bag (&key filterp)
  "Returns a bag of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (b x) (wb-bag-tree-with b x #'compare))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil
    (:consume 2 #'(lambda (b x n) (wb-bag-tree-with b x #'compare n)))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type wb-bag (&key filterp compare-fn-name)
  "Returns a wb-bag of the values, optionally filtered by `filterp'.  To use a
non-default comparison function in the result, supply `compare-fn-name`."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (wb-bag-tree-with s x ,cf-var))
	  #'(lambda (s) (make-wb-bag s (wb-bag-org ,proto-var)))
	  ,filterp
	  ((,proto-var (empty-wb-bag ,compare-fn-name))
	   ((,cf-var (tree-set-org-compare-fn (wb-bag-org ,proto-var))))))))

(gmap:def-gmap-res-type wb-bag-pairs (&key filterp compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-bag of the pairs.
Note that `filterp', if supplied, must take two arguments.  To use a non-default
comparison function in the result, supply `compare-fn-name`."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil (:consume 2 #'(lambda (tree x n) (wb-bag-tree-with tree x ,cf-var n)))
	  #'(lambda (tree) (make-wb-bag tree (wb-bag-org ,proto-var)))
	  ,filterp
	  ((,proto-var (empty-wb-bag ,compare-fn-name))
	   ((,cf-var (tree-set-org-compare-fn (wb-bag-org ,proto-var))))))))

(gmap:def-arg-type ch-bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  ;; I haven't written an internal non-pair ch-bag iterator.  OTOH, there's a driver.
  (let ((elt-var (gensymx #:elt-))
	(n-var (gensymx #:n-)))
    `((make-ch-bag-tree-pair-iterator-internal (ch-bag-contents ,bag))
      (fn (it) (and (zerop ,n-var)
		    (ch-bag-tree-pair-iterator-done? it)))
      (fn (it)
	(when (zerop ,n-var)
	  (let ((elt mult (ch-bag-tree-pair-iterator-get it)))
	    (setq ,elt-var elt)
	    (setq ,n-var mult)))
	(decf ,n-var)
	,elt-var)
      nil
      ((,elt-var nil)
       (,n-var 0))
      nil
      (do-bag ,bag)
      nil)))

(gmap:def-arg-type ch-bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((make-ch-bag-tree-pair-iterator-internal (ch-bag-contents ,bag))
    #'ch-bag-tree-pair-iterator-done?
    (:values 2 #'ch-bag-tree-pair-iterator-get)
    nil nil nil
    (do-ch-bag-tree-pairs (ch-bag-contents ,bag))))

(gmap:def-result-type fset2:bag (&key filterp)
  "Returns a bag of the values, optionally filtered by `filterp'."
  `((make-transient (fset2:empty-bag))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-result-type ch-bag (&key filterp compare-fn-name)
  "Returns a ch-bag of the values, optionally filtered by `filterp'.  To use a
non-default comparison function in the result, supply `compare-fn-name'."
  `((make-transient (empty-ch-bag ,compare-fn-name))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-result-type fset2:bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `((make-transient (empty-ch-bag))
    (:consume 2 #'include!)
    #'make-persistent
    ,filterp))

(gmap:def-result-type ch-bag-pairs (&key filterp compare-fn-name)
  "Consumes two values from the mapped function; returns a ch-bag of the pairs.
Note that `filterp', if supplied, must take two arguments.  To use a non-default
comparison function in the result, supply `compare-fn-name'."
  `((make-transient (empty-ch-bag ,compare-fn-name))
    (:consume 2 #'include!)
    #'make-persistent
    ,filterp))

(gmap:def-gmap-res-type bag-sum (&key filterp)
  "Returns the bag-sum of the values, optionally filtered by `filterp'."
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (b x) (if (null b) x (bag-sum b x)))
	#'(lambda (b) (or b (ch-bag)))
	,filterp))

(gmap:def-gmap-res-type bag-product (&key filterp)
  "Returns the bag-product of the values, optionally filtered by `filterp'.
Signals an error if no bags are supplied \(there's nothing reasonable it could
return in this case\)."
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (b x) (if (null b) x (bag-product b x)))
	#'(lambda (b) (or b (full-set)))
	,filterp))

;;; ----------------
;;; Maps

(gmap:def-gmap-arg-type map (map)
  "Yields each pair of `map', as two values."
  `((the function (iterator ,map))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))
    nil nil nil
    (do-map ,map)))

(gmap:def-arg-type-synonym fset2:map map)

(gmap:def-gmap-arg-type wb-map (map)
  "Yields each pair of `map', as two values."
  `((make-wb-map-tree-iterator-internal (wb-map-contents ,map))
    #'wb-map-tree-iterator-done?
    (:values 2 #'wb-map-tree-iterator-get)
    nil nil nil
    (do-wb-map-tree-pairs (wb-map-contents ,map))))

(gmap:def-arg-type-synonym fset2:wb-map wb-map)

(gmap:def-arg-type fun-map (map &key from-end?)
  `((fun-iterator ,map :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-result-type map (&key filterp default)
  "Consumes two values from the mapped function; returns a map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (m x y) (wb-map-tree-with m x y #'compare #'compare)))
	#'(lambda (tree) (make-wb-map tree +fset-default-tree-map-org+ ,default))
	,filterp))

(gmap:def-result-type fset2:map (&key filterp (default nil default?) no-default?)
  "Consumes two values from the mapped function; returns a map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `((make-transient (fset2:empty-map ,@(and default? `(:default ,default))
				     ,@(and no-default? `(:no-default? ,no-default?))))
    (:consume 2 #'include!)
    #'make-persistent
    ,filterp))

(gmap:def-result-type wb-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((proto-var (gensymx #:prototype-))
	(kcf-var (gensymx #:key-cmp-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 #'(lambda (tree k v) (wb-map-tree-with tree k v ,kcf-var ,vcf-var)))
	  #'(lambda (tree) (make-wb-map tree (wb-map-org ,proto-var) ,default))
	  ,filterp
	  ((,proto-var (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name))
	   ((,kcf-var (tree-map-org-key-compare-fn (wb-map-org ,proto-var)))
	    (,vcf-var (tree-map-org-val-compare-fn (wb-map-org ,proto-var))))))))

(gmap:def-result-type fset2:wb-map (&key filterp (default nil default?) no-default?
					 key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((proto-var (gensymx #:prototype-))
	(kcf-var (gensymx #:key-cmp-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 #'(lambda (tree k v) (wb-map-tree-with tree k v ,kcf-var ,vcf-var)))
	  #'(lambda (tree) (make-wb-map tree (wb-map-org ,proto-var)
					(fset2-default ,default? ,default ,no-default?)))
	  ,filterp
	  ((,proto-var (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name))
	   ((,kcf-var (tree-map-org-key-compare-fn (wb-map-org ,proto-var)))
	    (,vcf-var (tree-map-org-val-compare-fn (wb-map-org ,proto-var))))))))

(gmap:def-arg-type ch-map (map)
  "Yields each pair of `map', as two values."
  `((make-ch-map-tree-iterator-internal (ch-map-contents ,map))
    #'ch-map-tree-iterator-done?
    (:values 2 #'ch-map-tree-iterator-get)
    nil nil nil
    (do-ch-map-tree-pairs (ch-map-contents ,map))))

(gmap:def-arg-type-synonym fset2:ch-map ch-map)

(gmap:def-result-type ch-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `((make-transient (empty-ch-map ,default ,key-compare-fn-name ,val-compare-fn-name))
    (:consume 2 #'include!)
    #'make-persistent
    ,filterp))

(gmap:def-result-type fset2:ch-map (&key filterp (default nil default?) no-default?
					 key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `((make-transient (fset2:empty-ch-map ,@(and default? `(:default ,default))
					,@(and no-default? `(:no-default? ,no-default?))
					:key-compare-fn-name ,key-compare-fn-name
					:val-compare-fn-name ,val-compare-fn-name))
    (:consume 2 #'include!)
    #'make-persistent
    ,filterp))

(gmap:def-gmap-res-type map-union (&key (val-fn nil val-fn?) (default nil default?) filterp)
  "Returns the map-union of the values, optionally filtered by `filterp'.  If `val-fn'
is supplied, it is passed to `map-union' (q.v.).  If `default' is supplied, it is used
as the map default."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
       #'(lambda (prev-m m)
	   (if (null prev-m) m
	     ,(if val-fn? `(map-union prev-m m ,val-fn-var) '(map-union prev-m m))))
       ,(and default? `#'(lambda (m) (with-default (or m (empty-wb-map)) ,default)))
       ,filterp
       (,@(and val-fn? `((,val-fn-var ,val-fn)))))))

(gmap:def-result-type fset2:map-union (&key (val-fn nil val-fn?) (default nil default?) no-default? filterp)
  "Returns the map-union of the values, optionally filtered by `filterp'.  If `val-fn'
is supplied, it is passed to `map-union' (q.v.).  If `default' is supplied, it is used
as the map default."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
       #'(lambda (prev-m m)
	   (if (null prev-m) m
	     ,(if val-fn? `(map-union prev-m m ,val-fn-var) '(map-union prev-m m))))
       ,(and default? `#'(lambda (m) (with-default (or m (empty-ch-map)) (fset2-default t ,default ,no-default?))))
       ,filterp
       (,@(and val-fn? `((,val-fn-var ,val-fn)))))))

(gmap:def-gmap-res-type map-intersection (&key (val-fn nil val-fn?) (default nil default?) filterp)
  "Returns the map-intersection of the values, optionally filtered by `filterp'.
If `val-fn' is supplied, it is passed to `map-intersection' (q.v.).  If
`default' is supplied, it is used as the map default.  If zero maps are
intersected, returns nil."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
      #'(lambda (prev-m m)
	  (if (null prev-m) m
	    ,(if val-fn? `(map-intersection prev-m m ,val-fn-var) '(map-intersection prev-m m))))
      ,(and default? `#'(lambda (m) (and m (with-default m ,default))))
      ,filterp
      (,@(and val-fn? `((,val-fn-var ,val-fn)))))))

(gmap:def-result-type fset2:map-intersection (&key (val-fn nil val-fn?) (default nil default?) no-default? filterp)
  "Returns the map-intersection of the values, optionally filtered by `filterp'.
If `val-fn' is supplied, it is passed to `map-intersection' (q.v.).  If
`default' is supplied, it is used as the map default.  If zero maps are
intersected, returns nil."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
      #'(lambda (prev-m m)
	  (if (null prev-m) m
	    ,(if val-fn? `(map-intersection prev-m m ,val-fn-var) '(map-intersection prev-m m))))
      ,(and default? `#'(lambda (m) (and m (with-default m (fset2-default t ,default ,no-default?)))))
      ,filterp
      (,@(and val-fn? `((,val-fn-var ,val-fn)))))))

(gmap:def-result-type map-to-sets (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function.  Returns a map from the first
values, with each one mapped to a set of the corresponding second values.
Note, if you supply `val-compare-fn-name', to customize the ordering of the
range sets, the returned map will use `eql-compare' as its `val-compare-fn'.
Also note that `filterp', if supplied, must take two arguments."
  (let ((vcfn-var (gensymx #:vcfn-)))
    `((empty-wb-map (empty-wb-set ,vcfn-var) ,key-compare-fn-name (and ,vcfn-var 'eql-compare))
      (:consume 2 #'(lambda (m x y) (with m x (with (lookup m x) y))))
      nil ,filterp
      ((,vcfn-var ,val-compare-fn-name)))))
(gmap:def-result-type fset2:map-to-sets (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function.  Returns a map from the first
values, with each one mapped to a set of the corresponding second values.
Note, if you supply `val-compare-fn-name', to customize the ordering of the
range sets, the returned map will use `eql-compare' as its `val-compare-fn'.
Also note that `filterp', if supplied, must take two arguments."
  `((make-transient (fset2:empty-ch-2-relation :key-compare-fn-name ,key-compare-fn-name
					       :val-compare-fn-name ,val-compare-fn-name))
    (:consume 2 #'include!)
    (fn (m) (convert 'map-to-sets (make-persistent m)))
    ,filterp))


;;; ----------------
;;; Seqs

(gmap:def-arg-type-synonym seq wb-seq)

(gmap:def-arg-type-synonym fset2:seq wb-seq)
;;; Grandfather in the old syntax
(gmap:def-arg-type-synonym :seq wb-seq)

(gmap:def-arg-type-synonym fset2:wb-seq wb-seq)

(gmap:def-gmap-arg-type wb-seq (seq &key start end from-end?)
  "Yields the elements of `seq'.  The keyword parameters `start' and `end'
can be supplied to restrict the range of the iteration; `start' is inclusive
and defaults to 0, while `end' is exclusive and defaults to the size of the
seq.  If `from-end?' is true, the elements will be yielded in reverse order."
  (let ((seq-var (gensymx #:seq-)))
    (cond ((null from-end?)
	   `((make-wb-seq-tree-iterator-internal (wb-seq-contents ,seq-var) ,start ,end)
	     #'wb-seq-tree-iterator-done?
	     #'wb-seq-tree-iterator-get
	     nil
	     ((,seq-var ,seq))
	     nil
	     (do-wb-seq-tree-members-gen (wb-seq-contents ,seq-var)
	       ,(or start 0) ,(or end `(size ,seq-var)) nil)
	     ((,seq-var ,seq))))
	  ((eq from-end? 't)
	   `((make-wb-seq-tree-rev-iterator-internal (wb-seq-contents ,seq-var) ,start ,end)
	     #'wb-seq-tree-rev-iterator-done?
	     #'wb-seq-tree-rev-iterator-get
	     nil
	     ((,seq-var ,seq))
	     nil
	     (do-wb-seq-tree-members-gen (wb-seq-contents ,seq-var)
	       (or ,start 0) (or ,end (size ,seq-var)) t)
	     ((,seq-var ,seq))))
	  (t
	   ;; Can't do much better than this if we don't statically know the direction.
	   `((the function (iterator ,seq :start ,start :end ,end :from-end? ,from-end?))
	     #'(lambda (it) (funcall it ':done?))
	     #'(lambda (it) (funcall it ':get)))))))

(gmap:def-arg-type-synonym fset2:wb-seq wb-seq)

(gmap:def-result-type-synonym seq wb-seq)

(gmap:def-result-type wb-seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(gmap:def-result-type-synonym fset2:seq fset2:wb-seq)

(gmap:def-result-type fset2:wb-seq (&key filterp default no-default?)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'fset2:seq (nreverse s)
			   ,@(and default `(:default ,default))
			   ,@(and no-default? `(:no-default? ,no-default?))))
    ,filterp))

(gmap:def-result-type concat (&key filterp)
  "Returns the concatenation of the seq values, optionally filtered by `filterp'."
  `((empty-seq) #'concat nil ,filterp))

;;; ----------------
;;; Tuples

(gmap:def-gmap-arg-type tuple (tuple)
  "Yields each pair of `tuple', as two values."
  `((convert 'list ,tuple)
    #'null
    (:values 2 #'(lambda (al) (values (caar al) (cdar al))))
    #'cdr
    nil nil
    (do-tuple ,tuple)))

(gmap:def-arg-type-synonym dyn-tuple tuple)

(gmap:def-gmap-res-type tuple (&key filterp)
  `((empty-dyn-tuple) (:consume 2 #'tuple-with) nil ,filterp))

(gmap:def-result-type-synonym dyn-tuple tuple)

;;; ----------------
;;; Relations

(gmap:def-arg-type 2-relation (rel)
  "Yields each pair of `rel', as two values."
  `((the function (iterator ,rel))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))
    nil nil nil
    (do-2-relation ,rel)))

(gmap:def-arg-type-synonym wb-2-relation 2-relation)
(gmap:def-arg-type-synonym ch-2-relation 2-relation)

;;; People might expect it under this name.
(gmap:def-arg-type-synonym fun-2-relation fun-map)

(gmap:def-result-type-synonym 2-relation ch-2-relation)

(gmap:def-result-type wb-2-relation (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((org-var (gensymx #:org-))
	(size-var (gensymx #:size-))
	(kcf-var (gensymx #:key-cmp-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 #'(lambda (tree k v)
			  (let ((ignore prev (wb-map-tree-lookup tree k ,kcf-var))
				((new (wb-set-tree-with prev v ,vcf-var))))
			    (declare (ignore ignore))
			    (if (eq prev new) tree
			      (progn
				(incf ,size-var)
				(wb-map-tree-with tree k new ,kcf-var #'eql-compare))))))
	  #'(lambda (tree) (make-wb-2-relation ,size-var tree nil ,org-var))
	  ,filterp
	  ((,org-var (wb-2-relation-org (empty-wb-2-relation ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,kcf-var (tree-map-org-key-compare-fn ,org-var))
	     (,vcf-var (tree-map-org-val-compare-fn ,org-var)))
	   (,size-var 0)))))

(gmap:def-result-type ch-2-relation (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a ch-2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((org-var (gensymx #:org-))
	(size-var (gensymx #:size-))
	(kcf-var (gensymx #:key-cmp-))
	(khf-var (gensymx #:key-hash-))
	(vcf-var (gensymx #:val-cmp-))
	(vhf-var (gensymx #:val-hash-)))
    `(nil (:consume 2 #'(lambda (tree k v)
			  (let ((ignore prev (ch-map-tree-lookup tree k ,khf-var ,kcf-var))
				((new (ch-set-tree-with prev v ,vhf-var ,vcf-var))))
			    (declare (ignore ignore))
			    (if (eq prev new) tree
			      (progn
				(incf ,size-var)
				(ch-map-tree-with tree k new ,khf-var ,kcf-var
						  #'ch-set-tree-hash-value #'eql-compare))))))
	  #'(lambda (tree) (make-ch-2-relation ,size-var tree nil ,org-var))
	  ,filterp
	  ((,org-var (ch-2-relation-org (empty-ch-2-relation ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,kcf-var (hash-map-org-key-compare-fn ,org-var))
	    (,khf-var (hash-map-org-key-hash-fn ,org-var))
	    (,vcf-var (hash-map-org-val-compare-fn ,org-var))
	    (,vhf-var (hash-map-org-val-hash-fn ,org-var)))
	   (,size-var 0)))))

(gmap:def-arg-type list-relation (rel)
  `((the function (iterator (convert 'set ,rel)))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-arg-type wb-list-relation (rel)
  `((make-wb-set-tree-iterator-internal (wb-list-relation-tuples ,rel))
    #'wb-set-tree-iterator-done?
    #'wb-set-tree-iterator-get
    nil nil nil
    (do-wb-set-tree-members (wb-list-relation-tuples ,rel))))

(gmap:def-arg-type ch-list-relation (rel)
  `((make-ch-set-tree-iterator-internal (ch-list-relation-tuples ,rel))
    #'ch-set-tree-iterator-done?
    #'ch-set-tree-iterator-get
    nil nil nil
    (do-ch-set-tree-members (ch-list-relation-tuples ,rel))))


;;; ----------------
;;; Replay sets

;;; Faster than `set', if you know it's a `replay-set'.
(gmap:def-arg-type replay-set (set)
  "Yields the elements of `set'."
  `((make-wb-seq-tree-iterator-internal (replay-set-ordering ,set))
    #'wb-seq-tree-iterator-done?
    #'wb-seq-tree-iterator-get
    nil nil nil
    (do-wb-seq-tree-members (replay-set-ordering ,set))))

;;; CHAMP is the default now.
(gmap:def-result-type replay-set (&key filterp)
  "Returns a replay-set of the values, optionally filtered by `filterp'."
  `((make-transient (empty-ch-replay-set))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-result-type wb-replay-set (&key filterp compare-fn-name)
  "Returns a wb-replay-set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering for the internal
set, though, of course, this doesn't affect the iteration order."
  ;; I don't care enough about this now to rewrite it to use internal trees.
  `((empty-wb-replay-set ,compare-fn-name)
    #'with
    nil
    ,filterp))

(gmap:def-result-type ch-replay-set (&key filterp compare-fn-name)
  "Returns a ch-replay-set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering for the internal
set, though, of course, this doesn't affect the iteration order."
  `((make-transient (empty-ch-replay-set ,compare-fn-name))
    #'include!
    #'make-persistent
    ,filterp))

(gmap:def-result-type append-unique ()
  "Returns a list of the unique elements of the lists returned by the
mapped function, in the order in which they were first encountered."
  `((empty-ch-replay-set)
    #'(lambda (rs new-elts)
	(dolist (x new-elts)
	  (includef rs x))
	rs)
    #'(lambda (rs) (convert 'list rs))))

;;; ----------------
;;; Replay maps

(gmap:def-arg-type replay-map (map)
  "Yields each pair of `map', as two values."
  (let ((map-var (gensymx #:map-)))
    `((make-wb-seq-tree-iterator-internal (replay-map-ordering ,map-var))
      #'wb-seq-tree-iterator-done?
      (:values 2 #'(lambda (it) (let ((key key? (wb-seq-tree-iterator-get it)))
				  (if (not key?)
				      (values nil nil)
				    (values key (let ((val val? (lookup ,map-var key)))
						  (assert val? () "Bug in `replay-map' GMap arg type")
						  val))))))
      nil
      ((,map-var ,map)))))

(gmap:def-result-type replay-map (&key filterp default)
  "Consumes two values from the mapped function; returns a replay-map of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  (let ((ordering-var (gensymx #:ordering-)))
    `(nil (:consume 2 #'(lambda (s k v)
			  (let ((ts (ch-map-tree-with s k v #'hash-value #'compare #'hash-value #'compare)))
			    (unless (= (ch-map-tree-size ts) (ch-map-tree-size s))
			      (push k ,ordering-var))
			    ts)))
	  #'(lambda (s) (make-ch-replay-map s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    +fset-default-hash-map-org+ ,default))
	  ,filterp
	  ((,ordering-var nil)))))

(gmap:def-result-type wb-replay-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-replay-map of the
pairs.  Note that `filterp', if supplied, must take two arguments.  If
`key-compare-fn-name' and/or `val-compare-fn-name' are nonnull, they specify a
custom organization for the internal map, though, of course, this doesn't affect
the iteration order."
  (let ((org-var (gensymx #:org-))
	(kcf-var (gensymx #:kcmp-))
	(vcf-var (gensymx #:vcmp-))
	(ordering-var (gensymx #:ordering-)))
    `(nil (:consume 2 #'(lambda (s k v)
			  (let ((ts (wb-map-tree-with s k v ,kcf-var ,vcf-var)))
			    (unless (= (wb-map-tree-size ts) (wb-map-tree-size s))
			      (push k ,ordering-var))
			    ts)))
	  #'(lambda (s) (make-wb-replay-map s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    ,org-var ,default))
	  ,filterp
	  ((,org-var (wb-map-org (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,kcf-var (tree-map-org-key-compare-fn ,org-var))
	    (,vcf-var (tree-map-org-val-compare-fn ,org-var)))
	   (,ordering-var nil)))))

(gmap:def-result-type ch-replay-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a ch-replay-map of the
pairs.  Note that `filterp', if supplied, must take two arguments.  If
`key-compare-fn-name' and/or `val-compare-fn-name' are nonnull, they specify a
custom organization for the internal map, though, of course, this doesn't affect
the iteration order."
  (let ((org-var (gensymx #:org-))
	(khf-var (gensymx #:khash-))
	(kcf-var (gensymx #:kcmp-))
	(vhf-var (gensymx #:vhash-))
	(vcf-var (gensymx #:vcmp-))
	(ordering-var (gensymx #:ordering-)))
    `(nil (:consume 2 #'(lambda (s k v)
			  (let ((ts (ch-map-tree-with s k v ,khf-var ,kcf-var ,vhf-var ,vcf-var)))
			    (unless (= (ch-map-tree-size ts) (ch-map-tree-size s))
			      (push k ,ordering-var))
			    ts)))
	  #'(lambda (s) (make-ch-replay-map s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    ,org-var ,default))
	  ,filterp
	  ((,org-var (ch-map-org (empty-ch-map nil ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,khf-var (hash-map-org-key-hash-fn ,org-var))
	    (,kcf-var (hash-map-org-key-compare-fn ,org-var))
	    (,vhf-var (hash-map-org-val-hash-fn ,org-var))
	    (,vcf-var (hash-map-org-val-compare-fn ,org-var)))
	   (,ordering-var nil)))))


;;; --------------------------------
;;; For internal use only

(defmacro check-two-arguments (arg2? op type)
  `(when ,arg2?
     (error 'simple-program-error
	    :format-control "~A on a ~A takes only two arguments"
	    :format-arguments (list ,(copy-tree op) ,(copy-tree type)))))

(defmacro check-three-arguments (arg2? op type)
  `(unless ,arg2?
     (error 'simple-program-error
	    :format-control "~A on a ~A takes three arguments"
	    :format-arguments (list ,(copy-tree op) ,(copy-tree type)))))

(defmacro equal?-fn (cmp-fn)
  `(lambda (a b) (or (gen eql a b) (eq (funcall ,cmp-fn a b) ':equal))))

(defmacro equal?-cmp (a b cmp-fn)
  (once-only (a b)
    `(or (gen eql ,a ,b)
	 (eq (funcall ,cmp-fn ,a ,b) ':equal))))

(defmacro less-than?-cmp (a b cmp-fn)
  `(eq (funcall ,cmp-fn ,a ,b) ':less))

(defmacro greater-than?-cmp (a b cmp-fn)
  `(eq (funcall ,cmp-fn ,a ,b) ':greater))

(defmacro define-wb-set-method (name param-list &body body)
  (let ((doc-string decls body (parse-body body)))
    `(progn
       (defmethod ,name ,(change-specializer-class param-list 'wb-set 'wb-default-set)
	 ,@(and doc-string (list doc-string))
	 ,@decls
	 (macrolet ((contents (coll)
		      `(wb-set-contents ,coll))
		    (compare-fn (coll)
		      (declare (ignore coll))
		      '#'compare)
		    ;; Done this way so we don't get unreachable-code warnings for the `else' branch.
		    (if-same-compare-fns ((coll1 coll2) then else)
			(declare (ignore coll1 coll2 else))
		      then)
		    (make (like-coll contents)
		      (declare (ignore like-coll))
		      `(make-wb-set ,contents)))
	   . ,body))
       (defmethod ,name ,(change-specializer-class param-list 'wb-set 'wb-custom-set)
	 ,@(and doc-string (list doc-string))
	 ,@decls
	 (macrolet ((contents (coll)
		      `(wb-custom-set-contents ,coll))
		    (compare-fn (coll)
		      `(wb-set-compare-fn ,coll))
		    (if-same-compare-fns ((coll1 coll2) then else)
		        ;; I originally thought I would compare the names instead of the function objects.
			;; That would have had the advantage that merely recompiling the function without
			;; changing it (as tends to happen during development) would still allow use of
			;; the O(n) algorithms.  The downside, of course, would have been that if you did
			;; actually change the function, things would have broken badly.  This way, changing
			;; the function could cause reduced performance, but correctness is maintained
			;; (unless you're depending on how `compare' orders two sets).
			`(if (eq (wb-set-compare-fn ,coll1)
				 (wb-set-compare-fn ,coll2))
			     ,then
			   ,else))
		    (make (like-coll contents)
		      `(make-wb-custom-set ,contents (wb-custom-set-org ,like-coll))))
	   . ,body)))))

(defmacro define-wb-set-methods (methods param-list &body body)
  `(progn . ,(mapcar (fn (method)
		       `(define-wb-set-method ,method ,param-list . ,body))
		     methods)))

(defmacro define-wb-seq-method (name param-list &body body)
  (let ((doc-string decls body (parse-body body)))
    `(progn
       (defmethod ,name ,param-list
	 ,@(and doc-string (list doc-string))
	 ,@decls
	 (macrolet ((call-selected (norm-fn ht-fn &rest args)
		      (declare (ignore ht-fn))
		      `(,norm-fn . ,args)))
	   . ,body))
       (defmethod ,name ,(change-specializer-class param-list 'wb-seq 'wb-ht-seq)
	 ,@(and doc-string (list doc-string))
	 ,@decls
	 (macrolet ((call-selected (norm-fn ht-fn &rest args)
		      (declare (ignore norm-fn))
		      `(,ht-fn . ,args)))
	   . ,body)))))

(defmacro define-wb-seq-methods (methods param-list &body body)
  `(progn . ,(mapcar (fn (method)
		       `(define-wb-seq-method ,method ,param-list . ,body))
		     methods)))

(defun parse-body (body)
  (let ((decls nil)
	(doc-string body (if (stringp (car body)) (values (car body) (cdr body))
			   (values nil body))))
    (while (and (listp (car body)) (eq (caar body) 'declare))
      (push (pop body) decls))
    (values doc-string (nreverse decls) body)))

(defun change-specializer-class (param-list from-class to-class)
  (do ((param-list param-list (cdr param-list))
       (new-pl nil))
      ((or (null param-list) (member (car param-list) '(&optional &key &rest)))
       (revappend new-pl param-list))
    (let ((plelt (car param-list)))
      (push (if (and (listp plelt) (eq (cadr plelt) from-class))
		`(,(car plelt) ,to-class)
	      plelt)
	    new-pl))))

(defmacro if-same-ch-set-orgs ((s1 s2 hsorg-var) then else)
  (let ((s1-var (gensymx #:s1-))
	(s2-var (gensymx #:s2-))
	(hsorg1-var (gensymx #:hsorg1-))
	(hsorg2-var (gensymx #:hsorg2-)))
    `(let ((,s1-var ,s1)
	   (,s2-var ,s2)
	   ((,hsorg1-var (ch-set-org ,s1-var))
	    (,hsorg2-var (ch-set-org ,s2-var))))
       (if (or (eq ,hsorg1-var ,hsorg2-var)
	       (and (eq (hash-set-org-hash-fn ,hsorg1-var) (hash-set-org-hash-fn ,hsorg2-var))
		    (eq (hash-set-org-compare-fn ,hsorg1-var) (hash-set-org-compare-fn ,hsorg2-var))))
	   (let ((,hsorg-var ,hsorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-wb-bag-orgs ((b1 b2 tsorg-var) then else)
  (let ((b1-var (gensymx #:b1-))
	(b2-var (gensymx #:b2-))
	(tsorg1-var (gensymx #:tsorg1-))
	(tsorg2-var (gensymx #:tsorg2-)))
    `(let ((,b1-var ,b1)
	   (,b2-var ,b2)
	   ((,tsorg1-var (wb-bag-org ,b1-var))
	    (,tsorg2-var (wb-bag-org ,b2-var))))
       (if (or (eq ,tsorg1-var ,tsorg2-var)
	       (eq (tree-set-org-compare-fn ,tsorg1-var) (tree-set-org-compare-fn ,tsorg2-var)))
	   (let ((,tsorg-var ,tsorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-ch-bag-orgs ((s1 s2 hsorg-var) then else)
  (let ((s1-var (gensymx #:s1-))
	(s2-var (gensymx #:s2-))
	(hsorg1-var (gensymx #:hsorg1-))
	(hsorg2-var (gensymx #:hsorg2-)))
    `(let ((,s1-var ,s1)
	   (,s2-var ,s2)
	   ((,hsorg1-var (ch-bag-org ,s1-var))
	    (,hsorg2-var (ch-bag-org ,s2-var))))
       (if (or (eq ,hsorg1-var ,hsorg2-var)
	       (and (eq (hash-set-org-hash-fn ,hsorg1-var) (hash-set-org-hash-fn ,hsorg2-var))
		    (eq (hash-set-org-compare-fn ,hsorg1-var) (hash-set-org-compare-fn ,hsorg2-var))))
	   (let ((,hsorg-var ,hsorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-wb-map-orgs ((m1 m2 tmorg-var) then else)
  (let ((m1-var (gensymx #:m1-))
	(m2-var (gensymx #:m2-))
	(tmorg1-var (gensymx #:tmorg1-))
	(tmorg2-var (gensymx #:tmorg2-)))
    `(let ((,m1-var ,m1)
	   (,m2-var ,m2)
	   ((,tmorg1-var (wb-map-org ,m1-var))
	    (,tmorg2-var (wb-map-org ,m2-var))))
       (if (or (eq ,tmorg1-var ,tmorg2-var)
	       (and (eq (tree-map-org-key-compare-fn ,tmorg1-var) (tree-map-org-key-compare-fn ,tmorg2-var))
		    (eq (tree-map-org-val-compare-fn ,tmorg1-var) (tree-map-org-val-compare-fn ,tmorg2-var))))
	   (let ((,tmorg-var ,tmorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-ch-map-orgs ((m1 m2 hmorg-var) then else)
  (let ((m1-var (gensymx #:m1-))
	(m2-var (gensymx #:m2-))
	(hmorg1-var (gensymx #:hmorg1-))
	(hmorg2-var (gensymx #:hmorg2-)))
    `(let ((,m1-var ,m1)
	   (,m2-var ,m2)
	   ((,hmorg1-var (ch-map-org ,m1-var))
	    (,hmorg2-var (ch-map-org ,m2-var))))
       (if (or (eq ,hmorg1-var ,hmorg2-var)
	       (and (eq (hash-map-org-key-hash-fn ,hmorg1-var) (hash-map-org-key-hash-fn ,hmorg2-var))
		    (eq (hash-map-org-key-compare-fn ,hmorg1-var) (hash-map-org-key-compare-fn ,hmorg2-var))
		    (eq (hash-map-org-val-hash-fn ,hmorg1-var) (hash-map-org-val-hash-fn ,hmorg2-var))
		    (eq (hash-map-org-val-compare-fn ,hmorg1-var) (hash-map-org-val-compare-fn ,hmorg2-var))))
	   (let ((,hmorg-var ,hmorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-wb-2-relation-orgs ((rel1 rel2 tmorg-var) then else)
  (let ((rel1-var (gensymx #:rel1-))
	(rel2-var (gensymx #:rel2-))
	(tmorg1-var (gensymx #:tmorg1-))
	(tmorg2-var (gensymx #:tmorg2-)))
    `(let ((,rel1-var ,rel1)
	   (,rel2-var ,rel2)
	   ((,tmorg1-var (wb-2-relation-org ,rel1-var))
	    (,tmorg2-var (wb-2-relation-org ,rel2-var))))
       (if (or (eq ,tmorg1-var ,tmorg2-var)
	       (and (eq (tree-map-org-key-compare-fn ,tmorg1-var) (tree-map-org-key-compare-fn ,tmorg2-var))
		    (eq (tree-map-org-val-compare-fn ,tmorg1-var) (tree-map-org-val-compare-fn ,tmorg2-var))))
	   (let ((,tmorg-var ,tmorg1-var))
	     ,then)
	 ,else))))

(defmacro if-same-ch-2-relation-orgs ((rel1 rel2 hmorg-var) then else)
  (let ((rel1-var (gensymx #:rel1-))
	(rel2-var (gensymx #:rel2-))
	(hmorg1-var (gensymx #:hmorg1-))
	(hmorg2-var (gensymx #:hmorg2-)))
    `(let ((,rel1-var ,rel1)
	   (,rel2-var ,rel2)
	   ((,hmorg1-var (ch-2-relation-org ,rel1-var))
	    (,hmorg2-var (ch-2-relation-org ,rel2-var))))
       (if (or (eq ,hmorg1-var ,hmorg2-var)
	       (and (eq (hash-map-org-key-hash-fn ,hmorg1-var) (hash-map-org-key-hash-fn ,hmorg2-var))
		    (eq (hash-map-org-key-compare-fn ,hmorg1-var) (hash-map-org-key-compare-fn ,hmorg2-var))
		    (eq (hash-map-org-val-hash-fn ,hmorg1-var) (hash-map-org-val-hash-fn ,hmorg2-var))
		    (eq (hash-map-org-val-compare-fn ,hmorg1-var) (hash-map-org-val-compare-fn ,hmorg2-var))))
	   (let ((,hmorg-var ,hmorg1-var))
	     ,then)
	 ,else))))


;;; Boilerplate macros.  The free references to and bindings of fixed names are intentional.

(defmacro convert-to-wb-set (coll identity-test &body contents-forms)
  (let ((body `(let ((tree (progn . ,contents-forms)))
		 (if (eq compare-fn #'compare)
		     (make-wb-default-set tree)
		   (make-wb-custom-set tree (wb-custom-set-org prototype))))))
    `(let ((prototype (empty-wb-set compare-fn-name))
	   ((compare-fn (wb-set-compare-fn prototype))))
       ,(if identity-test
	    `(if ,identity-test ,coll ,body)
	  body))))

(defmacro convert-to-wb-bag (coll identity-test &body contents-forms)
  (let ((body `(let ((tree (progn . ,contents-forms)))
		 (make-wb-bag tree (wb-bag-org prototype)))))
    `(let ((prototype (empty-wb-bag compare-fn-name))
	   ((compare-fn (tree-set-org-compare-fn (wb-bag-org prototype)))))
       ,(if identity-test
	    `(if ,identity-test ,coll ,body)
	  body))))

(defmacro convert-to-wb-map (coll default identity-test &body contents-forms)
  (let ((body `(let ((tree (progn . ,contents-forms)))
		 (make-wb-map tree tmorg ,default))))
    `(let ((prototype (empty-wb-map nil key-compare-fn-name val-compare-fn-name))
	   ((tmorg (wb-map-org prototype))
	    ((key-compare-fn (tree-map-org-key-compare-fn tmorg))
	     (val-compare-fn (tree-map-org-val-compare-fn tmorg)))))
       ,(if identity-test
	    `(if ,identity-test ,coll ,body)
	  body))))

(defmacro convert-to-wb-2-relation (coll default identity-test &body contents-forms)
  (let ((body `(let ((tree0 tree1 (progn . ,contents-forms)))
		 (make-wb-2-relation tree0 tree1 tmorg ,default))))
    `(let ((prototype (empty-wb-map nil key-compare-fn-name val-compare-fn-name))
	   ((tmorg (wb-map-org prototype))
	    ((key-compare-fn (tree-map-org-key-compare-fn tmorg))
	     (val-compare-fn (tree-map-org-val-compare-fn tmorg)))))
       ,(if identity-test
	    `(if ,identity-test ,coll ,body)
	  body))))

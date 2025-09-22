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


;;; ================================================================================
;;; Macros related to order.lisp

;;; Makes it easy to define `compare' methods on new classes.  Just say:
;;;
;;; (defmethod compare ((f1 frob) (f2 frob))
;;;   (compare-slots f1 f2 'foo #'frob-bar))
;;;
;;; where `foo' is a slot and `frob-bar' is an accessor (or any other
;;; function on your class).
;;;
;;; If you want distinct instances to never compare `:equal', put `:eql'
;;; at the end of the accessor list to specify that `eql' is the final
;;; determiner of equality for your type:
;;;
;;; (defmethod compare ((f1 frob) (f2 frob))
;;;   (compare-slots f1 f2 'foo #'frob-bar :eql))
;;;

(defmacro compare-slots (obj1 obj2 &rest accessors)
  "A handy macro for writing the bodies of `compare' methods for user classes.
Returns the result of comparing the two objects by comparing the results of
calling each of `accessors', in order, on the objects.  Despite the name, an
accessor can actually be any function on the class in question; it can also
be a symbol, which will be used to access the slot via `slot-value'.  For
example, if class `frob' has accessor `frob-foo' and slot `bar':

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots f1 f2 #'frob-foo 'bar))

If the symbol `:eql' is supplied as the last accessor, then if the comparisons
by the other supplied accessors all return `:equal' but `obj1' and `obj2' are
not eql, this returns `:unequal'."
  (let ((default-var (gensymx #:default-))
	(comp-var (gensymx #:comp-))
	(obj1-var (gensymx #:obj1-))
	(obj2-var (gensymx #:obj2-)))
    (labels ((rec (accs)
	       (if (or (null accs)
		       (and (eq (car accs) ':eql)
			    (or (null (cdr accs))
				(error "If ~S is supplied to ~S, it must be ~
					the last argument"
				       ':eql 'compare-slots))))
		   default-var
		 `(let ((,comp-var (compare ,(call (car accs) obj1-var)
					    ,(call (car accs) obj2-var))))
		    (if (or (eq ,comp-var ':less) (eq ,comp-var ':greater))
			,comp-var
		      (let ((,default-var (if (eq ,comp-var ':unequal)
					      ':unequal ,default-var)))
			,(rec (cdr accs)))))))
	     (call (fn arg)
	       ;; Makes the expansion more readable, if nothing else
	       (cond ((and (listp fn)
			   (eq (car fn) 'function))
		      `(,(cadr fn) ,arg))
		     ((and (listp fn)
			   (eq (car fn) 'lambda))
		      `(,fn ,arg))
		     ((and (listp fn)
			   (eq (car fn) 'quote)
			   (symbolp (cadr fn)))
		      `(slot-value ,arg ,fn))
		     (t `(funcall ,fn ,arg)))))
      `(let ((,obj1-var ,obj1)
	     (,obj2-var ,obj2)
	     (,default-var ,(if (member ':eql accessors) '':unequal '':equal)))
	(if (eql ,obj1-var ,obj2-var) ':equal
	    ,(rec accessors))))))

(defmacro compare-slots-no-unequal (obj1 obj2 &rest accessors)
  "A handy macro for writing the bodies of `compare' methods for user classes,
in the case when you know the comparison will never need to return `:unequal'
(a case handled correctly by `compare-slots', but with a slight time cost).

Returns the result of comparing the two objects by comparing the results of
calling each of `accessors', in order, on the objects, using a nested call to
`compare'.  Despite the name, an accessor can actually be any function on the
class in question; it can also be a symbol, which will be used to access the
slot via `slot-value'.  For example, if class `frob' has accessor `frob-foo' and
slot `bar':

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots-no-unequal f1 f2 #'frob-foo 'bar))

Additionally, an accessor can be a list of the form `(:compare acc less-fn)', in
which `acc` is an accessor as defined above, and `less-fn' is a function to be
used to compare the two values, returning true iff the first is less than the
second.  This feature allows you to avoid the nested call to `compare'.  For
example, if your objects have an `id' slot that holds a unique integer:

  (defmethod compare ((f1 frob) (f2 frob))
    (compare-slots-no-unequal f1 f2 (:compare 'id #'<))"
  (let ((comp-var (gensymx #:comp-))
	(obj1-var (gensymx #:obj1-))
	(obj2-var (gensymx #:obj2-)))
    (labels ((rec (accs)
	       (if (null accs)
		   ':equal
		 (if (null (cdr accs))
		     (comp (car accs))
		   `(let ((,comp-var ,(comp (car accs))))
		      (if (or (eq ,comp-var ':less) (eq ,comp-var ':greater))
			  ,comp-var
			,(rec (cdr accs)))))))
	     (comp (acc)
	       (if (and (listp acc) (eq (car acc) ':compare))
		   (let ((accval1-var (gensymx #:accval1-))
			 (accval2-var (gensymx #:accval2-)))
		     `(let ((,accval1-var ,(call (second acc) obj1-var))
			    (,accval2-var ,(call (second acc) obj2-var)))
			(if ,(call (third acc) accval1-var accval2-var)
			    ':less
			  (if ,(call (third acc) accval2-var accval1-var)
			      ':greater
			    ':equal))))
		 `(compare ,(call acc obj1-var) ,(call acc obj2-var))))
	     (call (fn &rest args)
	       ;; Makes the expansion more readable, if nothing else
	       (cond ((and (listp fn)
			   (eq (car fn) 'function))
		      `(,(cadr fn) . ,args))
		     ((and (listp fn)
			   (eq (car fn) 'lambda))
		      `(,fn . ,args))
		     ((and (null (cdr args))
			   (listp fn)
			   (eq (car fn) 'quote)
			   (symbolp (cadr fn)))
		      `(slot-value ,(car args) ,fn))
		     (t `(funcall ,fn . ,args)))))
      `(let ((,obj1-var ,obj1)
	     (,obj2-var ,obj2))
	 (if (eq ,obj1-var ,obj2-var) ':equal
	     ,(rec accessors))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deflex +Master-Type-Ordering+ nil
    "Keeps track of the types for which explicit cross-comparison methods have
been generated, and against which subsequent such methods will be generated.
This is a list in reverse order."))

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
  (pushnew type +Master-Type-Ordering+)
  (let ((types (member type +Master-Type-Ordering+))
	((prev-types (cdr types))))
    `(progn
       (let ((mto-len (length +Master-Type-Ordering+)))
	 (unless (if (< mto-len ,(length types))
		     (equal +Master-Type-Ordering+
			    (cl:subseq ',prev-types (- ,(length prev-types) mto-len)))
		   (equal (cl:subseq +Master-Type-Ordering+
				     (- mto-len ,(length types)))
			  ',types))
	   ;; This can happen if calls to this macro are compiled in a different
	   ;; order on different occasions.
	   (error "FSet master type ordering out of sync.~@
		   See fset::define-cross-type-compare-methods.")))
       (unless (member ',type +Master-Type-Ordering+)
	 ;; You might think we would set it to the full expansion-time value,
	 ;; but that would cause problems if FSet is recompiled in a session
	 ;; in which this macro has been invoked on other types -- it would cause
	 ;; this fasl to contain symbols from those packages.
	 (setq +Master-Type-Ordering+ ',types))
       . ,(cl:reduce #'append
		     (mapcar (lambda (type2)
			       `((defmethod compare ((a ,type2) (b ,type))
				   ':less)
				 (defmethod compare ((a ,type) (b ,type2))
				   ':greater)))
			     prev-types)))))


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
by the other supplied accessors all return `:equal' but `obj1' and `obj2' are
not eql, the generated `compare' method returns `:unequal'.

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
       (hash-slots x . ,(if (eq (last slots/accessors) ':eql) (butlast slots/accessors)
			  slots/accessors)))))

(defmacro define-comparison-slots (class &rest slots/accessors)
  "Old name of `define-equality-slots'.  Deprecated."
  `(define-equality-slots ,class . ,slots/accessors))

;;; &&& Add `(:cache slot/acc)' syntax for auto-caching
(defmacro hash-slots (obj &rest slots/accessors)
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
#+FSet-Use-Package-Locks
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
    `(insert . ,args)))

#-FSet-Use-Package-Locks
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
    (insert seq idx val)))

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

(declaim (inline xconcat))
(defun xconcat (seq1 seq2)
  (concat seq2 seq1))


;;; --------------------------------
;;; SETF expanders and `@'

(define-setf-expander lookup (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (let ((temps vals stores store-form access-form
	  (get-setf-expansion collection env))
	(key-temp (gensymx #:key-))
	(val-temp (gensymx #:val-))
	((coll-temp (car stores))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `lookup'"))
    (values (cons key-temp temps)
	    (cons key vals)
	    (list val-temp)
	    `(let ((,coll-temp (internal-with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(internal-lookup ,access-form ,key-temp))))

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
  (if (= (length args) 1)
      (let ((fn-var (gensymx #:fn-))
	    (arg-var (gensymx #:arg-)))
	`(let ((,fn-var ,fn-or-collection)
	       (,arg-var ,(car args)))
	   (if (functionp ,fn-var)
	       (funcall ,fn-var ,arg-var)
	     (internal-lookup ,fn-var ,arg-var))))
    `(funcall ,fn-or-collection . ,args)))

;;; Have to do the same thing for `@', since `setf' would not know what to
;;; do with its normal expansion.
(define-setf-expander @ (collection key &environment env)
  "Adds a pair to a map or updates an existing pair, or adds an element to a
sequence or updates an existing element.  This does NOT modify the map or
sequence; it modifies the place (generalized variable) HOLDING the map or
sequence (just like `(setf (ldb ...) ...)').  That is, the `collection' subform
must be `setf'able itself."
  (let ((temps vals stores store-form access-form
	  (get-setf-expansion collection env))
	(key-temp (gensymx #:key-))
	(val-temp (gensymx #:val-))
	((coll-temp (car stores))))
    (when (cdr stores)
      (error "Too many values required in `setf' of `@'"))
    (values (cons key-temp temps)
	    (cons key vals)
	    (list val-temp)
	    `(let ((,coll-temp (internal-with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(internal-lookup ,access-form ,key-temp))))

(declaim (inline internal-lookup))
(defun internal-lookup (&rest args)
  (apply #'lookup args))


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
  "For each member of `set', binds `var' to it and executes `body'.  When done,
returns `value'."
  `(block nil		; in case `body' contains `(return ...)'
     (let ((elt-fn #'(lambda (,var) . ,body))
	   (value-fn #'(lambda () ,value)))
       ;; SBCL evidently figures this out without our help, but other implementations may benefit.
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-set ,set elt-fn value-fn))))


(defmacro do-bag-pairs ((value-var mult-var bag &optional value)
			&body body)
  "For each member of `bag', binds `value-var' and `mult-var' to the member and
its multiplicity respectively, and executes `body'.  When done, returns `value'."
  `(block nil
     (let ((elt-fn #'(lambda (,value-var ,mult-var) . ,body))
	   (value-fn #'(lambda () ,value)))
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-bag-pairs ,bag elt-fn value-fn))))

(defmacro do-bag ((value-var bag &optional value)
		  &body body)
  "For each member of `bag', binds `value-var' to it and and executes `body' a
number of times equal to the member's multiplicity.  When done, returns `value'."
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
  `(block nil
     (let ((elt-fn #'(lambda (,var . ,(and index? `(,index))) . ,body))
	   (value-fn #'(lambda () ,value)))
       (declare (dynamic-extent elt-fn value-fn))
       (internal-do-seq ,seq elt-fn value-fn ,index?
			,@(and start? `(:start ,start))
			,@(and end? `(:end ,end))
			,@(and from-end?? `(:from-end? ,from-end?))))))

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
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-res-type set (&key filterp)
  "Returns a set of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (s x) (WB-Set-Tree-With s x #'compare)) #'make-wb-set ,filterp))


;;; A bit faster than `set', if you know it's a `wb-set'.
(gmap:def-gmap-arg-type wb-set (set)
  "Yields the elements of `set'."
  `((Make-WB-Set-Tree-Iterator-Internal (wb-set-contents ,set))
    #'WB-Set-Tree-Iterator-Done?
    #'WB-Set-Tree-Iterator-Get))

(gmap:def-gmap-res-type wb-set (&key filterp compare-fn-name)
  "Returns a set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering."
  (let ((org-var (gensymx #:org-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (WB-Set-Tree-With s x ,cf-var))
	  #'(lambda (s)
	      (if (eq ,cf-var #'compare)
		  (make-wb-set s)
		(make-wb-custom-set s ,org-var)))
	  ,filterp
	  ((,org-var (wb-set-org (empty-wb-set ,compare-fn-name)))
	   ((,cf-var (tree-set-org-compare-fn ,org-var)))))))


(gmap:def-gmap-res-type union (&key filterp)
  "Returns the union of the sets, optionally filtered by `filterp'."
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (s x) (if (null s) x (union s x)))
	#'(lambda (s) (or s (set)))
	,filterp))

(gmap:def-gmap-res-type intersection (&key filterp)
  "Returns the intersection of the sets, optionally filtered by `filterp'.
\(If zero sets are supplied, returns the full set; see `full-set'.\)"
  ;; Written to take the implementation and organization from the first value.
  `(nil #'(lambda (s x) (if (null s) x (intersection s x)))
	#'(lambda (s) (or s (full-set)))
	,filterp))

;;; A bit faster than `set', if you know it's a `ch-set'.
(gmap:def-gmap-arg-type ch-set (set)
  `((make-ch-set-tree-iterator-internal (ch-set-contents ,set))
    #'ch-set-tree-iterator-done?
    #'ch-set-tree-iterator-get))

(gmap:def-gmap-res-type ch-set (&key filterp compare-fn-name)
  "Returns a set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering."
  (let ((org-var (gensymx #:org-))
	(hf-var (gensymx #:hash-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (ch-set-tree-with s x ,hf-var ,cf-var))
	  #'(lambda (s) (make-ch-set s ,org-var))
	  ,filterp
	  ((,org-var (ch-set-org (empty-ch-set ,compare-fn-name)))
	   ((,hf-var (hash-set-org-hash-fn ,org-var))
	    (,cf-var (hash-set-org-compare-fn ,org-var)))))))

;;; ----------------
;;; Bags

(gmap:def-gmap-arg-type bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((the function (iterator ,bag))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-arg-type bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((the function (iterator ,bag :pairs? t))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-gmap-arg-type wb-bag (bag)
  "Yields each element of `bag', as many times as its multiplicity."
  `((Make-WB-Bag-Tree-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Iterator-Done?
    #'WB-Bag-Tree-Iterator-Get))

(gmap:def-gmap-arg-type wb-bag-pairs (bag)
  "Yields each element of `bag' and its multiplicity as two values."
  `((Make-WB-Bag-Tree-Pair-Iterator-Internal (wb-bag-contents ,bag))
    #'WB-Bag-Tree-Pair-Iterator-Done?
    (:values 2 #'WB-Bag-Tree-Pair-Iterator-Get)))

(gmap:def-arg-type fun-bag-pairs (bag &key from-end?)
  `((fun-iterator ,bag :pairs? t :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-gmap-res-type bag (&key filterp)
  "Returns a bag of the values, optionally filtered by `filterp'."
  `(nil
    (fn (b x) (WB-Bag-Tree-With b x #'compare))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type bag-pairs (&key filterp)
  "Consumes two values from the mapped function; returns a bag of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil
    (:consume 2 (fn (b x n) (WB-Bag-Tree-With b x #'compare n)))
    #'(lambda (b) (make-wb-bag b +fset-default-tree-set-org+))
    ,filterp))

(gmap:def-gmap-res-type wb-bag (&key filterp compare-fn-name)
  "Returns a wb-bag of the values, optionally filtered by `filterp'.  To use a
non-default comparison function in the result, supply `compare-fn-name`."
  (let ((proto-var (gensymx #:prototype-))
	(cf-var (gensymx #:cmp-)))
    `(nil #'(lambda (s x) (WB-Bag-Tree-With s x ,cf-var))
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
    `(nil (:consume 2 #'(lambda (tree x n) (WB-Bag-Tree-With tree x ,cf-var n)))
	  #'(lambda (tree) (make-wb-bag tree (wb-bag-org ,proto-var)))
	  ,filterp
	  ((,proto-var (empty-wb-bag ,compare-fn-name))
	   ((,cf-var (tree-set-org-compare-fn (wb-bag-org ,proto-var))))))))

(gmap:def-gmap-res-type bag-sum (&key filterp)
  "Returns the bag-sum of the values, optionally filtered by `filterp'."
  `((empty-bag) #'bag-sum nil ,filterp))

(gmap:def-gmap-res-type bag-product (&key filterp)
  "Returns the bag-product of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (prev bag) (if (null prev) bag (bag-product prev bag))) nil ,filterp))

;;; ----------------
;;; Maps

(gmap:def-gmap-arg-type map (map)
  "Yields each pair of `map', as two values."
  `((the function (iterator ,map))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-gmap-arg-type wb-map (map)
  "Yields each pair of `map', as two values."
  `((Make-WB-Map-Tree-Iterator-Internal (wb-map-contents ,map))
    #'WB-Map-Tree-Iterator-Done?
    (:values 2 #'WB-Map-Tree-Iterator-Get)))

(gmap:def-arg-type fun-map (map &key from-end?)
  `((fun-iterator ,map :from-end? ,from-end?)
    #'(lambda (it) (funcall it ':empty?))
    (:values 2 #'(lambda (it) (funcall it ':first)))
    #'(lambda (it) (funcall it ':rest))))

(gmap:def-gmap-res-type map (&key filterp default)
  "Consumes two values from the mapped function; returns a map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 (fn (m x y) (WB-Map-Tree-With m x y #'compare #'compare)))
	#'(lambda (tree) (make-wb-map tree (wb-map-org *empty-wb-map*) ,default))
    ,filterp))

(gmap:def-gmap-res-type wb-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((proto-var (gensymx #:prototype-))
	(kcf-var (gensymx #:key-cmp-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 (fn (tree k v) (WB-Map-Tree-With tree k v ,kcf-var ,vcf-var)))
	  #'(lambda (tree) (make-wb-map tree (wb-map-org ,proto-var) ,default))
	  ,filterp
	  ((,proto-var (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name))
	   ((,kcf-var (tree-map-org-key-compare-fn (wb-map-org ,proto-var)))
	    (,vcf-var (tree-map-org-val-compare-fn (wb-map-org ,proto-var))))))))

(gmap:def-gmap-res-type map-union (&key (val-fn nil val-fn?)
				    (default nil default?) filterp)
  "Returns the map-union of the values, optionally filtered by `filterp'.  If `val-fn'
is supplied, it is passed to `map-union' (q.v.).  If `default' is supplied, it is used
as the map default."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
       #'(lambda (prev-m m)
	   (if (null prev-m) m
	     ,(if val-fn? `(map-union prev-m m ,val-fn-var) '#'map-union)))
       ,(and default? `(fn (m) (with-default (or m (empty-wb-map)) ,default)))
       ,filterp
       ((,val-fn-var ,val-fn)))))

(gmap:def-gmap-res-type map-intersection (&key (val-fn nil val-fn?)
					   (default nil default?) filterp)
  "Returns the map-intersection of the values, optionally filtered by `filterp'.
If `val-fn' is supplied, it is passed to `map-intersection' (q.v.).  If
`default' is supplied, it is used as the map default.  If zero maps are
intersected, returns nil."
  (let ((val-fn-var (gensymx #:val-fn-)))
    `(nil
      #'(lambda (prev-m m)
	  (if (null prev-m) m
	    ,(if val-fn? `(map-intersection prev-m m ,val-fn-var) '#'map-intersection)))
      ,(and default? `#'(lambda (m) (and m (with-default m ,default))))
      ,filterp
      ((,val-fn-var ,val-fn)))))

(gmap:def-gmap-res-type map-to-sets (&key filterp key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function.  Returns a map from the first
values, with each one mapped to a set of the corresponding second values.
Note that `filterp', if supplied, must take two arguments."
  `((empty-wb-map (set) ,key-compare-fn-name ,val-compare-fn-name)
    ;; &&& Could use `WB-Map-Tree-Update-Value' here.
    (:consume 2 (fn (m x y) (with m x (with (lookup m x) y))))
    nil ,filterp))

(gmap:def-gmap-arg-type ch-map (map)
  "Yields each pair of `map', as two values."
  `((make-ch-map-tree-iterator-internal (ch-map-contents ,map))
    #'ch-map-tree-iterator-done?
    (:values 2 #'ch-map-tree-iterator-get)))

(gmap:def-gmap-res-type ch-map (&key filterp default key-compare-fn-name val-compare-fn-name)
  "Consumes two values from the mapped function; returns a wb-map of the pairs.
Note that `filterp', if supplied, must take two arguments."
  (let ((org-var (gensymx #:org-))
	(khf-var (gensymx #:key-hash-))
	(kcf-var (gensymx #:key-cmp-))
	(vhf-var (gensymx #:val-hash-))
	(vcf-var (gensymx #:val-cmp-)))
    `(nil (:consume 2 (fn (tree k v) (ch-map-tree-with tree k v ,khf-var ,kcf-var ,vhf-var ,vcf-var)))
	  #'(lambda (tree) (make-ch-map tree ,org-var ,default))
	  ,filterp
	  ((,org-var (ch-map-org (empty-ch-map nil ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,khf-var (hash-map-org-key-hash-fn ,org-var))
	    (,kcf-var (hash-map-org-key-compare-fn ,org-var))
	    (,vhf-var (hash-map-org-val-hash-fn ,org-var))
	    (,vcf-var (hash-map-org-val-compare-fn ,org-var)))))))

;;; ----------------
;;; Seqs

(gmap:def-gmap-arg-type seq (seq &key start end from-end?)
  "Yields the elements of `seq'.  The keyword arguments `start' and `end'
can be supplied to restrict the range of the iteration; `start' is inclusive
and defaults to 0, while `end' is exclusive and defaults to the size of the
seq.  If `from-end?' is true, the elements will be yielded in reverse order."
  `((the function (iterator ,seq :start ,start :end ,end :from-end? ,from-end?))
    #'(lambda (it) (funcall it ':done?))
    #'(lambda (it) (funcall it ':get))))

(gmap:def-gmap-arg-type wb-seq (seq &key start end from-end?)
  "Yields the elements of `seq'.  The keyword arguments `start' and `end'
can be supplied to restrict the range of the iteration; `start' is inclusive
and defaults to 0, while `end' is exclusive and defaults to the size of the
seq.  If `from-end?' is true, the elements will be yielded in reverse order."
  (flet ((fwd ()
	   `((Make-WB-Seq-Tree-Iterator-Internal (wb-seq-contents ,seq) ,start ,end)
	     #'WB-Seq-Tree-Iterator-Done?
	     #'WB-Seq-Tree-Iterator-Get))
	 (rev ()
	   `((Make-WB-Seq-Tree-Rev-Iterator-Internal (wb-seq-contents ,seq) ,start ,end)
	     #'WB-Seq-Tree-Rev-Iterator-Done?
	     #'WB-Seq-Tree-Rev-Iterator-Get)))
    (cond ((null from-end?) (fwd))
	  ((atom from-end?) (rev))
	  (t `(if ,from-end? ,(rev) ,(fwd))))))

(gmap:def-gmap-res-type seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(gmap:def-gmap-res-type wb-seq (&key filterp)
  "Returns a seq of the values, optionally filtered by `filterp'."
  `(nil
    #'(lambda (a b) (cons b a))
    #'(lambda (s) (convert 'seq (nreverse s)))
    ,filterp))

(gmap:def-gmap-res-type concat (&key filterp)
  "Returns the concatenation of the seq values, optionally filtered by `filterp'."
  `((empty-seq) #'concat nil ,filterp))

;;; ----------------
;;; Tuples

(gmap:def-gmap-arg-type tuple (tuple)
  "Yields each pair of `tuple', as two values."
  `((convert 'list ,tuple)
    #'null
    (:values 2 #'(lambda (al) (values (caar al) (cdar al))))
    #'cdr))

(gmap:def-gmap-res-type tuple (&key filterp)
  `((empty-dyn-tuple) (:consume 2 #'Tuple-With) nil ,filterp))

;;; ----------------
;;; Relations

(gmap:def-arg-type-synonym 2-relation wb-2-relation)

(gmap:def-gmap-arg-type wb-2-relation (rel)
  "Yields each pair of `rel', as two values."
  `((the function (iterator ,rel))
    #'(lambda (it) (funcall it ':done?))
    (:values 2 #'(lambda (it) (funcall it ':get)))))

(gmap:def-result-type-synonym 2-relation wb-2-relation)

(gmap:def-gmap-res-type wb-2-relation (&key filterp)
  "Consumes two values from the mapped function; returns a 2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (alist x y) (cons (cons x y) alist)))
	#'(lambda (alist) (list-to-wb-2-relation alist #'car #'cdr))
	,filterp))

(gmap:def-gmap-arg-type list-relation (rel)
  `((Make-WB-Set-Tree-Iterator-Internal (wb-set-contents (wb-list-relation-tuples ,rel)))
    #'WB-Set-Tree-Iterator-Done?
    #'WB-Set-Tree-Iterator-Get))


;;; ----------------
;;; Replay sets

;;; A bit faster than `set', if you know it's a `replay-set'.
(gmap:def-arg-type replay-set (set)
  "Yields the elements of `set'."
  `((make-wb-seq-tree-iterator-internal (replay-set-ordering ,set))
    #'wb-seq-tree-iterator-done?
    #'wb-seq-tree-iterator-get))

;;; Deprecated in favor of arg type `replay-set', above.
(gmap:def-gmap-arg-type wb-replay-set (set)
  "Yields the elements of `set'."
  `((make-wb-seq-tree-iterator-internal (replay-set-ordering ,set))
    #'wb-seq-tree-iterator-done?
    #'wb-seq-tree-iterator-get))

;;; CHAMP is the default now.
(gmap:def-result-type replay-set (&key filterp)
  "Returns a replay-set of the values, optionally filtered by `filterp'."
  (let ((ordering-var (gensymx #:ordering-)))
    `(nil #'(lambda (s x)
	      (let ((ts (ch-set-tree-with s x #'hash-value #'compare)))
		(unless (eq ts s)
		  (push x ,ordering-var))
		ts))
	  #'(lambda (s) (make-ch-replay-set s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    (ch-set-org *empty-ch-set*)))
	  ,filterp
	  ((,ordering-var nil)))))

(gmap:def-result-type wb-replay-set (&key filterp compare-fn-name)
  "Returns a wb-replay-set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering for the internal
set, though, of course, this doesn't affect the iteration order."
  `((empty-wb-replay-set ,compare-fn-name)
    #'with
    nil
    ,filterp))

(gmap:def-gmap-res-type append-unique ()
  "Returns a list of the unique elements of the lists returned by the
mapped function, in the order in which they were first encountered."
  `((empty-ch-replay-set)
    #'(lambda (rs new-elts)
	(dolist (x new-elts)
	  (includef rs x))
	rs)
    #'(lambda (rs) (convert 'list rs))))

(gmap:def-result-type ch-replay-set (&key filterp compare-fn-name)
  "Returns a ch-replay-set of the values, optionally filtered by `filterp'.  If
`compare-fn-name' is nonnull, it specifies a custom ordering for the internal
set, though, of course, this doesn't affect the iteration order."
  (let ((org-var (gensymx #:org-))
	(hf-var (gensymx #:hash-))
	(cf-var (gensymx #:cmp-))
	(ordering-var (gensymx #:ordering-)))
    `(nil #'(lambda (s x)
	      (let ((ts (ch-set-tree-with s x ,hf-var ,cf-var)))
		(unless (eq ts s)
		  (push x ,ordering-var))
		ts))
	  #'(lambda (s) (make-ch-replay-set s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    ,org-var))
	  ,filterp
	  ((,org-var (ch-set-org (empty-ch-set ,compare-fn-name)))
	   ((,hf-var (hash-set-org-hash-fn ,org-var))
	    (,cf-var (hash-set-org-compare-fn ,org-var)))
	   (,ordering-var nil)))))

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

(gmap:def-gmap-res-type replay-map (&key filterp default)
  "Consumes two values from the mapped function; returns a replay-map of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  (let ((ordering-var (gensymx #:ordering-)))
    `(nil (:consume 2 #'(lambda (s k v)
			  (let ((ts (ch-map-tree-with s k v #'hash-value #'compare #'hash-value #'compare)))
			    (unless (eq ts s)
			      (push k ,ordering-var))
			    ts)))
	  #'(lambda (s) (make-ch-replay-map s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    (ch-map-org *empty-ch-map*) ,default))
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
			    (unless (eq ts s)
			      (push k ,ordering-var))
			    ts)))
	  #'(lambda (s) (make-wb-replay-map s (wb-seq-tree-from-list (nreverse ,ordering-var))
					    ,org-var ,default))
	  ,filterp
	  ((,org-var (wb-map-org (empty-wb-map nil ,key-compare-fn-name ,val-compare-fn-name)))
	   ((,kcf-var (tree-map-org-key-compare-fn ,org-var))
	    (,vcf-var (tree-map-org-val-compare-fn ,org-var)))
	   (,ordering-var nil)))))

(gmap:def-gmap-res-type ch-replay-map (&key filterp default key-compare-fn-name val-compare-fn-name)
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
			    (unless (eq ts s)
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

(defmacro define-wb-set-method (name param-list &body body)
  (let ((decls nil)
	(doc-string body (if (stringp (car body)) (values (car body) (cdr body))
			   (values nil body))))
    (while (and (listp (car body)) (eq (caar body) 'declare))
      (push (pop body) decls))
    (setq decls (nreverse decls))
    (flet ((mod-param-list (new-class)
	     (do ((param-list param-list (cdr param-list))
		  (new-pl nil))
		 ((or (null param-list) (member (car param-list) '(&optional &key &rest)))
		  (revappend new-pl param-list))
	       (let ((plelt (car param-list)))
		 (push (if (and (listp plelt) (eq (cadr plelt) 'wb-set))
			   `(,(car plelt) ,new-class)
			 plelt)
		       new-pl)))))
      `(progn
	 (defmethod ,name ,(mod-param-list 'wb-default-set)
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
	 (defmethod ,name ,(mod-param-list 'wb-custom-set)
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
			;; (unless you're depending on how `compare` orders two sets).
			`(if (eq (wb-set-compare-fn ,coll1)
				 (wb-set-compare-fn ,coll2))
			     ,then
			   ,else))
		      (make (like-coll contents)
			`(make-wb-custom-set ,contents (wb-custom-set-org ,like-coll))))
	     . ,body))))))

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

(defmacro convert-to-ch-set (coll identity-test &body contents-forms)
  (let ((body `(let ((tree (progn . ,contents-forms)))
		 (make-ch-set tree hsorg))))
    `(let ((prototype (empty-ch-set compare-fn-name))
	   ((hsorg (ch-set-org prototype))
	    ((hash-fn (hash-set-org-hash-fn hsorg))
	     (compare-fn (hash-set-org-compare-fn hsorg)))))
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

(defmacro convert-to-ch-map (coll default identity-test &body contents-forms)
  (let ((body `(let ((tree (progn . ,contents-forms)))
		 (make-ch-map tree hmorg ,default))))
    `(let ((prototype (empty-ch-map nil key-compare-fn-name val-compare-fn-name))
	   ((hmorg (ch-map-org prototype))
	    ((key-hash-fn (hash-map-org-key-hash-fn hmorg))
	     (key-compare-fn (hash-map-org-key-compare-fn hmorg))
	     (val-hash-fn (hash-map-org-val-hash-fn hmorg))
	     (val-compare-fn (hash-map-org-val-compare-fn hmorg)))))
       ,(if identity-test
	    `(if ,identity-test ,coll ,body)
	  body))))

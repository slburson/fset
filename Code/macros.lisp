;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: macros.lisp
;;; Contents: Collected macros, separated out for compilation and coverage purposes
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; ================================================================================
;;; Miscellany

;;; A small notational convenience.  I want to give `gensym' an uninterned symbol (in a
;;; possibly futile effort to write case-mode-generic code), but it's not specified to
;;; call `string' on a symbol argument.
(defmacro gensymx (arg)
  `(gensym (string ',arg)))

(defmacro postincf (place &optional (delta 1) &environment env)
  (let ((vars vals store-vars setter getter (get-setf-expansion place env))
	(tmp (gensymx #:tmp-)))
    `(let* (,@(mapcar #'list vars vals)
	    (,(car store-vars) ,getter)
	    (,tmp ,(car store-vars)))
       (incf ,(car store-vars) ,delta)
       ,setter
       ,tmp)))


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
		,(rec `(hash-value ,(call (car slots/accessors) x-var)) (cdr slots/accessors)))
      (rec (value accs)
	(if (null accs) value
	  (rec `(logxor (logand most-positive-fixnum (* 17 ,value))
			(hash-value ,(call (car accs) x-var)))
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
(defun define-class-equality-slots-extension (class slots)
  `(define-equality-slots ,class . ,(mapcar (fn (x) `',x) slots)))


;;; ================================================================================
;;; Macros related to fset.lisp

;;; --------------------------------
;;; Modify macros

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

;;; Not completely thrilled with these names, but nothing better occurs to me.
(define-modify-macro set-differencef (set)
  set-difference)

;;; It might seem more natural to use `&optional' than `&rest', but then this macro would have to
;;; know the correct default value for `val-fn'; otherwise `define-modify-macro' would fill in `nil'.
(define-modify-macro map-unionf (map &rest stuff)
  map-union)

;;; Ditto.
(define-modify-macro map-intersectf (map &rest stuff)
  map-intersection)

(define-modify-macro imagef (fn)
  ximage)

(defun ximage (coll fn)
  (image fn coll))

(define-modify-macro updatef (fn &rest keys)
  xupdate)

(defun xupdate (coll fn &rest keys)
  (apply #'update fn coll keys))

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

(define-modify-macro insertf (idx value)
  insert)

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
	    `(let ((,coll-temp (with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(lookup ,access-form ,key-temp))))

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
	     (lookup ,fn-var ,arg-var))))
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
	    `(let ((,coll-temp (with ,access-form ,key-temp ,val-temp)))
	       ,store-form
	       ,val-temp)
	    `(lookup ,access-form ,key-temp))))


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

(defmacro define-wb-set-methods (name param-list &body body)
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

(defmacro if-same-ch-set-orgs ((s1 s2 hscomp-var) then else)
  (let ((s1-var (gensymx #:s1-))
	(s2-var (gensymx #:s2-))
	(hscomp1-var (gensymx #:hscomp1-))
	(hscomp2-var (gensymx #:hscomp2-)))
    `(let ((,s1-var ,s1)
	   (,s2-var ,s2)
	   ((,hscomp1-var (ch-set-org ,s1-var))
	    (,hscomp2-var (ch-set-org ,s2-var))))
       (if (or (eq ,hscomp1-var ,hscomp2-var)
	       (and (eq (hash-set-org-hash-fn ,hscomp1-var) (hash-set-org-hash-fn ,hscomp2-var))
		    (eq (hash-set-org-compare-fn ,hscomp1-var) (hash-set-org-compare-fn ,hscomp2-var))))
	   (let ((,hscomp-var ,hscomp1-var))
	     ,then)
	 ,else))))

(defmacro if-same-wb-bag-orgs ((b1 b2 tscomp-var) then else)
  (let ((b1-var (gensymx #:b1-))
	(b2-var (gensymx #:b2-))
	(tscomp1-var (gensymx #:tscomp1-))
	(tscomp2-var (gensymx #:tscomp2-)))
    `(let ((,b1-var ,b1)
	   (,b2-var ,b2)
	   ((,tscomp1-var (wb-bag-org ,b1-var))
	    (,tscomp2-var (wb-bag-org ,b2-var))))
       (if (or (eq ,tscomp1-var ,tscomp2-var)
	       (eq (tree-set-org-compare-fn ,tscomp1-var) (tree-set-org-compare-fn ,tscomp2-var)))
	   (let ((,tscomp-var ,tscomp1-var))
	     ,then)
	 ,else))))

(defmacro if-same-wb-map-orgs ((m2 b2 tmcomp-var) then else)
  (let ((m2-var (gensymx #:m2-))
	(b2-var (gensymx #:b2-))
	(tmcomp1-var (gensymx #:tmcomp1-))
	(tmcomp2-var (gensymx #:tmcomp2-)))
    `(let ((,m2-var ,m2)
	   (,b2-var ,b2)
	   ((,tmcomp1-var (wb-map-org ,m2-var))
	    (,tmcomp2-var (wb-map-org ,b2-var))))
       (if (or (eq ,tmcomp1-var ,tmcomp2-var)
	       (and (eq (tree-map-org-key-compare-fn ,tmcomp1-var)
			(tree-map-org-key-compare-fn ,tmcomp2-var))
		    (eq (tree-map-org-val-compare-fn ,tmcomp1-var)
			(tree-map-org-val-compare-fn ,tmcomp2-var))))
	   (let ((,tmcomp-var ,tmcomp1-var))
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

;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: order.lisp
;;; Contents: Ordering function for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


(defgeneric compare (x y)
  (:documentation
    "Returns one of `:less', `:greater', `:equal', or `:unequal' according as `x'
is less than, greater than, or equal to `y', or none of these.  While the
ordering does not have to be total, it must be consistent: for two values
A and B that compare `:unequal' to each other, for any third value C, if A
compares `:less' or `:greater' to C, then B must compare to C the same way;
and no more than one of A and B can compare `:equal' to C."))

(defun less-than? (a b)
  (eq (compare a b) ':less))

(defun greater-than? (a b)
  (eq (compare a b) ':greater))

(defun equal? (a b)
  (or (eql a b)
      (eq (compare a b) ':equal)))

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
  (let ((default-var (gensym "DEFAULT-"))
	(comp-var (gensym "COMP-"))
	(obj1-var (gensym "OBJ1-"))
	(obj2-var (gensym "OBJ2-")))
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


;;; Abstract classes

(defstruct (collection
	    (:constructor nil)
	    (:predicate collection?)
	    (:copier nil))
  "The root class of the FSet functional collections hierarchy.  It is a
structure class.")

(defstruct (set
	    (:constructor nil)
	    (:include collection)
	    (:predicate set?)
	    (:copier nil))
  "The abstract class for FSet functional sets.  It is a structure class.")

(defstruct (bag
	    (:constructor nil)
	    (:include collection)
	    (:predicate bag?)
	    (:copier nil))
  "The abstract class for FSet functional bags (multisets).  It is a structure
class.")

(defstruct (map
	    (:constructor nil)
	    (:include collection)
	    (:predicate map?)
	    (:copier nil))
  "The abstract class for FSet functional maps.  It is a structure class."
  (default nil))

(defstruct (seq
	    (:constructor nil)
	    (:include collection)
	    (:predicate seq?)
	    (:copier nil))
  "The abstract class for FSet functional seqs (sequences, but we use the short
name to avoid confusion with `cl:sequence').  It is a structure class."
  (default nil))

(defstruct (tuple
	    (:constructor nil)
	    (:include collection)
	    (:predicate tuple?)
	    (:copier nil))
  "The abstract class for FSet functional tuples.  It is a structure class.")


;;; ================================================================================
;;; Identity ordering

(defclass identity-ordering-mixin ()
    ((serial-number :accessor serial-number)
     (next-serial-number :initform '0 :allocation :class))
  (:documentation
    "A mixin class for classes whose instances will be used in FSet collections,
and for which the appropriate equivalence relation is identity (`eq').
This is the right choice for the vast majority of mutable classes."))

(defmethod initialize-instance :before ((obj identity-ordering-mixin)
					&key &allow-other-keys)
  (setf (serial-number obj) (slot-value obj 'next-serial-number))
  (incf (slot-value obj 'next-serial-number)))

(defmethod compare ((obj1 identity-ordering-mixin) (obj2 identity-ordering-mixin))
  (compare-slots obj1 obj2 #'serial-number))


;;; ================================================================================
;;; Compare methods

;;; Default

;;; On Allegro it's about 4 times faster to have all the cross-type methods
;;; declared, as they are below, than to use this for all cross-type comparisons.
;;; But this is fast enough that I think it will suffice for user-defined types.
;;; Of course the user is free to define all the cross-type methods themselves
;;; if they want; a macro to assist with this is below.
(defmethod compare ((a t) (b t))
  (let ((a-type (cond ((realp a) 'real)
		      ((stringp a) 'string)	; We check for these ourselves
		      ((vectorp a) 'vector)	; because `type-of' may cons a list.
		      (t (type-of a))))
	(b-type (cond ((realp b) 'real)
		      ((stringp b) 'string)
		      ((vectorp b) 'vector)
		      (t (type-of b)))))
    (if (eq a-type b-type)
	;; If we get here, they haven't defined a compare method for their type.
	;; This is the best we can do.
	(if (eql a b) ':equal ':unequal)
      (if (and (symbolp a-type) (symbolp b-type))
	  ;; Just compare the type symbols.  But note, under rare circumstances
	  ;; involving `rename-package', this can return `:unequal'.
	  (compare a-type b-type)
	;; If we get here, one or both of them are probably instances of anonymous
	;; CLOS classes.  Again, this is the best we can do (or would an error
	;; be better??).
	':unequal))))


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
	   ;; order on different occasions, but only if neither call has been loaded.
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

;;; CL types
(define-cross-type-compare-methods null)
(define-cross-type-compare-methods real)
(define-cross-type-compare-methods character)
(define-cross-type-compare-methods symbol)
(define-cross-type-compare-methods string)
(define-cross-type-compare-methods vector)
(define-cross-type-compare-methods list)
(define-cross-type-compare-methods package)
(define-cross-type-compare-methods pathname)

;;; FSet types
(define-cross-type-compare-methods set)
(define-cross-type-compare-methods bag)
(define-cross-type-compare-methods map)
(define-cross-type-compare-methods seq)
(define-cross-type-compare-methods tuple)

;;; For users
(define-cross-type-compare-methods identity-ordering-mixin)


;;; Nil

(defmethod compare ((a null) (b null))
  ':equal)


;;; Reals

(defmethod compare ((a real) (b real))
  (cond ((< a b) ':less)
	((> a b) ':greater)
	((eql a b) ':equal)
	;; Happens when you compare a rational and a float that have the same
	;; value (within the precision of the float), or two equal floats
	;; of different precisions.
	(t ':unequal)))


;;; Characters

;;; `char<' is called directly in many places in the code where we know two
;;; characters are being compared. 
(defmethod compare ((a character) (b character))
  (cond ((char< a b) ':less)
	((char> a b) ':greater)
	(t ':equal)))


;;; Symbols

(defmethod compare ((a symbol) (b symbol))
  (if (eq a b) ':equal
    (let ((pkg-comp (compare (symbol-package a) (symbol-package b))))
      (if (or (eq pkg-comp ':equal) (eq pkg-comp ':unequal))
	  ;; We've already checked for `eq', so they can't be equal, but they can
	  ;; be "unequal" in two cases: uninterned symbols of the same name;
	  ;; symbols of the same name in packages one of which has the name that
	  ;; the other had before `rename-package' was done on it.
	  (let ((comp (Compare-Strings (symbol-name a) (symbol-name b))))
	    (if (eq comp ':equal) ':unequal
	      comp))
	pkg-comp))))


;;; Strings

(defmethod compare ((a string) (b string))
  (Compare-Strings a b))

;;; Abstracted out for use by `(compare symbol symbol)'.  Do not use otherwise.
(defun Compare-Strings (a b)
  (let ((len-a (length a))
	(len-b (length b)))
    (cond ((< len-a len-b) ':less)
	  ((> len-a len-b) ':greater)
	  (t
	   (if (and (simple-string-p a) (simple-string-p b))
	       (dotimes (i len-a ':equal)
		 (let ((ca (schar a i))
		       (cb (schar b i)))
		   (cond ((char< ca cb) (return ':less))
			 ((char> ca cb) (return ':greater)))))
	     (dotimes (i len-a ':equal)
	       (let ((ca (char a i))
		     (cb (char b i)))
		 (cond ((char< ca cb) (return ':less))
		       ((char> ca cb) (return ':greater))))))))))


;;; Vectors

(defmethod compare ((a vector) (b vector))
  (let ((len-a (length a))
	(len-b (length b))
	(default ':equal))
    (cond ((eq a b) ':equal)
	  ((< len-a len-b) ':less)
	  ((> len-a len-b) ':greater)
	  ((and (simple-vector-p a) (simple-vector-p b))
	   (dotimes (i len-a default)
	     (let ((res (compare (svref a i) (svref b i))))
	       (when (or (eq res ':less) (eq res ':greater))
		 (return res))
	       (when (eq res ':unequal)
		 (setq default ':unequal)))))
	  (t
	   (dotimes (i len-a default)
	     (let ((res (compare (aref a i) (aref b i))))
	       (when (or (eq res ':less) (eq res ':greater))
		 (return res))
	       (when (eq res ':unequal)
		 (setq default ':unequal))))))))


;;; Lists

(defmethod compare ((a list) (b list))
  ;; We don't compare lengths first, as we did for vectors, because `length'
  ;; on a list takes linear time, not constant time.
  ;; Also, we want to handle dotted lists.
  (compare-lists-lexicographically a b))

(defun compare-lists-lexicographically (a b)
  (do ((a a (cdr a))
       (b b (cdr b))
       (default ':equal))
      ((or (atom a) (atom b))
       (let ((comp (compare a b)))
	 (if (or (eq comp ':less) (eq comp ':greater))
	     comp
	   default)))
    (when (eq a b)			; we could get lucky
      (return default))
    (let ((comp (compare (car a) (car b))))
      (when (or (eq comp ':less) (eq comp ':greater))
	(return comp))
      (when (eq comp ':unequal)
	(setq default ':unequal)))))


;;; Packages (needed for symbols)

(deflex +Package-Original-Name+ (make-hash-table)
  "FSet uses this to protect itself from the effects of `rename-package',
which could otherwise change the ordering of packages, and thus of symbols,
and thus of types named by those symbols.")

(defmethod compare ((a package) (b package))
  ;; This is a bit subtle.  In order to keep things fast in the most common
  ;; case -- comparing symbols in the same package -- we do the `eq' test first,
  ;; and if it succeeds, we don't squirrel away the current package name.  This
  ;; is okay, because if a package has never been involved in an interpackage
  ;; comparison, then FSet can't be counting on the results of that comparison
  ;; to remain consistent.
  (if (eq a b)
      ':equal
    (flet ((pkg-name (pkg)
	     (or (gethash pkg +Package-Original-Name+)
		 (setf (gethash pkg +Package-Original-Name+)
		       (package-name pkg)))))
      (let ((a-name (pkg-name a))
	    (b-name (pkg-name b))
	    ((comp (compare a-name b-name))))
	(if (eq comp ':equal)
	    ':unequal			; we already checked for the `eq' case
	  comp)))))


;;; Pathnames

(defmethod compare ((a pathname) (b pathname))
  (compare-slots a b #'pathname-host #'pathname-device #'pathname-directory
		 #'pathname-name #'pathname-type #'pathname-version))


;;; ================================================================================
;;; Lexicographic comparison of sequences

;;; User code that specifically wants lexicographic comparison can call this
;;; in the `compare' method for the user type in question.
(defgeneric compare-lexicographically (a b)
  (:documentation
    "Returns the result of a lexicographic comparison of `a' and `b', which
can be strings, vectors, lists, or seqs."))

(defmethod compare-lexicographically ((a string) (b string))
  (if (eq a b)
      ':equal
    (let ((len-a (length a))
	  (len-b (length b)))
      (if (and (simple-string-p a) (simple-string-p b))
	  (dotimes (i (min len-a len-b)
		    (cond ((< len-a len-b) ':less)
			  ((> len-a len-b) ':greater)
			  (t ':equal)))
	    (let ((ca (schar a i))
		  (cb (schar b i)))
	      (cond ((char< ca cb) (return ':less))
		    ((char> ca cb) (return ':greater)))))
	(dotimes (i (min len-a len-b)
		  (cond ((< len-a len-b) ':less)
			((> len-a len-b) ':greater)
			(t ':equal)))
	  (let ((ca (char a i))
		(cb (char b i)))
	    (cond ((char< ca cb) (return ':less))
		  ((char> ca cb) (return ':greater)))))))))

(defmethod compare-lexicographically ((a list) (b list))
  (compare-lists-lexicographically a b))

(defmethod compare-lexicographically ((a vector) (b vector))
  (if (eq a b)
      ':equal
    (let ((len-a (length a))
	  (len-b (length b))
	  (default ':equal))
      (if (and (simple-vector-p a) (simple-vector-p b))
	  (dotimes (i (min len-a len-b)
		    (cond ((< len-a len-b) ':less)
			  ((> len-a len-b) ':greater)
			  (t default)))
	    (let ((res (compare (svref a i) (svref b i))))
	      (when (or (eq res ':less) (eq res ':greater))
		(return res))
	      (when (eq res ':unequal)
		(setq default ':unequal))))
	(dotimes (i (min len-a len-b)
		  (cond ((< len-a len-b) ':less)
			((> len-a len-b) ':greater)
			(t default)))
	  (let ((res (compare (aref a i) (aref b i))))
	    (when (or (eq res ':less) (eq res ':greater))
	      (return res))
	    (when (eq res ':unequal)
	      (setq default ':unequal))))))))

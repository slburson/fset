(in-package :fset)

;;; Macros used in fset.lisp
;;; These have been moved here so coverage on them can be collected in SBCL

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
  (if (> (length args) 1)
      ;; Hmm.  We _could_ listify `args' and use that as the map key.
      `(funcall ,fn-or-collection . ,args)
    (let ((fn-var (gensym "FN-")))
      `(let ((,fn-var ,fn-or-collection))
	 (if (functionp ,fn-var)
	     (funcall ,fn-var . ,args)
	   ;; We do it this way rather than just `(lookup fn-or-collection (car args))'
	   ;; so that we get the right error when `args' is not of length 1.  If this
	   ;; doesn't get compiled well everyplace we care about, we could test the
	   ;; length and issue the error ourselves (if that helps).
	     (lookup ,fn-var . ,args))))))

(defmacro check-two-arguments (arg2? op type)
  `(when ,arg2?
     (error 'simple-program-error
	    :format-control "~A on a ~A takes only two arguments"
	    :format-arguments (list ,op ,type))))

(defmacro check-three-arguments (arg2? op type)
  `(unless ,arg2?
     (error 'simple-program-error
	    :format-control "~A on a ~A takes three arguments"
	    :format-arguments (list ,op ,type))))

(defmacro do-set ((var set &optional value) &body body)
  "For each member of `set', binds `var' to it and executes `body'.  When done,
returns `value'."
  `(block nil		; in case `body' contains `(return ...)'
     ;; &&& Here and in similar cases below, `dynamic-extent' declarations could
     ;; be helpful.  (The closures will have to be bound to variables.)
     (internal-do-set ,set #'(lambda (,var) . ,body)
			   #'(lambda () ,value))))


(defmacro do-bag-pairs ((value-var mult-var bag &optional value)
			&body body)
  "For each member of `bag', binds `value-var' and `mult-var' to the member and
its multiplicity respectively, and executes `body'.  When done, returns `value'."
  `(block nil
     (internal-do-bag-pairs ,bag #'(lambda (,value-var ,mult-var) . ,body)
			    #'(lambda () ,value))))

(defmacro do-bag ((value-var bag &optional value)
		  &body body)
  "For each member of `bag', binds `value-var' to it and and executes `body' a
number of times equal to the member's multiplicity.  When done, returns `value'."
  (let ((mult-var (gensym "MULT-"))
	(idx-var (gensym "IDX-")))
    `(block nil
       (internal-do-bag-pairs ,bag #'(lambda (,value-var ,mult-var)
				       ;; Seems safe to assume it's a fixnum here.
				       (declare (type fixnum ,mult-var))
				       (dotimes (,idx-var ,mult-var)
					 (declare (type fixnum ,idx-var))
					 . ,body))
			      #'(lambda () ,value)))))

(defmacro do-map ((key-var value-var map &optional value) &body body)
  "For each pair of `map', binds `key-var' and `value-var' and executes `body'.
When done, returns `value'."
  `(block nil
     (internal-do-map ,map
		      #'(lambda (,key-var ,value-var) . ,body)
		      #'(lambda () ,value))))

(defmacro do-map-domain ((key-var map &optional value) &body body)
  "For each pair of `map', binds `key-var' and executes `body'.  When done,
returns `value'."
  (let ((value-var (gensym "VAL-")))
    `(block nil
       (internal-do-map ,map
			#'(lambda (,key-var ,value-var)
			    (declare (ignore ,value-var))
			    . ,body)
			#'(lambda () ,value)))))

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
     (internal-do-seq ,seq
		      #'(lambda (,var . ,(and index? `(,index))) . ,body)
		      #'(lambda () ,value)
		      ,index?
		      ,@(and start? `(:start ,start))
		      ,@(and end? `(:end ,end))
		      ,@(and from-end?? `(:from-end? ,from-end?)))))

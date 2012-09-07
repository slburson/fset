;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: FSet -*-

;;; File: reader.lisp
;;; Contents: Reader macros and supporting code for FSet
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(in-package :fset)

;;; This file defines two different kinds of convenience syntax for constructing
;;; the FSet datatypes: constructor macros, and reader macros that expand to
;;; invocations of the constructor macros.  (Note 2008-10-25: the reader macros
;;; haven't been used much; the constructor macros seem to be as much syntax as
;;; is desirable in Lisp.  But, they're here if you want them.)
;;;
;;; Each constructor macro has the same name as the type it constructs (making
;;; them somewhat like `cl:list', but with some additional features).  Some
;;; examples:
;;;
;;;   (set 1 2)                              => set containing 1 and 2
;;;   (let ((x 3)) (set 1 2 x))              => set containing 1, 2, and 3
;;;   (let ((s (set 1 2))) (set 3 ($ s) 4))  => set containing 1, 2, 3, and 4
;;;   (bag (% 11 3))                         => bag with 3 occurrences of 11
;;;   (let ((b (bag 17))) (bag ($ b) 13 (% 17 2)))
;;;					     => bag w/ 1 occ. of 13, 3 occs. of 17
;;;   (map (2 47) (3 23))		     => map from 2 to 47 and 3 to 23
;;;   (let ((m (map (2 47) (3 23)))) (map ($ m) (2 61)))
;;;					     => map from 2 to 61 and 3 to 23
;;;
;;; For complete documentation, see the documentation strings below.
;;;
;;; The reader macros expand directly into invocations of the constructor macros,
;;; so the syntax is similar.  Loading this file does _not_ cause these macros
;;; to be defined in the current readtable; see `fset-setup-readtable' below.
;;;
;;; Set syntax:
;;;
;;;   #{ <form>* }
;;;
;;; Any form can be prefixed with `#$' to indicate that it is to be a subset
;;; rather than an element.  Note that, unlike in quoted lists and `#(...)', the
;;; forms are evaluated.  Examples:
;;;
;;;   #{ 1 2 3 }
;;;   #{ 1 2 x }    ; X is evaluated!
;;;   #{ 1 2 #$x }  ; equivalent to `(union x #{1 2})'
;;;
;;; Bag syntax:
;;;
;;; #{% <subexpression>* %}
;;;
;;; The subexpressions are either forms, as in the set case, or expressions of the
;;; form `#%(<element> <count>)', indicating <count> occurrences of <element>.
;;; The forms are all evaluated.  Any form may be prefixed with `#$' to indicate
;;; that it is a subbag rather than an element; the subbags are combined with
;;; `bag-sum'.
;;;
;;; Map syntax:
;;;
;;;   #{| <subexpression>* |}
;;;
;;; where each subexpression is either a pair is written as a list of two forms, or a
;;; use of the `#$' notation.  Again, the forms are all evaluated.  Examples:
;;;
;;;   #{| (1 2) (3 'x) |}   ; maps 1 to 2, and 3 to the value of X
;;;   #{| #$x (1 2) |}      ; equivalent to `(map-union x #{| (1 2) |})'
;;;
;;; In any case where multiple values are provided for the same key, the rightmost
;;; subexpression takes precedence.
;;;
;;; Sequence syntax:
;;;
;;;   #[ <form>* ]
;;;
;;; Any form can be prefixed with `#$' to indicate that it is to be a subsequence
;;; rather than a member.  Note that, unlike in quoted lists and `#(...)', the
;;; forms are evaluated.  Examples:
;;;
;;;   #[ 1 2 3 ]
;;;   #[ 1 2 x ]    ; X is evaluated!
;;;   #[ 1 2 #$x ]  ; equivalent to `(concat #[1 2] x)'
;;;
;;; These examples are all written with spaces immediately inside the delimiters,
;;; but they are not required.
;;;
;;; Tuple syntax:
;;;
;;; #~< <subexpression>* >
;;;
;;; where each subexpression is either a pair is written as a list of two forms,
;;; or a use of the `#$' notation.  Again, the forms are all evaluated; the keys
;;; must all be instances of `tuple-key'.  Examples:
;;;
;;;   #~< (k1 2) (k3 'x) >   ; maps k1 to 2, and k3 to the value of X
;;;   #{| #$x (k1 2) |}     ; equivalent to `(tuple-merge x #< (1 2) >)'
;;;
;;; In any case where more than one value is provided for a given key, the rightmost
;;; subexpression takes precedence.
;;; 
;;; Discussion: having the reader macros return constructor macro invocations, so
;;; that the operands of the reader macro will be evaluated, is not the traditional
;;; Lisp way of doing things.  Consider the #(...) reader macro for vectors: the
;;; reader macro constructs and returns the vector itself, necessarily treating the
;;; operands (the s-expressions within the parentheses) as constants.  To write an
;;; expression that constructs a vector but evaluates some of its operands, you must
;;; either just call `vector', or use backquote:  `#(1 2 ,x)
;;;
;;; I didn't want these reader macros to work that way, partly because I've never
;;; been very fond of backquote, and partly because FSet was inspired by Refine, and
;;; in Refine syntax, collection expressions evaluate their operands.  Also, in
;;; Refine, these expressions are used for pattern matching:
;;;
;;;   ( s = [ $x, 'foo, $y ] --> ...)
;;;
;;; which searches sequence `s' for an occurrence of symbol `foo', and if it finds
;;; one, binds `x' and `y' to the left and right subsequences of `s' defined by that
;;; occurrence of `foo', and evaluates the expression to the right of the arrow.  I
;;; eventually want to add this kind of pattern matching to FSet, and I think the
;;; reader macros will be handy for that purpose (though not required; one can use
;;; the constructor macros instead).  If the reader macros worked the same as #(...),
;;; though, the only way to make them work for this would be to extend backquote to
;;; support the FSet types; and CL defines no portable interface for extending
;;; backquote.
;;;
;;; The downside, though, of having the FSet reader macros work the way they do, is
;;; the loss of readable printing: even though the reader macros accept the same
;;; delimiter syntax as the print functions for `wb-set' etc. produce, it is not
;;; possible to write out an FSet structure (to a file, say) and then read it back
;;; in using these reader macros, unless it contains only objects that are self-
;;; evaluating in CL like numbers, strings, and keyword symbols.  If it contains
;;; lists or non-keyword symbols, the form returned by the reader macro will attempt
;;; to evaluate these (and presumably fail).
;;;
;;; To me, the ideal solution would be to modify the Lisp printer so that when
;;; printing a non-self-evaluating object -- a non-keyword symbol or list -- it would
;;; quote it, thus:
;;;
;;;   * 'a
;;;   'A
;;;   * (list 'a 'b)
;;;   '(A B)
;;;
;;; This is, or is similar to, an approach of Brian C. Smith in his semantically
;;; normalized "2-Lisp".  Given this change, one could arrange for readable printing
;;; of the FSet types:
;;;
;;;   * #{ 1 'x }
;;;   #{ 1 'X }
;;;
;;; I think this would be a better way to do things, but there's no question it
;;; would confuse current users of CL (and also, of course, it can't be implemented
;;; portably).
;;;
;;; So, what to do?  All I can come up with at the moment is to provide two sets of
;;; reader macros: one that functions as described above (evaluating operands), and
;;; a second "rereading" set that is non-evaluating, like #(...), and so can be used
;;; to reread printed FSet values.
;;;
;;; It remains to be seen whether anyone uses the reader macros, anyway.



(defmacro set (&rest args)
  "Constructs a set of the default implementation according to the supplied
argument subforms.  Each argument subform can be an expression, whose value
will be a member of the result set; or a list of the form ($ `expression'), in
which case the expression must evaluate to a set, all of whose members become
members of the result set."
  (expand-set-constructor-form 'set args))

(defmacro wb-set (&rest args)
  "Constructs a wb-set according to the supplied argument subforms.  Each
argument subform can be an expression, whose value will be a member of the
result set; or a list of the form ($ `expression'), in which case the
expression must evaluate to a set, all of whose members become members of the
result set."
  (expand-set-constructor-form 'wb-set args))

(defun expand-set-constructor-form (type-name args)
  (let ((normal-args (remove-if #'(lambda (arg) (and (listp arg) (eq (car arg) '$)))
				args))
	(splice-args (remove-if-not #'(lambda (arg) (and (listp arg) (eq (car arg) '$)))
				    args))
	((start (if normal-args `(convert ',type-name (list . ,normal-args))
		  (ecase type-name
		    (set `(empty-set))
		    (wb-set `(empty-wb-set)))))))
    (labels ((recur (splice-args result)
	       (if (null splice-args) result
		 (if (= (length (car splice-args)) 2)
		     (recur (cdr splice-args) `(union ,(cadar splice-args) ,result))
		   (error "A splice-arg to the `~S' macro must be of the form ~@
			   ($ <sub-set>) -- not ~S" type-name (car splice-args))))))
      (recur splice-args start))))

(defmacro bag (&rest args)
  "Constructs a bag of the default implementation according to the supplied
argument subforms.  Each argument subform can be an expression, whose value
will be added to the bag with multiplicity 1; or a list of the form
\($ `expression'), in which case the expression must evaluate to a bag (or a
set), which is bag-summed into the result; or a list of the form
\(% `expression1' `expression2') (called a \"multi-arg\"), which indicates that
the value of `expression1' is bag-summed into the result with multiplicity
given by the value of `expression2'.  That is, the multiplicity of each member
of the result bag is the sum of its multiplicities as supplied by each of the
argument subforms."
  (expand-bag-constructor-form 'bag args))

(defmacro wb-bag (&rest args)
  "Constructs a wb-bag according to the supplied argument subforms.  Each
argument subform can be an expression, whose value will be added to the bag
with multiplicity 1; or a list of the form ($ `expression'), in which case the
expression must evaluate to a bag (or a set), which is bag-summed into the
result; or a list of the form (% `expression1' `expression2') (called a
\"multi-arg\"), which indicates that the value of `expression1' is bag-summed
into the result with multiplicity given by the value of `expression2'.  That
is, the multiplicity of each member of the result bag is the sum of its
multiplicities as supplied by each of the argument subforms."
  (expand-bag-constructor-form 'wb-bag args))

(defun expand-bag-constructor-form (type-name args)
  (let ((normal-args (remove-if #'(lambda (arg) (and (listp arg)
						     (member (car arg) '($ %))))
				args))
	(splice-args (remove-if-not #'(lambda (arg) (and (listp arg) (eq (car arg) '$)))
				    args))
	(multi-args (remove-if-not #'(lambda (arg) (and (listp arg) (eq (car arg) '%)))
				   args))
	((start (if normal-args `(convert ',type-name (list . ,normal-args))
		  (ecase type-name
		    (bag `(empty-bag))
		    (wb-bag `(empty-wb-bag)))))))
    (labels ((add-splice-args (splice-args result)
	       (if (null splice-args) result
		 (if (= (length (car splice-args)) 2)
		     `(bag-sum ,(cadar splice-args)
			       ,(add-splice-args (cdr splice-args) result))
		   (error "A splice-arg to the `~S' macro must be of the form~@
			   ($ <sub-bag>) -- not ~S"
			  type-name (car splice-args)))))
	     (add-multi-args (multi-args result)
	       (if (null multi-args) result
		 (let ((m-arg (car multi-args)))
		   (unless (and (listp m-arg) (= (length m-arg) 3))
		     (error "A multi-arg to the `~S' macro must be of the form~@
			     (% <element> <count>) -- not ~S"
			    type-name m-arg))
		 `(with ,(add-multi-args (cdr multi-args) result)
			,(second m-arg) ,(third m-arg))))))
      (add-multi-args multi-args
		      (add-splice-args splice-args start)))))

(defmacro map (&rest args)
  "Constructs a map of the default implementation according to the supplied
argument subforms.  Each argument subform can be a list of the form (`key-expr'
`value-expr'), denoting a mapping from the value of `key-expr' to the value of
`value-expr'; or a list of the form ($ `expression'), in which case the
expression must evaluate to a map, denoting all its mappings; or the symbol
`:default', in which case the next argument subform is a form whose value will
become the map's default.  The result is constructed from the denoted mappings
in left-to-right order; so if a given key is supplied by more than one argument
subform, its associated value will be given by the rightmost such subform."
  (expand-map-constructor-form 'map args))

(defmacro wb-map (&rest args)
  "Constructs a wb-map according to the supplied argument subforms.  Each
argument subform can be a list of the form (`key-expr' `value-expr'), denoting
a mapping from the value of `key-expr' to the value of `value-expr'; or a list
of the form ($ `expression'), in which case the expression must evaluate to a
map, denoting all its mappings; or the symbol `:default', in which case the
next argument subform is a form whose value will become the map's default.  The
result is constructed from the denoted mappings in left-to-right order; so if a
given key is supplied by more than one argument subform, its associated value
will be given by the rightmost such subform."
  (expand-map-constructor-form 'wb-map args))

(defun expand-map-constructor-form (type-name args)
  (let ((empty-form (ecase type-name
		      (map `(empty-map))
		      (wb-map `(empty-wb-map))))
	(default nil))
    (labels ((recur (args result)
	       (cond ((null args) result)
		     ((eq (car args) ':default)
		      (setq default (cadr args))
		      (recur (cddr args) result))
		     ((not (and (listp (car args))
				(= (length (car args)) 2)))
		      (error "Arguments to ~S must all be pairs expressed as 2-element~@
			      lists, or ($ x) subforms -- not ~S"
			     type-name (car args)))
		     ((eq (caar args) '$)
		      (if (eq result empty-form)
			  (recur (cdr args) (cadar args))
			(recur (cdr args) `(map-union ,result ,(cadar args)))))
		     (t
		      (recur (cdr args) `(with ,result ,(caar args) ,(cadar args)))))))
      `(with-default ,(recur args empty-form) ,default))))

(defmacro seq (&rest args)
  "Constructs a seq of the default implementation according to the supplied
argument subforms.  Each argument subform can be an expression whose value is
to appear in the sequence; or a list of the form ($ `expression'), in which
case the expression must evaluate to a sequence, all of whose values appear in
the result sequence.  The order of the result sequence reflects the order of
the argument subforms."
  (expand-seq-constructor-form 'seq args))

(defmacro wb-seq (&rest args)
  "Constructs a wb-seq according to the supplied argument subforms.  Each
argument subform can be an expression whose value is to appear in the sequence;
or a list of the form ($ `expression'), in which case the expression must
evaluate to a sequence, all of whose values appear in the result sequence.  The
order of the result sequence reflects the order of the argument subforms."
  (expand-seq-constructor-form 'wb-seq args))

(defun expand-seq-constructor-form (type-name args)
  (labels ((recur (args nonsplice-args)
	     (cond ((null args)
		    (if nonsplice-args
			`(convert ',type-name (list . ,(cl:reverse nonsplice-args)))
		      (ecase type-name
			(seq `(empty-seq))
			(wb-seq `(empty-wb-seq)))))
		   ((and (listp (car args))
			 (eq (caar args) '$))
		    (unless (= (length (car args)) 2)
		      (error "A splice-arg to the `~S' macro must be of the form~@
			      ($ <sub-seq>) -- not ~S"
			     type-name (car args)))
		    (let ((rest (if (cdr args)
				    `(concat ,(cadar args)
					     ,(recur (cdr args) nil))
				  (cadar args))))
		      (if nonsplice-args
			  `(concat (convert ',type-name
					    (list . ,(cl:reverse nonsplice-args)))
				   ,rest)
			rest)))
		   (t
		    (recur (cdr args) (cons (car args) nonsplice-args))))))
    (recur args nil)))

(defmacro tuple (&rest args)
  "Constructs a tuple of the default implementation according to the supplied
argument subforms.  Each argument subform can be a list of the form (`key-expr'
`value-expr'), denoting a mapping from the value of `key-expr' to the value of
`value-expr'; or a list of the form ($ `expression'), in which case the
expression must evaluate to a tuple, denoting all its mappings.  The result is
constructed from the denoted mappings in left-to-right order; so if a given key
is supplied by more than one argument subform, its associated value will be
given by the rightmost such subform."
  (expand-tuple-constructor-form 'tuple args))

(defmacro dyn-tuple (&rest args)
  "Constructs a dyn-tuple according to the supplied argument subforms.  Each
argument subform can be a list of the form (`key-expr' `value-expr'), denoting
a mapping from the value of `key-expr' to the value of `value-expr'; or a list
of the form ($ `expression'), in which case the expression must evaluate to a
tuple, denoting all its mappings.  The result is constructed from the denoted
mappings in left-to-right order; so if a given key is supplied by more than one
argument subform, its associated value will be given by the rightmost such
subform."
  (expand-tuple-constructor-form 'dyn-tuple args))

(defun expand-tuple-constructor-form (type-name args)
  (labels ((recur (args result)
	     (cond ((null args) result)
		   ((not (and (listp (car args))
			      (= (length (car args)) 2)))
		    (error "Arguments to ~S must all be pairs expressed as 2-element~@
			    lists, or ($ x) subforms -- not ~S"
			   type-name (car args)))
		   ((eq (caar args) '$)
		    (if (equal result (ecase type-name
					(tuple `(empty-tuple))
					(dyn-tuple `(empty-dyn-tuple))))
			(recur (cdr args) (cadar args))
		      (recur (cdr args) `(tuple-merge ,result ,(cadar args)))))
		   (t
		    (recur (cdr args) `(with ,result ,(caar args) ,(cadar args)))))))
    (recur args `(empty-tuple))))


(defun |#{-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (case (peek-char nil stream t nil t)
    (#\|
     (read-char stream t nil t)
     `(map . ,(prog1
		  (read-delimited-list #\| stream t)
		(unless (eql (read-char stream) #\})
		  (error "Incorrect #{| ... |} syntax")))))
    (#\%
     (read-char stream t nil t)
     `(bag . ,(prog1
		  (read-delimited-list #\% stream t)
		(unless (eql (read-char stream) #\})
		  (error "Incorrect #{% ... %} syntax")))))
    (otherwise
     `(set . ,(read-delimited-list #\} stream t)))))

(defun |#[-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `(seq . ,(read-delimited-list #\] stream t)))

(defun |#~-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (unless (eql (read-char stream) #\<)
    (error "\"#~~\" must be followed by \"<\""))
  `(tuple . ,(read-delimited-list #\> stream t)))

(defun |#$-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  `($ ,(read stream t nil t)))

(defun |#%-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((subform (read stream t nil t)))
    (unless (and (consp subform) (consp (cdr subform)) (null (cddr subform)))
      (error "\"#%\" must be followed by a 2-element list."))
    `(% . ,subform)))


(defun fset-setup-readtable (readtable)
  "Adds FSet reader macros to `readtable'.  Returns `readtable'."
  (set-dispatch-macro-character #\# #\{ #'|#{-reader| readtable)
  (set-macro-character #\} (get-macro-character #\)) nil readtable)
  (set-dispatch-macro-character #\# #\[ #'|#[-reader| readtable)
  (set-macro-character #\] (get-macro-character #\)) nil readtable)
  (set-dispatch-macro-character #\# #\~ #'|#~-reader| readtable)
  (set-dispatch-macro-character #\# #\$ #'|#$-reader| readtable)
  (set-dispatch-macro-character #\# #\% #'|#%-reader| readtable)
  readtable)

(defvar *fset-readtable* (fset-setup-readtable (copy-readtable nil))
  "A copy of the standard readtable with FSet reader macros installed.")


;;; These function in the traditional Lisp manner, constructing the structures
;;; at read time.  They can therefore be used to read back previously printed
;;; structure containing FSet collections.
(defun |rereading-#{-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (case (peek-char nil stream t nil t)
    (#\|
     (read-char stream t nil t)
     (convert 'map (prog1
		       (read-delimited-list #\| stream t)
		     (unless (eql (read-char stream) #\})
		       (error "Incorrect #{| ... |} syntax")))
	      :value-fn #'cadr))
    (#\%
     (read-char stream t nil t)
     (let ((stuff (read-delimited-list #\% stream t))
	   (result (empty-bag)))
       (unless (eql (read-char stream) #\})
	 (error "Incorrect #{% ... %} syntax"))
       (dolist (x stuff)
	 (if (and (consp x) (eq (car x) '%))
	     (adjoinf result (cadr x) (caddr x))
	   (adjoinf result x)))
       result))
    (otherwise
     (convert 'set (read-delimited-list #\} stream t)))))

(defun |rereading-#[-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (convert 'seq (read-delimited-list #\] stream t)))

(defun |rereading-#~-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (unless (eql (read-char stream) #\<)
    (error "\"#~~\" must be followed by \"<\""))
  (let ((stuff (read-delimited-list #\> stream t))
	(result (empty-tuple)))
    (dolist (pr stuff)
      (unless (and (consp pr) (consp (cdr pr)) (null (cddr pr)))
	(error "~S is not a 2-element list." pr))
      (setf result (with result (get-tuple-key (car pr)) (cadr pr))))
    result))

(defun fset-setup-rereading-readtable (readtable)
  "Adds the FSet rereading reader macros to `readtable'.  These reader macros
will correctly read structure printed by the FSet print functions.  Returns
`readtable'."
  (set-dispatch-macro-character #\# #\{ #'|rereading-#{-reader| readtable)
  (set-macro-character #\} (get-macro-character #\)) nil readtable)
  (set-dispatch-macro-character #\# #\[ #'|rereading-#[-reader| readtable)
  (set-macro-character #\] (get-macro-character #\)) nil readtable)
  (set-dispatch-macro-character #\# #\~ #'|rereading-#~-reader| readtable)
  (set-dispatch-macro-character #\# #\% #'|#%-reader| readtable)
  readtable)

(defvar *fset-rereading-readtable* (fset-setup-rereading-readtable (copy-readtable nil))
  "A copy of the standard readtable with the rereading FSet reader macros
installed.  This readtable can be used to read structure printed by the FSet
print functions.")

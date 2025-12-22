(in-package :fset/jzon)


;;; Exported insted of `jzon:json-element'.
(deftype json-element ()
  '(or json-atom seq map tuple))


;;; ================================================================================
;;; Parsing

(defstruct (parser
	     (:constructor raw-make-parser (jzon-parser allow-multiple-content max-depth object-type)))
  jzon-parser
  allow-multiple-content
  max-depth
  object-type)

(defmacro with-parser ((var in &rest kwd-args
			&key allow-comments allow-trailing-comma allow-multiple-content
			  max-string-length max-depth key-fn object-type
			  key-package key-case-mode key-prefix key-suffix)
		       &body body)
  "Binds `var' around `body' to an FSet/Jzon parser that will read from `in',
which can be a vector, a stream, or a pathname.  If `allow-comments' is true,
comments as in C \(`//' for single-line or `/* ... */' for block\) will be
accepted.  If `allow-trailing-comma', a comma will be permitted after the last
element of an array or object.  `max-string-length' limits the length of
strings, and `map-depth' the maximum nesting depth; these help protect against
DoS attacks.  `key-fn' controls whether and how string pooling is done; the
default uses an FSet set.

If `allow-multiple-content' is true, the input may contain multiple top-level
JSON values, which you can read by calling `parse-top-level' multiple times.

JSON arrays are returned as FSet seqs; JSON objects are returned according to
`object-type', which can be `replay-map', `map', or `tuple'.  If
`key-package' is supplied, keys will be interned in this package; this option
overrides `key-fn'.  If `object-type' is `tuple', `key-package' is
required, and may not be the CL keyword package.  If `key-package' is supplied:
\(a\) `key-case-mode' can be any of `:upcase', `:downcase', or `:preserve'; the
default is `:upcase'.  \(b\) If supplied, `key-prefix' is prepended and
`key-suffix' is appended to the case-converted key before interning."
  (declare (ignore allow-comments allow-trailing-comma allow-multiple-content
		   max-string-length max-depth key-fn object-type
		   key-package key-case-mode key-prefix key-suffix))
  `(let ((,var (make-parser ,in . ,kwd-args)))
     (unwind-protect (progn . ,body)
       (close-parser ,var))))

(defun make-parser (in &key allow-comments allow-trailing-comma allow-multiple-content
			 (max-string-length (min 1048576 (1- array-dimension-limit)))
			 (max-depth 128) (key-fn (make-fset-string-pool))
			 (object-type 'replay-map)
			 key-package (key-case-mode ':upcase) (key-prefix "") (key-suffix ""))
  "Constructs an FSet/Jzon parser that will read from `in', which can be a
vector, a stream, or a pathname.  If `allow-comments' is true, comments as in
C \(`//' for single-line or `/* ... */' for block\) will be accepted.  If
`allow-trailing-comma', a comma will be permitted after the last element of
an array or object.  `max-string-length' limits the length of strings, and
`map-depth' the maximum nesting depth; these help protect against DoS attacks.
`key-fn' controls whether and how string pooling is done; the default uses
an FSet set.

The returned parser should be closed after use; see `close-parser', and macro
`with-parser' which closes it automatically.

If `allow-multiple-content' is true, the input may contain multiple top-level
JSON values, which you can read by calling `parse-top-level' multiple times.

JSON arrays are returned as FSet seqs; JSON objects are returned according to
`object-type', which can be `replay-map', `map', or `tuple'.  If
`key-package' is supplied, keys will be interned in this package; this option
overrides `key-fn'.  If `object-type' is `tuple', `key-package' is
required, and may not be the CL keyword package.  If `key-package' is supplied:
\(a\) `key-case-mode' can be any of `:upcase', `:downcase', or `:preserve'; the
default is `:upcase'.  \(b\) If supplied, `key-prefix' is prepended and
`key-suffix' is appended to the case-converted key before interning."
  (check-type max-depth (integer 1 65535))
  (check-type object-type (member replay-map map tuple))
  (check-type key-package (or null string symbol package))
  (check-type key-case-mode (member :upcase :downcase :preserve))
  (when (eq object-type 'tuple)
    (when (null key-package)
      (error "To use object-type `tuple', you must also supply a `key-package'"))
    (unless (packagep key-package)
      (setq key-package (uiop:find-package* key-package)))
    (when (eq key-package (symbol-package ':upcase))
      ;; The problem is that keyword symbols are bound to themselves; we want to bind key names
      ;; to their key objects.
      (error "When object-type is `tuple', `key-package' may not be the keyword package")))
  (let ((key-prefix (string key-prefix))
	(key-suffix (string key-suffix))
	((jzon-parser
	   (com.inuoe.jzon:make-parser in :allow-comments allow-comments :allow-trailing-comma allow-trailing-comma
					  :allow-multiple-content allow-multiple-content
					  :max-string-length max-string-length
					  :key-fn (if key-package
						      (lambda (s)
							(declare (optimize (speed 3))
								 (string s))
							(let ((s (ecase key-case-mode
								   (:upcase (string-upcase s))
								   (:downcase (string-downcase s))
								   (:preserve s)))
							      ((s (if (and (string= key-prefix "")
									   (string= key-suffix ""))
								      s
								    (concatenate 'string key-prefix s key-suffix)))))
							  (intern s key-package)))
						    key-fn)))))
    (raw-make-parser jzon-parser allow-multiple-content max-depth object-type)))

(defun close-parser (parser)
  "Closes the parser.  If the input was specified as a pathname, closes the
input stream."
  (com.inuoe.jzon:close-parser (parser-jzon-parser parser)))

(defun parse-top-level (parser)
  "Reads one top-level JSON object from `parser'.  If `parser' was created
with `allow-multiple-content' true, this can be called repeatedly; it will
return `nil' when there are no more top-level objects in the input."
  (let ((ignore result (parse-value parser 0)))
    (declare (ignore ignore))
    (unless (parser-allow-multiple-content parser)
      (let ((event (com.inuoe.jzon:parse-next (parser-jzon-parser parser))))
	(when event
	  (error "Unexpected text following the top-level value"))))
    result))

(defun parse (in &key allow-comments allow-trailing-comma
		   (max-string-length (min 1048576 (1- array-dimension-limit)))
		   (max-depth 128) (key-fn (make-fset-string-pool))
		   (object-type 'replay-map)
		   key-package (key-case-mode ':upcase) (key-prefix "") (key-suffix ""))
  "Constructs an FSet/Jzon parser that will read from `in', which can be a
vector, a stream, or a pathname, and reads one top-level object from it.
If `allow-comments' is true, comments as in C \(`//' for single-line or
`/* ... */' for block\) will be accepted.  If `allow-trailing-comma', a
comma will be permitted after the last element of an array or object.
`max-string-length' limits the length of strings, and `map-depth' the maximum
nesting depth; these help protect against DoS attacks.  `key-fn' controls
whether and how string pooling is done; the default uses an FSet set.

JSON arrays are returned as FSet seqs; JSON objects are returned according to
`object-type', which can be `replay-map', `map', or `tuple'.  If
`key-package' is supplied, keys will be interned in this package; this option
overrides `key-fn'.  If `object-type' is `tuple', `key-package' is
required, and may not be the CL keyword package.  If `key-package' is supplied:
\(a\) `key-case-mode' can be any of `:upcase', `:downcase', or `:preserve'; the
default is `:preserve'.  \(b\) If supplied, `key-prefix' is prepended and
`key-suffix' is appended to the case-converted key before interning."
  (with-parser (parser in :allow-comments allow-comments :allow-trailing-comma allow-trailing-comma
			  :max-string-length max-string-length :max-depth max-depth :key-fn key-fn
			  :object-type object-type
			  :key-package key-package :key-case-mode key-case-mode
			  :key-prefix key-prefix :key-suffix key-suffix)
    (parse-top-level parser)))

(defun parse-value (parser depth)
  (declare (optimize (debug 3))
	   (fixnum depth))
  (flet ((check-depth ()
	   (when (= depth (parser-max-depth parser))
	     (signal-parse-error parser 'json-parse-limit-error (parser-max-depth parser)
				 "Maximum depth exceeded."))))
    (let ((event value (com.inuoe.jzon:parse-next (parser-jzon-parser parser))))
      (ecase event
	(:value (values event value))
	((nil) (values nil nil))
	(:begin-array
	  (check-depth)
	  (let ((result (seq)))
	    (loop
	      (let ((ev val (parse-value parser (1+ depth))))
		(when (eq ev ':end-array)
		  (return (values ':value result)))
		(push-last result val)))))
	(:begin-object
	  (check-depth)
	  (let ((result (ecase (parser-object-type parser)
			  (replay-map (replay-map))
			  (map (map))
			  (tuple (tuple)))))
	    (loop
	      (let ((kev kval (com.inuoe.jzon:parse-next (parser-jzon-parser parser))))
		(when (eq kev ':end-object)
		  (return (values ':value result)))
		(unless (eq kev ':object-key)
		  (error "Syntax error: expected object key; found ~S ~S" kev kval))
		(let ((ev val (parse-value parser (1+ depth)))
		      ((kval (if (eq (parser-object-type parser) 'tuple)
				 (if (boundp kval) (symbol-value kval)
				   (get-tuple-key kval))
			       kval))))
		  (unless (eq ev ':value)
		    (error "Internal error: expected value; found ~S ~S" ev val))
		  (setf (@ result kval) val))))))
	((:end-array :end-object)
	  event)))))

(defun signal-parse-error (parser error-class limit format &rest args)
  ;; Depends on Jzon internals.
  (let ((line column (funcall (slot-value (parser-jzon-parser parser) 'com.inuoe.jzon::%pos))))
    (error error-class :format-control format :format-arguments args
		       :line line :column column :limit limit)))

(defun make-fset-string-pool ()
  (let ((pool (set)))
    (lambda (k)
      (let ((present? prev-k (@ pool k)))
	(if present? prev-k
	  (progn
	    (includef pool k)
	    k))))))

;;; ================================================================================
;;; Writing

;;; We just re-export the main Jzon writing operations; the methods below handle
;;; writing the FSet types.

(defmethod coerce-key ((key seq))
  (convert 'string key))

(defgeneric coerce-tuple-key (key key-name-package-as-keyword)
  (:documentation
    "Generic function called to turn a `tuple-key' into a string.  The first
argument is the key; the second is the key's name's package name, as a
keyword symbol.  So you can write methods like

  (defmethod coerce-tuple-key (key (pkg (eql :my-package)))
    ...)")
  (:method (key pkg)
    "The default method returns the key's name in lowercase."
    (declare (ignore pkg))
    (string-downcase (tuple-key-name key))))

(defmethod coerce-key ((key tuple-key))
  (coerce-tuple-key key (intern (package-name (symbol-package (tuple-key-name key)))
				(symbol-package ':foo))))

(defmethod coerce-tuple-key (key pkg-name)
  (declare (ignore pkg-name))
  (symbol-name (tuple-key-name key)))

(defmethod com.inuoe.jzon:write-value ((writer writer) (s seq))
  (com.inuoe.jzon:with-array writer
    (do-seq (x s)
      (com.inuoe.jzon:write-value writer x))))

(defmethod com.inuoe.jzon:write-value ((writer writer) (m map))
  (com.inuoe.jzon:with-object writer
    (do-map (k v m)
      ;; Looks like `write-property' was intended to be exported.
      (com.inuoe.jzon::write-property writer k v))))

(defmethod com.inuoe.jzon:write-value ((writer writer) (tup tuple))
  (com.inuoe.jzon:with-object writer
    (do-tuple (k v tup)
      ;; Jzon will downcase the key, unless it is in mixed case already; there's no option
      ;; to override this behavior.
      (let ((type (tuple-key-type k))
	    ;; Jzon lets you further customize this behavior by specializing `coerced-fields'.
	    ;; A similar feature could be added here.
	    ((v (or v (cond ((and (subtypep 'boolean type) (subtypep type 'boolean))
			     nil)
			    ((and (subtypep 'list type) (subtypep type 'list))
			     (seq))
			    (t 'null))))))
	(com.inuoe.jzon::write-property writer k v)))))


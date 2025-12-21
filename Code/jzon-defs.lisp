(in-package :cl-user)

(defpackage :FSet/Jzon
  (:use :cl :new-let)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :fset2 #:set #:map)
  (:import-from :fset #:gensymx)
  (:import-from :fset2
		#:seq #:replay-map #:tuple #:push-last #:@ #:convert #:includef #:get-tuple-key
		#:do-seq #:do-map #:do-tuple #:tuple-key #:tuple-key-name #:tuple-key-type #:char-seq?)
  ;; Exports of names defined in this system
  (:export #:json-element #:with-parser #:make-parser #:close-parser #:parse #:parse-top-level
	   #:coerced-fields #:coerce-tuple-key)
  ;; We import and re-export (our guess at) the most commonly used parts of the Jzon API,
  ;; mostly writer-related.
  (:import-from :com.inuoe.jzon
		#:span #:stringify #:json-atom
		#:json-error #:json-limit-error #:json-parse-error #:json-parse-limit-error #:json-eof-error
		#:json-write-error #:json-write-limit-error #:json-recursive-write-error
		#:coerced-fields #:coerce-key
		#:writer #:make-writer #:close-writer #:with-writer)
  (:export #:span #:stringify #:json-atom
	   #:json-error #:json-limit-error #:json-parse-error #:json-parse-limit-error #:json-eof-error
	   #:json-write-error #:json-write-limit-error #:json-recursive-write-error
	   #:coerced-fields #:coerce-key
	   #:writer #:make-writer #:close-writer #:with-writer)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)))

(defpackage :FSet/Jzon/Test
  (:use :cl :new-let :fset/jzon)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :fset2 #:set #:map)
  (:import-from :fset2
		#:equal? #:seq #:replay-map #:tuple #:push-last #:@ #:includef #:define-tuple-key
		#:do-seq #:do-map #:do-tuple #:tuple-key-name))

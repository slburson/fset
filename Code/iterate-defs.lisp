;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: iterate-defs.lisp
;;; Contents: Package definition for FSet/Iterate
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)

(defpackage :FSet/Iterate
  (:nicknames :com.sympoiesis.fset/iterate)
  (:use :cl :fset :iterate :new-let)
  (:shadowing-import-from :fset
			  ;; Exported by Iterate
			  #:with
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery)
  (:shadowing-import-from :new-let #:let #:cond)
  (:import-from :iterate #:defclause #:defclause-driver #:top-level-check #:make-var-and-binding
		#:do-dsetq #:*loop-end* #:*loop-end-used?* #:*result-var* #:return-driver-code
		#:clause-error #:local-binding-check #:make-accum-var-binding #:extract-var
		#:walk-expr #:return-code #:return-reduction-code #:make-accum-var-default-binding
		#:if-1st-time)
  (:shadow #:unioning)
  (:export #:collect-set #:collect-bag #:collect-seq #:collect-map #:collect-map-to-sets
	   #:unioning #:intersecting #:concating #:map-unioning #:map-intersecting))


(defpackage :FSet/Iterate/Test
  (:nicknames :com.sympoiesis.fset/iterate/test)
  (:use :cl :fset :iterate :fset/iterate :new-let)
  (:shadowing-import-from :fset
			  ;; Exported by Iterate
			  #:with
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :fset/iterate #:unioning))

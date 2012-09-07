;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

(in-package :cl-user)

;;; File: defs.lisp
;;; Contents: Package, system, and other build-related definitions for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


(defpackage :fset
  (:use :cl :gmap :new-let :lexical-contexts)
  (:shadowing-import-from :new-let #:let #:cond)
  ;; For each of these shadowed symbols, using packages must either shadowing-
  ;; import it or shadowing-import the original Lisp symbol.
  (:shadow ;; Shadowed type/constructor names
	   #:set #:map
	   ;; Shadowed set operations
	   #:union #:intersection #:set-difference #:complement
	   ;; Shadowed sequence operations
	   #:first #:last #:subseq #:reverse #:sort #:stable-sort #:reduce
	   #:find #:find-if #:find-if-not
	   #:count #:count-if #:count-if-not
	   #:position #:position-if #:position-if-not
	   #:remove #:remove-if #:remove-if-not
	   #:substitute #:substitute-if #:substitute-if-not
	   #:some #:every #:notany #:notevery)
  (:export #:collection #:set #:bag #:map #:seq #:tuple
	   #:collection? #:set? #:bag? #:map? #:seq? #:tuple?
	   #:wb-set #:wb-bag #:wb-map #:wb-seq #:dyn-tuple
	   ;; `Equal?' is exported because users may want to call it; `Compare'
	   ;; because they may want to extend it; and `Compare-Slots' because it's
	   ;; useful in extending `Compare'.  But `Less-Than?' and `Greater-Than?'
	   ;; are unlikely to be useful in user code.
	   #:equal? #:compare #:compare-slots #:identity-ordering-mixin
	   #:define-cross-type-compare-methods
	   #:compare-lexicographically
	   #:empty? nonempty? #:size #:set-size #:arb
	   #:contains? #:domain-contains? #:range-contains? #:member? #:multiplicity
	   #:empty-set #:empty-bag #:empty-map #:empty-seq #:empty-tuple
	   #:empty-wb-set #:empty-wb-bag #:empty-wb-map #:empty-wb-seq
	   #:empty-dyn-tuple
	   #:least #:greatest #:lookup #:@
	   #:with #:less #:split-from #:split-above #:split-through #:split-below
	   #:union #:bag-sum #:intersection #:bag-product #:complement
	   #:set-difference #:set-difference-2 #:bag-difference
	   #:subset? #:disjoint? #:subbag?
	   #:filter #:filter-pairs #:partition
	   #:image #:reduce #:domain #:range #:with-default
	   #:map-union #:map-intersection #:map-difference-2
	   #:restrict #:restrict-not #:compose #:map-default
	   #:first #:last
	   #:lastcons #:head #:tail
	   #:with-first #:less-first #:push-first #:pop-first
	   #:with-last #:less-last #:push-last #:pop-last #:appendf #:prependf
	   #:insert #:splice #:subseq #:concat #:reverse #:sort #:stable-sort
	   #:find #:find-if #:find-if-not
	   #:count #:count-if #:count-if-not
	   #:position #:position-if #:position-if-not
	   #:remove #:remove-if #:remove-if-not
	   #:substitute #:substitute-if #:substitute-if-not
	   #:convert #:iterator 
	   #:do-set #:do-bag #:do-bag-pairs #:do-map #:do-seq #:do-tuple
	   #:adjoinf #:removef #:includef #:excludef
	   #:unionf #:intersectf #:imagef #:composef
	   #:define-tuple-key #:def-tuple-key #:get-tuple-key #:tuple-key-name
	   #:tuple-merge
	   #:fset-setup-readtable #:*fset-readtable*
	   #:$
	   ;; Used by the bag methods that convert to and from lists.
	   #:alist
	   ;; Bounded sets
	   #:bounded-set #:make-bounded-set #:bounded-set-contents
	   ;; Relations
	   #:relation #:bin-rel #:wb-bin-rel #:empty-bin-rel #:empty-wb-bin-rel
	   #:lookup-inv #:inverse #:join #:conflicts #:map-to-sets
	   #:list-relation #:wb-list-relation #:empty-list-relation
	   #:empty-wb-list-relation #:arity #:query #:query-multi #:do-list-relation
	   #:query-registry #:empty-query-registry #:with-query #:less-query
	   #:all-queries #:lookup-multi #:forward-key #:lookup-restricted
	   #:lookup-multi-restricted))


;;; A convenient package for experimenting with FSet.  Also serves as an example
;;; of how one might create a package for code to be written using FSet.  Note,
;;; though, that for each of the shadowing-imported symbols, it's up to you whether
;;; to import the FSet version or the CL version.  It's also up to you, of course,
;;; whether you want to use GMap and New-Let.
;;; You may also wish to do:
;;;   (setq *readtable* *fset-readtable*)
(defpackage :fset-user
  (:use :cl :fset :gmap :new-let :lexical-contexts)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :fset
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
			  #:some #:every #:notany #:notevery))


;;; The seq implementation tries to use strings for leaf vectors when possible.
;;; In some Lisp implementations, there are two kinds of strings; but in some
;;; of these, the larger form takes as much space as a general vector, so nothing
;;; is to be saved by using it.
(when (and (not (typep (make-string 1 :element-type 'extended-char) 'base-string))
	   (not (and (> (integer-length (1- char-code-limit)) 16)
		     (< (integer-length most-positive-fixnum) 32))))
  (pushnew ':FSet-Ext-Strings *features*))


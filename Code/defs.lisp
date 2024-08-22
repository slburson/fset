;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

;;; File: defs.lisp
;;; Contents: Package, system, and other build-related definitions for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(in-package :cl-user)


(defpackage :fset
  (:use :cl :new-let :lexical-contexts :rev-fun-bind)
  (:import-from :gmap #:gmap #:alist #:constant #:index #:index-inc #:sum)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :mt19937 #:make-random-state #:random #:*random-state*)
  ;; For each of these shadowed symbols, using packages must either shadowing-
  ;; import it or shadowing-import the original Lisp symbol.
  (:shadow ;; Shadowed type/constructor names
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
  (:export #:collection #:set #:bag #:map #:seq #:tuple
	   #:collection? #:set? #:bag? #:map? #:seq? #:tuple?
	   #:wb-set #:wb-bag #:wb-map #:wb-seq #:dyn-tuple
	   #:ch-set #:ch-map
	   #:replay-set #:replay-set? #:wb-replay-set #:wb-replay-set?
	   #:replay-map #:replay-map? #:wb-replay-map #:wb-replay-map?
	   ;; `Equal?' is exported because users may want to call it; `Compare'
	   ;; because they may want to extend it; and `Compare-Slots' because it's
	   ;; useful in extending `Compare'.  But `Less-Than?' and `Greater-Than?'
	   ;; are unlikely to be useful in user code.
	   #:equal? #:compare #:compare-slots #:compare-slots-no-unequal #:define-comparison-slots
	   #:identity-ordering-mixin
	   #:define-cross-type-compare-methods
	   #:compare-lexicographically
	   #:empty? #:nonempty? #:size #:set-size #:arb
	   #:contains? #:domain-contains? #:range-contains? #:member? #:multiplicity
	   #:empty-set #:empty-bag #:empty-map #:empty-seq #:empty-tuple
	   #:empty-wb-set #:empty-wb-bag #:empty-wb-map #:empty-wb-seq
	   #:empty-dyn-tuple #:empty-ch-set #:empty-ch-map
	   #:empty-replay-set #:empty-wb-replay-set #:empty-replay-map #:empty-wb-replay-map
	   #:least #:greatest #:lookup #:rank #:at-rank #:at-index #:@
	   #:with #:less #:split-from #:split-above #:split-through #:split-below
	   #:union #:bag-sum #:intersection #:bag-product #:complement
	   #:set-difference #:set-difference-2 #:bag-difference #:bag-pairs
	   #:subset? #:disjoint? #:subbag?
	   #:filter #:filter-pairs #:partition
	   #:image #:reduce #:domain #:range #:with-default #:update
	   #:map-union #:map-intersection #:map-difference-2
	   #:restrict #:restrict-not #:compose #:map-default
	   #:first #:last
	   #:lastcons #:head #:tail
	   #:with-first #:less-first #:push-first #:pop-first
	   #:with-last #:less-last #:push-last #:pop-last #:appendf #:prependf
	   #:insert #:splice #:subseq #:concat #:reverse #:sort #:stable-sort #:sort-and-group
	   #:find #:find-if #:find-if-not
	   #:count #:count-if #:count-if-not
	   #:position #:position-if #:position-if-not
	   #:remove #:remove-if #:remove-if-not
	   #:substitute #:substitute-if #:substitute-if-not
	   #:some #:every #:notany #:notevery
	   #:convert #:iterator
	   #:do-set #:do-bag #:do-bag-pairs #:do-map #:do-map-domain #:do-seq #:do-tuple
	   #:adjoinf #:removef #:includef #:excludef
	   #:unionf #:intersectf #:set-differencef #:map-unionf #:map-intersectf #:imagef #:composef
	   #:define-tuple-key #:def-tuple-key #:get-tuple-key #:tuple-key-name #:tuple-key?
	   #:tuple-merge
	   #:fset-setup-readtable #:*fset-readtable*
	   #:$ #:%
	   ;; Used by the bag methods that convert to and from lists.
	   #:alist
	   ;; Bounded sets
	   #:bounded-set #:make-bounded-set #:bounded-set-contents
	   ;; Complement sets
	   #:full-set
	   ;; Relations
	   #:relation #:relation? #:2-relation #:2-relation? #:wb-2-relation #:wb-2-relation?
	   #:empty-2-relation #:empty-wb-2-relation #:do-2-relation
	   #:lookup-inv #:inverse #:join #:conflicts #:map-to-sets
	   #:list-relation #:list-relation? #:wb-list-relation #:wb-list-relation?
	   #:empty-list-relation #:empty-wb-list-relation #:do-list-relation
	   #:arity #:query #:query-multi #:query-multi-restricted
	   #:assertion-db #:empty-assertion-db #:empty-wb-assertion-db
	   #:query-registry #:empty-query-registry #:with-query #:less-query
	   #:all-queries #:do-all-queries #:lookup-multi #:forward-key #:lookup-restricted
	   #:lookup-multi-restricted

           ;; named-readtable readtable
           #:fset-readtable))


;;; The need has arisen to define a second FSet package.  There are two motivations:
;;; () the planned introduction of the CHAMP data structure for sets, maps, and bags,
;;; which will become the default, incompatibly altering the behavior of the corresponding
;;; constructor macros, and requiring methods on `hash-value' as well as `compare' for
;;; user types;
;;; () miscellaneous infelicities in the API which cannot be fixed compatibly.
;;;
;;; The plan is for both packages to continue to exist indefinitely.  New client code is
;;; encouraged to use package `fset2' instead of `fset'; existing clients may update
;;; their code at their leisure.
;;; NOTE: This package is not ready for use yet.  I will announce when it is.
(defpackage :fset2
  (:use :cl :fset :new-let :lexical-contexts)
  (:import-from :gmap #:gmap #:alist #:constant #:index #:sum)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :mt19937 #:make-random-state #:random #:*random-state*)
  ;; For each of these shadowed symbols, using packages must either shadowing-
  ;; import it or shadowing-import the original Lisp symbol.
  (:shadow ;; Shadowed type/constructor names
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
	   #:some #:every #:notany #:notevery
	   ;; Additional shadowed names from `fset:'
	   #:map-intersection)
  (:export #:collection #:set #:bag #:map #:seq #:tuple
	   #:collection? #:set? #:bag? #:map? #:seq? #:tuple?
	   #:wb-set #:wb-bag #:wb-map #:wb-seq #:dyn-tuple
	   ;; `Equal?' is exported because users may want to call it; `Compare'
	   ;; because they may want to extend it; and `Compare-Slots' because it's
	   ;; useful in extending `Compare'.  But `Less-Than?' and `Greater-Than?'
	   ;; are unlikely to be useful in user code.
	   #:equal? #:compare #:compare-slots #:compare-slots-no-unequal
	   #:hash-value ; new for FSet2
	   #:identity-ordering-mixin
	   #:define-cross-type-compare-methods
	   #:compare-lexicographically
	   #:empty? #:nonempty? #:size #:set-size #:arb
	   #:contains? #:domain-contains? #:range-contains? #:member? #:multiplicity
	   #:empty-set #:empty-bag #:empty-map #:empty-seq #:empty-tuple
	   #:empty-wb-set #:empty-wb-bag #:empty-wb-map #:empty-wb-seq
	   #:empty-dyn-tuple
	   #:least #:greatest #:lookup #:rank #:at-rank #:@
	   #:with #:less #:split-from #:split-above #:split-through #:split-below
	   #:union #:bag-sum #:intersection #:bag-product #:complement
	   #:set-difference #:set-difference-2 #:bag-difference #:bag-pairs
	   #:subset? #:disjoint? #:subbag?
	   #:filter #:filter-pairs #:partition
	   #:image #:reduce #:domain #:range #:with-default #:update
	   #:map-union #:map-intersection #:map-difference-2
	   #:restrict #:restrict-not #:compose #:map-default
	   #:first #:last
	   #:lastcons #:head #:tail
	   #:with-first #:less-first #:push-first #:pop-first
	   #:with-last #:less-last #:push-last #:pop-last #:appendf #:prependf
	   #:insert #:splice #:subseq #:concat #:reverse #:sort #:stable-sort #:sort-and-group
	   #:find #:find-if #:find-if-not
	   #:count #:count-if #:count-if-not
	   #:position #:position-if #:position-if-not
	   #:remove #:remove-if #:remove-if-not
	   #:substitute #:substitute-if #:substitute-if-not
	   #:some #:every #:notany #:notevery
	   #:convert #:iterator
	   #:do-set #:do-bag #:do-bag-pairs #:do-map #:do-map-domain #:do-seq #:do-tuple
	   #:adjoinf #:removef #:includef #:excludef
	   #:unionf #:intersectf #:set-differencef #:map-unionf #:map-intersectf #:imagef #:composef
	   #:define-tuple-key #:def-tuple-key #:get-tuple-key #:tuple-key-name #:tuple-key?
	   #:tuple-merge
	   #:fset-setup-readtable #:*fset-readtable*
	   #:$ #:%
	   ;; Used by the bag methods that convert to and from lists.
	   #:alist
	   ;; Bounded sets
	   #:bounded-set #:make-bounded-set #:bounded-set-contents
	   ;; Complement sets
	   #:full-set
	   ;; Relations
	   #:relation #:relation? #:2-relation #:2-relation? #:wb-2-relation #:wb-2-relation?
	   #:empty-2-relation #:empty-wb-2-relation #:do-2-relation
	   #:lookup-inv #:inverse #:join #:conflicts #:map-to-sets
	   #:list-relation #:list-relation? #:wb-list-relation #:wb-list-relation?
	   #:empty-list-relation #:empty-wb-list-relation #:do-list-relation
	   #:arity #:query #:query-multi #:query-multi-restricted
	   #:assertion-db #:empty-assertion-db #:empty-wb-assertion-db
	   #:query-registry #:empty-query-registry #:with-query #:less-query
	   #:all-queries #:do-all-queries #:lookup-multi #:forward-key #:lookup-restricted
	   #:lookup-multi-restricted

           ;; named-readtable readtable
           #:fset-readtable))


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


(pushnew ':FSet *features*)

;;; The seq implementation tries to use strings for leaf vectors when possible.
;;; In some Lisp implementations, there are two kinds of strings; but in some
;;; of these, the larger form takes as much space as a general vector, so nothing
;;; is to be saved by using it.
(when (and (not (typep (make-string 1 :element-type 'character :initial-element #\A)
		       'base-string))
	   (not (and (> (integer-length (1- char-code-limit)) 16)
		     (< (integer-length most-positive-fixnum) 32))))
  (pushnew ':FSet-Ext-Strings *features*))


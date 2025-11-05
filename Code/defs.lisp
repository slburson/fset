;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

;;; File: defs.lisp
;;; Contents: Package, system, and other build-related definitions for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :cl-user)


(defpackage :fset
  (:nicknames :com.ergy.fset :com.sympoiesis.fset)
  (:use :cl :new-let :lexical-contexts :rev-fun-bind :misc-extensions.define-class)
  (:import-from :gmap #:gmap #:alist #:constant #:index #:index-inc #:sum)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :mt19937 #:make-random-state #:random #:*random-state*)
  #+allegro (:implementation-packages "FSET" "FSET2")
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
	   #:replay-set #:replay-set? #:wb-replay-set #:wb-replay-set? #:ch-replay-set #:ch-replay-set?
	   #:replay-map #:replay-map? #:wb-replay-map #:wb-replay-map? #:ch-replay-map #:ch-replay-map?
	   #:wb-custom-set #:wb-custom-bag #:wb-custom-map
	   #:ch-custom-set #:ch-custom-map
	   #:wb-custom-replay-set #:wb-custom-replay-map #:ch-custom-replay-set #:ch-custom-replay-map
	   #:equal? #:compare #:compare-slots #:compare-slots-no-unequal #:hash-slots #:eql-compare
	   #:define-equality-slots #:define-comparison-slots #:define-hash-function
	   #:unwrap-equivalent-node ; custom comparison functions may have to call this
	   #:hash-value #:hash-value-fixnum #:zero #:hash-mix #:hash-mixf #:hash-multiply
	   #:identity-equality-mixin #:identity-ordering-mixin
	   #:define-cross-type-compare-methods
	   #:compare-lexicographically #:compare-lists-lexicographically #:compare-strings-lexicographically
	   #:compare-vectors-lexicographically #:compare-seqs-lexicographically
	   #:empty? #:nonempty? #:size #:set-size #:arb
	   #:contains? #:domain-contains? #:range-contains? #:member? #:multiplicity
	   #:empty-set #:empty-bag #:empty-map #:empty-seq #:empty-tuple
	   #:empty-wb-set #:empty-wb-bag #:empty-wb-map #:empty-wb-seq
	   #:empty-dyn-tuple #:empty-ch-set #:empty-ch-map
	   #:empty-replay-set #:empty-wb-replay-set #:empty-replay-map #:empty-wb-replay-map
	   #:empty-ch-replay-set #:empty-ch-replay-map
	   #:least #:greatest #:lookup #:rank #:at-rank #:index #:at-index #:@
	   #:with #:less #:split-from #:split-above #:split-through #:split-below
	   #:union #:bag-sum #:intersection #:bag-product #:complement
	   #:set-difference #:set-difference-2 #:bag-difference #:bag-pairs #:wb-bag-pairs
	   #:subset? #:proper-subset? #:disjoint? #:subbag? #:proper-subbag?
	   #:filter #:filter-pairs #:partition
	   #:image #:reduce #:domain #:range #:update
	   #:with-default #:default
	   #:map-union #:map-intersection #:map-difference-2
	   #:restrict #:restrict-not #:compose
	   #:first #:last
	   #:lastcons #:head #:tail
	   #:with-first #:less-first #:push-first #:pop-first
	   #:with-last #:less-last #:push-last #:pop-last #:appendf #:prependf #:insertf #:splicef
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
	   #:fset-setup-rereading-readtable #:*fset-rereading-readtable*
	   #:$ #:% 			; for the 'map' and 'bag' constructor macros
	   #:?				; for `query' on 'list-relation'
	   ;; Used by the bag methods that convert to and from lists.
	   #:alist
	   ;; Miscellaneous GMap arg types
	   #:fun-sequence #:fun-bag-pairs #:fun-map
	   ;; Miscellaneous GMap result types
	   #:map-to-sets #:append-unique
	   ;; Bounded sets
	   #:bounded-set #:make-bounded-set #:bounded-set-contents
	   ;; Complement sets
	   #:full-set
	   ;; Relations
	   #:relation #:relation? #:2-relation #:2-relation? #:wb-2-relation #:wb-2-relation?
	   #:empty-2-relation #:empty-wb-2-relation #:empty-ch-2-relation #:do-2-relation
	   #:lookup-inv #:inverse #:join #:conflicts #:map-to-sets
	   #:list-relation #:list-relation? #:wb-list-relation #:wb-list-relation?
	   #:empty-list-relation #:empty-wb-list-relation #:empty-ch-list-relation #:do-list-relation
	   #:arity #:query #:query-multi #:query-multi-restricted
	   #:assertion-db #:empty-assertion-db #:empty-wb-assertion-db
	   #:query-registry #:empty-query-registry #:empty-ch-query-registry #:with-query #:less-query
	   #:all-queries #:do-all-queries #:lookup-multi #:forward-key #:lookup-restricted
	   #:lookup-multi-restricted

           ;; named-readtable readtables
           #:fset-readtable #:fset-rereading-readtable))

;;; Since we've shadowed `cl:count', we need to do this.
(gmap:def-result-type-synonym fset:count cl:count)

;;; We want to import this below, so make sure it exists.
(defvar fset::erapmoc)


;;; For a discussion of how the `fset2' package differs from `fset', see:
;;; https://gitlab.common-lisp.net/fset/fset/-/merge_requests/2
(defpackage :fset2
  (:nicknames :com.ergy.fset2 :com.sympoiesis.fset2)
  (:use :cl :fset :new-let :lexical-contexts :rev-fun-bind :misc-extensions.define-class)
  (:import-from :gmap #:gmap #:alist #:constant #:index #:sum)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :mt19937 #:make-random-state #:random #:*random-state*)
  #+allegro (:implementation-packages "FSET" "FSET2")
  ;; For each of these shadowed CL symbols, using packages must either shadowing-
  ;; import it or shadowing-import the original Lisp symbol.
  (:shadowing-import-from :fset
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
			  ;; Other exported symbols
			  #:wb-bag-pairs #:$ #:% #:?
			  ;; Internal, for testing
			  #:erapmoc)
  ;; These are shadowed `fset:' symbols, with different definitions in `fset2:'.
  (:shadow ;; Names shadowed from `fset:' to implement FSet2 semantics
	   #:set #:map #:wb-map #:wb-custom-map #:ch-map #:ch-custom-map #:seq #:wb-seq
	   #:replay-map #:wb-replay-map #:wb-custom-replay-map #:ch-replay-map #:ch-custom-replay-map
	   #:empty-set #:empty-map #:empty-wb-map #:empty-ch-map #:empty-seq #:empty-wb-seq
	   #:rank #:define-tuple-key #:get-tuple-key #:map-to-sets #:concat
	   ;; These just changed from `&optional' to `&key'
	   #:empty-wb-set #:empty-ch-set #:empty-wb-bag #:empty-wb-replay-set #:empty-ch-replay-set
	   #:empty-replay-map #:empty-wb-replay-map #:empty-ch-replay-map
	   #:empty-wb-2-relation #:empty-ch-2-relation
	   #:empty-list-relation #:empty-wb-list-relation #:empty-ch-list-relation
	   ;; Map and seq operations that handle defaults differently
	   #:lookup #:map-union #:map-intersection #:map-difference-2 #:compose
	   ;; Functions that call `lookup', transitively
	   #:internal-lookup #:@ #:image #:filter #:partition)
  (:export #:collection #:set #:bag #:map #:seq #:tuple
	   #:collection? #:set? #:bag? #:map? #:seq? #:tuple?
	   #:wb-set #:wb-bag #:wb-map #:wb-seq #:dyn-tuple
	   #:ch-set #:ch-map
	   #:replay-set #:replay-set? #:wb-replay-set #:wb-replay-set? #:ch-replay-set #:ch-replay-set?
	   #:replay-map #:replay-map? #:wb-replay-map #:wb-replay-map? #:ch-replay-map #:ch-replay-map?
	   #:wb-custom-set #:wb-custom-bag #:wb-custom-map
	   #:ch-custom-set #:ch-custom-map
	   #:wb-custom-replay-set #:wb-custom-replay-map #:ch-custom-replay-set #:ch-custom-replay-map
	   #:equal? #:compare #:compare-slots #:compare-slots-no-unequal #:hash-slots #:eql-compare
	   #:define-equality-slots #:define-comparison-slots #:define-hash-function
	   #:unwrap-equivalent-node ; custom comparison functions may have to call this
	   #:hash-value #:hash-value-fixnum #:zero #:hash-mix #:hash-mixf #:hash-multiply
	   #:identity-equality-mixin
	   #:define-cross-type-compare-methods
	   #:compare-lexicographically #:compare-lists-lexicographically #:compare-strings-lexicographically
	   #:compare-vectors-lexicographically #:compare-seqs-lexicographically
	   #:empty? #:nonempty? #:size #:set-size #:arb
	   #:contains? #:domain-contains? #:range-contains? #:member? #:multiplicity
	   #:empty-set #:empty-bag #:empty-map #:empty-seq #:empty-tuple
	   #:empty-wb-set #:empty-wb-bag #:empty-wb-map #:empty-wb-seq
	   #:empty-dyn-tuple #:empty-ch-set #:empty-ch-map
	   #:empty-replay-set #:empty-wb-replay-set #:empty-replay-map #:empty-wb-replay-map
	   #:empty-ch-replay-set #:empty-ch-replay-map
	   #:least #:greatest #:lookup #:rank #:at-rank #:index #:at-index #:@
	   #:with #:less #:split-from #:split-above #:split-through #:split-below
	   #:union #:bag-sum #:intersection #:bag-product #:complement
	   #:set-difference #:set-difference-2 #:bag-difference #:bag-pairs #:wb-bag-pairs
	   #:subset? #:proper-subset #:disjoint? #:subbag? #:proper-subbag?
	   #:filter #:filter-pairs #:partition
	   #:image #:reduce #:domain #:range #:update
	   #:with-default #:without-default #:default
	   #:lookup-error #:map-domain-error #:map-domain-error-map #:map-domain-error-key
	   #:seq-bounds-error #:seq-bounds-error-seq #:seq-bounds-error-index
	   #:tuple-key-unbound-error #:tuple-key-unbound-error-tuple #:tuple-key-unbound-error-key
	   #:empty-seq-error #:empty-seq-error-seq
	   #:map-union #:map-intersection #:map-difference-2
	   #:restrict #:restrict-not #:compose
	   #:first #:last
	   #:lastcons #:head #:tail
	   #:with-first #:less-first #:push-first #:pop-first
	   #:with-last #:less-last #:push-last #:pop-last #:appendf #:prependf #:insertf #:splicef
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
	   #:define-tuple-key #:get-tuple-key #:tuple-key-name #:tuple-key?
	   #:tuple-merge
	   #:fset-setup-readtable #:*fset-readtable*
	   #:fset-setup-rereading-readtable #:*fset-rereading-readtable*
	   #:$ #:% 			; for the 'map' and 'bag' constructor macros
	   #:?				; for `query' on 'list-relation'
	   ;; Used by the bag methods that convert to and from lists.
	   #:alist
	   ;; Miscellaneous GMap arg types
	   #:fun-sequence #:fun-bag-pairs #:fun-map
	   ;; Miscellaneous GMap result types
	   #:map-to-sets #:append-unique
	   ;; Bounded sets
	   #:bounded-set #:make-bounded-set #:bounded-set-contents
	   ;; Complement sets
	   #:full-set
	   ;; Relations
	   #:relation #:relation? #:2-relation #:2-relation? #:wb-2-relation #:wb-2-relation?
	   #:empty-2-relation #:empty-wb-2-relation #:empty-ch-2-relation #:do-2-relation
	   #:lookup-inv #:inverse #:join #:conflicts #:map-to-sets
	   #:list-relation #:list-relation? #:wb-list-relation #:wb-list-relation?
	   #:empty-list-relation #:empty-wb-list-relation #:empty-ch-list-relation #:do-list-relation
	   #:arity #:query #:query-multi #:query-multi-restricted
	   #:assertion-db #:empty-assertion-db #:empty-wb-assertion-db
	   #:query-registry #:empty-query-registry #:empty-ch-query-registry #:with-query #:less-query
	   #:all-queries #:do-all-queries #:lookup-multi #:forward-key #:lookup-restricted
	   #:lookup-multi-restricted

           ;; named-readtable readtables
           #:fset-readtable #:fset-rereading-readtable))

;;; Since we've shadowed `cl:count', we need to do this.
(gmap:def-result-type-synonym fset2:count cl:count)

#+sbcl
(progn
  (sb-ext:add-implementation-package ':fset2 ':fset)
  (sb-ext:add-implementation-package ':fset ':fset2))


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

(defpackage :fset2-user
  (:use :cl :fset2 :gmap :new-let :lexical-contexts)
  (:shadowing-import-from :new-let #:let #:cond)
  (:shadowing-import-from :fset2
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
(pushnew ':FSet2 *features*)

;;; The seq implementation tries to use strings for leaf vectors when possible.
;;; In some Lisp implementations, there are two kinds of strings; but in some
;;; of these, the larger form takes as much space as a general vector, so nothing
;;; is to be saved by using it.
(when (and (not (typep (make-string 1 :element-type 'character :initial-element #\A)
		       'base-string))
	   (not (and (> (integer-length (1- char-code-limit)) 16)
		     (< (integer-length most-positive-fixnum) 32))))
  (pushnew ':FSet-Ext-Strings *features*))



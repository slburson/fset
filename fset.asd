;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.asd
;;; Contents: ASDF definitions for FSet
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2024 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


(asdf:defsystem FSet
  :description "A functional set-theoretic collections library.
See: https://gitlab.common-lisp.net/fset/fset/-/wikis/home
"
  :author "Scott L. Burson <Scott@sympoiesis.com>"
  :version "1.4.6"
  :homepage "https://gitlab.common-lisp.net/fset/fset/-/wikis/home"
  :source-control "https://github.com/slburson/fset"
  :licence "LLGPL"
  :depends-on (:misc-extensions :mt19937 :named-readtables)
  :serial t
  :components
  ((:module "Code"
	    :serial t
	    :components
	    ((:file "defs")
	     (:file "port")
	     (:file "macros")
	     (:file "order")
	     (:file "hash")
	     (:file "wb-trees")
	     (:file "champ")
	     (:file "reader")
	     (:file "fset")
	     (:file "replay")
	     (:file "tuples")
	     (:file "interval")
	     (:file "relations")
	     (:file "complement-sets")
	     (:file "bounded-sets")
	     (:file "testing")
	     #+swank
	     (:file "swank")
             #+lispworks
             (:file "lispworks-inspect")))))

(asdf:defsystem :FSet/test
  :description "Test system for FSet"
  :depends-on (:fset)
  :components
  ((:module "Code"
	    :components ((:file "testing")))))

(defmethod perform ((o test-op) (c (eql (find-system :fset))))
  (load-system :fset/test)
  (funcall (intern "RUN-TEST-SUITE" :fset) 20))

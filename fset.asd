;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.asd
;;; Contents: ASDF definitions for FSet
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.


(defsystem FSet
  :description "A functional set-theoretic collections library.
See: https://gitlab.common-lisp.net/fset/fset/-/wikis/home
"
  :author "Scott L. Burson <Scott@sympoiesis.com>"
  :version "2.2.1"
  :homepage "https://gitlab.common-lisp.net/fset/fset/-/wikis/home"
  :source-control "https://github.com/slburson/fset"
  :license "BSD-2-Clause"
  :depends-on ((:version :misc-extensions "4.2.3") :mt19937 :named-readtables)
  :in-order-to ((test-op (test-op "fset/test")))
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
	     (:file "post")
	     #+swank
	     (:file "swank")
             #+lispworks
             (:file "lispworks-inspect")))))

(defsystem :FSet/test
  :description "Test system for FSet"
  :depends-on (:fset)
  :perform (test-op (o c) (symbol-call :fset :run-test-suite 200))
  :components
  ((:module "Code"
	    :components ((:file "testing")))))

(defsystem :FSet/Iterate
  :description "FSet definitions for the Iterate macro."
  :author "Scott L. Burson <Scott@sympoiesis.com>"
  :homepage "https://gitlab.common-lisp.net/fset/fset/-/wikis/home"
  :source-control "https://github.com/slburson/fset"
  :license "BSD-2-Clause"
  :depends-on ("fset" "iterate")
  :in-order-to ((test-op (test-op "fset/iterate/test")))
  :components ((:module "Code"
		:serial t
		:components ((:file "iterate-defs")
			     (:file "iterate")))))

(defsystem :FSet/Iterate/test
  :description "Test system for FSet/Iterate"
  :depends-on (:fset/iterate)
  :perform (test-op (o c) (symbol-call :fset/iterate/test :test-fset/iterate))
  :components ((:module "Code"
		:serial t
		:components ((:file "iterate-tests")))))

(defsystem :FSet/Jzon
  :description "FSet definitions for the Jzon JSON reader/writer."
  :author "Scott L. Burson <Scott@sympoiesis.com>"
  :homepage "https://gitlab.common-lisp.net/fset/fset/-/wikis/home"
  :source-control "https://github.com/slburson/fset"
  :license "BSD-2-Clause"
  :depends-on ("fset" "com.inuoe.jzon")
  :in-order-to ((test-op (test-op "fset/jzon/test")))
  :components ((:module "Code"
		:serial t
		:components ((:file "jzon-defs")
			     (:file "jzon")))))

(defsystem :FSet/Jzon/test
  :description "Test system for FSet/Iterate"
  :depends-on (:fset/jzon)
  :perform (test-op (o c) (symbol-call :fset/jzon/test :test-fset/jzon))
  :components ((:module "Code"
		:serial t
		:components ((:file "jzon-tests")))))

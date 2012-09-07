;;; -*- Mode: Lisp; Package: CL-User; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.asd
;;; Contents: ASDF definitions for FSet
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


(asdf:defsystem FSet
  :description "A functional set-theoretic collections library.
See: http://common-lisp.net/project/fset/
"
  :version "1.3.0"
  :licence "LLGPL"
  :depends-on (:misc-extensions)
  :serial t
  :components
  ((:system :misc-extensions)
   (:module "Code"
	    :serial t
	    :components
	    ((:file "defs")
	     (:file "port")
	     (:file "order")
	     (:file "wb-trees")
	     (:file "fset")
	     (:file "tuples")
	     (:file "reader")
	     (:file "testing")
	     (:file "interval")
	     (:file "relations")
	     (:file "complement-sets")
	     (:file "bounded-sets")))))

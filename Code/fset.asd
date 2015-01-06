;;; -*- Mode: Lisp; Package: ASDF; Syntax: ANSI-Common-Lisp -*-

;;; File: fset.asd
;;; Contents: ASDF definitions for FSet
;;;
;;; This is an alternate .asd file that I use for development.  You probably
;;; want the one in the parent directory.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2012 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.

(defsystem FSet
  :depends-on (:gmap :mt19937)
  :components
  ((:file "defs")
   (:file "port" :depends-on ("defs"))
   (:file "order" :depends-on ("port"))
   (:file "wb-trees" :depends-on ("order"))
   (:file "fset" :depends-on ("wb-trees"))
   (:file "tuples" :depends-on ("fset"))
   (:file "reader" :depends-on ("tuples"))
   (:file "testing" :depends-on ("reader"))
   (:file "interval" :depends-on ("fset"))
   (:file "relations" :depends-on ("fset" "reader"))
   (:file "complement-sets" :depends-on ("fset"))
   (:file "bounded-sets" :depends-on ("complement-sets"))))

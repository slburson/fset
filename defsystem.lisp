;;; -*- Mode: Lisp; Package: CL-USER; Syntax: ANSI-Common-Lisp -*-

(in-package :cl-user)

;;; File: defsystem.lisp
;;; Contents: LispWorks system definition
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.


(defsystem "FSET"
  (:default-pathname "Code")
  :members ("defs"
            "port"
            "order"
            "wb-trees"
            "fset"
            "tuples"
            "reader"
            "testing"
            "interval"
            "relations"
            "complement-sets"
            "bounded-sets"
            "lispworks-inspect")
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))

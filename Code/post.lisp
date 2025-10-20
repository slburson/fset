;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: post.lisp
;;; Contents: Fixups to do at the end of loading FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; For SBCL, these declarations must come after all methods on these generic functions
;;; in order to take effect in user code.
(declaim (ftype (function (t &key &allow-other-keys) function) iterator))
(declaim (ftype (function (t &key (:from-end? t) &allow-other-keys) function) fun-iterator))


;;; Must be last!
#+FSet-Use-Package-Locks
(lock-fset-packages)


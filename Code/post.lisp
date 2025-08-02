;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: post.lisp
;;; Contents: Fixups to do at the end of loading FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; For SBCL, these declarations must come after all methods on these generic functions
;;; in order to take effect in user code.
(declaim (ftype (function (t &key &allow-other-keys) function) iterator))
(declaim (ftype (function (t &key (:from-end? t) &allow-other-keys) function) fun-iterator))


#+FSet-Use-Package-Locks
(lock-fset-package)

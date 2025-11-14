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


;;; Copy class definitions from `fset:' symbols to `fset2:' symbols that shadow them,
;;; so `defmethod', `typep', etc. will work without package prefixes.
(dolist (sym '(set map wb-map ch-map seq wb-seq replay-map wb-replay-map ch-replay-map))
  (let ((fset2-sym (intern (string sym) :fset2)))
    (setf (find-class fset2-sym) (find-class sym))
    ;; Needed in CCL for `typep' to work.
    #+ccl (setf (ccl::info-type-kind fset2-sym) ':instance)))


;;; Moved here from `order.lisp' because its expansion depends on stuff after that file.
(define-equality-slots class
  #'class-name)


;;; Must be last!
#+FSet-Use-Package-Locks
(lock-fset-packages)


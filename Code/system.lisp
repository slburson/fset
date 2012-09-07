(in-package :RU)

(read-lisp-file "defs")

(compile-and-load-lisp-file "port")

(compile-and-load-lisp-file "order")

(compile-and-load-lisp-file "wb-trees")

(compile-and-load-lisp-file "fset")

;;; Must follow "fset" because it uses macros therefrom.
(compile-and-load-lisp-file "tuples")

(compile-and-load-lisp-file "testing")

(compile-and-load-lisp-file "reader")

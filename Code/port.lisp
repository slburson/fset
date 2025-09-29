;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

;;; File: port.lisp
;;; Contents: Portability-related definitions for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007-2025 Scott L. Burson.
;;; FSet is licensed under the 2-clause BSD license; see LICENSE.
;;; This license provides NO WARRANTY.

(in-package :fset)


;;; On non-kernel-threads implementations, we use something like
;;; `without-interrupts'.  On kernel-threads implementations, we have to do
;;; real locking.

#+allegro
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-process-lock (and name `(:name ,name))))
  ;; If `wait?' is false, and the lock is not available, returns without executing the body.
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(mp:with-process-lock (,lock :timeout ,(cond ((eq wait? 't) nil)  ; hush, Allegro
						  ((eq wait? 'nil) 0)
						  (t `(if ,wait? nil 0))))
       . ,body))
  ;; For those implementations that support SMP but don't give us direct ways
  ;; to generate memory barriers, we assume that grabbing a lock suffices.
  (deflex *Memory-Barrier-Lock*
    (mp:make-process-lock :name "Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    '(mp:with-process-lock (*Memory-Barrier-Lock*)
       nil))
  (defmacro write-memory-barrier ()
    '(mp:with-process-lock (*Memory-Barrier-Lock*)
       nil)))


#+lispworks
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-lock (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(mp:with-lock (,lock :timeout (if ,wait? nil 0))
       . ,body))
  ;; For those implementations that support SMP but don't give us direct ways
  ;; to generate memory barriers, we assume that grabbing a lock suffices.
  (deflex *Memory-Barrier-Lock*
    (mp:make-lock :name "Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    '(mp:with-lock (*Memory-Barrier-Lock*)
       nil))
  (defmacro write-memory-barrier ()
    '(mp:with-lock (*Memory-Barrier-Lock*)
       nil)))


#+cmu
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(sys:without-interrupts . ,body))
  (defmacro read-memory-barrier ()
    'nil)
  (defmacro write-memory-barrier ()
    'nil))


#+(and sbcl (not sb-thread))
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(progn
       . ,body))
  (progn
    (defmacro read-memory-barrier ()
      'nil)
    (defmacro write-memory-barrier ()
      'nil)))

#+(and sbcl sb-thread)
(progn
  (defun make-lock (&optional name)
    (apply #'sb-thread:make-mutex (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(sb-thread:with-mutex (,lock :wait-p ,wait?)
       . ,body))
  (defmacro read-memory-barrier ()
    '(sb-thread:barrier (:read)))
  (defmacro write-memory-barrier ()
    '(sb-thread:barrier (:write))))

#+(and clasp threads)
(progn
  (defun make-lock (&optional name)
    (mp:make-lock :name (or name :anonymous)))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(mp:with-lock (,lock) ,@body))
  (defvar *Memory-Barrier-Lock*
    (mp:make-lock :name "Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    '(mp:with-lock (*Memory-Barrier-Lock*) nil))
  (defmacro write-memory-barrier ()
    '(mp:with-lock (*Memory-Barrier-Lock*) nil)))

#+scl
(progn
  (defun make-lock (&optional name)
    (thread:make-lock name :type ':recursive :auto-free t))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(thread:with-lock-held (,lock "Lock Wait" :wait ,wait?)
       . ,body))
  (defmacro read-memory-barrier ()
    '(kernel:read-memory-barrier))
  (defmacro write-memory-barrier ()
    '(kernel:write-memory-barrier)))


#+openmcl
(progn
  (defun make-lock (&optional name)
    (ccl:make-lock name))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (let ((lock-var (gensym "LOCK-"))
	  (wait?-var (gensym "WAIT?-"))
	  (try-succeeded?-var (gensym "TRY-SUCCEEDED?-")))
      `(let ((,lock-var ,lock)
	     . ,(and (not (eq wait? 't))
		     `((,wait?-var ,wait?)
		       (,try-succeeded?-var nil))))
	 ,(if (eq wait? 't)
	      `(ccl:with-lock-grabbed (,lock-var)
		. ,body)
	    `(unwind-protect
		 (and (or ,wait?-var (and (ccl:try-lock ,lock-var)
					  (setq ,try-succeeded?-var t)))
		      (ccl:with-lock-grabbed (,lock-var)
			. ,body))
	       (when ,try-succeeded?-var
		 (ccl:release-lock ,lock-var)))))))
  ;; For those implementations that support SMP but don't give us direct ways
  ;; to generate memory barriers, we assume that grabbing a lock suffices.
  (deflex *Memory-Barrier-Lock*
    (ccl:make-lock "Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    `(ccl:with-lock-grabbed (*Memory-Barrier-Lock*)
       nil))
  (defmacro write-memory-barrier ()
    `(ccl:with-lock-grabbed (*Memory-Barrier-Lock*)
       nil)))


#+(and genera new-scheduler)
(progn
  (defun make-lock (&optional name)
    (process:make-lock name))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(process:with-lock (,lock)
       . ,body))
  (defmacro read-memory-barrier ()
    'nil)
  (defmacro read-memory-barrier ()
    'nil))


#+clisp
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(progn . ,body))
  (defmacro read-memory-barrier ()
    'nil)
  (defmacro write-memory-barrier ()
    'nil))


#+(and ecl (not threads))
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(progn . ,body))
  (defmacro read-memory-barrier ()
    'nil)
  (defmacro write-memory-barrier ()
    'nil))

#+(and ecl threads)
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-lock :recursive t (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (let ((lock-var (gensym "LOCK-"))
	  (wait?-var (gensym "WAIT?-"))
	  (try-succeeded?-var (gensym "TRY-SUCCEEDED?-")))
      `(let ((,lock-var ,lock)
	     . ,(and (not (eq wait? 't))
		     `((,wait?-var ,wait?)
		       (,try-succeeded?-var nil))))
	 ,(if (eq wait? 't)
	      `(mp:with-lock (,lock-var)
		. ,body)
	    `(unwind-protect
		 (and (or ,wait?-var (and (mp:get-lock ,lock-var nil)
					  (setq ,try-succeeded?-var t)))
		      (mp:with-lock (,lock-var)
			. ,body))
  	       (when ,try-succeeded?-var
		 (mp:giveup-lock ,lock-var)))))))
  (deflex *ECL-Read-Memory-Barrier-Lock*
    (mp:make-lock :name "Read Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    '(mp:with-lock (*ECL-Read-Memory-Barrier-Lock*)
       nil))
  (deflex *ECL-Write-Memory-Barrier-Lock*
    (mp:make-lock :name "Write Memory Barrier Lock"))
  (defmacro write-memory-barrier ()
    '(mp:with-lock (*ECL-Write-Memory-Barrier-Lock*)
       nil)))


#+abcl
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    (threads:make-mutex))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore wait?))
    `(threads:with-mutex (,lock)
       . ,body))
  ;; For those implementations that support SMP but don't give us direct ways
  ;; to generate memory barriers, we assume that grabbing a lock suffices.
  (deflex *Memory-Barrier-Lock*
    (threads:make-mutex))
  (defmacro read-memory-barrier ()
    '(threads:with-mutex (*Memory-Barrier-Lock*)
       nil))
  (defmacro write-memory-barrier ()
    '(threads:with-mutex (*Memory-Barrier-Lock*)
       nil)))


#+sbcl
(defmacro defglobal (name value &optional doc-string)
  `(sb-ext:defglobal ,name ,value ,doc-string))

#-sbcl
(defmacro defglobal (name value &optional doc-string)
  `(deflex ,name ,value ,doc-string))

(defmacro define-atomic-series (name &optional doc-string)
  #+(and sbcl 64-bit)
  `(progn
     (sb-ext:defglobal ,name 0 . ,(and doc-string `(,doc-string)))
     (declaim (type fixnum ,name)))
  #-(and sbcl 64-bit)
  `(deflex ,name (cons 0 (make-lock ',name)) . ,(and doc-string `(,doc-string))))

(defmacro increment-atomic-series (name)
  #+(and sbcl 64-bit)
  `(sb-ext:atomic-incf ,name) ; (postincrement)
  #+(and allegro smp-macros)
  `(1- (excl:incf-atomic (car ,name)))
  #+ccl
  `(1- (ccl::atomic-incf (car ,name)))
  #-(or (and sbcl 64-bit) (and allegro smp-macros) ccl)
  `(with-lock ((cdr ,name))
     (1- (incf (car ,name)))))

;;; When a library exports macros, as FSet does, and the expansions of those macros sometimes
;;; reference other exported function and macro names, there is a possibility that client code
;;; will inadvertently break a macro within a local scope by binding one of the referenced names
;;; in the function namespace with `labels', `flet', or `macrolet'.  There are two ways to
;;; prevent this.  Some CL implementations have extensions that allow us to lock the FSet
;;; package the same way that `cl:' is locked, so that an attempt to fbind one of its exported
;;; functions or macros will cause a warning or error.  For the rest, the best we can do is to
;;; define unexported aliases, and use those in the macro expansions instead.
#+(or)  ; #+sbcl
(progn
  (pushnew ':FSet-Use-Package-Locks *features*)
  (defun lock-fset-package ()
    (sb-ext:lock-package (symbol-package 'lock-fset-package))))

#+(or)  ; #+allegro
(progn
  (pushnew ':FSet-Use-Package-Locks *features*)
  (defun lock-fset-package ()
    (let ((pkg (symbol-package 'lock-fset-package)))
      (setf (excl:package-lock pkg) t)
      (setf (excl:package-definition-lock pkg) t))))


;;; ----------------

;;; Constants used by the tuple implementation.  We choose the widths of
;;; two bitfields to fit in a fixnum less the sign bit.
;;; These numbers are noncritical except possibly for small fixnums.

;;; Fixnum widths of known implementations, exclusive of sign bit:
;;; SBCL >= 1.0.53, 64-bit:		62
;;; ECL, 64-bit:			61
;;; SBCL < 1.0.53, OpenMCL/Clozure CL,
;;;   Scieneer CL, Allegro, 64-bit:	60
;;; Symbolics L-, I-machine; ABCL:	31
;;; Allegro, CMUCL, SBCL, ECL
;;;   LispWorks (most), 32-bit:		29
;;; CADR, LMI Lambda:			24

(defconstant Tuple-Value-Index-Size
  (floor (+ 5 (integer-length most-positive-fixnum)) 3)
  "This limits the number of key/value pairs in any tuple.")

(defconstant Tuple-Key-Number-Size
  (- (integer-length most-positive-fixnum) Tuple-Value-Index-Size)
  "This limits the number of tuple-keys that can exist in a session.")

(defconstant Tuple-Key-Number-Mask
  (1- (ash 1 Tuple-Key-Number-Size)))


;;; ----------------

#-lispworks
(declaim (inline base-char-p))
#-lispworks
(defun base-char-p (x)
  (typep x 'base-char))

;;; I think this may be faster than `(typep x 'base-char)'.  Maybe not.
#+lispworks
(defun base-char-p (x) (lw:base-char-p x))

;;; SBCL has a distinct `extended-char' type but no `make-char'.
#+sbcl
(defun make-char (code bits)
  ;; Kinda weird, but this is only used by the test suite to generate random chars.
  (code-char (+ code (ash bits 8))))

#+lispworks
(defun make-char (code bits)
  ;; Unfortunately, an attempt to use `bits' runs into LispWorks bugs; e.g.,
  ;; `(concatenate 'string ...)' tries to make a `base-string', and fails.
  (declare (ignore bits))
  (code-char code))

#+clasp
(defun make-char (code bits)
  (code-char (+ code (ash bits 8))))

#+ecl
(defun make-char (code bits)
  (code-char (+ code (ash bits 8))))

;;; I'm one of these weird people who detests `loop' (except in its CLtL1 form).
(defmacro while (pred &body body)
  `(do () ((not ,pred))
     . ,body))


;;; ----------------

;;; These are macros because Allegro (still!) doesn't inline user functions.
(defmacro hash-mix (&rest args)
  "Returns the \"mix\" of the values, where \"mix\" is a commutative and
associative operation with an inverse.  All values MUST be fixnums; the
result is a fixnum."
  ;; On implementations where we can reliably get fixnum addition and subtraction without
  ;; overflow checks at speed-3 safety-0, using them will give us better distributional properties.
  ;; On other implementations, we fall back to XOR.
  #+(or sbcl ccl allegro lispworks)
  ;; To make SBCL happy, we have to build a binary tree with `the fixnum' at each level.
  (labels ((build (fn expr args)
	     (if (null args) expr
	       (build fn `(the fixnum (,fn ,expr (the fixnum ,(car args)))) (cdr args)))))
    `(locally (declare (optimize (speed 3) (safety 0)))
       ,(build '+ `(the fixnum ,(car args)) (cdr args))))
  ;; Oddly, addition doesn't seem to work on ABCL, even though it's using an `ladd' instruction; it
  ;; makes a bignum (given suitable operands) anyway.
  ;; CLASP also checks for overflow on addition.
  #-(or sbcl ccl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (the fixnum (logxor . ,(mapcar (fn (x) `(the fixnum ,x)) args)))))

(defmacro hash-unmix (hash &rest to-unmix)
  "Returns the result of \"unmixing\" each of `to-unmix' from `hash'.  All
values MUST be fixnums; the result is a fixnum."
  ;; As above.
  #+(or sbcl ccl allegro lispworks)
  (labels ((build (fn expr args)
	     (if (null args) expr
	       (build fn `(the fixnum (,fn ,expr (the fixnum ,(car args)))) (cdr args)))))
    `(locally (declare (optimize (speed 3) (safety 0)))
       ,(build '- `(the fixnum ,hash) to-unmix)))
  #-(or sbcl ccl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (the fixnum (logxor (the fixnum ,hash) . ,(mapcar (fn (x) `(the fixnum ,x)) to-unmix)))))

(defmacro hash-multiply (ha hb)
  "Returns the product of `ha' and `hb' modulo 2^fixnum_length.  Both MUST be
fixnums; the result is a fixnum."
  #+(or sbcl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (the fixnum (* (the fixnum ,ha) (the fixnum ,hb))))
  #-(or sbcl allegro lispworks)
  `(locally (declare (optimize (speed 3) (safety 0)))
     (let ((prod (* (the fixnum ,ha) (the fixnum ,hb)))
	   ((fprod (the fixnum (logand most-positive-fixnum prod)))))
       (if (< prod 0) (- fprod) fprod))))

(define-modify-macro hash-mixf (&rest args)
  hash-mix)
(define-modify-macro hash-unmixf (hash &rest to-unmix)
  hash-unmix)

(declaim (inline symbol-hash-value))
(defun symbol-hash-value (sym)
  #-(or ccl allegro lispworks)
  (sxhash sym)
  ;; Some CLs don't cache the hash, AFAICT.  So we use a plist slot.  Of course, this works best if
  ;; the plist is otherwise empty; but it usually is.
  #+(or ccl allegro lispworks)
  (or (get sym 'hash-value)
      (setf (get sym 'hash-value) (sxhash sym))))

(declaim (inline class-hash-value))
(defun class-hash-value (x)
  #-(or ccl lispworks)
  (sxhash (class-of x))
  #+ccl
  (symbol-hash-value (slot-value (class-of x) 'ccl::name))
  #+lispworks
  (symbol-hash-value (slot-value (class-of x) 'clos::name)))


;;; ----------------

;;; A macro used mostly by the bag code to get generic arithmetic in speed-3
;;; routines without all those compiler notes from CMUCL, SBCL, or Scieneer
;;; CL.
(defmacro gen (op &rest args)
  (let ((vars (mapcar (lambda (x) (and (not (or (symbolp x) (numberp x)))
				       (gensym "VAR-")))
		      args)))
    `(let ,(cl:remove nil (mapcar (lambda (var arg)
				    (and var `(,var ,arg)))
				  vars args))
       (locally (declare (optimize (speed 1) (safety 1)))
	 (,op . ,(mapcar (lambda (var arg) (or var arg))
			 vars args))))))

(defmacro without-optimization (&body body)
  "Turns off optimization, and thus the compiler's complaints when it can't."
  `(locally (declare (optimize (speed 1) (safety 1)))
     . ,body))


;;; This little oddity exists because of a limitation in the SBCL/CMUCL compiler.
;;; Given a call to `length' on type `(or null simple-vector)', it isn't quite
;;; smart enough to optimize the call unless we do the case breakdown for it like this.
#+(or cmu scl)
(defmacro length-nv (x)
  (ext:once-only ((x x))
    `(if (null ,x) 0 (cl:length ,x))))
#+sbcl
(defmacro length-nv (x)
  (sb-int:once-only ((x x))
    `(if (null ,x) 0 (cl:length ,x))))
#-(or cmu scl sbcl)
(defmacro length-nv (x)
  `(length ,x))


;;; ----------------
;;; Miscellaneous utilities

(defun eqv (a b &rest more)
  "True if `b' and all of `more' are boolean-equal to `a`, i.e., all null or
all nonnull.  (Not the even-parity function.)"
  (and (or (eq a b) (and a b))
       (gmap (:result and) (fn (x) (or (eq x a) (and x a))) (:arg list more))))

(defun swap-if (pred x y)
  (if pred (values y x)
    (values x y)))

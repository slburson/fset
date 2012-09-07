;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: port.lisp
;;; Contents: Portability-related definitions for FSet.
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


;;; On non-kernel-threads implementations, we use something like
;;; `without-interrupts'.  On kernel-threads implementations, we have to do
;;; real locking.

#+(and allegro (not os-threads))
(progn
  (defun make-lock (&optional name)
    (declare (ignore name))
    nil)
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    (declare (ignore lock wait?))
    `(excl:without-interrupts . ,body))
  (defmacro read-memory-barrier ()
    'nil)
  (defmacro write-memory-barrier ()
    'nil))

#+(and allegro os-threads)          ; &&& untested
(progn
  (defun make-lock (&optional name)
    (apply #'mp:make-process-lock (and name `(:name ,name))))
  (defmacro with-lock ((lock &key (wait? t)) &body body)
    `(mp:with-process-lock (,lock :timeout (if ,wait? nil 0))
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
  ;; For those implementations that support SMP but don't give us direct ways
  ;; to generate memory barriers, we assume that grabbing a lock suffices.
  (deflex *Memory-Barrier-Lock*
    (sb-thread:make-mutex :name "Memory Barrier Lock"))
  (defmacro read-memory-barrier ()
    '(sb-thread:with-mutex (*Memory-Barrier-Lock*)
       nil))
  (defmacro write-memory-barrier ()
    '(sb-thread:with-mutex (*Memory-Barrier-Lock*)
       nil)))


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
    (apply #'mp:make-lock (and name `(:name ,name))))
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


;;; ----------------

;;; Constants used by the tuple implementation.  We choose the widths of
;;; two bitfields to fit in a fixnum less the sign bit.
;;; These numbers are noncritical except possibly for small fixnums.

;;; Fixnum widths of known implementations:
;;; SBCL >= 1.0.53, 64-bit:		62
;;; ECL, 64-bit:			61
;;; SBCL < 1.0.53, OpenMCL/Clozure CL,
;;;   Scieneer CL, 64-bit		60
;;; CLISP, 64-bit			48
;;; Symbolics L-, I-machine; ABCL	31
;;; Allegro, CMUCL, SBCL, ECL
;;;   LispWorks (most), 32-bit		29
;;; CLISP, 32-bit; CADR, LMI Lambda	24
;;; LispWorks 4 on Linux		23

(defconstant Tuple-Value-Index-Size
  (floor (+ 5 (integer-length most-positive-fixnum)) 3)
  "This limits the number of key/value pairs in any tuple.")

(defconstant Tuple-Key-Number-Size
  (- (integer-length most-positive-fixnum) Tuple-Value-Index-Size)
  "This limits the number of tuple-keys that can exist in a session.")

(defconstant Tuple-Key-Number-Mask
  (1- (ash 1 Tuple-Key-Number-Size)))


;;; ----------------

;;; Unfortunately, CL doesn't specify that `make-random-state' should be able
;;; to accept an integer seed.  We want to be able to supply it one, so that
;;; (for testing) we can have multiple reproducible sequences of pseudorandom
;;; numbers.  (May not be possible on all implementations.)
(defun make-seeded-random-state (seed)
  (if (null seed)
      (make-random-state)
    #+(or cmu scl)
    (progn
      (assert (plusp seed))
      (kernel::make-random-object :state
				  (kernel::init-random-state (logand seed #xFFFFFFFF))))
    #+sbcl
    (progn
      (assert (plusp seed))
      (sb-kernel::%make-random-state
	:state (sb-kernel::init-random-state (logand seed #xFFFFFFFF))))
    #+openmcl
    (ccl::initialize-random-state (ash (logand seed #xFFFFFFFF) -16)
				  (logand seed #xFFFF))
    #+genera
    (fcli::make-random-state-internal 71 35 seed)
    #-(or cmu scl sbcl openmcl genera)
    (error "Implementation-specific code needed in `make-seeded-random-state'")))


;;; ----------------

#-lispworks
(defun base-char-p (x)
  (typep x 'base-char))

;;; I think this may be faster than `(typep x 'base-char)'.  Maybe not.
#+lispworks
(defun base-char-p (x) (lw:base-char-p x))

#-lispworks
(declaim (inline base-char-p))


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


;;; I'm one of these weird people who detests `loop' (except in its CLtL1 form).
(defmacro while (pred &body body)
  `(do () ((not ,pred))
     . ,body))


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


;;; This little oddity exists because of a limitation in Python (that's the
;;; CMUCL compiler).  Given a call to `length' on type `(or null simple-vector)',
;;; Python isn't quite smart enough to optimize the call unless we do the case
;;; breakdown for it like this.
#+(or cmu scl)
(defmacro length-nv (x)
  (ext:once-only ((x x))
    `(if (null ,x) 0 (cl:length ,x))))
#+sbcl
(defmacro length-nv (x)
  (sb-ext::once-only ((x x))
    `(if (null ,x) 0 (cl:length ,x))))
#-(or cmu scl sbcl)
(defmacro length-nv (x)
  `(length ,x))

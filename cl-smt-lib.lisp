;;; cl-smt-lib.lisp --- Common Lisp SMT-Lib Integration
(defpackage :cl-smt-lib
  (:use :common-lisp)
  (:export
   :make-smt
   :smt-error
   :ignore-smt-error
   :return-smt-error
   :write-to-smt
   :read-from-smt
   :with-smt
   :enable-preserving-case-syntax
   :disable-preserving-case-syntax
   :*smt-debug*
   ;; smt accessors
   :smt-output-stream
   :smt-input-stream
   :smt-process))
(in-package :cl-smt-lib)

(defvar *smt-debug* nil
  "Set to a stream to duplicate smt input and output to the *SMT-DEBUG*.")

(defstruct (smt (:include two-way-stream)
             (:constructor %make-smt (input-stream output-stream process))
             (:copier nil)
             (:predicate nil))
  (process (sb-impl::missing-arg) :read-only t))

#+sbcl (sb-impl::defprinter (smt) process input-stream output-stream)

(defun make-smt (program &optional args)
  "Wrap PROCESS in an SMT object"
  (let ((process #+sbcl (sb-ext:run-program program args
                                            :input :stream
                                            :output :stream
                                            :wait nil
                                            :search t)
                 #-sbcl (error "CL-SMT-LIB currently only supports SBCL.")))
    (%make-smt (sb-ext:process-output process)
               (sb-ext:process-input process)
               process)))

(define-condition smt-error (error)
  ((text :initarg :text :initform nil :reader text)
   (smt :initarg :smt :initform nil :reader smt))
  (:report (lambda (condition stream)
             (format stream "SMT: ~a~%~S"
                     (text condition) (smt condition)))))

(defun write-to-smt (smt forms)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil))
        (format-string "~{~S~^~%~}~%"))
    (setf (readtable-case *readtable*) :preserve)
    (format smt format-string forms)
    (when *smt-debug*
      (format *smt-debug* "~&;; WRITE-TO-SMT~%")
      (format *smt-debug* format-string forms)
      (finish-output *smt-debug*))
    (finish-output smt)))

(defun read-from-smt (smt &optional preserve-case-p (eof-error-p t) eof-value)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil)))
    (when preserve-case-p
      (setf (readtable-case *readtable*) :preserve))
    (let ((value (read smt eof-error-p eof-value)))
      (when *smt-debug*
        (format *smt-debug* "~&;; READ-FROM-SMT~%")
        (write value :stream *smt-debug*)
        (finish-output *smt-debug*))
      (restart-case
          (if (and (listp value)
                   (equal (if preserve-case-p '|error| 'ERROR) (car value)))
              (error (make-condition 'smt-error
                       :text (second value)
                       :smt smt))
              value)
        (ignore-smt-error () :report "Ignore SMT error." nil)
        (return-smt-error () :report "Return SMT error." value)))))

(defmacro with-smt ((smt (program &optional args) &optional preserve-case-p)
                    &body body)
  (let ((form (gensym)))
    `(with-open-stream (,smt (make-smt ,program ,args))
       (unwind-protect
            (progn
              ,@body
              (close (smt-output-stream ,smt))
              (loop :for ,form = (read-from-smt ,smt ,preserve-case-p nil :eof)
                 :while (not (equal :eof ,form))
                 :collect ,form))
         ;; Ensure the process is terminated.
         #+sbcl (sb-ext:process-kill (smt-process ,smt) sb-unix:sigterm)
         #-sbcl (error "CL-SMT-LIB currently only supports SBCL.")))))

(defvar *previous-readtables* nil
  "Holds *readtable* before cl-smt-lib enabled the #! case preserving reader.")

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(defmacro enable-preserving-case-syntax ()
  "Register the #! reader macro to read the next form preserving case."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (push *readtable* *previous-readtables*)
    (set-dispatch-macro-character #\# #\! #'read-preserving-case)))

(defmacro disable-preserving-case-syntax ()
  "Un-register the #! reader macro restoring the previous *readtable*."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (pop *previous-readtables*))))
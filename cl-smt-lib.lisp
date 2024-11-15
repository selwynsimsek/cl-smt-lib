;;; cl-smt-lib.lisp --- Common Lisp SMT-Lib Integration
(defpackage :cl-smt-lib/cl-smt-lib
  (:nicknames :cl-smt-lib)
  (:use :common-lisp :named-readtables :cl-smt-lib/process-two-way-stream)
  (:import-from :uiop/launch-program :terminate-process :wait-process)
  (:export
   :make-smt
   :smt-error
   :ignore-smt-error
   :return-smt-error
   :write-to-smt
   :read-from-smt
   :with-smt
   :*smt-debug*
   ;; smt accessors
   :smt-output-stream
   :smt-input-stream
   :smt-process))
(in-package :cl-smt-lib/cl-smt-lib)
#+debug (declaim (optimize (debug 3)))

(defvar *smt-debug* nil
  "Set to a stream to duplicate smt input and output to the *SMT-DEBUG*.")

(defvar *print-nil-as-list* nil
  "When bound to non-nil print NIL as the empty list.")

(defvar *omit-package-names* nil
  "When bound to non-NIL don't print symbol package names.")

;;; Implementation depends on if two-way-stream is a class or structure.

(defclass smt (process-two-way-stream) ()
  (:documentation "An SMT process with input and output streams."))

(defun make-smt (program &rest args)
  "Wrap PROCESS in an SMT object."
  (apply #'make-process-two-way-stream program args))

(define-condition smt-error (error)
  ((text :initarg :text :initform nil :reader text)
   (smt :initarg :smt :initform nil :reader smt))
  (:report (lambda (condition stream)
             (format stream "SMT: ~a~%~S"
                     (text condition) (smt condition)))))

(defstruct smt-symbol (name "" :type string))

(export '(make-smt-symbol smt-symbol))
(defvar *print-smt-symbol* t "When bound to non-NIL prints SMT symbols using only their names.")

(defmethod print-object :around ((object smt-symbol) stream)
  (if *print-smt-symbol*
      (write-string (smt-symbol-name object) stream)
      (call-next-method)))

(defmethod print-object :around ((object (eql nil)) stream)
  (if *print-nil-as-list*
      (write-string "()" stream)
      (call-next-method)))

(defmethod print-object :around ((object symbol) stream)
  (if *omit-package-names*
      (write-string (symbol-name object) stream)
      (call-next-method)))

(defun write-to-smt (smt forms)
  "Write FORMS to the process in SMT over it's STDIN.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid
case-sensitive smt libv2 format."
  (let ((*readtable* (copy-readtable nil))
        (*print-nil-as-list* t)
        (*omit-package-names* t)
        (*print-smt-symbol* t)
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

(defmacro with-smt ((smt (program &rest args) &optional preserve-case-p)
                    &body body)
  (declare (ignore preserve-case-p))
  (let ((form (gensym))
        (status (gensym)))
    `(with-open-stream (,smt (make-smt ,program ,@args))
       (unwind-protect
            (progn
              ,@body
              (close (output ,smt))
              (let ((,status (wait-process (process ,smt))))
                (unless (zerop ,status) (error "SMT solver failed with exit status ~S" ,status)))
              (loop :for ,form = (read-from-smt ,smt t nil :eof)
                 :while (not (equal :eof ,form))
                 :collect ,form))
         ;; Ensure the process is terminated.
         (terminate-process (process ,smt))))))

(defun read-preserving-case (stream char n)
  (declare (ignorable char) (ignorable n))
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (read stream t nil t)))

(unless (find-readtable :cl-smt-lib)
  (defreadtable :cl-smt-lib
    (:merge :current)
    (:dispatch-macro-char #\# #\! #'read-preserving-case)))

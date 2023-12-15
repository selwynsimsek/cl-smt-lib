(defpackage :cl-smt-lib/process-two-way-stream
  (:use :cl :cl-smt-lib/fundamental-two-way-stream :uiop/launch-program)
  (:export :process-two-way-stream
           :make-process-two-way-stream
           :input
           :output
           :process))
(in-package :cl-smt-lib/process-two-way-stream)

;;; A process wrapped in a two-way stream.
(defclass process-two-way-stream (fundamental-two-way-stream)
  ((process :initarg :process :initform (error "process argument is required")
            :reader process))
  (:documentation
   "A fundamental-two-way-stream wrapping a single process' input and output."))

(defun make-process-two-way-stream (program &rest args)
  "Wrap PROCESS in an PROCESS-TWO-WAY-STREAM object."
  (let ((process (launch-program (format nil "~{~a~^ ~}" (cons program args))
                                 :input :stream
                                 :output :stream
                                 :error-output :stream
                                 :wait nil
                                 :search t)))
    (let ((error-output *error-output*)
          (error-input (process-info-error-output process)))
      (bt:make-thread (lambda () (loop while (open-stream-p error-input) do (write-char (read-char error-input) error-output)))))
    ;(setf *error-output* (make-broadcast-stream *error-output* ))
    (make-instance 'process-two-way-stream
                   #+ALLEGRO :element-type #+ALLEGRO '(unsigned-byte 8)
                   :input (process-info-output process)
                   :output (process-info-input process)
                   :process process)))

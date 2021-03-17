;;;; compile.lisp --- Form reading and hook into compiler.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sbcl-ir-visualizer)

(defvar *inspect* nil)

(defun maybe-inspect-component (component)
  (when *inspect*
    (setf (car *inspect*) component)))
(setf sb-c::*compile-component-hook* 'maybe-inspect-component)

(defun string->form (string)
  (eclector.reader:read-from-string string))

(defun instrument-form (form policy)
  (destructuring-bind (lambda lambda-list &rest body) form
    `(,lambda ,lambda-list
       (declare (optimize ,@policy))
       ,@body)))

(defun form->component (form)
  (let* ((cell     (list nil))
         (output   (make-string-output-stream))
         (function (let ((*inspect*         cell)
                         (*standard-output* output)
                         (*error-output*    output)
                         (*trace-output*    output))
                     (compile nil form))))
    (values (car cell)
            (let ((string (get-output-stream-string output)))
              (if (a:emptyp string) nil string))
            (with-output-to-string (stream)
              (sb-disassem:disassemble-code-component
               function :stream stream)))))

(defun string->component (string policy)
  (form->component (instrument-form (string->form string) policy)))

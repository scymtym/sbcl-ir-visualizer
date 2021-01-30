;;;; application.lisp --- Application frame.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sbcl-ir-visualizer)

;;; Form reading and hook into compiler

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
  (let* ((cell      (list nil))
         (*inspect* cell))
    (compile nil form)
    (car cell)))

(defun string->component (string policy)
  (form->component (instrument-form (string->form string) policy)))

;;; Form

(defclass form-editor (clime:always-repaint-background-mixin ; TDOO should not be needed
                       clim:outlined-pane
                       clim:value-gadget)
  ()
  (:default-initargs
   :thickness  2
   :background clim:+black+))

(defmethod initialize-instance :after ((instance form-editor) &key value)
  (flet ((value-changed (gadget new-value)
           (declare (ignore gadget))
           (setf (clim:gadget-value instance :invoke-callback t)
                 (handler-case
                     (prog1
                         (string->form new-value)
                       (setf (clim:pane-background instance) clim:+background-ink+)
                       (clim:repaint-sheet instance clim:+everywhere+))
                   (error (condition)
                     (princ condition *trace-output*) ; TODO show in user interface
                     (terpri *trace-output*)

                     (setf (clim:pane-background instance) clim:+dark-red+)
                     (clim:repaint-sheet instance clim:+everywhere+)
                     (princ-to-string condition))))))
    (let ((editor (clim:make-pane :text-editor
                                  :client                 instance
                                  :value                  value
                                  :value-changed-callback #'value-changed
                                  :text-style             (clim:make-text-style :fix nil nil)
                                  :nlines                 12)))
      (clim:sheet-adopt-child instance editor))))

;;; Optimization quality button

(defun make-optimization-quality-pane (frame quality label)
  (clim:horizontally ()
    (1/2 (clim:labelling (:label label)))
    (1/2 (clim:make-pane :slider :client           frame
                                 :min-value        0
                                 :max-value        3
                                 :value            1
                                 :decimal-places   0
                                 :number-of-quanta 3
                                 :orientation      :horizontal
                                 :show-value-p     t
                                 :value-changed-callback
                                 (lambda (gadget value)
                                   (let* ((frame  (clim:gadget-client gadget))
                                          (policy (policy frame))
                                          (cell   (find quality policy :key #'first)))
                                     (setf (second cell) (floor value))
                                     (setf (policy frame) policy)))))))

(defun make-optimization-button (frame label policy &key background)
  (apply #'clim:make-pane :push-button
         :client frame
         :label  label
         :activate-callback
         (lambda (gadget)
           (let ((frame (clim:gadget-client gadget)))
             (setf (policy frame) policy)))
         (when background
           (list :background  background))))

;;; Frame

(defvar *example-lambda-expression*
  (format nil "~0@T(lambda (x y)~@
               ~0@T  (declare (type (integer 0 10) x y))~@
               ~0@T  (labels ((rec (x)~@
               ~0@T             (abs (rec x))))~@
               ~0@T    (if (plusp (rec x))~@
               ~0@T        (+ x y)~@
               ~0@T        (list x))))"))

(clim:define-application-frame ir-inspector ()
  ((%form   :accessor form)
   (%policy :accessor policy
            :initform (list (list 'speed             1)
                            (list 'safety            1)
                            (list 'debug             1)
                            (list 'space             1)
                            (list 'compilation-speed 1))))
  (:panes
   (form-editor       form-editor
                      :value *example-lambda-expression*
                      :value-changed-callback
                      (lambda (gadget value)
                        (setf (form (clim:gadget-client gadget)) value)))
   (speed             (make-optimization-quality-pane clim:*application-frame* 'speed             "Speed"))
   (safety            (make-optimization-quality-pane clim:*application-frame* 'safety            "Safety"))
   (debug             (make-optimization-quality-pane clim:*application-frame* 'debug             "Debug"))
   (space             (make-optimization-quality-pane clim:*application-frame* 'space             "Space"))
   (compilation-speed (make-optimization-quality-pane clim:*application-frame* 'compilation-speed "Compilation Speed"))
   (safe              (make-optimization-button
                       clim:*application-frame* "Safe"
                       '((speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0))
                       :background clim:+light-green+))
   (default           (make-optimization-button
                       clim:*application-frame* "Default"
                       '((speed 1) (safety 1) (debug 1) (space 1) (compilation-speed 1))))
   (fast              (make-optimization-button
                       clim:*application-frame* "Fast"
                       '((speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0))
                       :background clim:+salmon+))
   (ir                 clouseau:inspector-pane)
  (:layouts
   (default
    (clim:spacing (:thickness 4)
      (clim:vertically (:spacing 8)
        (clim:horizontally (:spacing 8)
          (:fill (clim:labelling (:label "Lambda Expression")
                   form-editor))
          (clim:vertically (:spacing 8)
            speed
            safety
            debug
            space
            compilation-speed
            (clim:horizontally (:spacing 8)
              safe default fast)
            :fill))
        (:fill (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page)
                 ("Intermediate Representation"
                  (clim:scrolling (:scroll-bars :both) ir))
                 ("Disassembly"
                  (clim:labelling (:label "TODO")))))))))
  (:menu-bar nil)
  (:command-table (ir-inspector-commadn-table
                   :inherit-from (clouseau:inspector-command-table))))

(defmethod (setf form) :after ((new-value t) (frame ir-inspector))
  (a:when-let ((ir (clim:find-pane-named frame 'ir)))
    (handler-case
        (setf (clouseau:root-object ir :run-hook-p t)
              (form->component (instrument-form new-value (policy frame))))
      (error (condition)
        (princ condition *trace-output*)))))

(defmethod (setf policy) :after ((new-value t) (frame ir-inspector))
  (labels ((update-slider (quality)
             (let ((slider (first (clim:sheet-children
                                   (clim:find-pane-named frame quality)))))
               (setf (clim:gadget-value slider :value-changed-callback nil)
                     (second (find quality new-value :key #'first))))))
    (map nil #'update-slider '(speed safety debug space compilation-speed)))
  (a:when-let ((ir (clim:find-pane-named frame 'ir)))
    (handler-case
        (setf (clouseau:root-object ir :run-hook-p t)
              (form->component (instrument-form (form frame) new-value)))
      (error (condition)
        (princ condition *trace-output*)))))

;;; Interface

(defun run (&key new-process)
  (let ((frame (clim:make-application-frame 'ir-inspector)))
    (flet ((do-it ()
             (clim:run-frame-top-level frame)))
      (if new-process
          (bt:make-thread #'do-it)
          (do-it)))
    frame))

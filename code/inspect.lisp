;;;; inspect.lisp --- Clouseau extensions for SBCL IR objects.
;;;;
;;;; Copyright (C) 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:sbcl-ir-visualizer)

;;; lvar utilities

(defvar *lvar-numbers*)

(defun lvar-number (lvar)
  (let ((numbers *lvar-numbers*))
    (a:ensure-gethash lvar numbers (1+ (hash-table-count numbers)))))

(defun lvar-ink (lvar)
  (let ((number (typecase lvar
                  (integer lvar)
                  (t       (lvar-number lvar)))))
    (clim:make-contrasting-inks 8 (mod number 8))))

(defun inspect-lvar (container lvar stream)
  (clouseau:formatting-place
      (container 'clouseau:pseudo-place lvar nil present-object)
    (present-object stream)))

;;; Type utilities

(deftype single-value-type-specifier ()
  '(cons (eql values) (cons t (cons (eql &optional) null))))

(defun print-ctype (ctype stream)
  (let* ((specifier (sb-c::type-specifier ctype))
         (specifier (if (typep specifier 'single-value-type-specifier)
                        (second specifier)
                        specifier)))
    (let ((*print-right-margin* most-positive-fixnum))
      (princ specifier stream))))

(defun print-type-annotation (ctype stream)
  (clim:with-drawing-options (stream :text-size :tiny :ink clim:+dark-green+)
    (write-char #\Space stream)
    (print-ctype ctype stream)))

;;; `component'

;;; TODO collapsed -> name

(defmethod clouseau:make-object-state ((object sb-c::component) (place t))
  ;; Do not show instance slots by default.
  (let ((class (clouseau:object-state-class object place)))
    (make-instance class :place      place
                         :slot-style nil)))

(defun draw-control-arc (stream from to x1 y1 x2 y2
                         &rest args &key &allow-other-keys)
  (cond ((< x2 x1)
         (multiple-value-bind (x2 y2)
             (clim:with-bounding-rectangle* (x1 y1 x2 y2) from
               (declare (ignore y2))
               (values (/ (+ x1 x2) 2) y1))
           (multiple-value-bind (x1 y1)
               (clim:with-bounding-rectangle* (x1 y1 x2 y2) to
                 (declare (ignore y2))
                 (values (/ (+ x1 x2) 2) y1))
             (let* ((margin 100)
                    (y0     (- (min y1 y2) margin)))
               (apply #'clim:draw-line* stream x2 y2 x2 y0 args)
               (apply #'clim:draw-line* stream x1 y0 x2 y0 args)
               (apply #'clim:draw-arrow* stream  x1 y0 x1 y1 args)))))
        (t
         (apply #'clim:draw-arrow* stream x1 y1 x2 y2 args))))

(defun draw-ir1-control-arc (stream from to x1 y1 x2 y2)
  (let* ((from* (clim:graph-node-object from))
         (to*   (clim:graph-node-object to))
         (last  (sb-c::block-last from*))
         (ink   (cond ((not (typep last 'sb-c::cif))
                       nil)
                      ((eq (sb-c::if-consequent last) to*)
                       clim:+green+)
                      ((eq (sb-c::if-alternative last) to*)
                       clim:+red+))))
    (apply #'draw-control-arc stream from to x1 y1 x2 y2
           (when ink (list :ink ink)))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::component)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (let ((*lvar-numbers* (make-hash-table :test #'eq)))
    ;; IR1
    (clouseau:with-section (stream) "IR1 Control Flow Graph"
      #+TODO (map nil (lambda (lambda)
                        (princ lambda stream))
                  (sb-c::component-lambdas object))
      (clim:format-graph-from-root
       (sb-c::component-head object)
       (lambda (node stream)
         (clouseau:formatting-place
             (object 'clouseau:pseudo-place node nil present-object)
           (present-object stream)))
       (lambda (node)
         (sb-c::block-succ node))
       :merge-duplicates t :duplicate-test #'eq
       :maximize-generations t
       :stream stream
       :arc-drawer 'draw-ir1-control-arc))
    ;; IR2
    (clouseau:with-section (stream) "IR2 Control Flow Graph"
      (clim:format-graph-from-roots
       (map 'list #'sb-c::block-info
            (sb-c::block-succ (sb-c::component-head object)))
       (lambda (node stream)
         (clouseau:formatting-place
             (object 'clouseau:pseudo-place node nil present-object)
           (present-object stream)))
       (lambda (node)
         (map 'list #'sb-c::block-info ; TODO is this the best way?
              (sb-c::block-succ (sb-c::ir2-block-block node))))
       :merge-duplicates t :duplicate-test #'eq
       :maximize-generations t
       :stream stream
       :arc-drawer 'draw-control-arc))
    ;; Instance slots
    (call-next-method)))

;;; `cblock'

(defun block-head? (block)
  (eq block (sb-c::component-head (sb-c::block-component block))))

(defun block-tail? (block)
  (eq block (sb-c::component-tail (sb-c::block-component block))))

(defmethod clouseau:make-object-state ((object sb-c::cblock) (place t))
  ;; When the block is inspected as the content of a slot, use
  ;; defaults. Otherwise pre-expand.
  (if (typep place 'clouseau:pseudo-place)
      (let ((class (clouseau:object-state-class object place))
            (style (if (or (block-head? object) (block-tail? object))
                       :collapsed
                       :expanded)))
        (make-instance class :place      place
                             :style      style
                             :slot-style nil))
      (call-next-method)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::cblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (cond ((block-head? object)
         (clim:with-room-for-graphics (stream)
           (clim:draw-circle* stream 0 0 5 :filled nil)))
        ((block-tail? object)
         (clim:with-room-for-graphics (stream)
           (clim:draw-circle* stream 0 0 5 :filled t)))
        (t
         (assert (sb-c::block-start object))
         (format stream "#<~A ~A>"
                 (symbol-name (class-name (class-of object)))
                 (sb-c::block-number object)))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::cblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (clouseau::inspect-class-as-name (class-of object) stream)
  (format stream " ~A " (sb-c::block-number object)))

(defun inspect-nodes (block start-node stream)
  (labels ((do-it (ctran)
             (when ctran
               (let ((node (sb-c::ctran-next ctran)))
                 (fresh-line stream)
                 (clouseau:formatting-place
                     (block 'clouseau:pseudo-place node nil present-object)
                   (present-object stream))
                 (do-it (sb-c::node-next node))))))
    (do-it start-node)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::cblock)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))

  (a:when-let ((start (sb-c::block-start object)))
    (clouseau:with-section (stream) "Nodes"
      (inspect-nodes object start stream))
    (terpri stream))
  ;; Instance slots
  (call-next-method))

;;; `node'

(defmethod clouseau:inspect-object-using-state ((object sb-c::node)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (clim:with-drawing-options (stream :text-family :fix :ink clim:+dark-blue+)
    (format stream "~(~A~)" (symbol-name (class-name (class-of object))))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::bind)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  (let ((lambda (sb-c::bind-lambda object)))
    (format stream " ~A" (sb-c::functional-debug-name lambda))))

(defmethod clouseau:inspect-object-using-state :after ((object sb-c::valued-node)
                                                       (state  clouseau:inspected-instance)
                                                       (style  (eql :collapsed))
                                                       (stream clim:extended-output-stream))
  ;; Result lvar.
  (a:when-let ((result (sb-c::node-lvar object)))
    (write-string " → " stream)
    (inspect-lvar object result stream))
  ;; Derived type.
  (print-type-annotation (sb-c::node-derived-type object) stream))

(defmethod clouseau:inspect-object-using-state ((object sb-c::ref)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  ;; Try to present the referenced leaf nicely.
  (let ((leaf (sb-c::ref-leaf object)))
   (format stream " ~A"
           (cond ((sb-c::constant-p leaf)
                  (let ((value (sb-c::constant-value leaf)))
                    (cond ((and (listp value) (sb-c::lambda-p (first value)))
                           (sb-c::lambda-%debug-name (first value)))
                          ((and (listp value) (sb-c::optional-dispatch-p (first value)))
                           "optional dispatch")
                          (t
                           value))))
                 ((sb-c::global-var-p leaf)
                  (sb-c::global-var-%source-name leaf))
                 ((sb-c::lambda-p leaf)
                  (sb-c::lambda-%debug-name leaf))
                 (t
                  (sb-c::ref-%source-name object))))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::cast)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  ;; Input value.
  (write-char #\Space stream)
  (inspect-lvar object (sb-c::cast-value object) stream)
  ;; Asserted type.
  (clim:with-drawing-options (stream :text-size :tiny :ink clim:+dark-green+)
    (print-ctype (sb-c::cast-asserted-type object) stream)
    (write-char #\! stream)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::combination)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  ;; Combination kind and possibly "tail" annotation.n
  (clim:with-drawing-options (stream :text-size :tiny :ink clim:+dark-red+)
    (when (sb-c::combination-tail-p object)
      (let* ((text   "tail")
             ; (height (nth-value 1 (clim:text-size stream text)))
             )
        (clouseau:with-preserved-cursor-y (stream)
          (clouseau:with-preserved-cursor-x (stream)
            ;; TODO dy should be something like (- (+ height 2)) but
            ;; moving the cursor closes the current text output record
            ;; which in turn messes with the baseline so that doesn't
            ;; work.
            (clim:stream-increment-cursor-position stream 0 -2)
            (write-string text stream)))))
    (format stream "~(~A~)" (sb-c::combination-kind object))
    (write-char #\Space stream))
  ;; Function lvar. Print the function name if possible.
  (let* ((fun  (sb-c::combination-fun object))
         (name (sb-c::lvar-fun-name* fun)))
    (write-char #\Space stream)
    (inspect-lvar object fun stream)
    (when name
      (clim:with-drawing-options (stream :text-size :tiny :ink clim:+gray40+)
        (format stream "~A" name))))
  ;; Argument lvars.
  (map nil (lambda (lvar)
             (write-char #\Space stream)
             (inspect-lvar object lvar stream))
       (sb-c::combination-args object)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::cif)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (call-next-method)
  ;; Test lvar
  (write-char #\Space stream)
  (inspect-lvar object (sb-c::if-test object) stream))

(defmethod clouseau:inspect-object-using-state :after ((object sb-c::creturn)
                                                       (state  clouseau:inspected-instance)
                                                       (style  (eql :collapsed))
                                                       (stream clim:extended-output-stream))
  ;; Result lvar
  (write-char #\Space stream)
  (inspect-lvar object (sb-c::return-result object) stream)
  ;; Result type
  (print-type-annotation (sb-c::return-result-type object) stream))

;;; `lvar'
;;;
;;; We just make up a number and corresponding color.

(defmethod clouseau:inspect-object-using-state ((object sb-c::lvar)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (let ((number (lvar-number object)))
    (clim:with-drawing-options (stream :ink (lvar-ink number))
      (format stream "~D" number))))

;;; `ir2block'

(defmethod clouseau:make-object-state ((object sb-c::ir2-block) (place t))
  ;; When the block is inspected as the content of a slot, use
  ;; defaults. Otherwise pre-expand.
  (if (typep place 'clouseau:pseudo-place)
      (let ((class (clouseau:object-state-class object place)))
        (make-instance class :place      place
                             :style      :expanded
                             :slot-style nil))
      (call-next-method)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::ir2-block)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (format stream "#<~A ~A>"
          (symbol-name (class-name (class-of object)))
          (sb-c::ir2-block-number object)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::ir2-block)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (clouseau::inspect-class-as-name (class-of object) stream)
  (format stream " ~A " (sb-c::ir2-block-number object)))

(defmethod clouseau:inspect-object-using-state ((object sb-c::ir2-block)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (clouseau:with-section (stream) "VOPs"
    (labels ((do-it (current)
               (when current
                 (fresh-line stream)
                 (clouseau:formatting-place
                     (object 'clouseau:pseudo-place current nil present-object)
                   (present-object stream))
                 (do-it (sb-c::vop-next current)))))
      (do-it (sb-c::ir2-block-start-vop object))))
  (terpri stream)

  (call-next-method))

;;; `vop'

(defmethod clouseau:inspect-object-using-state ((object sb-c::vop)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (let ((info (sb-c::vop-info object)))
    (clim:with-drawing-options (stream :text-family :fix :ink clim:+dark-blue+)
      (format stream "~(~A~)" (sb-c::vop-info-name info)))
    (labels ((collect (ref)
               (when ref
                 (list* (sb-c::tn-number (sb-c:tn-ref-tn ref))
                        (collect (sb-c::tn-ref-across ref))))))
      (format stream "~@[ ~{~A~^ ~}~]~@[ → ~{~A~^ ~}~]"
              (collect (sb-c::vop-args object))
              (collect (sb-c::vop-results object))))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::vop)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-header))
                                                (stream clim:extended-output-stream))
  (let ((info (sb-c::vop-info object)))
    (format stream "~A" (sb-c::vop-info-name info))))

(defmethod clouseau:inspect-object-using-state ((object sb-c::vop)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :expanded-body))
                                                (stream clim:extended-output-stream))
  (call-next-method))

;;; `tn'

(defmethod clouseau:inspect-object-using-state ((object sb-c::tn)
                                                (state  clouseau:inspected-instance)
                                                (style  (eql :collapsed))
                                                (stream clim:extended-output-stream))
  (format stream "~D" (sb-c::tn-number object)))

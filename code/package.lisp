;;;; package.lisp --- Package definition for the sbcl-ir-visualizer system.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:sbcl-ir-visualizer
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria))

  (:export
   #:run))

;;;; sbcl-ir-visualizer.asd --- System definition for the sbcl-ir-visualizer system.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "sbcl-ir-visualizer"
  :description "A simple interactive visualizer for SBCL's intermediate representations"
  :license     "GPLv3" ; see COPYING file for details

  :version     "0.1"
  :depends-on  ("alexandria"

                "eclector"

                "mcclim"
                "clouseau")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "inspect")
                              (:file       "application"))))

  :perform     (prepare-op :before (operation component)
                 #-sbcl (error "This system only works on SBCL")))

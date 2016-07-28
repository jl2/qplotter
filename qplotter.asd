;;;; qplotter.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@solidfire.com>

(asdf:defsystem #:qplotter
  :description "Plot parametric curves using Qt."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:qtools
               #:qtgui
               #:qtcore)
  :serial t
  :components ((:file "package")
               (:file "qplotter")))


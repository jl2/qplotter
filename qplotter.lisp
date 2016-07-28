;;;; qplotter.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:qplotter)
(named-readtables:in-readtable :qtools)

(define-widget main-window (QMainWindow)
  ())

(define-menu (main-window File)
  (:item ("Save" (ctrl s))
         (q+:close main-window))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Function plotter using QTools.")))


;; map-val is used to map logical coordinates to screen coordinates.
(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

(defun default-x (tv)
  "X component of the parametric equation for an epitrochoid curve."
  tv)

(defun default-y (tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (+ (sin tv) (* 2.0 (cos (* 3.0 tv))) (* 0.3 (sin (* 50.0 tv)))))

(define-widget parametric-plotter (QWidget)
  ((steps :initform 2000)
   (t-min :initform (- pi))
   (t-max :initform pi)
   (x-min :initform (- pi))
   (x-max :initform pi)
   (y-min :initform -1.5)
   (y-max :initform 1.5)
   (x-function :initform #'default-x)
   (y-function :initform #'default-y))
  (:documentation "The parametric-plotter widget draws a parametric curve using the currently specified parameters."))


(define-override (parametric-plotter paint-event paint) (ev)
  "Handle paint events."
  (declare (ignore ev))

  ;; Set max-radius to an estimate on the max radius of the curve, given the current values of a, b, and h.
  ;; Multiply by 1.1 to give a bit of empty space around the sides.
  (let ((dt (/ (- t-max t-min) steps)))
    ;; Define some local functions for convenience
    (flet (
           ;; xmapper maps logical x coordinates in the range x-min to x-max to screen coordinates in the range 0 to width
           (xmapper (x) (map-val x x-min x-max 0 (q+:width parametric-plotter)))

           ;; ymapper does the same thing, but for y coordinates
           (ymapper (y) (map-val y y-min y-max 0 (q+:height parametric-plotter)))
           
           ;; xfun and yfun hide funcall and make the code easier to read below
           (xfun (tv) (funcall x-function tv))
           (yfun (tv) (funcall y-function tv)))
      
      (with-finalizing 
          ;; Create a painter object to draw on
          ((painter (q+:make-qpainter parametric-plotter)))

        ;; Clear the background
        (q+:fill-rect painter (q+:rect parametric-plotter) (q+:qt.white))

        ;; Draw the curve
        (loop
           for i below steps
           for cur-t = t-min then (+ (* i dt) t-min)
           do
             (q+:draw-line painter
                           (truncate (xmapper (xfun cur-t)))
                           (truncate (ymapper (yfun cur-t)))
                           (truncate (xmapper (xfun (+ dt cur-t))))
                           (truncate (ymapper (yfun (+ dt cur-t))))))))))

(define-widget plotter-controls (QWidget)
  ()
  (:documentation "The plotter-controls widget contains input controls for all of the data fields
                   in a parametric-plotter object."))


;; Create all of the controls
(define-subwidget (plotter-controls sviewer) (make-instance 'parametric-plotter)
  "The parametric-plotter itself.")

(define-subwidget (plotter-controls t-min-spin) (q+:make-qdoublespinbox plotter-controls)
  "The minimum t value spinbox."
  (q+:set-decimals t-min-spin 2)
  (q+:set-single-step t-min-spin 0.01)
  (q+:set-maximum t-min-spin (* 1000.0 pi))
  (q+:set-minimum t-min-spin (* -1000.0 pi))
  (q+:set-value t-min-spin (slot-value sviewer 't-min)))

(define-subwidget (plotter-controls t-max-spin) (q+:make-qdoublespinbox plotter-controls)
  "The maximum t value spinbox."
  (q+:set-decimals t-max-spin 2)
  (q+:set-single-step t-max-spin 0.01)
  (q+:set-maximum t-max-spin (* 1000.0 pi))
  (q+:set-minimum t-max-spin (* -1000.0 pi))
  (q+:set-value t-max-spin (slot-value sviewer 't-max)))


(define-subwidget (plotter-controls x-min-spin) (q+:make-qdoublespinbox plotter-controls)
  "The minimum x value spinbox."
  (q+:set-decimals x-min-spin 2)
  (q+:set-single-step x-min-spin 0.01)
  (q+:set-maximum x-min-spin (* 1000.0 pi))
  (q+:set-minimum x-min-spin (* -1000.0 pi))
  (q+:set-value x-min-spin (slot-value sviewer 'x-min)))

(define-subwidget (plotter-controls x-max-spin) (q+:make-qdoublespinbox plotter-controls)
  "The maximum x value spinbox."
  (q+:set-decimals x-max-spin 2)
  (q+:set-single-step x-max-spin 0.01)
  (q+:set-maximum x-max-spin (* 1000.0 pi))
  (q+:set-minimum x-max-spin (* -1000.0 pi))
  (q+:set-value x-max-spin (slot-value sviewer 'x-max)))


(define-subwidget (plotter-controls y-min-spin) (q+:make-qdoublespinbox plotter-controls)
  "The minimum x value spinbox."
  (q+:set-decimals y-min-spin 2)
  (q+:set-single-step y-min-spin 0.01)
  (q+:set-maximum y-min-spin (* 1000.0 pi))
  (q+:set-minimum y-min-spin (* -1000.0 pi))
  (q+:set-value y-min-spin (slot-value sviewer 'y-min)))

(define-subwidget (plotter-controls y-max-spin) (q+:make-qdoublespinbox plotter-controls)
  "The maximum x value spinbox."
  (q+:set-decimals y-max-spin 2)
  (q+:set-single-step y-max-spin 0.01)
  (q+:set-maximum y-max-spin (* 1000.0 pi))
  (q+:set-minimum y-max-spin (* -1000.0 pi))
  (q+:set-value y-max-spin (slot-value sviewer 'y-max)))

(define-subwidget (plotter-controls y-max-spin) (q+:make-qdoublespinbox plotter-controls)
  "The maximum x value spinbox."
  (q+:set-decimals y-max-spin 2)
  (q+:set-single-step y-max-spin 0.01)
  (q+:set-maximum y-max-spin (* 1000.0 pi))
  (q+:set-minimum y-max-spin (* -1000.0 pi))
  (q+:set-value y-max-spin (slot-value sviewer 'y-max)))

(define-subwidget (plotter-controls y-max-spin) (q+:make-qdoublespinbox plotter-controls)
  "The maximum x value spinbox."
  (q+:set-decimals y-max-spin 2)
  (q+:set-single-step y-max-spin 0.01)
  (q+:set-maximum y-max-spin (* 1000.0 pi))
  (q+:set-minimum y-max-spin (* -1000.0 pi))
  (q+:set-value y-max-spin (slot-value sviewer 'y-max)))

(define-subwidget (plotter-controls steps-spin) (q+:make-qspinbox plotter-controls)
  "The spinbox for the number of steps."
  (q+:set-maximum steps-spin 10000000)
  (q+:set-minimum steps-spin 4)
  (q+:set-value steps-spin (slot-value sviewer 'steps)))

(define-subwidget (plotter-controls x-fun-edit) (q+:make-qlineedit plotter-controls)
  (setf (q+:text x-fun-edit) "tv"))

(define-subwidget (plotter-controls y-fun-edit) (q+:make-qlineedit plotter-controls)
  (setf (q+:text y-fun-edit) "(+ (sin tv) (* 2.0 (cos (* 3.0 tv))) (* 0.3 (sin (* 50.0 tv))))"))

;; It would be nice to handle all spin box changes in one slot, but I don't know 
;; how to ignore the value type.
(define-slot (plotter-controls steps-changed) ((value int))
  "Handle changes to the steps-spin box."
  (declare (connected steps-spin (value-changed int)))
  (setf (slot-value sviewer 'steps) (q+:value steps-spin))
  (q+:repaint sviewer))

(define-slot (plotter-controls values-changed) ((value double))
  "Handle changes to all of the spin boxes except steps."
  (declare (connected t-min-spin (value-changed double)))
  (declare (connected t-max-spin (value-changed double)))
  (declare (connected x-min-spin (value-changed double)))
  (declare (connected x-max-spin (value-changed double)))
  (declare (connected y-min-spin (value-changed double)))
  (declare (connected y-max-spin (value-changed double)))
  (setf (slot-value sviewer 't-min) (q+:value t-min-spin))
  (setf (slot-value sviewer 't-max) (q+:value t-max-spin))
  (setf (slot-value sviewer 'x-min) (q+:value x-min-spin))
  (setf (slot-value sviewer 'x-max) (q+:value x-max-spin))
  (setf (slot-value sviewer 'y-min) (q+:value y-min-spin))
  (setf (slot-value sviewer 'y-max) (q+:value y-max-spin))
  (q+:repaint sviewer))

(define-slot (plotter-controls editing-finished) ()
  (declare (connected x-fun-edit (editing-finished void)))
  (declare (connected y-fun-edit (editing-finished void)))
  (setf (slot-value sviewer 'x-function) (eval (read-from-string (format nil "(lambda (tv) ~a)" (q+:text x-fun-edit)))))
  (setf (slot-value sviewer 'y-function) (eval (read-from-string (format nil "(lambda (tv) ~a)" (q+:text y-fun-edit)))))
  (q+:repaint sviewer))

(define-subwidget (plotter-controls control-layout) (q+:make-qvboxlayout plotter-controls)
  "Layout all of the control widgets in a vertical box layout."

  ;; Create horizontal layouts to hold the labels and spinboxes
  (let ((x-inner (q+:make-qhboxlayout))
        (y-inner (q+:make-qhboxlayout))
        (t-inner (q+:make-qhboxlayout))
        (steps-inner (q+:make-qhboxlayout))
        (x-fun-inner (q+:make-qhboxlayout))
        (y-fun-inner (q+:make-qhboxlayout)))
    
    ;; Populate the horizontal layouts and add them to the top level vertical layout
    (q+:add-widget x-inner (q+:make-qlabel "X min: " plotter-controls))
    (q+:add-widget x-inner x-min-spin)
    (q+:add-widget x-inner (q+:make-qlabel "X max: " plotter-controls))
    (q+:add-widget x-inner x-max-spin)
    (q+:add-layout control-layout x-inner)

    (q+:add-widget y-inner (q+:make-qlabel "Y min: " plotter-controls))
    (q+:add-widget y-inner y-min-spin)
    (q+:add-widget y-inner (q+:make-qlabel "Y max: " plotter-controls))
    (q+:add-widget y-inner y-max-spin)
    (q+:add-layout control-layout y-inner)

    (q+:add-widget t-inner (q+:make-qlabel "t min: " plotter-controls))
    (q+:add-widget t-inner t-min-spin)
    (q+:add-widget t-inner (q+:make-qlabel "t max: " plotter-controls))
    (q+:add-widget t-inner t-max-spin)
    (q+:add-layout control-layout t-inner)

    (q+:add-widget steps-inner (q+:make-qlabel "Steps: " plotter-controls))
    (q+:add-widget steps-inner steps-spin)
    (q+:add-layout control-layout steps-inner)

    (q+:add-widget x-fun-inner (q+:make-qlabel "x(tv) = " plotter-controls))
    (q+:add-widget x-fun-inner x-fun-edit)
    (q+:add-layout control-layout x-fun-inner)

    (q+:add-widget y-fun-inner (q+:make-qlabel "y(tv) = " plotter-controls))
    (q+:add-widget y-fun-inner y-fun-edit)
    (q+:add-layout control-layout y-fun-inner)

    ;; Finally add the plotter directly to the vertical layout
    (q+:add-widget control-layout sviewer)
    ))


(define-subwidget (main-window controls) (make-instance 'plotter-controls )
  "The main-window's plotter-controls widget."
  )

(define-initializer (main-window setup)
  "Set the window title and set the plotter-controls to be the central widget."
  (setf (q+:window-title main-window) "Parametric Curve Plotter")
  (setf (q+:central-widget main-window) controls))

(defun main ()
  "Create the main window."
  (with-main-window (window (make-instance 'main-window))))

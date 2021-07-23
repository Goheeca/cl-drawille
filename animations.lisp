(defpackage :cl-drawille/examples-animations
  (:use :common-lisp :cl-drawille :cl-charms)
  (:export :sine-tracking-example))

(in-package :cl-drawille/examples-animations)
      
(defun animate (animation)
  (cl-charms:with-curses ()
    (cl-charms:disable-echoing)
    (cl-charms/low-level:timeout 10)
    (cl-charms/low-level:curs-set 0)
    (cl-charms:clear-window cl-charms:*standard-window*)
    (cl-charms:refresh-window cl-charms:*standard-window*)
    (loop named curses-loop
          for input = (cl-charms:get-char cl-charms:*standard-window* :ignore-error t) 
          do (when (eq #\q input) (return-from curses-loop))
             (multiple-value-bind (frame delay) (funcall animation)
               (loop for y from 0 and row in frame
                     do (cl-charms:write-string-at-point cl-charms:*standard-window* row 0 y))
               (cl-charms:refresh-window cl-charms:*standard-window*)
               (sleep delay)))))

(defun sine-tracking ()
  (let ((canvas (cl-drawille:make-canvas))
        (i 0)
        (height 40))
    (lambda ()
      (cl-drawille:clear canvas)
      (loop for (x y) in (cl-drawille::line 0 height 180 (+ height (* height (sin (/ (* i pi) 180)))))
            do (cl-drawille:set-pixel canvas x y))
      (loop for x from 0 below 360 by 2
            do (cl-drawille:set-pixel canvas (/ x 2) (+ height (* height (sin (/ (* (+ x i) pi) 180))))))
      (incf i 2)
      (values (cl-drawille:rows canvas) 1/60))))
  
(defun sine-tracking-example ()
  (animate (sine-tracking)))

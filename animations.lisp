(defpackage :cl-drawille/examples-animations
  (:use :common-lisp :cl-drawille :cl-charms)
  (:export :sine-tracking-example :rotating-cube-example))

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
                     do (ignore-errors (cl-charms:write-string-at-point cl-charms:*standard-window* row 0 y)))
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


(defun rotate-x (input angle)
  (let ((cos (cos (/ (* angle pi) 180)))
        (sin (sin (/ (* angle pi) 180))))
    (vector (aref input 0)
            (- (* cos (aref input 1)) (* sin (aref input 2)))
            (+ (* sin (aref input 1)) (* cos (aref input 2))))))

(defun rotate-y (input angle)
  (let ((cos (cos (/ (* angle pi) 180)))
        (sin (sin (/ (* angle pi) 180))))
    (vector (- (* cos (aref input 2)) (* sin (aref input 0)))
            (aref input 1)
            (+ (* sin (aref input 2)) (* cos (aref input 0))))))

(defun rotate-z (input angle)
  (let ((cos (cos (/ (* angle pi) 180)))
        (sin (sin (/ (* angle pi) 180))))
    (vector (- (* cos (aref input 0)) (* sin (aref input 1)))
            (+ (* sin (aref input 0)) (* cos (aref input 1)))
            (aref input 2))))

(defun project (input width height fov distance)
  (let ((factor (/ fov (+ distance (aref input 2)))))
    (vector (+ (/ width 2) (* factor (aref input 0)))
            (+ (/ height 2) (* factor (aref input 1)))
            1)))

(defun rotating-cube (&key (projection nil))
  (let ((canvas (cl-drawille:make-canvas))
        (vertices '(#(-20 20 -20)
                    #(20 20 -20)
                    #(20 -20 -20)
                    #(-20 -20 -20)
                    #(-20 20 20)
                    #(20 20 20)
                    #(20 -20 20)
                    #(-20 -20 20)))
        (faces '((0 1 2 3) (1 5 6 2) (5 4 7 6) (4 0 3 7) (0 4 5 1) (3 2 6 7)))
        (angle-x 0)
        (angle-y 0)
        (angle-z 0))
    (lambda ()
      (cl-drawille:clear canvas)
      (flet ((cond-project (input &rest args) (if projection (apply #'project input args) input)))
        (let ((transformed
		(loop for vertex in vertices collect (cond-project (rotate-z (rotate-y (rotate-x vertex angle-x) angle-y) angle-z) 50 50 50 50))))
          (loop for (a b c d) in faces
		do (loop for (x y) in (cl-drawille::line (aref (nth a transformed) 0) (aref (nth a transformed) 1) (aref (nth b transformed) 0) (aref (nth b transformed) 1))
			 do (cl-drawille:set-pixel canvas x y))
		   (loop for (x y) in (cl-drawille::line (aref (nth b transformed) 0) (aref (nth b transformed) 1) (aref (nth c transformed) 0) (aref (nth c transformed) 1))
			 do (cl-drawille:set-pixel canvas x y))
		   (loop for (x y) in (cl-drawille::line (aref (nth c transformed) 0) (aref (nth c transformed) 1) (aref (nth d transformed) 0) (aref (nth d transformed) 1))
			 do (cl-drawille:set-pixel canvas x y))
		   (loop for (x y) in (cl-drawille::line (aref (nth d transformed) 0) (aref (nth d transformed) 1) (aref (nth a transformed) 0) (aref (nth a transformed) 1))
			 do (cl-drawille:set-pixel canvas x y)))))
      (incf angle-x 2)
      (incf angle-y 3)
      (incf angle-z 5)
      (values (cl-drawille:rows canvas :min-x -40 :min-y -40 :max-x 80 :max-y 80) 1/20))))

(defun rotating-cube-example (&key (projection nil))
  (animate (rotating-cube :projection projection)))

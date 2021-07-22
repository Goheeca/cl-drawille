(defpackage :cl-drawille/examples
  (:use :common-lisp :cl-drawille)
  (:export :turtle-example :basic-example))

(in-package :cl-drawille/examples)


(defun turtle-example ()
  (let ((turtle (cl-drawille:make-turtle)))
    (loop repeat 36
	  do (cl-drawille:right turtle 10)
	     (loop repeat 36
		   do (cl-drawille:right turtle 10)
		      (cl-drawille:forward turtle 8)))
    (format t "~a~%" (cl-drawille:frame turtle))))


(defun basic-example ()
  (let ((canvas (cl-drawille:make-canvas)))
    (loop for x below 1800
	  do (cl-drawille:set-pixel canvas (/ x 10) (* 10 (sin (* (/ x 180) pi)))))
    (format t "~a~%" (cl-drawille:frame canvas))
    (cl-drawille:clear canvas)
    (loop for x below 1800 by 10
	  do (cl-drawille:set-pixel canvas (/ x 10) (+ 10 (* 10 (sin (* (/ x 180) pi)))))
	     (cl-drawille:set-pixel canvas (/ x 10) (+ 10 (* 10 (cos (* (/ x 180) pi))))))
    (format t "~a~%" (cl-drawille:frame canvas))
    (cl-drawille:clear canvas)
    (loop for x below 3600 by 20
	  do (cl-drawille:set-pixel canvas (/ x 20) (+ 4 (* 4 (sin (* (/ x 180) pi))))))
    (format t "~a~%" (cl-drawille:frame canvas))
    (cl-drawille:clear canvas)
    (loop for x below 360 by 4
	  do (cl-drawille:set-pixel canvas (/ x 4) (+ 30 (* 30 (sin (* (/ x 180) pi))))))
    (loop for x below 30
      do (loop for y below 30
	       do (cl-drawille:set-pixel canvas x y)
		  (cl-drawille:toggle-pixel canvas (+ 30 x) (+ 30 y))
		  (cl-drawille:toggle-pixel canvas (+ 60 x) y)))
    (format t "~a~%" (cl-drawille:frame canvas))))

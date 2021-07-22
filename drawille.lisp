(defpackage :cl-drawille
  (:use :common-lisp :cffi :osicat :alexandria)
  (:export :terminal-size :make-canvas :clear
           :set-pixel :unset-pixel :toggle-pixel
	   :rows :frame :set-text :make-turtle
	   :up :down :forward :move :right :left :back))

(in-package :cl-drawille)


(defun terminal-size ()
  (multiple-value-bind (columns rows)
      (ignore-errors
       (cffi:with-foreign-object (window-size '(:struct osicat-posix:winsize))
	 (osicat-posix:ioctl 0 osicat-posix:tiocgwinsz window-size)
	 (let ((columns (cffi:foreign-slot-value window-size '(:struct osicat-posix:winsize) 'osicat-posix:col))
	       (rows (cffi:foreign-slot-value window-size '(:struct osicat-posix:winsize) 'osicat-posix:row)))
	   (values columns rows))))
    (if columns
	(values columns rows)
	(let ((columns (osicat:environment-variable "COLUMNS"))
	      (rows (osicat:environment-variable "LINES")))
	  (values (if columns (parse-integer columns) 80) (if rows (parse-integer rows) 25))))))
        
        
(alexandria:define-constant +pixel-map+ #2A((#x01 #x08) (#x02 #x10) (#x04 #x20) (#x40 #x80)) :test #'equalp)

(defconstant +braille-offset+ #x2800)


(defun normalize (coord)
  (round coord)) 

(defun get-pos (x y)
  (values (floor x 2) (floor y 4)))

(defun pixel (x y)
  (aref +pixel-map+ (mod y 4) (mod x 2)))


(defclass canvas ()
  ((chars
    :initform (make-hash-table :test #'equal)
    :accessor chars)))

(defun make-canvas ()
  (make-instance 'canvas))


(defgeneric clear (c))

(defmethod clear ((c canvas))
  (setf (chars c) (make-hash-table :test #'equal)))


(defmacro access-char (&body body)
  `(let ((x (normalize x))
	 (y (normalize y)))
     (multiple-value-bind (col row) (get-pos x y)
       (let* ((key (cons col row))
	      (char (gethash key (chars c)))
	      (char (typecase char
		      (integer char)
		      (t 0)))
	      (pixel (pixel x y)))
	 ,@body
	 (if (equal 0 (gethash key (chars c)))
	     (remhash key (chars c)))))))

(defmacro process-char (&body body)
  `(access-char (setf (gethash key (chars c)) ,@body)))


(defgeneric set-pixel (c x y))

(defmethod set-pixel ((c canvas) x y)
  (process-char (logior char pixel)))


(defgeneric unset-pixel (c x y))

(defmethod unset-pixel ((c canvas) x y)
  (process-char (logandc2 char pixel)))


(defgeneric toggle-pixel (c x y))

(defmethod toggle-pixel ((c canvas) x y)
  (process-char (logxor char pixel)))


(defgeneric get-pixel (c x y))

(defmethod get-pixel ((c canvas) x y)
  (access-char (/= 0 (logand char pixel))))


(defgeneric get-char (c col row))

(defmethod get-char ((c canvas) col row)
  (let* ((key (cons col row))
	 (char (gethash key (chars c)))
	 (char (if char char 0)))
    (typecase char
      (integer (code-char (+ +braille-offset+ char)))
      (t char))))


(defgeneric rows (c &key))

(defmethod rows ((c canvas) &key (min-x nil) (min-y nil) (max-x nil) (max-y nil))
  (multiple-value-bind (computed-max-x computed-min-x computed-max-y computed-min-y)
      (loop for key being the hash-key of (chars c)
	    maximize (1+ (car key)) into max-x
	    minimize (car key) into min-x
	    maximize (1+ (cdr key)) into max-y
	    minimize (cdr key) into min-y
	    finally (return (values max-x min-x max-y min-y)))
    (let ((max-x (if max-x (ceiling max-x 2) computed-max-x))
	  (min-x (if min-x (floor min-x 2) computed-min-x))
	  (max-y (if max-y (ceiling max-y 4) computed-max-y))
	  (min-y (if min-y (floor min-y 4) computed-min-y)))
      (loop for y from min-y below max-y
	    collect (format nil "~{~c~}"
			    (loop for x from min-x below max-x
				  collect (get-char c x y)))))))


(defgeneric frame (c &key))

(defmethod frame ((c canvas) &key (min-x nil) (min-y nil) (max-x nil) (max-y nil))
  (format nil "~{~a~^~%~}" (rows c :min-x min-x :min-y min-y :max-x max-x :max-y max-y)))


(defgeneric set-text (c x y text))

(defmethod set-text ((c canvas) x y text)
  (multiple-value-bind (col row) (get-pos (normalize x) (normalize y))
    (loop for char across text
	  and i from 0
	  do (setf (gethash (cons (+ col i) row) (chars c)) char))))
  

(defun line (x1 y1 x2 y2)
  (let* ((x1 (normalize x1))
	 (y1 (normalize y1))
	 (x2 (normalize x2))
	 (y2 (normalize y2))
	 (xdiff (abs (- x1 x2)))
	 (ydiff (abs (- y1 y2)))
	 (xdir (if (<= x1 x2) 1 -1))
	 (ydir (if (<= y1 y2) 1 -1))
         (r (max xdiff ydiff)))
    (loop for i from 0 to r
	  collect (list
		   (+ x1 (/ (* i xdiff xdir) r))
		   (+ y1 (/ (* i ydiff ydir) r))))))

(defun polygon (&optional (center-x 0) (center-y 0) (sides 4) (radius 4))
  (let ((angle (/ (* 2 pi) sides))
	(radius (1+ radius)))
    (loop for n below sides
	  append (line (+ center-x (/ (* radius (cos (* n angle))) 2))
		       (+ center-y (/ (* radius (sin (* n angle))) 2))
		       (+ center-x (/ (* radius (cos (* (1+ n) angle))) 2))
		       (+ center-y (/ (* radius (sin (* (1+ n) angle))) 2))))))


(defclass turtle (canvas)
  ((pos-x
    :initarg :pos-x
    :accessor pos-x)
   (pos-y
    :initarg :pos-y
    :accessor pos-y)
   (rotation
    :initform 0
    :accessor rotation)
   (brush
    :initform t
    :accessor brush)))

(defun make-turtle (&optional (pos-x 0) (pos-y 0))
  (make-instance 'turtle :pos-x pos-x :pos-y pos-y))

(defgeneric up (tur))

(defmethod up ((tur turtle))
  (setf (brush tur) nil))


(defgeneric down (tur))

(defmethod down ((tur turtle))
  (setf (brush tur) t))


(defgeneric forward (tur step))

(defmethod forward ((tur turtle) step)
  (let ((x (+ (pos-x tur) (* step (cos (/ (* pi (rotation tur)) 180)))))
	(y (+ (pos-y tur) (* step (sin (/ (* pi (rotation tur)) 180))))))
    (move tur x y)))


(defgeneric move (tur x y))

(defmethod move ((tur turtle) x y)
  (when (brush tur)
    (loop for (x y) in (line (pos-x tur) (pos-y tur) x y)
	  do (set-pixel tur x y)))
  (setf (pos-x tur) x)
  (setf (pos-y tur) y))


(defgeneric right (tur angle))

(defmethod right ((tur turtle) angle)
  (incf (rotation tur) angle))


(defgeneric left (tur angle))

(defmethod left ((tur turtle) angle)
  (decf (rotation tur) angle))


(defgeneric back (tur step))

(defmethod back ((tur turtle) step)
  (forward tur (- step)))

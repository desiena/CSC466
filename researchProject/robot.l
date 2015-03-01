;;;;Robot.l
;;;;Feb 2015
;;;;Ace DeSiena
;;;;An ai project for CSC466, AI2 at Oswego. This project will implement a simulated robot that uses simultaneous localization and mapping algorithms.


;=============================================================================
;Robot

(defClass robot ()
  (
   (world :accessor robot-world :initarg :world)
   (speed :accessor robot-speed :initarg :speed :initform 1)
   ))

(defmethod move ((direction symbol)(distance number))
  (dotimes (moves (/ distance speed))
    (move direction)))

(defmethod move ((direction symbol))
  (


;=============================================================================
;World

(defClass world ()
  (
   (robot-position :accessor world-robot-position :initarg :robot-position)
   (wall-list :accessor world-wall-list :initarg :wall-list)))

(defmethod display ((world world))
  (format t "Robot Position: ~A~%" (world-robot-position world))
  (dolist (wall (world-wall-list world))
    (format t " Wall Position: ~A~%" wall)))

(defmethod demophase1 ()
  (setf world (make-instance 'world))
  (setf (world-robot-position world) '(0.0 0.0))
  (setf (world-wall-list world) '(((2 3)(-1 3))((0 5)(0 3))))
  (display world))
   
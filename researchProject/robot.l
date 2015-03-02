;;;;Robot.l
;;;;Feb 2015
;;;;Ace DeSiena
;;;;An ai project for CSC466, AI2 at Oswego. This project will implement a simulated robot that uses simultaneous localization and mapping algorithms.

(defun init ()
  (setf *pose* '(0.0 0.0 0.0)) ;Pose is defined as [x y theta]
  (setf *walls* '(((3.0 3.0)(-3.0 3.0))((0.0 3.0)(0.0 5.0))))
  (setf *a1* 0.0);translational error from translation
  (setf *a2* 0.0);translational error from rotation
  (setf *a3* 0.0);rotational error from translation
  (setf *a4* 0.0);rotational error from rotation
  (setf *a5* 0.0);circularity error from translation 
  (setf *a6* 0.0);circularity error from rotation
  (demo1))

(defun display-world ()
  (format t "Robot Position: ~A~%" *pose*)
  (format t "         Walls: ~A~%" (car *walls*))
  (dolist (wall (cdr *walls*))
    (format t "              : ~A~%" wall))
  (terpri))

(defun move (translational-velocity rotational-velocity)
  (let* ((tv (+ translational-velocity (sample-normal (+ (* (expt translational-velocity 2) *a1*) (* (expt rotational-velocity 2) *a2*)))))
	 (rv (+ rotational-velocity (sample-normal (+ (* (expt translational-velocity 2) *a3*) (* (expt rotational-velocity 2) *a4*)))))
	 (circularity-error (sample-normal (+ (* (expt translational-velocity 2) *a1*) (* (expt rotational-velocity 2) *a2*))))
	 (newx (+ (- (car *pose*) (* (/ tv rv) (sin (caddr *pose*))))(* (/ tv rv) (sin (+ (caddr *pose*) (* 1 rv)))))); * 1 is for delta-t    
	 (newy (- (+ (cadr *pose*) (* (/ tv rv) (cos (caddr *pose*))))(* (/ tv rv) (cos (+ (caddr *pose*) (* 1 rv)))))); * 1 is for delta-t
	 (newtheta (mod (+ (caddr *pose*) (* rv 1) (* circularity-error 1))(* 2 pi)))); * 1 is for delta-t
    (setf *pose* (list newx newy newtheta))))

(defun sample-normal (standard-variability)
  (if (= standard-variability 0.0)
      0
    (let ((total 0.0)(variance (sqrt standard-variability)))
      (dotimes (x 12)
	(setf total (+ total (- (random (* 2 variance)) variance))))
      (/ total 2))))
      

(defun demo1 (&aux command)
  (loop while (not (eq command 'quit)) do
	(display-world)
	(setf command (read))
	(if (listp command)
	    (move (car command)(cadr command)))))

(init)
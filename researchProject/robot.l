;;;;Robot.l
;;;;Feb 2015
;;;;Ace DeSiena
;;;;An ai project for CSC466, AI2 at Oswego. This project will implement a simulated robot that uses simultaneous localization and mapping algorithms.

(defun init ()
  (setf *mapsize* 10)
  (setf *makefigures* t)
  (setf *pose* '(0.0 0.0 0.0)) ;Pose is defined as [x y theta]
  (setf *walls* '(
		  ((-3 3)(3 3))
		  ((-3 3)(-3 -3))
		  ((3 3)(3 -3))
		  ((-1.0 1.0)(-1.0 -1.0))
		  ((-1.0 -1.0)(1.0 -1.0))
		  ((1.0 -1.0)(1.0 1.0))))
  (setf *beamcount* 16)
  (setf *beamrange* 10)
  (setf *time* 0)
  (setf *a1* 0.0);translational movement error from translation
  (setf *a2* 0.0);translational movement error from rotation
  (setf *a3* 0.0);rotational movement error from translation
  (setf *a4* 0.0);rotational movement error from rotation
  (setf *a5* 0.0);circularity movement error from translation 
  (setf *a6* 0.0);circularity movement error from rotation
  (setf *beams* ())
  (let ((i 0))
    (dotimes (x *beamcount*)
      (setf *beams* (cons (+ i (/ (* 2 pi) *beamcount*))  *beams*))
      (setf i (car *beams*))))
  (setf *sense* (sense))
  (if *makefigures* (pyplot '(0 0))))

(defun display-world ()
  (format t "Robot Position: ~3$, ~3$, ~3$~%" (car *pose*) (cadr *pose*) (caddr *pose*))
  (format t "         Walls: ~A~%" (car *walls*))
  (dolist (wall (cdr *walls*))
    (format t "              : ~A~%" wall))
  (terpri))

(defun pyplot (title)
  (with-open-file (str (format () "plots/time~A.py" *time*)
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create)
    (format str "import matplotlib.pyplot as plt~%plt.ylim(~A, ~A)~%plt.xlim(~A, ~A)~%plt.gca().set_aspect('equal', adjustable='box')~%" (- 0 *mapsize*) *mapsize* (- 0 *mapsize*) *mapsize*)
    ;(dolist (wall *walls*)
    ;  (format str "plt.plot([~6$,~6$], [~6$,~6$], 'k-', linewidth=1)~%" (caar wall) (caadr wall) (cadar wall) (cadadr wall)))
    (dotimes (n *beamcount*)
      (format str "plt.plot([~6$,~6$], [~6$,~6$], 'r-', linewidth=.4)~%"
	      (car *pose*)
	      (+ (car *pose*) (* (cos (+ (caddr *pose*) (nth n *beams*)))(nth n *sense*)))
	      (cadr *pose*)
	      (+ (cadr *pose*) (* (sin (+ (caddr *pose*) (nth n *beams*)))(nth n *sense*)))))
    (format str "plt.suptitle('time = ~A, translation = ~A, rotation = ~A', fontsize=12)~%" *time* (car title) (cadr title))
    (format str "plt.arrow(~6$, ~6$, ~6$, ~6$, ec=\"k\", fc=\"k\", zorder=4 ,head_width=0.4, head_length=0.6)~%" 
	    (- (car *pose*) (* (cos (caddr *pose*)) .3)) 
	    (- (cadr *pose*) (* (sin (caddr *pose*)) .25))
	    (* (cos (caddr *pose*)) .0001)
	    (* (sin (caddr *pose*)) .0001))
    (format str "plt.savefig('nowalltime~A.png', bbox_inches='tight')~%plt.clf()~%" *time*)))
		    

(defun sense ()
  (let ((senseinput ()))
    (dolist (beam *beams*)
      (let ((closest *beamrange*))
	(dolist (wall *walls*)
	  (let ((distance (intercept-distance beam wall)))
	    (if distance 
		(if (< distance  closest)
		    (setf closest distance)))))
	(setf senseinput (cons closest senseinput))))
    (reverse senseinput)))

(defun intercept-distance (beam wall)
  (let* ((p0x (car *pose*))
	 (p0y (cadr *pose*))
	 (p1x (+ (car *pose*)(* (cos (+ beam (caddr *pose*))) *beamrange*))) 
	 (p1y (+ (cadr *pose*)(* (sin (+ beam (caddr *pose*))) *beamrange*)))
	 (p2x (caar wall))
	 (p2y (cadar wall))
	 (p3x (caadr wall))
	 (p3y (cadadr wall))
	 (s1x (- p1x p0x))
	 (s1y (- p1y p0y))
	 (s2x (- p3x p2x))
	 (s2y (- p3y p2y))
	 (s (/ (+ (* (- 0 s1y) (- p0x p2x)) (* s1x (- p0y p2y)))(+ (* (- 0 s2x) s1y) (* s1x s2y)))) 
	 (u (/ (- (* s2x (- p0y p2y)) (* s2y (- p0x p2x)))(+ (* (- 0 s2x) s1y) (* s1x s2y)))))
    (if (and (>= 1 s) (<= 0 s) (>= 1 u) (<= 0 u))
      (let ((ix (+ p0x (* u s1x)))
	    (iy (+ p0y (* u s1y))))
	(distance-to-robot ix iy))
      ())))

(defun distance-to-robot (x y)
  (sqrt (+ (expt(- x (car *pose*)) 2) (expt (- y (cadr *pose*)) 2))))

(defun move (translational-velocity rotational-velocity &aux newx newy)
  (setf *time* (+ 1 *time*))
  (let* ((tv (+ translational-velocity (sample-normal (+ (* (expt translational-velocity 2) *a1*) (* (expt rotational-velocity 2) *a2*)))))
	 (rv (+ rotational-velocity (sample-normal (+ (* (expt translational-velocity 2) *a3*) (* (expt rotational-velocity 2) *a4*)))))
	 (circularity-error (sample-normal (+ (* (expt translational-velocity 2) *a1*) (* (expt rotational-velocity 2) *a2*)))))
	 (if (= rv 0)
	     (progn
	       (setf newx (+ (car *pose*) (* tv (cos (caddr *pose*)))))  
	       (setf newy (+ (cadr *pose*) (* tv (sin (caddr *pose*))))))
	   (progn
	     (setf newx (+ (- (car *pose*) (* (/ tv rv) (sin (caddr *pose*))))(* (/ tv rv) (sin (+ (caddr *pose*) (* 1 rv)))))); * 1 is for delta-t    
	     (setf newy (- (+ (cadr *pose*) (* (/ tv rv) (cos (caddr *pose*))))(* (/ tv rv) (cos (+ (caddr *pose*) (* 1 rv)))))))); * 1 is for delta-t
	 (setf newtheta (mod (+ (caddr *pose*) (* rv 1) (* circularity-error 1))(* 2 pi)))); * 1 is for delta-t
    (setf *pose* (list newx newy newtheta))
    (setf *sense* (sense))
    (if *makefigures* (pyplot (list translational-velocity rotational-velocity))))

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
;;;;Robot.l
;;;;Feb 2015
;;;;Ace DeSiena
;;;;An ai project for CSC466, AI2 at Oswego. This project will implement a simulated robot that uses simultaneous localization and mapping algorithms.

(defun init ()
  (setf *position* '(0.0 0.0))
  (setf *walls* '(((3.0 3.0)(-3.0 3.0))((0.0 3.0)(0.0 5.0))))
  (repl))

(defun display-world ()
  (format t "Robot Position: ~A~%" *position*)
  (format t "         Walls: ~A~%" (car *walls*))
  (dolist (wall (cdr *walls*))
    (format t "              : ~A~%" wall))
  (terpri))

(defun movetimes (direction-distance)
  (dotimes (d (cadr direction-distance))
    (move (car direction-distance))))

(defun move (direction)
  (cond
   ((eq direction 'n)
    (setf (car *position*)(+ 1 (car *position*))))
   ((eq direction 's)
    (setf (car *position*)(- (car *position*) 1)))
   ((eq direction 'w)
    (setf (cadr *position*)(- (cadr *position*) 1)))
   ((eq direction 'e)
    (setf (cadr *position*)(+ 1 (cadr *position*))))))

(defun repl (&aux command)
  (loop while (not (eq command 'quit)) do
	(display-world)
	(setf command (read))
	(if (listp command)
	    (movetimes command)
	  (move command))))


(init)
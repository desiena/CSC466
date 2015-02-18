;;;Tic Tac Toe
;;;Ace DeSiena
;;;January-Febuary 2015
;;;CSC466

(defmethod select ((list list))
  (nth (random (length list)) list))

(defmethod snoc ((item symbol)( list list )) 
  (if (null list)
      (list item)
    (cons (car list ) (snoc item (cdr list)))))

(defmethod visualize ((game list) &aux grid turnlist)
    (setf grid '(nw n ne w c e sw s se))
    (setf turnlist '(X1 O2 X3 O4 X5 O6 X7 O8 X9))
    (dotimes (x (length game))
      (setf grid (replacenth (position (nth x game) grid) grid (nth x turnlist))))
    (format t "~a ~a ~a ~%~a ~a ~a~%~a ~a ~a~%" 
	    (nth 0 grid)
	    (nth 1 grid)
	    (nth 2 grid)
	    (nth 3 grid)
	    (nth 4 grid)
	    (nth 5 grid)
	    (nth 6 grid)
	    (nth 7 grid)	   
	    (nth 8 grid)))

(defmethod replacenth ((index integer)(old list)(item symbol))
  (if (= index 0)
      (cons item (cdr old))
    (cons (car old) (replacenth (- index 1) (cdr old) item)))) 

(defmethod play (&aux play avail move)
  (setf play ())
  (setf avail '(nw n ne w c e sw s se))
  (dolist (player '(x o x o x o x o x))
    (cond
     ((eq player 'x)
      (setf move (select avail))
      (setf avail (remove move avail))
      (setf play (snoc move play)))
     ((eq player 'o)
      (setf move (select avail))
      (setf avail (remove move avail))
      (setf play (snoc move play)))))
  play)

(defmethod analyze ((game list) &aux outcome)
  (setf outcome 'd)
  (dotimes (x 7)
    (let ((temp (analyzePart game)))
      (cond 
       ((eq temp 'w) (setf outcome 'w))
       ((eq temp 'L) (setf outcome 'l))))
    (setf game (butlast game)))
  outcome)
    

;;analyzepart takes a game of any length and determines if a win or loss for x has occured.
;;precondition: all previous subgames have been analyzed. 
(defmethod analyzePart((game list)&aux xs os)
  (setf xs ())
  (setf os ())
  (dotimes (x (length game))
    (if (eq (mod x 2) 0)
	(setf xs (snoc (nth x game) xs))
      (setf os (snoc (nth x game) os))))
  (cond
   ((win xs)
    'w)
   ((win os)
    'l)
   (t ())))

(defmethod win((moves list))
  (cond 
   ;;horizontal
   ((and (member 'n moves) (member 'ne moves) (member 'nw moves)) t)
   ((and (member 'c moves) (member 'e moves) (member 'w moves)) t)
   ((and (member 's moves) (member 'se moves) (member 'sw moves)) t)  

   ;;vertical
   ((and (member 'sw moves) (member 'w moves) (member 'nw moves)) t)
   ((and (member 'c moves) (member 's moves) (member 'n moves)) t)
   ((and (member 'e moves) (member 'se moves) (member 'ne moves)) t)

   ;;diagonal
   ((and (member 'se moves) (member 'c moves) (member 'nw moves)) t)
   ((and (member 'sw moves) (member 'c moves) (member 'ne moves)) t)
   (t ())))

;;this method simulates one play, visualizes it, and analyzes it
(defmethod demo-va (&aux p)
  (setf p (play))
  (format t "~a~%" p)
  (visualize p)
  (format t "~a~%" (analyze p))())

(defmethod stats((n number) (demo t) &aux w l d)
  (if demo (format t "Begin gathering statistics...~%"))
  (setf w 0 l 0 d 0)
  (dotimes (i n)
    (setf p (play))
    (if demo (format t "~A~%" p))
    (if demo (visualize p))
    (setf result (analyze p))
    (if demo (format t "~A~%" result))
    (cond 
     ((eq result 'w) (setf w (+ w 1)))
     ((eq result 'l) (setf l (+ l 1)))
     ((eq result 'd) (setf d (+ d 1)))))
  (setf results (mapcar #'probability (list w l d) (list n n n)))
  (if demo (format t "End gathering statistics~%"))
  (mapcar #'list '(w l d) results))

(defmethod probability ((special integer)(total integer))
  (/ (float special) (float total)))

;;;Task 3 modeling players, a human player and a random player. The human player class is really just an advice taker.

;;The root of the player hierarchy

(defclass player ()
  ((name :accessor player-name :initarg :name :initform 'emanon)))

;;a random machine player is a player.
(defclass random-machine-player (player )())

;;a human player is a player
(defclass human-player (player) ())

;;textual display of a random machine player
(defmethod display ((p random-machine-player))
  (format t "Random machine player ...~%")
  (format t "name = ~A~%" (player-name p))
  (format t "~%")())

;;textual display of a human player
(defmethod display ((p human-player))
  (format t "Human player ...~%")
  (format t "name = ~A~%" (player-name p))
  (format t "~%")())

;;a move making method for a random machine player
(defmethod make-move ((p random-machine-player)(report t) &aux move)
  (if report (format t "Begin random player move ...~%"))
  (setf move (select *avail*))
  (if report (format t "randomly selected ~A for my move~%" move))
  (setf *avail* (remove move *avail*))
  (if report (format t "End random player move ...~%"))
  move)

;;a move making method for a human player
(defmethod make-move ((p human-player)(report t) &aux move)
  (if report (format t "Begin human player move ...~%"))
  (format t "Please select a move from ~A~%" *avail*)
  (setf move (read))
  (cond 
   ((not (member move *avail*))
    (make-move p))
   (t
    (setf *avail* (remove move *avail*))
    move))
  (if report (format t "End human player move~%"))
  move)

;;a generic play method
(defmethod generic-play ((x player)(o player)(report t) &aux move)
  (setf *avail* '(nw n ne w c e sw s se))
  (setf *play-so-far* ())
  (dolist (player '(x o x o x o x o x))
    (if (or report (equal (type-of o) 'human-player-machine))
	(visualize *play-so-far*))
    (cond
     ((eq player 'x)
      (setf move (make-move x report)))
     ((eq player 'o)
      (setf move (make-move o report))))
    (setf *play-so-far* (snoc move *play-so-far*))
    (if (game-over-p *play-so-far*)(return nil)))
  *play-so-far*)

;;predicate to determine if the game is over or not
(defmethod game-over-p ((play list))
  (cond
   ((line-p (odd play)) 'w)
   ((line-p (even play)) 'l)
   ((= (length play) 9) 'd)
   (t nil)))

(defmethod odd ((l list))
  (cond
   ((null l) ())
   ((null (cdr l))(list (car l)))
   (t (cons (car l) (odd (cddr l))))))

(defmethod even ((l list))
  (cond
   ((null l)())
   ((null (cdr l))())
   (t (cons (cadr l)(even (cddr l))))))

(defmethod line-p ((l list))
  (cond
   ((<(length l) 3)
    ())
   ((= (length l) 3)
    (line (first l)(second l)(third l)))
   ((= (length l) 4)
    (or
     (line (first l)(second l)(third l))
     (line (first l)(second l)(fourth l))
     (line (first l)(third l)(fourth l))
     (line (second l)(third l)(fourth l))))
   ((= (length l) 5)
    (or
     (line (first l)(second l)(third l))
     (line (first l)(second l)(fourth l))
     (line (first l)(second l)(fifth l))
     (line (first l)(third l)(fourth l))
     (line (first l)(fourth l)(fifth l))
     (line (second l)(third l)(fourth l))
     (line (second l)(third l)(fifth l))
     (line (second l)(fourth l)(fifth l))
     (line (third l)(fourth l)(fifth l))))))

(defmethod line ((moveone symbol)(movetwo symbol)(movethree symbol))
  (setf moves (list moveone movetwo movethree))
  (cond 
   ;;horizontal
   ((and (member 'n moves) (member 'ne moves) (member 'nw moves)) t)
   ((and (member 'c moves) (member 'e moves) (member 'w moves)) t)
   ((and (member 's moves) (member 'se moves) (member 'sw moves)) t)  
   
   ;;vertical
   ((and (member 'sw moves) (member 'w moves) (member 'nw moves)) t)
   ((and (member 'c moves) (member 's moves) (member 'n moves)) t)
   ((and (member 'e moves) (member 'se moves) (member 'ne moves)) t)
   
   ;;diagonal
   ((and (member 'se moves) (member 'c moves) (member 'nw moves)) t)
   ((and (member 'sw moves) (member 'c moves) (member 'ne moves)) t)
   (t ())))

;;two random machine players play one game
(defmethod demo-random-random (&aux p x o)
  (setf x (make-instance 'random-machine-player))
  (setf o (make-instance 'random-machine-player))
  (setf p (generic-play x o t))
  (format t "~A~%" p)
  (visualize p)
  (format t "~A~%" (analyze p))
  ())

;;a random machine player plays one game against a human player
(defmethod demo-random-human (&aux p x o)
  (setf x (make-instance 'random-machine-player))
  (setf o (make-instance 'human-player))
  (setf p (generic-play x o t))
  (format t "~A~%" p)
  (visualize p)
  (format t "~A~%" (analyze p))
  ())
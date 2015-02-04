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
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;;;Tic Tac Toe One
;;;Ace DeSiena
;;;January 2015
;;;CSC466
(defmethod select ((list list))
  (nth (random (length list )) list))

(defmethod snoc ((item symbol)( list list )) 
  (if (null list)
      (list item)
    (cons (car list ) (snoc item (cdr list )))))

(defmethod play (&aux play avail move) ( setf play ())
  (setf avail ’(nw n ne w c e sw s se)) 
  (dolist (player ’(xoxoxoxox))
    (cond
     ((eq player 'x)
      (setf move (select avail))
      (setf play (snoc move play)))
     ((eq player 'o)
      (setf move (select avail))
      (setf avail (remove move avail))
      (setf play (snoc move play)))))
  play)
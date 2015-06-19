; Mancala.lsp
; Girish Chaudhari & Nishant Kumar 12/17/2013


(defvar total-score 0)

(defvar *game* nil )
(defstruct player 
  board-status
  player1-score
  comp-player2-score
  turn
  game-type 
)

(defun array-max (board scores-array)
"Array-max function returns index (move) with highest possible score"
  (let ((n (length scores-array))   
       (max (aref scores-array 0))
       (index 0))
    (dotimes (k n k)
        (setf max (max (aref scores-array k) max)))
        (if (eq max 0)
             (setq index (next-best-random-move board))
          (progn (dotimes (k n index)
            (if(eq max (aref scores-array k))
              (setq index k) )))) index ))
              
              
(defun copy-array (array)
"Copy-array :function copy one array into other array"
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))
    
    
(defun copy-player (game)
" copy-player function copy game structure to temporary game structure in order to get best move"
(let* ((new-structure (make-player)))
(setf (player-board-status new-structure) (copy-array (player-board-status game))) 
            (setf (player-player1-score new-structure) (player-player1-score game))
            (setf (player-comp-player2-score new-structure) (player-comp-player2-score game))
            (setf (player-turn new-structure) (player-turn game))
            (setf (player-game-type new-structure) (player-game-type game))
            new-structure))
            

(defun initialize-game (n)
"initialize-game: function initialize game structure with default  required values."
  (setf *game* (make-player 
                :board-status (make-array 12 :initial-element n) 
                :player1-score 0
                :comp-player2-score 0
                :turn 1
                :game-type 1)
                )        
  )
  
 (defun print-board (game) 
 "print-board: function print manacala game board"
   (let ((board-status (player-board-status game))
               (player1-score (player-player1-score game))
               (comp-player2-score (player-comp-player2-score game))
             )
                               (terpri)
                               (terpri)
                                        (princ "----------***MANCALA GAME***----------")
                                (terpri)(princ "_______________________________________")
                               (terpri)(terpri)                                 
               
               (princ "PLAYER1 : " )
                        
                     (dotimes (j (/ (length board-status) 2))
                     (princ (aref  board-status j))
                     (princ " | ")
                  )
                    
                    (terpri)
                    (terpri)
          
                (princ " P1-SCORE :") (princ player1-score) 
               (princ "   ")
               
               (if (eq 1 (player-game-type game))
               
               (progn (princ " CMP-SCORE :")(princ  comp-player2-score)(terpri)(terpri)          
               (princ "COMPUTER: " ))
                      
               (progn (princ " P2-SCORE :")(princ  comp-player2-score)(terpri)(terpri)          
               (princ "PLAYER2 : " )))
                      
                          
          (loop for k from (- (length board-status) 1) downto (/ (length board-status) 2)
                     do (princ (aref  board-status k)) (princ " | "))

                    (terpri)
                    (princ "_______________________________________")
                    (terpri)
                    ))
                    
(defun start-game ()
 "start-game: funtion start manacala game"
   (terpri)
   (princ "Enter Pits Element Count :") 
   (setq count (read))
   (initialize-game count)
   (terpri)
   (princ "ONE PLAYER : PRESS 1 " )
   (terpri)
   (princ "TWO PLAYER : PRESS 2"  )
   (terpri)
   (terpri)
   (princ "CHOOSE OPTION : ")
   (setf (player-game-type *game*) (read))
   (setf total-score (* 12 count))
   (print-board *game*)
   (play-game)
)
   
   
   
(defun win-status (game)
"win-status :function implement logic to get  winner  of mancala game  "
  (let  ((player1-score (player-player1-score game))
               (comp-player2-score (player-comp-player2-score game))
             )
          (cond (( eq player1-score comp-player2-score)(prog (terpri) (princ "GAME DRAW")))
((< player1-score comp-player2-score) (prog (terpri) (if(eq (player-game-type game) 1) (princ "COMPUTER WINS THE GAME")
         (princ "PLAYER 2 WINS THE GAME"))))
          (( > player1-score comp-player2-score) (prog (terpri) (princ "PLAYER 1 WINS THE GAME"))) 
    )
  )
)
  
  (defun game-over-check (game)
  "game-over-check:function check whether game is over or not after particular scenarios"
    (let ((board-status (player-board-status game))
          (goflag1 t)
          (goflag2 t))
          
                (dotimes (j (/ (length board-status) 2))
                         (if  (not (= 0 (aref board-status j))) (setq goflag1 nil)))
         
                (loop for k from (- (length board-status) 1) downto (/ (length board-status) 2)
                         do
                         (if  (not (= 0 (aref board-status k))) (setq goflag2 nil)))
                
                 (if ( or ( > (player-player1-score game) ( / total-score  2)) 
                             ( > (player-comp-player2-score game) ( / total-score 2) ))
                             (setq goflag1  t))
         
                 (if ( or  goflag1 goflag2 )
                  T)
                 
    )
  ) 
  
  
(defun decf-board(board-status position)
  (setf (aref board-status position) (+ (aref board-status position) 1)))


(defun player-move(game pit-position)
  (let((board-status (player-board-status game))
       (position pit-position)
       (p 12))
    (setf move-count (aref board-status pit-position))
    (setf (aref board-status pit-position) 0)
    (dotimes (j move-count)
      (if (>= (- pit-position 1) 0)
          (decf-board board-status (decf pit-position))
        (progn(setf pit-position 11)
        (decf-board board-status pit-position))))
        (capture-stones game pit-position)
      ))
    
    
    (defun player-terms (game)
    "Player-terms: function  is to play moves based on moves selected by player or computer"
      (let ((pit-position 0))
      (terpri)
      
      (if(eq (player-turn game) 1)
      
      (progn (princ "Move should be inbetween (1--->6)")(terpri)
      (princ "Player 1 Enter Your Move: ") 
      (setq pit-position (- (read) 1))
      (player-move game pit-position) (setf (player-turn game) 2) (print-board game ))
      
      (if (eq (player-game-type game) 2)
      (progn (princ "Move should be inbetween 6<---1")(terpri) 
      (princ "Player 2 Enter Your Move: ") 
      (setq pit-position (+ (read) 5))
      (player-move game pit-position) (setf (player-turn game) 1) (print-board game ))
      
      (progn 
      (setq pit-position (comp-best-move game 2))
      (sleep 2)(princ "AI-COMPUTER Played its Move as : ") (princ (- pit-position 5))
      (player-move game pit-position) (setf (player-turn game) 1) (print-board game )))
      
      )))
      
      
     


(defun comp-best-move (game player-turn)
"This function returns best move with highest possible score"
  (let((board-status (player-board-status game)))
   (setf scores (make-array (length board-status) :initial-element 0))    
   (if (eq player-turn 2)
       (dotimes (j (/ (length board-status) 2))
         (progn(setf *game1* (copy-player game))
           (player-move *game1* (+ j (/ (length board-status) 2)))
           (setf (aref scores (+ j (/ (length board-status) 2))) (- (player-comp-player2-score *game1*) 
           (player-comp-player2-score *game*)))
           ))) (array-max  board-status scores)))


         (defun next-best-random-move (board-status)
          "This function select random move when all possible moves leads to same score"
         (let ((move (random-in 6 11)))
            (if (isEmpty board-status move )
                  (next-best-random-move board-status)
                   move )))

     (defun isEmpty (board move)
     "This function checkes pits empty condition in order to select no empty pit for move"
        (if ( eq 0 (aref board move )) 
             T))
             
    (defun random-in (lo hi)
     "Utility: answer a random integer in lo..hi, lo <= hi"
     (+ lo (random (+ 1 (- hi lo)))))

    
    (defun play-game ()
    "This function handles game play and over mechanism"
        (loop do 
             (if (game-over-check *game*)
                    (progn (terpri)(win-status *game*)(return T))
                    (player-terms *game*))))    


  (defun capture-stones(game end-position)
  "Capture-stones: function is to handle capture rule logic  for manacala "
    (let((turn (player-turn game))
         (board-status (player-board-status game))
         (player1-score (player-player1-score game))
         (comp-player2-score (player-comp-player2-score game)))
      (if (eq turn 1)
          (if (and (>= end-position (/ (length board-status) 2)) (<= end-position (- (length board-status) 1)))
              (if (and (<= (aref board-status end-position) 3) (> (aref board-status end-position) 1) )
                  (progn (setf (player-player1-score game) (+ (aref board-status end-position) player1-score))
                    (setf (aref board-status end-position) 0)(capture-stones game (+ end-position 1)))
                )
            )
        (if (and (>= end-position 0) (<= end-position (- (/ (length board-status) 2) 1)))
              (if (and (<= (aref board-status end-position) 3) (> (aref board-status end-position) 1) )
                  (progn (setf (player-comp-player2-score game) (+ (aref board-status end-position) comp-player2-score))
                    (setf (aref board-status end-position) 0) (capture-stones game (+ end-position 1)))
                )
          )
        )))


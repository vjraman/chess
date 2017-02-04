#lang racket
(require racket/gui/base)

(define *board* #F)

(define *guiboard* #f)

(define initChess
  (lambda()
    (set! *board* (vector 7 7 7 7 7 7 7 7 7 7;
                          7 7 7 7 7 7 7 7 7 7;
                          7 7 4 2 3 5 9 3 2 4;
                          7 7 1 1 1 1 1 0 1 1;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 0 0 0 0 0 1 0 0;
                          7 7 -1  -1 -1 -1 -1 -1 -1 -1;
                          7 7 -4 -2  -3 -5 -9 -3 -2 -4;
                          7 7 7 7 7 7 7 7 7 7;
                          7 7 7 7 7 7 7 7 7 7))))

(define materialstrength
  (lambda(white? board)
    (do ((val 0)(p 22 (+ 1 p))) ((> p 99) (abs val))
      (if(and(not (equal? (vector-ref board p) 7)) (> (* (vector-ref board p) white?) 0)) (set! val (+ val (vector-ref board p)))'()))))
      

(define printPiece
  (lambda(piece blackSquare?)
    (if (zero? piece)
        (if blackSquare? (display " X ") (display " . "))
        (begin
          (if(< piece 0) (display " B") (display " W"))
          (display (list-ref '("P" "N" "B" "R" "Q" "" "" "" "K") (- (abs piece) 1)))))))

(define printChessBoard  
  (lambda()
    (printBoard printPiece *board*)))

(define printComputerBoard
  (lambda()
    (printBoard printValue *computer-square-control*)))

(define printHumanBoard
  (lambda()
    (printBoard printValue *human-square-control*)))

(define printValue
  (lambda(posval blackSquare?)
    (display posval) (display "  ")))
    
  
(define printBoard
  (lambda(proc thisboard)
    (for-each
       (lambda(sn) (begin 
                     (do ((pos sn (+ 1 pos))) ((equal? pos (+ sn 8))) 
                     (proc (vector-ref thisboard pos) (even? (+ (remainder pos 10) (quotient pos 10))))); remainder, quotient to determine black square?
                     (newline)))
       '(92 82 72 62 52 42 32 22))))


(define PAWN 1)
(define KNIGHT 2)
(define BISHOP 3)
(define ROOK 4)
(define QUEEN 5)
(define KING 9)


(define nzero?
  (lambda(x)
    (not (zero? x))))

(define ismember?
  (lambda(m l)
    (if (empty? l) #f
        (or (equal? (car l) m) (ismember? m (cdr l))))))



(define insertmarks
  (lambda(squares piece)
    (if(empty? squares) '()
       (begin
         (vector-set! *board* (car squares) piece)
         (insertmarks (cdr squares) piece
                      )))))

(define whereis
  (lambda(piece)
    (do ((i 22 (+ 1 i))) ((equal? i 120)) 
      ;(display (vector-ref *board* i)) (display "@") (display i) (newline)
       (if (equal? (vector-ref *board* i) piece) (begin  (display i) (newline)) (void)))))

(define *computer-square-control* (make-vector 120 0))
(define *human-square-control* (make-vector 120 0))


(define moves4P
  (lambda(board stsqr)
     (let* ( 
           [piece (vector-ref board stsqr)]
           [isblk (equal? piece -1)]
           [Pstartsqrs (if isblk '(82 83 84 85 86 87 88 89) '(32 33 34 35 36 37 38 39)) ]
           [Pmovelist '()]
         )
         (begin
            (if(and (equal? (vector-ref board (+ stsqr (* piece 10))) 0)) (set! Pmovelist (cons (list (list stsqr (+ stsqr (* piece 10)))) Pmovelist)) '())
            (if(and (ismember? stsqr Pstartsqrs) (equal? (vector-ref board (+ stsqr (* piece 10))) 0) (equal? (vector-ref board (+ stsqr (* piece 20))) 0)) (set! Pmovelist (cons (list (list stsqr (+ stsqr (* piece 20)))) Pmovelist)) '())
            (if(and (not (equal? (vector-ref board (+ stsqr (* piece 11))) 7)) (< (* piece (vector-ref board (+ stsqr (* piece 11)))) 0)) (set! Pmovelist (cons (list (list stsqr (+ stsqr (* piece 11)))) Pmovelist))'())
            (if(and (not (equal? (vector-ref board (+ stsqr (* piece 9))) 7)) (< (* piece (vector-ref board (+ stsqr (* piece 9)))) 0)) (set! Pmovelist (cons (list (list stsqr (+ stsqr (* piece 9)))) Pmovelist)) '()) Pmovelist))))
                  

(define moves4Longrange
  (lambda(board stsqr goodsqr inc)
    (cond((equal? (vector-ref board (+ goodsqr inc)) 0)
           (cons (list (list stsqr (+ goodsqr inc))) (moves4Longrange board stsqr (+ goodsqr inc) inc)))
         ((and (not (equal? (vector-ref board (+ goodsqr inc)) 7)) (< (* (vector-ref board (+ goodsqr inc)) (vector-ref board stsqr)) 0))
           (list (list (list stsqr (+ goodsqr inc)))))
         (else '()))))


(define istarget?
  (lambda(sqr controlmoves)
    (do ((cm controlmoves (cdr cm))(yes #f)) ((or (null? cm) yes) yes)
      (let*((controlledsqr  (cadaar cm)))
        (set! yes (equal? sqr controlledsqr)) ))))

(define moves4Shortrange
   (lambda(board stsqr goodsqr inc)
     (let* ((piece (vector-ref board stsqr))
            (white? (/ piece  (abs piece)))
            (candidatesqr (+ goodsqr inc)) 
            (candidatemove (list stsqr candidatesqr)) 
            (sqrempty? (equal? (vector-ref board candidatesqr) 0))
            (sqroffboard? (equal? (vector-ref board candidatesqr) 7))
            (sqrattacked? (istarget? candidatesqr (allcontrolmoves board (* -1 white?))))
            (kingattacked? (and sqrattacked? (equal? (abs piece) KING))))
       (cond ((or kingattacked? sqroffboard?) '())
             (sqrempty? (list (list candidatemove)))
             ((< (* (vector-ref board candidatesqr) piece) 0)  (list(list candidatemove)))
             (else '()) ))))

(define controlMoves4Shortrange
   (lambda(board stsqr goodsqr inc)
     (let* ((piece (vector-ref board stsqr))
            (candidatesqr (+ goodsqr inc)) 
            (candidatemove (list stsqr candidatesqr)) 
            (sqrempty? (equal? (vector-ref board candidatesqr) 0))
            (sqroffboard? (equal? (vector-ref board candidatesqr) 7)))
       (cond (sqrempty? (list (list candidatemove)))
             ((< (* (vector-ref board candidatesqr) piece) 0)  (list(list candidatemove)))
             (else '()) ))))



(define allmoves  ; add king not in check limit and castling
  (lambda(board white)
    (do  ( (movelist '()) (sx '(22 32 42 52 62 72 82 92) (cdr sx))) ((null? sx) movelist)
        (do ( (stpos (car sx)) (pos (car sx) (+ 1 pos))) ( (> pos (+ stpos  7)))
          (let ( (piece (vector-ref board pos)))
            (if(> (* piece white) 0)
               (begin
                 (if(equal? (abs piece) QUEEN) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(1 -1 10 -10 -9  -11 9 11))) movelist)) '())
                 (if(equal? (abs piece) BISHOP) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(-9 -11 9 11))) movelist)) '()) 
                 (if(equal? (abs piece) ROOK) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(1 -1 10 -10))) movelist)) '())
                 (if(equal? (abs piece) PAWN) (set! movelist (append (moves4P board pos) movelist))'())
                 (if(equal? (abs piece) KNIGHT) (set! movelist (append (merge (map (lambda(jump)(moves4Shortrange board pos pos jump)) '(8 -8 12 -12 19 -19 21 -21))) movelist)) '())
                 (if(equal? (abs piece) KING) (set! movelist (append (merge (map (lambda(jump)(moves4Shortrange board pos pos jump)) '(1 -1 10 -10 9 -9 11 -11))) movelist)) '()))
               '())
               
            )))))


(define allcontrolmoves
  (lambda(board white)
    (do  ( (movelist '()) (sx '(22 32 42 52 62 72 82 92) (cdr sx))) ((null? sx) movelist)
        (do ( (stpos (car sx)) (pos (car sx) (+ 1 pos))) ( (> pos (+ stpos  7)))
          (let ( (piece (vector-ref board pos)))
            (if(> (* piece white) 0)
               (begin
                 (if(equal? (abs piece) QUEEN) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(1 -1 10 -10 -9  -11 9 11))) movelist)) '())
                 (if(equal? (abs piece) BISHOP) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(-9 -11 9 11))) movelist)) '()) 
                 (if(equal? (abs piece) ROOK) (set! movelist (append (merge (map (lambda(jump)(moves4Longrange board pos pos jump)) '(1 -1 10 -10))) movelist)) '())
                 (if(equal? (abs piece) PAWN) (set! movelist (append (ControlMoves4P board pos) movelist))'())
                 (if(equal? (abs piece) KNIGHT) (set! movelist (append (merge (map (lambda(jump)(controlMoves4Shortrange board pos pos jump)) '(8 -8 12 -12 19 -19 21 -21))) movelist)) '())
                 (if(equal? (abs piece) KING) (set! movelist (append (merge (map (lambda(jump)(controlMoves4Shortrange board pos pos jump)) '(1 -1 10 -10 9 -9 11 -11))) movelist)) '()))
               '())
               
            )))))

(define controlset 
  (lambda (moveset controlboard)
    (do((ml moveset (cdr ml))) ((null? ml))
      (let* ((ctrlmove (caar ml)) (controlledsqr (cadr ctrlmove)))
        (vector-set! controlboard controlledsqr (+ 1 (vector-ref controlboard controlledsqr)))))))



(define merge
  (lambda(l)
    (if(null? l) '() (append (car l) (merge (cdr l))))))
                    
            

(define ControlMoves4P
  (lambda(board stsqr)
     (let* ( 
           [piece (vector-ref board stsqr)]
           [isblk (equal? piece -1)]
           [Pmovelist '()]
         )
         (begin
            (if(and (not (equal? (vector-ref board (+ stsqr (* piece 11))) 7)) (<= (* piece (vector-ref board (+ stsqr (* piece 11)))) 0)) (set! Pmovelist (cons (list (list stsqr (+ stsqr (* piece 11)))) Pmovelist))'())
            (if(and (not (equal? (vector-ref board (+ stsqr (* piece 9))) 7)) (<= (* piece (vector-ref board (+ stsqr (* piece 9)))) 0)) (set! Pmovelist (cons  (list (list stsqr (+ stsqr (* piece 9)))) Pmovelist)) '())
            Pmovelist))))
    

(initChess)

(define getInputMove
  (lambda(response)
    (let ( (fromColLetter (string (string-ref response 0))) (toColLetter (string (string-ref response 3))) (fromRowNumber (string->number (string (string-ref response 1)))) (toRowNumber (string->number (string (string-ref response 4))))
           (assocNumbers '( ("a" 2) ("b" 3) ("c" 4) ("d" 5) ("e" 6) ("f" 7) ("g" 8) ("h" 9))))
       (list(list (+ (* 10 (+ 1 fromRowNumber)) (cadr (assoc fromColLetter assocNumbers))) (+ (* 10 (+ 1 toRowNumber)) (cadr (assoc toColLetter assocNumbers))))))))  

(define applymove
  (lambda(m wb board chgboard)
    ; m is a list ((22 26)) or ( (22 25) (24 23)) for castling
    (if(or (equal? 1 wb) (equal? -1 wb))
       (let ((newboard (vector-copy board)) (legalmoves (allmoves board wb)))
         (if(ismember? m legalmoves) ; make sure move is legal!!
           (do ( (compoundmove m (cdr compoundmove)) ) ((null? compoundmove) newboard)
             ;(print compoundmove)
             (let* ( [move (car compoundmove)] [fromSq (car move)] [toSqr (cadr move)] [piecetomove (vector-ref board fromSq)])
             ; (print move)
                 (vector-set! newboard fromSq 0)
                 (vector-set! newboard toSqr  piecetomove)
                 (when chgboard (vector-copy! board 0 newboard)) ))
           #f)) #f)))



;*******************************************************************************

(define whitegui '("" "\u2659" "\u2658" "\u2657" "\u2656" "\u2655" "" "" "" "\u2654"))
(define blackgui '("" "\u265F" "\u265E" "\u265D" "\u265C" "\u265B" "" "" "" "\u265A"))

(define frame (new frame% [label "Chess"] [min-width 300] [min-height 450]))

(send frame show #t)

(define rows '())

(define movefrom #f)
(define movefrombutton #f)
(define movefromwhite 0)
(define moveto #f)
(define movetobutton #f)


(define chessqr% 
  (class button%
    (init sqr)
    (define sqr-num sqr)
    (super-new)
    
    (define/public (get-sqr) sqr-num)
    (define/public (set-sqr sqr) (set! sqr-num sqr)) ))


(define  Scatterbrain 
  (lambda(white? board)
    (let* ( (legalmoves (allmoves board white?))(index (random (length legalmoves))))
      (list-ref legalmoves index))))

(define OptimizeCapture
  (lambda (white? board move); eval strength of opponent after this move
    (let*((newboard (applymove move white? board #f)) (val (materialstrength (* -1 white?) newboard)))
      (begin
        (printChessBoard) 
        (printBoard printPiece newboard)
        ;(read)
        (display "OptimizeCapture:") (display move) (display val) val
      ))))

;( OptimizeCapture white? board (list(car x)))

(define Rankbrain
  (lambda(white? board)
    (let* ( (legalmoves (allmoves board white?))     
            (rankedmoves (sort legalmoves #:key (lambda(x) ( OptimizeCapture white? board x)) < ))
             ) (list-ref rankedmoves 0))))
    

  (define playComputerMove
  (lambda(white?)
    (let*( (bestmove (Rankbrain white? *board*))
           (move (car bestmove))
           (movefrom (car move)) 
           (moveto (cadr move))
           (movefrombutton (vector-ref *guiboard* movefrom))
           (movetobutton (vector-ref *guiboard* moveto))
           (capture? (< (* (vector-ref *board* movefrom) (vector-ref *board* moveto)) 0)))
         (if(applymove bestmove white? *board* #t)
                (begin
                  (display "COMPUTER LEGAL MOVE!!") (display move) (display "\n")
                  (send movefrombutton set-label (guipiecetxt movefrom (send movefrombutton get-label)))                  
                  (send movetobutton set-label (guipiecetxt moveto (send movetobutton get-label))) 
                  (if capture? (play-sound "C:/Users/vijay/Documents/racket_src/MOVEHIT1.wav" #t)(play-sound "C:/Users/vijay/Documents/racket_src/MOVE.wav" #t))
                 )
                '()))))
                  
          
       

(define movesfromgui
  (lambda(b e)
    (let* ((moveclk (send b get-sqr)) (piece (vector-ref *board* moveclk))  (movedpiecetxt ""))
    (begin
      (if (not movefrom) ; use this click to assign from square
         (begin
           (set! movefrom moveclk) 
           (if (nzero? (vector-ref *board* movefrom))
               (begin
                 (play-sound "C:/Users/vijay/Documents/racket_src/Tick.wav" #t)
                 (set! movefrombutton b)
                 (set! movefromwhite (if(nzero? piece)(/ piece (abs piece)) 0))
                 (display "Considering move from:")(display (number->string movefrom)) (display "---"))
               (begin
                (set! movefrom #f)
                (play-sound "C:/Users/vijay/Documents/racket_src/wiggle.wav" #t)))); unsuccessful in capturing a non empty movefrom!                   
         (begin ; movefrom has been captured, so get moveto!          
           (set! moveto moveclk)
           (display "move is:") (display (list(list movefrom moveto)))        
           (let ((capture? (< (* (vector-ref *board* movefrom) (vector-ref *board* moveto)) 0)))
             (if(applymove (list (list movefrom moveto)) movefromwhite *board* #t)
                (begin
                  (display "LEGAL MOVE!!\n")
                  (set! movetobutton b)
                  (send movefrombutton set-label (guipiecetxt movefrom (send movefrombutton get-label)))
                  (set! movedpiecetxt (guipiecetxt moveto (send b get-label)))
                  (send movetobutton set-label movedpiecetxt) 
                  (display "capture") (display capture?)
                  (if capture? (play-sound "C:/Users/vijay/Documents/racket_src/MOVEHIT1.wav" #t)(play-sound "C:/Users/vijay/Documents/racket_src/MOVE.wav" #t))
                  (send frame set-label (string-append (number->string movefrom) " to " (number->string moveto)))
                  (set! movefrom #f)
                  (playComputerMove (* -1 movefromwhite)))
            (begin
              (display "ILLEGAL MOVE\n")
              (play-sound "C:/Users/vijay/Documents/racket_src/airhorn.wav" #t)
               (set! movefrom #f))))))))))
           
      
(define guipiecetxt
  (lambda(sqr bm)
    (let*((piece (vector-ref *board* sqr))(dc (make-object bitmap-dc% bm))(sqrcolor "black"))
      (begin 
        (if(even? (+ (remainder sqr 10) (quotient sqr 10))) (set! sqrcolor "gray") (set! sqrcolor "white"))
        (send dc set-brush (make-object brush% sqrcolor 'solid))
        (send dc draw-rectangle 0 0 60 60)
        (send dc set-font (make-font #:size 40 #:family 'roman  #:weight 'bold))
        (send dc set-text-foreground "blue")
        (send dc draw-text (if(nzero? piece)(if(> piece 0)(list-ref whitegui piece) (list-ref blackgui (abs piece))) "") 5 1 )
        (send dc set-bitmap #f)bm))))
      
(define initChessGUI
  (lambda()
    (set! *guiboard* (vector 7 7 7 7 7 7 7 7 7 7;
                          7 7 7 7 7 7 7 7 7 7;
                          7 7 4 2 3 5 9 3 2 4;
                          7 7 1 1 1 1 1 0 1 1;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 0 0 0 0 0 1 0 0;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 0 0 0 0 0 0 0 0;
                          7 7 -1 -1 -1 -1 -1 -1 -1 -1;
                          7 7 -4 -2  -1 -5 -9 -3 -2 -4;
                          7 7 7 7 7 7 7 7 7 7;
                          7 7 7 7 7 7 7 7 7 7))
    (do ((i 9 (- i 1))) ((< i 2))
      (let ((p (new horizontal-panel% [parent frame])))
        (do ((j 2 (+ j 1))) ((> j 9))
            (let*(
                  (sq (+ (* 10 i) j))
                  (s (new chessqr% [sqr sq] [parent p] [label (guipiecetxt sq (make-object bitmap% 60 60))] [stretchable-width #t][stretchable-height #t] [callback movesfromgui]))
                  
                  )
                 
                  (vector-set! *guiboard* sq s)))))))
      
    

(initChessGUI)
;********************************************************************
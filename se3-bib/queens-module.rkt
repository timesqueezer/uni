#lang racket
#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(provide
 queen1 ; looks for a single sulution, without gui
 
 ; displays the result using the teachpack "show-queen"
 queens ; looks for all solutions, without gui
 queens-demo; graphical user interface calling the demos
 queen-coord ; data type for positions on the board
 queen-coord queen-coord-row queen-coord-col print-queen-coord
 queen-result queen-result-noMoves queen-result-positions
 successful?
 safe? check?
 testpos makeboard showboard) 

(require racket/trace
         htdp/show-queen
         racket/generator
         se3-bib/tools-module
         se3-bib/macos-module
         se3-bib/demo-gui-module)

;;; =====================================================
;;; 8-Damen
;;; =====================================================

(define thePause 0.01) ; wait between successive displays

(struct
  queen-coord 
  (row col)) 
; a structure with two fields
; constructor make-queen-coord
; accessors: queen-coord-row, queen-coord-col

(define (print-queen-coord pos)
  (display (list "row: " (queen-coord-row pos)
                 " col: " (queen-coord-col pos))))

(struct queen-result (noMoves positions)) 
; representation for backtracking results: a structure with two fields:
;    noMoves: The number of tries so far
;    the solution: a list of queen-positions (type queen-coord)
; constructor queen-result
; accessors: queen-result-noMoves, queen-result-positions

(define (successful? result)
  ; is the result a solution?
  (not (null? (queen-result-positions result))))

;;; =====================================================
;;; 8-Damen graphisch
;;; =====================================================

(define (makeboard size positions)
  ; create a matrix (a vector of vectors of columns)
  ; the cell is #t, if a queen is placed at (column,row),
  ; otherwise #f
  (define (emptyColumn) (make-vector size #f))
  
  (define (columnWithQueenAtPos pos) 
    ;pos: queen-coord
    (let ([theColumn  (emptyColumn)])
      (vector-set! 
       theColumn 
       (-(queen-coord-row pos) 1) ; the row
       #t)
      theColumn))
  
  (define (makeEmptyBoard)
    (make-vector size (emptyColumn)))
  
  (define (placeQueen q board)
    ; place a queen at queen-pos q 
    (vector-set!
     board ; the column vector
     (- (queen-coord-col q) 1) ; the row 
     (columnWithQueenAtPos q); here is a queen
     ))
  (let ([theBoard (makeEmptyBoard)])
    (map (curryr placeQueen theBoard) positions); place all the queens
    theBoard ;result: the board with all queens
    ))

(define (showboard positions size duration)
  ; show the current board
  ;    positions: a list of queen positions (type queen-coord)
  ;    size: the number of rows of the board
  ; uses teachpack "show-queens" duration: pause in ms
  (let* ([theBoardAsMatrix (makeboard size positions)]
         [theBoardAsList  
          (map vector->list 
               (vector->list theBoardAsMatrix))
          ])
    ; the format required by show-queen
    
    (show-queen theBoardAsList)
    (sleep duration)    
    ))

; an additional queen is safe at new-pos, if
(define (safe? positionsSofar newPos)
  ; positionsSofar: list of queen-coord
  ; newPos: queen-coord
  (andmap
   (curry (negate check?) newPos) ; no check
   positionsSofar))

(define (check? pos-1 pos-2)
  ; Bedroht eine Dame in Position pos-1 
  ; die Position pos-2?
  (let ([r1 (queen-coord-row pos-1)]
        [c1 (queen-coord-col pos-1)]
        [r2 (queen-coord-row pos-2)]
        [c2 (queen-coord-col pos-2)])
    (or (= c1 c2) ; gleiche Spalte 
        (= (+ r1 c1) (+ r2 c2)); Diagonale 1 
        (= (- r1 c1) (- r2 c2))))); Diagonale 2 

(define  (try-queen size positions row column noMoves)
  ; find a single solution
  (let* ((positionToTry (queen-coord row column))
         (npositions (cons positionToTry positions)))
    (cond 
      [(> row size) 
       (showboard positions size thePause)
       (queen-result noMoves positions )] ; all queens successfully placed
      [(> column size)(queen-result noMoves '() )] ; failure, no safe position in this row found
      [(not (safe? positions positionToTry)) ; move queen to next column
       (begin
         (showboard npositions size thePause)
         (try-queen size positions row (+ 1 column) (+ 1 noMoves)))]
      [else ; the queen is safe, try to place a queen in the next row
       (begin
         (showboard npositions size thePause)
         (let ([result1 (try-queen 
                         size 
                         npositions
                         (+ 1 row)
                         1
                         (+ 1 noMoves)
                         )])
           (if (successful? result1) 
               result1 ; return the result
               ; backtrack, if not successful
               (try-queen size 
                          positions 
                          row 
                          (+ 1 column)
                          (+ (queen-result-noMoves result1) 
                             noMoves)))))])))

(define (queen1 size  pause)
  ; call try-queen and pretty-print the result
  ; size: width of the board
  ; pause: animation pause in seconds
  (set! thePause pause)
  
  (let ((theResult
         (try-queen size '() 1 1 0)))
    
    (display (list "Number of moves: " 
                   (queen-result-noMoves theResult)))
    (if (successful? theResult )
        (begin 
          (display "positions: ") 
          (map print-queen-coord
               (queen-result-positions theResult)))
        (display "  no solution found"))
    theResult))

(define 
  (try-all-queens size positions row column)
  ; find all solutions
  (let ((positionToTry (queen-coord row column)))
    (cond 
      [(not (safe? positions positionToTry)) '()] 
      [(= row size) ; all queens successfully placed
       (let ([theSolution 
              (cons positionToTry positions)])
         (showboard theSolution size thePause)
         (list theSolution))];return the solution
      [else 
       ; the queen is safe, try all positions next row 
       (apply append; combine all lists of results
              (build-list 
               size 
               (compose 
                (curry try-all-queens 
                       size 
                       (if (= row 0) '()
                           (cons  positionToTry positions))
                       (+ 1 row)) add1)))]))) 
; try on all columns 1..size

(define (queens size pause)
  ; call try-all-queens and count the number of solutions
  ; show the solutions using the show-queen teachpack
  ; size: width of the board
  ; pause: animation pause in seconds
  (set! thePause pause)
  (let ([solutions
         (try-all-queens 
          size '()  0 1)])
    (writeln "\nnumber of solutions: "
             (length solutions))
    solutions
    ))

; for testing
(define testpos 
  (list (queen-coord 1 1)
        (queen-coord 2 3)
        (queen-coord 3 2)))


;;; A graphical user interface, demos

(define (queens-demo1 numberQueens) 
  ; find one solution
  (let* ((theResult (queen1 numberQueens 
                            (/ 1.0 numberQueens numberQueens)))
         ; only short pause if numberQueens is high
         (moves (queen-result-noMoves theResult))
         (message     
          (string-append 
           (if (successful? theResult)         
               "success! " 
               "Sorry, no success, giving up")
           " after " (number->string moves) " tries")))
    (Fred message)
    ))

(define (queens-demo2 numberQueens) 
  ;find all solutions
  (let* ([solutions 
          (queens numberQueens (/ 0.1 numberQueens))]
         [message 
          (if (not (null? solutions)) 
              (string-append 
               "success! " 
               (number->string (length solutions))
               " solutions found") 
              "Sorry, no success, giving up")
          ])
    (Fred message)
    ))


; queens with generator  
(define 
  (generate-all-queens size)
  (generator
   ()
   (letrec 
       ([generate-all-queens 
         (lambda (positions row column)
           (let ((positionToTry (queen-coord row column)))
             (cond 
               [(not (safe? positions positionToTry)) '()] 
               [(= row size) ; all queens successfully placed
                (let ([theSolution 
                       (cons positionToTry  positions)])
                  (showboard theSolution size thePause)
                  (yield theSolution))];return the solution
               [else ; the queen is safe, try all positions next row
                (for ([col-pos (in-range 1 (+ 1 size ))])
                     (generate-all-queens 
                      (if (= row 0) '()
                          (cons  positionToTry positions))
                      (+ 1 row) col-pos) )])))]) ; try on all columns 1..size
     (generate-all-queens '() 0 1))))

(define *demos-available*
  #|
    all the available demos:
    format: 
          query string, 
          procedure to call,
  |#
  `(    ("Find first solution" ,queens-demo1)
        ("Find all solutions"  ,queens-demo2)))

(define *max-queens* 14)
(define *param-query-string* "How many queens?")

(define (queens-demo)
  (demo-with-numeric-params 
   *demos-available*
   *param-query-string*
   *max-queens*
   ))


(define next-queen 
  (generate-all-queens 8))

;(next-queen)

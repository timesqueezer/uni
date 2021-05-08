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

(require test-engine/racket-tests
         racket/trace
         (except-in 2htdp/universe space)
         2htdp/image
         se3-bib/tools-module
         se3-bib/examples/images-module)

; Basic utilities
(check-expect ;id
 (eq? 1 (id 1)) #t)

(check-expect ; last, length > 1
 (last '(2 1)) 1)

(check-expect ;last, length = 1
 (last '(1)) 1)

(check-expect ;random real
 (real? (random-real)) #t)

; eval
(define (my-eval expr)
  (apply (car expr) 
         (cdr expr)))

(define expr (list max 1 2 3 0))

(check-expect ; my-eval
 (my-eval expr) 3)

; high order functions


;apply
(define data '(2 4 7 3 5 1.)) 
(check-expect ;apply
 (apply max data)  7)

; sort
(check-expect ;sort
 (sort  '(3 5 5 4 6 2) <) '(2 3 4 5 5 6))

;map filter
(define (zwei-hoch n) (expt 2 n))

(check-expect ; map
 (map zwei-hoch '(0 1 2 3 4 5 6 7)) 
 '(1 2 4 8 16 32 64 128))

(check-expect ; mehr als eine Liste
 (map + '(  1   2   3   4) 
        '( 10  20  30  40) 
        '(100 200 300 400))
 '(111 222 333 444))

(define *computer-scientists*
  '( (Charles Babbage)
     (Alonzo Church)
     (Freiherr Gottfried Wilhelm von Leibniz)
     (Rear Admiral Grace Murray Hopper)
     (Lady Ada Countess of Lovelace)
     (John McCarthy)
     (John von Neumann)
     (Alan Turing)
     (Blaise Pascal)))

(define (last-name name)
  ;;; Select the last name of a name 
  ;;; represented as a list."
  (car (reverse name))) 

(define (first-name name)
  ;;; select the first name from a name 
  ;;; represented as a list.
  (car name))

(define *titles*
  '(Freiherr Rear Admiral Lady von Herr Frau 
             Mr. Mrs. Miss Sir Madam Dr. Prof. ))

(define (first-name2 name)
  ;; select the first name from a name 
  (if (member (car name) *titles*)
      (first-name2 (cdr name))
      (car name)))

(check-expect ;last-name
 (map 
 last-name 
 *computer-scientists*) 
 '(Babbage Church Leibniz Hopper 
   Lovelace McCarthy Neumann Turing Pascal))

(check-expect ; first-name2
 (map first-name2 *computer-scientists*) 
 '(Charles Alonzo Gottfried Grace
   Ada John John Alan Blaise))

(check-expect ; map list, zipping
(map list
      (map first-name2 *computer-scientists*)
      (map last-name *computer-scientists*))  
 '((Charles Babbage) 
   (Alonzo Church) 
   (Gottfried Leibniz) 
   (Grace Hopper) 
   (Ada Lovelace) 
   (John McCarthy) 
   (John Neumann) 
   (Alan Turing) 
   (Blaise Pascal)))

(check-expect ; assoc-list
 (map cons
      (map first-name2 *computer-scientists*)
      (map last-name *computer-scientists*)) 
 '((Charles . Babbage)
   (Alonzo . Church)
   (Gottfried . Leibniz) 
   (Grace . Hopper)
   (Ada . Lovelace) 
   (John . McCarthy) 
   (John . Neumann) 
   (Alan . Turing) 
   (Blaise . Pascal)))

(check-expect ;filter
 (filter odd? '(1 3 5 4 3 5 2 7)) 
 '(1 3 5 3 5 7))

(check-expect ;filter, empty list
 (filter odd? '()) '())

(check-expect ; filter pair?
 (filter pair? 
         '(auto (bus) (2 3) haus))
 '((bus) (2 3)))

(check-expect ;filter boolean?
 (filter boolean? 
         '( 1 2 haus #t pi #f)) '(#t #f))

(define (myfoldl f seed xs )
  ; falte die Liste xs unter Verwendung 
  ; der Funktion f und 
  ; des neutralen Elements seed.
  (cond [(null? xs) seed]
        [else
         (myfoldl f
                  (f (car xs) seed )
                  (cdr xs)
                  )]))

(define (myfoldr f seed xs)
  ; allgemein rekursive Reduktion
  (if (null? xs)
      seed
      (f (car xs) 
         (myfoldr  f seed (cdr xs)))))

(trace myfoldl myfoldr)

(check-expect ;foldl empty
 (foldl cons  '() '()) 
 '())

(check-expect ;foldl cons
 (foldl cons '() '(3 2 1) ) 
 '(1 2 3))

(check-expect ;length via reduce
 (foldl + 
        (map 
         (lambda (x) 1)
         '(1 2 3)) 0) 
 3)


; fold right
(check-expect ; commutative
 (foldr + 0 '(1 2 3 4) ) 10)

(check-expect ;nicht Kommutativ, cons
 (foldr cons '() '(4 3 2 1) )  
 '(4 3 2 1))

;;; iteration
(define powers-of-two 
  (iterate (lambda (x) (* 2 x))
           (lambda (x) (> x 64))
           1))

(check-expect powers-of-two '(1 2 4 8 16 32 64 128))

(define (naturals n)
  (iterate add1 
           (lambda (x) (>= x n))
           1))

(define (spaces n)
  (reduce 
   string-append
   (map (lambda (x) " ")
        (naturals n))
   ""))

(check-expect ;naturals
 (naturals 7) 
 '(1 2 3 4 5 6 7))

(check-expect ; spaces
 (spaces 6)
 "      ")

;; iter-until
(check-expect ;until Klingeling
  (klingeling 60)  
  (void))

#|
; a contract for sort

(define contListofNumber 
  (listof (flat-contract number?)))

(define contSorted<= 
  (flat-contract
   (lambda (xs)
     (apply <= xs))))

(define/contract 
  sort<
  (contListofNumber . -> . contSorted<= )
  (lambda (nums)
    (sort nums <)))

;  call back functions
; gui-demo
(define w 
    (create-window 
     (list 
      (list 
       (make-button 
        "QUIT" 
        (lambda (e) 
          (hide-window w)))))))
  ;A button appears on the screen. Click on the button and it will disappear. 
  (show-window w)
  
  (define text1
    (make-text "Please enter your name"))
  
  (define msg1
    (make-message 
     (string-append 
      "Hello, World" 
      (make-string 33 #\SPACE))))
  
  (define (respond e)
    (draw-message 
     msg1 
     (string-append "Hello, " 
                    (text-contents text1))))
  
  (define w 
    (create-window
     (list
      (list text1)
      (list msg1)
      (list (make-button "OKAY" respond)
            (make-button 
             "QUIT" 
             (lambda (e) (hide-window w)))))))
  
|#

;;; universe.ss
#| KATZEN

(define-struct cat-universe 
  (year num-cats)) 

(define *cat-canvas-w* 700)
; the width of the canvas
(define *cat-canvas-h* 500)
; height of canvas
(define kittens-per-year 4)
; four kittens per cat per year 

(define *maxnumcats* (* (floor (/ *cat-canvas-h* 
                                  (image-height *katze-winzig*)))
                        (floor (/ *cat-canvas-w* 
                                  (image-width *katze-winzig*)))))

(define (end-of-time mes)
  (display mes))

(define (draw-cats picture num-cats)
  ; draw-cats: Image natural -> Scene
  (let ((h (image-height picture))
        (w (image-width picture))
        (initial-canvas 
         (empty-scene   
          *cat-canvas-w*
          *cat-canvas-h*)))
    (letrec ((cats-at ; returns a new canvas
              (lambda (r c n canvas)
                (cond ((= n 0) canvas)
                      ( (> r *cat-canvas-h*); canvas full
                        (end-of-time "too many cats")
                        canvas)
                      ((> c *cat-canvas-w*); row full, start next row
                       (cats-at (+ r h) (/ w 2) n canvas))
                      (else  
                       (cats-at 
                        r (+ c w) (- n 1)
                        (place-image 
                         picture c r canvas)
                        ))))
              ))
      (cats-at
       (/ h 2) (/ w 2) num-cats 
       initial-canvas)
      ) ))

(define (show-cat-world world)
  ;show-cat-world: cat-universe --> Scene
  (display (list 
            "year: " 
            (cat-universe-year world)
            "number of cats: " 
            (cat-universe-num-cats world )
            ))
  (display "\n")
  (draw-cats *katze-winzig* 
             (cat-universe-num-cats world ))
  )

(define (last-cat-world? w)
  (> (cat-universe-num-cats w) *maxnumcats*
     ))

(define (next-generation world)
  ; next-generation: cat-universe -> cat-universe
  (let (( new-world
          (make-cat-universe 
           (add1 (cat-universe-year world))
           (* (add1 kittens-per-year) 
              (cat-universe-num-cats world)))))
    
    new-world))

(define (maikatzen-sim tick) 
  ; tick: time between clock ticks (seconds)
  (let ((first-generation 
         (make-cat-universe 1 1)))
    ; year 1, one cat)
    (big-bang 
     *cat-canvas-w*
     *cat-canvas-h*
     tick
     first-generation )
    
    (on-tick-event next-generation)
    (on-redraw show-cat-world)
    (stop-when last-cat-world?)
    )) 
|#

#| ANALOG-UHR

(define *clock-canvas-w* 
  (image-width *clock-face*))
; the with of the canvas
(define *clock-canvas-h* 
  (image-height *clock-face*))
; height of canvas

(define (next-clock-world world )
  ; next-clock-world: any --> date
  (seconds->date 
   (current-seconds)))

(define (place-hand frac-of-cycle len colo canvas)
  ; drwa a hand of the clock
  ; number: the fraction of the 12 hour cycle
  ; number: the length relative to the longest hand
  ; color: the color
  (let* ((y (* len 0.4 *clock-canvas-h*))
         (angRad (- (* frac-of-cycle 2 pi)))
         ;x'=x*cos(angle)-y*sin(angle)
         ;y'=y*cos(angle)+x*sin(angle)
         (xn (- (* y (sin angRad)))); rotated x at the origin
         (yn (* y (cos angRad)));rotated y at the origin
         (xm (/  *clock-canvas-w* 2))
         (ym (/  *clock-canvas-h* 2)))
    ; y-axis points down, not up!
    (add-line canvas xm ym (+ xn xm) (- ym yn ) colo)
    ))

(define (hour-hand hour minute canvas)
  ; real --> Scene
  (place-hand (/ (+ hour (/ minute 60))
                 12) 0.6 'blue canvas))

(define (minute-hand minutes seconds canvas)
  ; real --> Scene
  (place-hand (/ (+ minutes (/ seconds 60))
                 60) 1 'blue canvas))

(define (seconds-hand seconds canvas)
  ; real --> Scene
  (place-hand (/ seconds 60)
              1.0 'red canvas))

(define (draw-clockworld world)
  ; draw-clockworld: date --> Image
  (let ((h (image-height *clock-face*))
        (w (image-width *clock-face*))
        (canvas 
         (empty-scene   
          *clock-canvas-w*
          *clock-canvas-h*))
        (hour (date-hour world))
        (min (date-minute world))
        (sec (date-second world))
        (daylightSaving? (date-dst? world)))
    (display (list 
              hour ":" min ":" sec
              (if daylightSaving? "MESZ" "MEZ")))
    (display        "\n")
    
    (hour-hand 
     hour min
     (minute-hand
      min sec
      (seconds-hand 
       sec 
       (place-image
        *clock-face* 
        (/ *clock-canvas-w* 2)
        (-(/ *clock-canvas-h* 2) 2)
        canvas))))
    ))

(define *last-clock-world* (seconds->date 2000000000))
; return special date to signal end of time

(define (clock-stopped? world)
  (equal? world *last-clock-world*))

(define (handle-key world key)
  (if (equal? key #\q) 
      (begin (end-of-time "clock stopped")
             *last-clock-world*)
      world)
  ) 


(define (sim-clock tick)
  ; sim-cloc: positive-real --> true 
  ; tick: time between clock ticks (seconds)
  (let ((first-clock 
         (next-clock-world #t)))
    (display "Type 'q' to stop the clock.")
    (big-bang 
     *clock-canvas-w*
     *clock-canvas-h*
     tick
     first-clock )
    
    (on-tick-event next-clock-world)
    (on-redraw draw-clockworld)
    (on-key-event handle-key)
    (stop-when clock-stopped?)
    ))
|#

(require racket/generator)

(define mseq (fgenerator (curry + 1)  1))
(gen-iterate (curry + 1) (curryr > 4 ) 1)
(gen-iterate 
 (curry cons 1) 
 (compose (curryr > 3 )
          length)
 '())



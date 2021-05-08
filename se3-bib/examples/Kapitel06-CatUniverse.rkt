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

(require  (except-in 2htdp/universe space)
          2htdp/image
          htdp/gui
          test-engine/racket-tests 
          racket/trace
          se3-bib/tools-module
          se3-bib/examples/images-module)

(define-struct cat-universe 
  (year num-cats)) 

(define *cat-canvas-w* 700)
; the width of the canvas
(define *cat-canvas-h* 500)
; height of canvas
(define kittens-per-year 4)
; four kittens per cat per year 

(define *maxnumcats* 
  (* (floor (/ *cat-canvas-h* 
               (image-height *katze-winzig*)))
     (floor (/ *cat-canvas-w* 
               (image-width *katze-winzig*)))))

(define (draw-cats picture num-cats)
  ; draw-cats: Image natural -> Scene
  (let ([h (image-height picture)]
        [w (image-width picture)]
        [initial-canvas 
         (empty-scene   
          *cat-canvas-w*
          *cat-canvas-h*)])
    (letrec 
        ([cats-at ; returns a new canvas
          (lambda (r c n canvas); row column n-cats
            (cond [(= n 0) canvas]
                  [ (> r *cat-canvas-h*); canvas full
                    (display "too many cats")
                    canvas]
                  [(> c *cat-canvas-w*)
                   ; row full, start next row
                   (cats-at (+ r h) (/ w 2) n canvas)]
                  [else  
                   (cats-at 
                    r (+ c w) (- n 1)
                    (place-image 
                     picture c r canvas)
                    )]))
          ])
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
  (let ([ new-world
          (make-cat-universe 
           (add1 (cat-universe-year world))
           (* (add1 kittens-per-year) 
              (cat-universe-num-cats world)))])
    new-world))

(define (handle-key world key)
  ; Abbruch der Simulation mit "q"
  (if (or (string=? key "escape")
          (string=? key "q"))
      (begin (display "End of time\n")
             (stop-with world))
      world)) 

(define (maikatzen-sim tick) 
  ; tick: time between clock ticks (seconds)
    (big-bang 
     (make-cat-universe 1 1) ; year 1, one cat
     (on-tick next-generation tick)
     (to-draw show-cat-world
     *cat-canvas-w* *cat-canvas-h*)
     (stop-when last-cat-world?)
     (on-key handle-key)
     (name "Maikatzen")))
  


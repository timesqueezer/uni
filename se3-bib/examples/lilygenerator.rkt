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

(require 2htdp/image
         (except-in 2htdp/universe space)
         racket/generator
         se3-bib/tools-module
         (only-in htdp/image make-alpha-color))

(define *screensize* 400)

#|
(define *clear-background*
  (crop ; Rand abschneiden 
   *screensize* *screensize* *screensize* *screensize*
   (square (* 4.0 *screensize*) 'outline 'red)))
|#

(define *clear-background*
  (crop ; Rand abschneiden 
   1 1 *screensize* *screensize*
   (square (+ 2 *screensize*) 'outline 'red)))

(define (make-angle ang)
  ; ensure 0 <= angle < 360.0
  (cond [(>= ang 360.0) (make-angle (- ang 360.0))  ]
        [(< ang 0.0) (make-angle (+ ang 360.0)) ]
        [else ang]))

(define (shape-seq-generator 
         shape ; image to be rotated
         scale-fac ; scale factor between successiv images
         rotation-inc; increment in degrees
         )
  (letrec 
      ([mscene (empty-scene *screensize* *screensize*) ]
       [next-shape 
        (lambda (scene-n fac ang)
          (let ([scene-n+1 (place-image
                            (scale fac (rotate ang shape))
                            (/ *screensize* 2)
                            (/ *screensize* 2)
                            scene-n
                            )])
            (yield scene-n+1)
            (if (and (>= (image-width shape) 10)
                     (>= (image-height shape) 10))
                (next-shape 
                 scene-n+1
                 (* fac scale-fac)
                 (make-angle (+ ang rotation-inc)))
                shape)          
            ))])
    (infinite-generator 
     (next-shape 
      (empty-scene *screensize* *screensize*)
      1.0 0.0))))


(define *purple-triangles* 
  (shape-seq-generator 
   (overlay
    (triangle *screensize* 'outline 'purple)
    *clear-background*)
   ; image to be rotated
   0.98 ; scale factor between successiv images
   4.0; increment in degrees
   ))

(define *olive-ellipses* 
  (shape-seq-generator 
   (overlay/xy 
    *clear-background* 0 (/ *screensize* 3.0)
    (ellipse (/ *screensize* 1.1) 
             (/ *screensize* 2.0)'outline 'olivedrab))
   
   ; image to be rotated
   0.95 ; scale factor between successiv images
   3.0; increment in degrees
   ))

(define (triangle-animation)
  (animate (lambda (ignore) (*purple-triangles*) )))

(define (oval-animation)
  (animate (lambda (ignore) (*olive-ellipses* ) )))
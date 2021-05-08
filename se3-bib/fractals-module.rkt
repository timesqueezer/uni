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

(provide fractals-demo fractal 
         snowflake-line snowflake 
         snowflake-invers snowflake-both
         all-snowflakes
         quad-koch-line quadkoch 
         quadkoch-invers quadkoch-both
         quadkoch-all-iterations)

(require 2htdp/image
         2htdp/universe
         lang/posn
         se3-bib/macos-module
         se3-bib/demo-gui-module
         se3-bib/vector-graphics-module)

;;; --------------------------------------------------------
;;; Koch snow flake fractal
;;; --------------------------------------------------------


(define (fractal 
         start-segments ; the initial polygon:
         ; a list of points relative to the origin, sized between 0-1
         iterator ; the function to draw the segment
         ; (point-from point-to iter color) -> number-segments:integer
         color ; the drawing color
         iter) ; how many iterations
  ; returns the number of iterations
  (let* ( ;positions relative to the origin of the canvas
         (origin-positions 
          (map-positions-to-canvas-coordinates 
           start-segments))
         (origin-positions-points-from ;drop the last point
          (reverse (cdr (reverse origin-positions))))
         (origin-positions-points-to ;drop the first point
          (cdr origin-positions)))
    
    (start (* 2 *defsize*) (* 2 *defsize*)) ; create the canvas
    (apply + ; count the number of segments
           (map  (curryr iterator iter color)
                 ; apply the iterator to all segments
                 origin-positions-points-from
                 origin-positions-points-to))))


(define (map-positions-to-canvas-coordinates unit-postions)
#|
    The radius of the fractal is *defsize*.
    The canvas has a width and a height of (2 * *defsize*).
    The origin of the canvas is in the upper left corner.
    The coordinates of the fractal are given relativ its center
    and scaled to a radius of one.
    So the coordinates have to be scaled to *defsize* and
    translated to the center of the canvas.
|#
  (let* ((center (make-posn *defsize* *defsize*))
         ;positions relative to the center of the fractal
         (scaled-positions 
          (map (curryr scale-pos *defsize*) unit-postions))
         ;positions relative to the origin of the canvas
         (origin-positions 
          (map (curryr translate-pos center) scaled-positions)))
    origin-positions))


(define (snowflake-line from to iter color)
  ; draw a Koch-snowflake from point "from" to point "to"
  ; return the number of segments
  (if (= iter 0)
      (begin (draw-solid-line from to color) 1)
      ; split the line into 4 segments
      (let* ([p1/3 (interpolate from to 1/3)]
             [p2/3 (interpolate from to 2/3)]
             [pm (interpolate from to 1/2)]
             [tipOffset 
              (normal from to 
              ; the offset of the tip of the new triangle
                      (* (distance from to) 1/3 (sqrt 3/4)))]
             [tip (translate-pos pm tipOffset)])
        (+ (snowflake-line from p1/3 (- iter 1) color)
           (snowflake-line p1/3 tip (- iter 1) color)
           (snowflake-line tip p2/3 (- iter 1) color)
           (snowflake-line p2/3 to (- iter 1) color))
        )))

(define (snowflake-all-lines from to iter final-color iter-color)
  ; draw a Koch-snowflake from point "from" to point "to"
  ; return the number of segments
  ; draw also the previous iterations
  
  (if (= iter 0)
      (begin (draw-solid-line from to final-color) 1)
      ; split the line into 4 segments
      (let* ((p1/3 (interpolate from to 1/3))
             (p2/3 (interpolate from to 2/3))
             (pm (interpolate from to 1/2))
             (tipOffset (normal from to 
                                ; the offset of the tip of the new triangle
                                (* (distance from to) 1/3 (sqrt 3/4))))
             (tip (translate-pos pm tipOffset)))
        (draw-solid-line from to iter-color)
        (+ (snowflake-all-lines from p1/3 
                                (- iter 1) final-color  iter-color)
           (snowflake-all-lines p1/3 tip 
                                (- iter 1) final-color iter-color)
           (snowflake-all-lines tip p2/3 
                                (- iter 1) final-color iter-color)
           (snowflake-all-lines p2/3 to 
                                (- iter 1) final-color iter-color))
        )))

(define (snowflake iter)
  ; returns the number of iterations
  (let (;unit positions relative to the center of the flake
        (corners 
         (list *x* 
               (rotate-pos *x* -120.0) 
               (rotate-pos *x* -240.0) 
               *x*)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     snowflake-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))

(define (snowflake-invers iter)
  ; returns the number of iterations
  (let (;unit positions relative to the center of the flake
        (corners 
         (list 
          *x* 
          (rotate-pos *x* 120.0) 
          (rotate-pos *x* 240.0) *x*)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     snowflake-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))

(define (snowflake-both iter)
  ; returns the number of iterations
  (let (;unit positions relative to the center of the flake
        (outer-corners 
         (list *x* 
               (rotate-pos *x* -120.0) 
               (rotate-pos *x* -240.0) *x*))
        (inner-corners 
         (list  (rotate-pos *x* 120.0) 
                (rotate-pos *x* 240.0) *x*))
        )
    (fractal 
     (append outer-corners inner-corners); the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     snowflake-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))

(define (all-snowflakes iter)
  ; returns the number of iterations
  (let (;unit positions relative to the center of the flake
        (corners 
         (list *x* 
               (rotate-pos *x* -120.0) 
               (rotate-pos *x* -240.0) *x*))
        (color-iterations 'blue)
        (color-final-iteration 'red))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     (curryr snowflake-all-lines color-iterations); the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     color-final-iteration ; the drawing color
     iter) ; how many iterations
    )
  )


;;; --------------------------------------------------------
;;; Koch quadratic fractal
;;; --------------------------------------------------------

(define (quad-koch-line from to iter color)
  ; draw a Koch quadratic fractal from point "from" to point "to"
  (if (= iter 0)
      (begin (draw-solid-line from to color) 1)
      ; split the line into 4 segments
      (let* ((p1/4 (interpolate from to 1/4))
             (pm (interpolate from to 1/2))
             (p3/4 (interpolate from to 3/4))
             (toTheLeft 
              (normal from to 
                      ; the offset of the tip of the new rectangle
                      (* (distance from to) 1/4 )))
             (toTheRight (scale-pos toTheLeft -1))
             (p1/4l (translate-pos p1/4 toTheLeft))
             (pml (translate-pos pm toTheLeft))
             (pmr (translate-pos pm toTheRight))
             (p3/4r (translate-pos p3/4 toTheRight))
             (allpointsFrom
              (list from p1/4 p1/4l pml pm pmr p3/4r p3/4))
             (allpointsTo
              (list p1/4 p1/4l pml pm pmr p3/4r p3/4 to)))
        
        (apply + (map
                  (curryr quad-koch-line (- iter 1) color)
                  allpointsFrom ; startpoints
                  allpointsTo)) ; endpoints
        
        )))

(define (all-quad-koch-lines from to iter final-color iter-color)
  ; draw a Koch quadratic fractal from point "from" to point "to"
  (if (= iter 0)
      (begin (draw-solid-line from to final-color) 1)
      ; split the line into 4 segments
      (let* ((p1/4 (interpolate from to 1/4))
             (pm (interpolate from to 1/2))
             (p3/4 (interpolate from to 3/4))
             (toTheLeft (normal from to 
                                ; the offset of the tip of the new rectangle
                                (* (distance from to) 1/4 )))
             (toTheRight (scale-pos toTheLeft -1))
             (p1/4l (translate-pos p1/4 toTheLeft))
             (pml (translate-pos pm toTheLeft))
             (pmr (translate-pos pm toTheRight))
             (p3/4r (translate-pos p3/4 toTheRight))
             (allpointsFrom
              (list from p1/4 p1/4l pml pm pmr p3/4r p3/4))
             (allpointsTo
              (list p1/4 p1/4l pml pm pmr p3/4r p3/4 to)))
        (draw-solid-line from to iter-color)
        (apply + (map
                  (curryr all-quad-koch-lines (- iter 1) final-color iter-color)
                  allpointsFrom ; startpoints
                  allpointsTo) ); endpoints
        )))

(define (quadkoch iter)
  ; returns the number of iterations
  (let* ((r (scale-pos *x* 0.9));unit positions relative to the center of the flake
         (corners 
          (list r (rotate-pos r 90.0) 
                (rotate-pos r 180.0)(rotate-pos r 279.0) r)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     quad-koch-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))

(define (quadkoch-all-iterations iter)
  ; returns the number of iterations
  (let* (;unit positions relative to the center of the flake
         (r (scale-pos *x* 0.9))
         (corners 
          (list r (rotate-pos r 90.0) 
                (rotate-pos r 180.0)
                (rotate-pos r 270.0) 
                r)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     (curryr all-quad-koch-lines 'blue); the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'red ; the drawing color
     iter) ; how many iterations
    ))

(define (quadkoch-invers iter)
  ; returns the number of iterations
  (let* ((r (scale-pos *x* 0.9));unit positions relative to the center of the flake
         (corners 
          (list r (rotate-pos r -90.0) 
                (rotate-pos r -180.0)(rotate-pos r -270.0) r)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     quad-koch-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))
; 
(define (quadkoch-both iter)
  ; returns the number of iterations
  (let* ((r (scale-pos *x* 0.9));unit positions relative to the center of the flake
         (corners 
          (list r (rotate-pos r 90.0) 
                (rotate-pos r 180.0)(rotate-pos r 270.0) r
                (rotate-pos r -90.0) 
                (rotate-pos r -180.0)(rotate-pos r -270.0) r)))
    (fractal 
     corners ; the initial polygon:
     ; a list of points relative to the origin, sized between 0-1
     quad-koch-line ; the function to draw the segment
     ; (point-from point-to iter color) -> number-segments:integer
     'blue ; the drawing color
     iter) ; how many iterations
    ))

(define (spoken-result no-segments)
  
  (display-result)
  
  (Fred (string-append
         "Done. I have drawn "
         (number->string no-segments)
         " segments")))

(define *demos-available*
  #|
     all the available demos:
     format:  
          query string, 
          procedure to call
  |#
  `( ("Koch's snowflake, all iterations"       ,(compose spoken-result all-snowflakes))
     ("Koch's snowflake, outer border"         ,(compose spoken-result snowflake)) 
     ("Koch's snowflake, innerborder"          ,(compose spoken-result snowflake-invers))
     ("Koch's snowflake, both borders"         ,(compose spoken-result snowflake-both))
     ("Quadratic Koch fractal, all iterations" ,(compose spoken-result quadkoch-all-iterations))
     ("Quadratic Koch fractal, outer border"   ,(compose spoken-result quadkoch))
     ))

(define *max-iterations* 10)
(define *param-query-string* "How many iterations?")

(define (fractals-demo)
  (demo-with-numeric-params 
   *demos-available*
   *param-query-string*
   *max-iterations*
   ))

;Zum Testen:
;(fractals-demo)
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

(provide ; vector graphics            
 *x* *y* *-x* *-y* *o*
 start display-result 
 set-scene! get-scene
 scale-pos translate-pos rotate-pos
 rotateFixpoint distance normal 
 draw-solid-line draw-polyline draw-poly 
 interpolate normal
 *si*)

(provide/contract [*thescene* image?]
                  [*defsize* (integer-in 10 800)])

(require 2htdp/image
         2htdp/universe
         lang/posn)

(define *x* (make-posn 1 0))
(define *y* (make-posn 0 1))
(define *-x* (make-posn -1 0))
(define *-y* (make-posn 0 -1))
(define *o* (make-posn 0 0))

(define *defsize* 200) 
(define *si* 30)

(define *thescene* 
  (empty-scene (* 2 *defsize*) (* 2 *defsize*)))

(define (set-scene! new-scene)
  (set! *thescene* new-scene))

(define (get-scene) *thescene*)

(define (start w h)
  (set!  *thescene*
         (empty-scene w h))       
  )

(define (display-result)
  (big-bang 1    
            (on-tick add1)
            (to-draw (lambda (w) *thescene*)) 
            (stop-when (lambda (w) (> w 4)))
            ))


(define (scale-pos posn factor)
  (make-posn
   (* factor (posn-x posn))
   (* factor (posn-y posn))))

; affine Transformationen auf Positionen

(define (translate-pos posn trposn)
  (make-posn
   (+ (posn-x posn) (posn-x trposn))
   (+ (posn-y posn) (posn-y trposn))))



(define (rotate-pos posn angle) ; angle degrees
  ;x'=x*cos(angle)-y*sin(angle)
  ;y'=y*cos(angle)+x*sin(angle)
  (let* ((angRad (* angle (/ pi 180.0)))
         (sinAng (sin angRad))
         (cosAng (cos angRad)))
    (make-posn
     (- (* (posn-x posn) cosAng) 
        (* (posn-y posn) sinAng))
     (+ (* (posn-y posn) cosAng)
        (* (posn-x posn) sinAng)))))

(define (rotateFixpoint posn angle center) ; angle degrees
  ;x'=x*cos(angle)-y*sin(angle)
  ;y'=y*cos(angle)+x*sin(angle)
  (let* ((angRad (* angle (/ pi 180.0)))
         (sinAng (sin angRad))
         (cosAng (cos angRad))
         (xf (-(posn-x posn)(posn-x center)))
         (yf (-(posn-y posn)(posn-y center))))
    (translate-pos 
     (make-posn
      (- (* xf cosAng) (* yf sinAng))
      (+ (* yf cosAng) (* xf sinAng)))
     center)))

(define (distance from to) 
  ; euclidian distance between position from and to 
  (sqrt (+ (sqr (- (posn-x from) (posn-x to)))
           (sqr (- (posn-y from) (posn-y to))))))

(define (interpolate pos-1 pos-2 fract)
  ; compute a position between pos-1 and pos-2
  (let* ((dx  (- (posn-x pos-2) (posn-x pos-1)))
         (dy  (- (posn-y pos-2) (posn-y pos-1))))
    (make-posn
     (+ (posn-x pos-1)
        (* dx fract))
     (+ (posn-y pos-1)
        (* dy fract)))))

(define (normal pos-1 pos-2 nlength)
  ; compute a normal vector to the line from pos-1 to pos-2
  (let* ((dx  (- (posn-x pos-2) (posn-x pos-1)))
         (dy  (- (posn-y pos-2) (posn-y pos-1)))
         (dist (sqrt (+ (sqr dx) (sqr dy))))
         )
    (make-posn
     (-(* dy nlength (/ 1 dist) ))
     (* dx nlength  (/ 1 dist)))))

;======================================================
; add a line to a scene

(define (draw-solid-line from to color)
  (set! *thescene*
        (scene+line 
         *thescene* 
         (posn-x from) 
         (posn-y from)
         (posn-x to) 
         (posn-y to)
         color))
  *thescene* )

(define (draw-polyline positions color)
  ; draw a polygon given as a list of points
  (if (> (length positions) 1)
      (begin 
        (draw-solid-line (car positions) (cadr positions) color)
        (draw-polyline (cdr positions) color))
      *thescene* )
  )

(define (draw-poly positions color)
  ; draw a closed polygon given as a list of points, 
  ; connect the last point to the first point
  (if (> (length positions) 1)
      (begin 
        (draw-solid-line ; close the polygon
         (car positions) 
         (last positions) 
         color)
        (draw-polyline positions color))
      *thescene*))

;===========================================================



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

(provide drawRecursively
         lilyLeaf olilyLeaf lily1 lily2 lily3 olily2 olily3
         square2 square3
         lilyLeafPause drawRecursivelyPause lily1Pause) 

(require 2htdp/image
         (except-in 2htdp/universe space)
         lang/posn
         se3-bib/macos-module
         se3-bib/tools-module
         se3-bib/demo-gui-module
         se3-bib/vector-graphics-module
         )

;;; lily

(define (drawRecursively 
         shape ; the corners of the polyline, 
         ; a list of positions relative to the origin
         center; where to place the figure 
         howOften ; number of iterations
         rotationAngle ; initial rotation (degrees)
         rotationInc ; 
         scaleFactor ; initial scaleFactor between 0 and 1.0
         scaleInc;
         color
         )
  (if (> howOften 0)
      (let* ((rPoints 
              (map ; rotate and scale-pos the figure
               (lambda (p)
                 (scale-pos 
                  (rotate-pos p rotationAngle) scaleFactor))
               shape))
             (tPoints 
              (map (curryr translate-pos center) rPoints))) 
        ;translate-pos to the center
        (draw-polyline tPoints color)
        (drawRecursively 
         rPoints ; the corners of the polyline, 
         ; a list of positions relative to the origin
         center; where to place the figure 
         (- howOften 1); number of iterations
         (+ rotationAngle rotationInc) ; initial rotation (degrees)
         rotationInc ; 
         (* scaleFactor scaleInc) ; initial scaleFactor between 0 and 1.0
         scaleInc;
         color
         ))
      *thescene*;default: return the scene
      ))

(define (drawRecursivelyPause 
         ; wait for mouse click after drawing each polygon 
         shape ; the corners of the polyline, 
         ; a list of positions relative to the origin
         center; where to place the figure 
         howOften ; number of iterations
         rotationAngle ; initial rotation (degrees)
         rotationInc ; 
         scaleFactor ; initial scaleFactor between 0 and 1.0
         scaleInc;
         color
         )
  (if (> howOften 0)
      (let* ((rPoints 
              (map ; rotate and scale-pos the figure
               (lambda (p)
                 (scale-pos 
                  (rotate-pos p rotationAngle) scaleFactor))
               shape))
             (tPoints
              (map (curryr translate-pos center) rPoints))) 
        ;translate-pos to the center
        (draw-polyline tPoints color)
        (display *thescene*)
        ;(wait-for-mouse-click)
        
        (drawRecursivelyPause 
         rPoints ; the corners of the polyline, 
         ; a list of positions relative to the origin
         center; where to place the figure 
         (- howOften 1); number of iterations
         (+ rotationAngle rotationInc) ; initial rotation (degrees)
         rotationInc ; 
         (* scaleFactor scaleInc) ; initial scaleFactor between 0 and 1.0
         scaleInc;
         color
         ))
      *thescene*
      ))

(define (lilyLeaf size iter dir sextant)
  ; draw closed triangles
  (let* ((c1 (rotate-pos (scale-pos *x* size) (* sextant 60)))
         (c2 (rotate-pos c1 60))
         (m (scale-pos (translate-pos c1 c2) -1/3)) ;der Mittelpunkt von 0,c1,c2
         (c1r (translate-pos c1 m))
         (c2r (translate-pos c2 m))
         (o-r (translate-pos *o* m))
         (canvasCenter (make-posn size size))
         (plotAt (translate-pos canvasCenter (scale-pos m -1))))
    ; (start/cartesian-plane (* 2 size) (* 2 size))
    (drawRecursively 
     (list c1r o-r c2r c1r) ; the corners of the polyline, 
     ; a list of positions relative to the origin
     plotAt; where to place the figure 
     iter; number of iterations
     0 ; initial rotation (degrees)
     (* dir (/ 120 iter)) ; 
     1.0 ; initial scaleFactor between 0 and 1.0
     0.995;
     'black
     )))

(define (lilyLeafPause size iter dir sextant)
  ; draw closed triangles, wait for mouse click  after each triangle
  (let* ((c1 (rotate-pos (scale-pos *x* size) (* sextant 60)))
         (c2 (rotate-pos c1 60))
         (m (scale-pos (translate-pos c1 c2) -1/3)) ;der Mittelpunkt von 0,c1,c2
         (c1r (translate-pos c1 m))
         (c2r (translate-pos c2 m))
         (o-r (translate-pos *o* m))
         (canvasCenter (make-posn size size))
         (plotAt (translate-pos canvasCenter (scale-pos m -1))))
    ; (start/cartesian-plane (* 2 size) (* 2 size))
    (drawRecursivelyPause 
     (list c1r o-r c2r c1r) ; the corners of the polyline, 
     ; a list of positions relative to the origin
     plotAt; where to place the figure 
     iter; number of iterations
     0 ; initial rotation (degrees)
     (* dir (/ 120 iter)) ; 
     1.0 ; initial scaleFactor between 0 and 1.0
     0.995;
     'black
     )))
(define (olilyLeaf size iter dir sextant)
  ; draw open triangles
  (let* ((c1 (rotate-pos (scale-pos *x* size) (* sextant 60)))
         (c2 (rotate-pos c1 60))
         (m (scale-pos (translate-pos c1 c2) -1/3)) ;der Mittelpunkt von 0,c1,c2
         (c1r (translate-pos c1 m))
         (c2r (translate-pos c2 m))
         (o-r (translate-pos *o* m))
         (canvasCenter (make-posn size size))
         (plotAt (translate-pos canvasCenter (scale-pos m -1))))
    ; (start/cartesian-plane (* 2 size) (* 2 size))
    (drawRecursively 
     (list c1r o-r c2r) ; the corners of the polyline, 
     ; a list of positions relative to the origin
     plotAt; where to place the figure 
     iter; number of iterations
     0 ; initial rotation (degrees)
     (* dir (/ 120 iter)) ; 
     1.0 ; initial scaleFactor between 0 and 1.0
     0.995;
     'black
     )))

(define (lily1 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size))
    (lilyLeaf size iter -1 4)))

(define (lily1Pause iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size))
    (lilyLeafPause size iter -1 4)))

(define (lily2 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry lilyLeaf size iter 1) '(0 1 2 3 4 5)))
  (display-result))

(define (lily3 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry lilyLeaf size iter)
         '(1 -1 1 -1 1 -1)
         '(0 1 2 3 4 5)))
  (display-result))

(define (olily2 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry olilyLeaf size iter 1) '(0 1 2 3 4 5)))
  (display-result))

(define (olily3 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry olilyLeaf size iter)
         '(1 -1 1 -1 1 -1)
         '(0 1 2 3 4 5)))
  (display-result))

(define (square-leaf size iter dir quadrant)
  ; draw closed triangles
  (let* ((c1 (rotate-pos (scale-pos *x* size) (* quadrant 90)))
         (c2 (rotate-pos c1 90))
         (c3 (translate-pos c1 c2))
         (m (scale-pos (translate-pos (translate-pos c1 c2) c3) -1/4))
         ;der Mittelpunkt von c1,c2,c3, 0
         (c1r (translate-pos c1 m))
         (c2r (translate-pos c2 m))
         (c3r (translate-pos c3 m))
         (o-r (translate-pos *o* m))
         (canvasCenter (make-posn size size))
         (plotAt (translate-pos canvasCenter (scale-pos m -1))))
    ; (start/cartesian-plane (* 2 size) (* 2 size))
    (drawRecursively 
     (list c1r o-r c2r c3r c1r) ; the corners of the polyline, 
     ; a list of positions relative to the origin
     plotAt; where to place the figure 
     iter; number of iterations
     0 ; initial rotation (degrees)
     (* dir (/ 120 iter)) ; 
     1.0 ; initial scaleFactor between 0 and 1.0
     0.995;
     'black
     )))

(define (square2 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry square-leaf size iter 1) '(0 1 2 3)))
  (display-result))

(define (square3 iter)
  (let ((size *defsize*))
    (start (* 2 size) (* 2 size)) 
    (map (curry square-leaf size iter)
         '(1 -1 1 -1 )
         '(0 1 2 3 )))
  (display-result))

; test:
;  (olily3 500)
;  (lily1 300)
;  (square3 300)
; end module lilies
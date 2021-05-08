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
 show-set-card ; Anzeigen einer Spielkarte
 ; (show-set-card  n the-pattern the-mode the-color)
 *cardwidth* ; die Breite einer Spielkarte
 *cardheight* ; die Hoehe einer Spielkarte
 *bg-color* ; die hIntergrundfarbe der Karten
 set-cardwith! 
 set-bg-color!
 *example*) ;ein Bild mit Beispielkarten

(require 2htdp/image)

(define *cardwidth* 80)
(define *cardheight* (round (* *cardwidth* (sqrt 2.00))))
(define *patternwidth* (round (/ *cardwidth* (sqrt 2.00))))
(define *schraffurabstand* 4)

(define *bg-color* 'WhiteSmoke);'Gainsboro)

(define (set-cardwith! w)
  (set! *cardwidth* 80)
  (set! *cardheight* (round (* *cardwidth* (sqrt 2.00))))
  (set! *patternwidth* (round (/ *cardwidth* (sqrt 2.00))))
  )

(define (set-bg-color! c)
  (set! *bg-color* c))


(define (schraffur im y-pos)
  ; horizontale Schraffur in der Farbe *bg-color*,
  ; Abstand = *schraffurabstand*
  
  (if (>= y-pos (image-height im)) im
      (schraffur
       (add-line
        (add-line im 
                  0 y-pos (image-width im) y-pos *bg-color*)
        0 y-pos (image-width im) (+ 1 y-pos) *bg-color*)
       
       (+ y-pos *schraffurabstand*))))



(define (waves the-mode the-color)
  ; generate a wave 
  (let* ([radius (* *patternwidth* 0.5)]
         [inner-radius (* radius 0.3)]
         [lower-arch  ; Halbkreis nach unten
          (crop 0 radius *patternwidth* radius 
                ; x y width height image
                (overlay (circle (* 1 inner-radius) 'outline the-color)
                         ; Innenrand verstärken
                         (overlay (circle inner-radius the-mode *bg-color*)
                                  (circle radius the-mode the-color))))]
         [upper-arch ; Halbkreis nach oben
          (flip-vertical lower-arch)])
    (scale 0.7 
           (rotate 7
                   (scale/xy
                    1 0.5 
                    (overlay/xy 
                     upper-arch 
                     (+ radius inner-radius)
                     (- radius 1) lower-arch))) )))

(define (ovals the-mode the-color)
  (ellipse *patternwidth*
           (* 0.25 *patternwidth*) the-mode the-color))

(define (rects the-mode the-color)
  (rectangle *patternwidth*
             (* 0.25 *patternwidth*) 
             the-mode the-color))

(define (stack-patterns n pat)
  ; stapele das Muster
  (let ([gap (rectangle 
              1 (round(/ *cardheight* 30))
              'outline *bg-color*)])
    (if (= n 1) pat
        (above pat gap (stack-patterns (- n 1) pat)))))

(define (make-pattern  pat-gen n the-mode the-color )
  ; pat-gen: mode color → image?
  ; n: 1, 2, or 3
  ; the-mode: 'outline, 'solid, 'hatched
  ; the-color: 'red, 'green, 'blue
  (let* ([mode (if (eq? the-mode 'hatched)
                   'solid the-mode)]
         [pat-stack (stack-patterns n (pat-gen mode the-color))]
         )
    (if (eq? the-mode 'hatched)
        (schraffur pat-stack 1)
        pat-stack)))


(define (show-set-card  n the-pattern the-mode the-color)
  ; n: 1, 2, or 3
  ; the-pattern: 'waves, 'oval, 'rectangle
  ; the-mode: 'outline, 'solid, 'hatched
  ; the-color: 'red, 'green, 'blue
  (let ([pat-gen 
         (case the-pattern
           [(waves) waves]
           [(oval) ovals]
           [(rectangle) rects]
           [else rects])]; default
        [background 
         (overlay 
          (rectangle 
           *cardwidth* *cardheight* 'solid *bg-color*)
          (rectangle
           (+ 2 *cardwidth*) (+ 2 *cardheight*) 
           'outline 'silver)
          (rectangle
           (+ 4 *cardwidth*) (+ 4 *cardheight*) 
           'outline 'silver)
          )])
    (overlay 
     (make-pattern  pat-gen n the-mode the-color )
     background)
    ))


(define *example*
  (frame
   (above
    (beside
     (show-set-card  3 'rectangle 'solid 'green)
     (show-set-card  3 'rectangle 'hatched 'blue)
     (show-set-card  3 'rectangle 'outline 'red)
     (show-set-card  1 'oval 'outline 'red))
    (beside
     (show-set-card  2 'oval 'hatched 'red)
     (show-set-card  2 'waves 'hatched 'red)
     (show-set-card  2 'oval 'solid 'blue)
     (show-set-card  2 'waves 'outline 'blue))))
  )
#|
(save-image 
 (frame
  (beside
   (show-set-card  3 'waves 'solid 'green)
   (show-set-card  2 'rectangle 'hatched 'blue)
   (show-set-card  1 'oval 'outline 'red)
   (show-set-card  3 'waves 'outline 'green)
   (show-set-card  2 'oval 'hatched 'blue)
   (show-set-card  1 'waves 'hatched 'red)
   (show-set-card  3 'rectangle 'hatched 'green)))
 "karten.png")
|#

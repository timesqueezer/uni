#lang swindle

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

#|
 =================================================================
  The implementation of the view of sim-actor
  A mixin class for simulation objects,
  provides a framework for
      - displaying a picture of the actors
 =================================================================
|#

(provide 
 set-actor-pic! actor-pic 
 string->image b-string->image
 cjustify sim-info)

(require
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorViewClass-module
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/simActorImpl-module)

(defmethod actor-picture ((av sim-actor-view))
  "Default: return a picture of the actors state"
  (actor-pic av))

(defmethod actor-picture ((av <top>))
  "Default: return a picture of the actors state"
  (actor-pic av))

(defmethod has-picture? ((av sim-actor-view)) 
  "does the actor have an unique picture?"
  (not (eq? (actor-pic av) *defaultpic*)))

(defmethod has-picture? ((obj <top>)) 
  "does the actor have an unique picture?"
  #f)

(defmethod actor-pic ((obj <top>))
  "the class does not have an actor pic"
  *bug-pic*
  )

(defmethod sim-info ((av sim-actor-view))
  (writeln (actor-pic av)))

(define (string->image theString)
  ; make an image from a string
  (text  theString 
         (name-fontsize *default-view*)
         'red))

(define (b-string->image theString)
  ; make a bigger image from a string
  (text  theString
         (+ 1 (name-fontsize *default-view*)) 'blue))

(define (cjustify n s)
  (let* ((len (string-length s))
         (left (quotient (- n len) 2))
         (right (- n left len)))
    (if (>= n len)
        (string-append 
         (space left)  s (space right))
        (substring s 0 n))))

(defmethod picture-with-name
  ((av <top>))
  (let* ([a-pic (actor-pic av)]
         [name-pic (string->image (actor-name av))]
         [actor-over-name-pic (above a-pic name-pic)]
         [h (+ (image-height actor-over-name-pic)6)]
         [w (+ (image-width actor-over-name-pic)4)]
         [bg (rectangle w h 'solid 'gray)]); neutral background
    (overlay actor-over-name-pic bg)
    ))

(defmethod picture-with-name
  ((actor-view sim-actor-view))
  (call-next-method actor-view)
  )

;For testing:
#|
(require se3-bib/sim/simBase/simActorViewImpl-module)
(define a (make sim-actor-view))
|#
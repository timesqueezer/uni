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

(require
  swindle/setf
  swindle/extra
  swindle/misc
  (all-except 2htdp/universe space)
  2htdp/image
  test-engine/racket-tests 
  racket/trace
  (all-except racket/function negate thunk)
  (all-except se3-bib/tools-module
              mappend every some last concat identity)
  se3-bib/sim/simBase/sentinel-lists-module)

(provide 
 (all-from swindle/setf)
 (all-from swindle/misc)
 (all-from 2htdp/universe)
 (all-from 2htdp/image)
 (all-from test-engine/racket-tests) 
 (all-from racket/function)
 (all-from racket/trace)
 (all-from se3-bib/tools-module)
 (all-from se3-bib/sim/simBase/sentinel-lists-module)
 ;pinhole-x pinhole-y ; compatability to htdp
 get-keyword-arg
 )

#|
(define (pinhole-x im)
  (round (/ (image-width im) 2)))
(define (pinhole-y im)
  (round (/ (image-height im) 2)))
|#

(define (get-keyword-arg kw args default)
  ; retrieve the keyword argument prefixed by kw in 'args, 
  ; if not found, return 'default
  (let ([pos (member kw args)])
    (values
     (if (and pos 
              (not (null? pos))
              (not (null? (cdr pos))))
         (cadr pos) default)
     pos)))


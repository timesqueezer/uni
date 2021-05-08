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
         2htdp/universe)

(define (fname h)
  ;create a filename to save the image
  (string-append    "./"
                    (number->string h) 
                    ".png"))

(define (create-UFO-scene height)
  (let ([pic
         (underlay/xy 
          (rectangle 100 100 "solid" "white")
          50 height UFO)])
    (save-image pic (fname height))
    pic))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

(animate create-UFO-scene)


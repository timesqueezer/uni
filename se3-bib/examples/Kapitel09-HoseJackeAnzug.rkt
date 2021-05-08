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

(defgeneric preis ((<top>))
   :combination generic-+-combination)

 (defclass Jackett ()
   (preisJ :initarg :preisJ :reader preis)
   :printer  #t ) 
 (defclass Hose ()
   (preisH :initarg :preisH :reader preis)
   :printer  #t ) 
 (defclass Anzug (Jackett Hose)
   :printer  #t )
 
 (define a  
     (make Anzug :preisH 100 :preisJ 300))
 
(display a)
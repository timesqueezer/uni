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

(require htdp/gui
         test-engine/racket-tests 
         racket/trace
         se3-bib/tools-module)

; gui-demo
(define w1 
  (create-window 
   (list 
    (list 
     (make-button 
      "QUIT" 
      (lambda (e) 
        (hide-window w1)))))))
;A button appears on the screen. Click on the button and it will disappear. 
(show-window w1)

(define text1
  (make-text "Please enter your name"))

(define msg1
  (make-message 
   (string-append 
    "Hello, World" 
    (make-string 33 #\SPACE))))

(define (respond e)
  (draw-message 
   msg1 
   (string-append "Hello, " 
                  (text-contents text1))))

(define w2 
  (create-window
   (list
    (list text1)
    (list msg1)
    (list (make-button "OKAY" respond)
          (make-button 
           "QUIT" 
           (lambda (e) (hide-window w2)))))))

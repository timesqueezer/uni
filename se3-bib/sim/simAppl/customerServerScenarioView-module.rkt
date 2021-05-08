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

(provide clock-image bild)

(require   
  se3-bib/sim/simBase/sim-base-package  
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simDoorImpl-module
  se3-bib/sim/simAppl/simDoorView-module
  
  se3-bib/sim/simAppl/simServerStationsClass-module
  se3-bib/sim/simAppl/simServerStationsImpl-module
  se3-bib/sim/simAppl/simServerStationsViewClass-module
  se3-bib/sim/simAppl/simServerStationsViewImpl-module
  
  se3-bib/sim/simAppl/simCustomerClass-module
  se3-bib/sim/simAppl/simCustomerImpl-module
  se3-bib/sim/simAppl/simCustomerView-module
  
  se3-bib/sim/simAppl/customerServerScenarioClass-module
  se3-bib/sim/simAppl/customerServerScenarioCont-module)

(define (bild n)
  ; the n-th image of the movie
  (list-ref (reverse (actor-movie *current-universe*) n)))

(defmethod clock-image ((clock sim-clock))
  ;text : String Size Color -> Image 
  (string->image 
   (string-append "Clock: " 
                  (number->string (now)))))

(defmethod actor-picture 
  ((universe customer-server-scenario)) ;
  " a picture of all actors: clock, door, customers, and servers"
  (let* ([lines (all-stations-picture universe)]
         [clockIm (clock-image universe)]
         [eventImage (string->image (eventMes universe))]
         [doorImage (show-person-in-door *the-sim-door*)]
         [footpanel (rectangle (image-width (bg-screen universe))
                               (+ 4 (image-height doorImage))
                               'solid 'lightseagreen)]
         )
    
    ;(display-event  universe)
    ;(writeln (eventMes universe))
    
    (let* ([lines-on-screen
            (overlay/align 
             "left" "top" lines (bg-screen universe))]
           [event-mes-over-clock
            (above/align "left"  eventImage clockIm)]
           [add-door-to-footer
            (overlay/align 
             "right" "bottom"
             doorImage  footpanel)]
           [the-footer ; event and clock, door
            (overlay/align "left" "bottom"
                           event-mes-over-clock 
                           add-door-to-footer)]
           [scenario-picture 
            (overlay/align "left" "bottom" 
                           the-footer lines-on-screen)]
           )
      
      (push! scenario-picture (actor-movie universe))
      scenario-picture)))

; For testing:
; (require se3-bib/sim/simAppl/TestDemoFifthAvenue)
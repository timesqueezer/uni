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

(provide
  all-stations-picture
  sim-init!)

(require 
 se3-bib/sim/simBase/sim-base-package
 se3-bib/sim/simAppl/simServerStationsClass-module
 se3-bib/sim/simAppl/simServerStationsImpl-module
 se3-bib/sim/simAppl/simServerStationsViewClass-module
 se3-bib/sim/simAppl/simCustomerClass-module
 se3-bib/sim/simAppl/simCustomerImpl-module
 se3-bib/sim/simAppl/simCustomerView-module)

#|
Create a picture of the counter, the clerk, and the customers waiting:
  - billboard: sign with an advertisement of the goods
  - actor-picture: a picture of one server station
  - allStationsPicture: a picture of all stations

|#
(define *empty-pic* (b-string->image " idle "))

(defmethod picture-of-clerk ((s server-station-view))
  :documentation "a picture of the state of the clerk"
  (let ([clerk-pic
         (cond [(is-idle? s)  
                (idle-pic s)]
               [(is-serving? s) 
                (serving-pic s)]
               [(is-rejecting? s) 
                (reject-pic s)]
               [else (actor-pic s)])]);*default-serving-pic*])])
    (set-clerk-pic! s clerk-pic)
    clerk-pic))

(defmethod init-clerk-bg ((station-view server-station-view))
  ; compute the max. dimensions of the combined pictures 
  ; of the clerk and the service billboard
  (let* ([service-texts
          (map-actors 
           station-view 
           (compose b-string->image station-service)
           server-station)]
         [name-texts
          (map-actors 
           station-view 
           (compose b-string->image actor-name)
           server-station)]
         [clerk-images 
          (list (reject-pic station-view)
                (serving-pic station-view)
                (idle-pic station-view))]
         [allims (append service-texts name-texts clerk-images)]
         [gap 2]
         [w (+ gap (apply max (map  image-width allims)))]
         [h 
          (+ gap 
             (apply max (map  image-height allims))
               (* 2 (apply max (cons 0 (map  image-height service-texts)))))]
         [bg (overlay
              (rectangle w h 'solid 'gray)
              (rectangle w (+ h gap gap)'solid 'white))]
         )
    (set-bg-pic! station-view bg)
    ))

(defmethod picture-of-counter ((s server-station-view))
  :documentation 
  "Picture of clerk and an advertisement of the station service"
  (overlay
   (above 
    (b-string->image (actor-name s))
    (picture-of-clerk s); the clerk on top
    (b-string->image (station-service s))); the service plate below
   (bg-pic s))); neutral background


(defmethod sim-init! ((sv server-station-view))
  (picture-of-clerk sv)
  (init-clerk-bg sv))

; construct a picture of all waiting lines:
; left: the Server, left to right: the waiting line

(defmethod all-stations-picture ((sc sim-scenario))
  ; stack all the server stations top to bottom
  (let ([picsOfTheLines 
         (map actor-picture 
              (the-actors *all-servers*))])
    (cond [(null? picsOfTheLines) 
           (text "No server stations" 24 "olive")]
          [(null? (cdr  picsOfTheLines))
           (car picsOfTheLines)]
          [else (foldl (lambda (b t) 
                         (above/align "left" t b))
                       (car picsOfTheLines)
                       (cdr picsOfTheLines)) ])
    ;  (apply (curry above/align "left"); bug 
    ;         picsOfTheLines )
    ))

(defmethod 
  actor-picture 
  ; an image of the queue
  ((station-view server-station-view))
  (let* ([counterPic (picture-of-counter station-view)]
         [the-queue (map picture-with-name 
                         (enumerate-queue station-view))]
         [theQueuePic 
          (cond [(null? the-queue)  *empty-pic*]
                [(null? (cdr the-queue)) (car the-queue)]
                [else (apply (curry beside/align "baseline")
                             the-queue)])]
         [act-pic (beside counterPic theQueuePic)]) 
    (set-actor-pic! station-view act-pic)
    act-pic           
    )) 

;For testing:
#|
(require se3-bib/sim/simAppl/sim-application-package)

(actor-picture (make server-station))
(define s      (make server-station))
(define s2     (make server-station))
(make sim-clock)
(enqueue! s (make customer))
(enqueue! s2 (make customer))
(define p (all-stations-picture (make sim-scenario)))
  ; stack all the server stations top to bottom
p
(sim-init! s)
|#

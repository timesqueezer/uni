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

#|Specialized customer classes and server classes:
    not everybody is entitled to buy magic potion and weapons -
    customers may be rejected by the server.
    enemies, children, and pets can't buy magic potion and weopons.

    auto provide:
      classes:  magic-potion-station  weapons-station
                no-magic-potion-allowed no-weapons-allowed
                foreigner enemy child animal very-strong-person
      generics: reject
|#

(provide 
 aremorica-demo-2
 *classes-table*
 update-class update-pictures); for testing

;; differently defined in swindle  
(require 
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simAremorica/aremorica-scenario-package
  se3-bib/sim/simAremorica/aremoricaScenario2Class-module)

(defmethod rejecting? ((s magic-potion-station)
                       (c no-magic-potion-allowed))
  #t; the default, serve all customers
  )

(defmethod rejecting? ((s weapons-station)
                       (c  no-weapons-allowed))
  #t; the default, serve all customers
  )

(defmethod serve
  ((s magic-potion-station)
   (c no-magic-potion-allowed))
  (reject s c))

(defmethod serve
  ((s weapons-station)
   (c  no-weapons-allowed))
  (reject s c))

(define *magic-potion-stations* '("Miraculix"))
(define *weapons-stations* '("Automatix"))
(define *general-stations* '("Verleihnix" "Majestix"))

(define *enemies*
  '("Bossix" "Stupidix" "Augenblix" "Caliguliminix"
             "Pyramidonis"
             "Julius-Caesar" "Nixalsverdrus" "Keinentschlus"
             "Schmalzlockus"
             "Maulaf" "Olaf"
             ))
(define *foreigners* 
  '("Cleopatra" "Mac-Teefuerzweifix" 
                "O-Fuenfuehrteefix"  "Teefax" 
                "Sinfonix" "Haegar" "Hamlet" ))

(define *children*
  '("Tumirnix" "Cathedralgotix" "Alice" "Lucy" "Sally"))

(define *animals*
  '("Idefix" "Urmel" "Devil" "Tux"))

(define *very-strong* '("Obelix"))

; maintain a table of the classes of all actors
(define *classes-table* (make-hash-table 'equal ))
; compare keys using equal?

(define (store-class name cl)
  (hash-table-put!  *classes-table* name cl))

(define *pictures-table* (make-hash-table 'equal ) )

(define (store-picture name pics)
  (hash-table-put! *pictures-table* name pics))

(define (store-pictures)
  ; maintain a hashtable of additional pictures
  (let ((more-pictures 
         (list
          `("Miraculix"  :reject-pic ,*miraculix-pic2*)
          `("Majestix"   :reject-pic ,*majestix-pic*)            
          `("Verleihnix" :reject-pic ,*verleihnix-pic*)
          `("Automatix"  :reject-pic ,*automatix-pic2*))))
    
    (map (lambda (pics)
           (store-picture (car pics) (cdr pics)))
         more-pictures)))

(store-pictures)

(defmethod the-new-pictures ((a sim-actor))
  (hash-table-get 
   *pictures-table*
   (actor-name a)
   #f))

(define *class-actors*
  ; new class  : list of actors, who's class needs to be changed
  `((,magic-potion-station . , *magic-potion-stations*)
    (,weapons-station . , *weapons-stations*)
    (,foreigner . , *foreigners* )
    (,child . , *children*)
    (,animal . , *animals*) 
    (,very-strong-person . ,*very-strong*)
    ))

(define (store-classes)
  (map (lambda (class-actors-tupel)
         (let ((the-Class 
                (car class-actors-tupel))
               (list-of-actors 
                (cdr class-actors-tupel)))            
           (map (curryr store-class the-Class)
                list-of-actors )))
       *class-actors*))

(store-classes)

; change-class executes iinitialize again
; so the actor might be added twice to the list of actors

(defmethod update-class ((a sim-actor))
  (let ((newClass    
         (hash-table-get 
          *classes-table*
          (actor-name a)
          #f)))
    (when newClass
      (change-class! a newClass))))

(defmethod update-pictures 
  ((s server-station))
  
  (let ([add-pics (the-new-pictures s)])
    (let ([rej-pic (getarg add-pics :reject-pic #f)])
      (when rej-pic (set-reject-pic! s rej-pic))              
      )))

#|
(add-method ; Variante 2
   ; change the class of the iherited objects to the new classes
   initialize
   (qualified-method
    :after ((a customer) initargs )
       (updateClass a)
      (if (next-method?)
          (call-next-method a initargs)))))
|#    

(defmethod 
  create-the-event-sources :after
  ((village aremoricaR-scenario))
  "remove duplicates from the actor list and the server list"
  ; change class causes initialize to be executed again,
  ; so the after methods may cause duplicate entries
  (broadcast village update-class :sim-class customer)
  (broadcast village update-class :sim-class server-station)
  (broadcast village update-pictures :sim-class server-station)
  (remove-duplicates! *all-actors*)
  (remove-duplicates! *all-servers*)
  )

(define (aremorica-demo-2) 
  (run (make aremoricaR-scenario 
             :actor-name "Aremorica 2"
             :the-end-of-time 10 
             :the-max-num-events 200
             :scene-description 
             "Spezialisierte Kunden und Server im alten Gallien")))

;For testing
#|
(require se3-bib/sim/simAremorica/aremorica-scenario2-package)
(aremorica-demo-2)

;or, for stepwise debugging: 
(define gallia (make aremoricaR-scenario 
               :actor-name "Aremorica"
               :scene-description 
               "Brave gaulles in a little village in Aremorica"
               :the-end-of-time 10 
               :the-max-num-events 200))
(create-the-event-sources gallia)
    ; hier kann man die Akteure vor dem Simulationslauf inspizieren ...
(define obelix 
  (retrieve-by-name *current-universe* "Obelix"))
(print obelix)
(define miraculix 
  (retrieve-by-name *current-universe* "Miraculix"))
(print miraculix)

(simulate gallia)
|#
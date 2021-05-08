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
 A class of queues that can hold a sequence of items,
 subclasses with different enqueue strategies
 Author: Leonie Dreschler-Fischer
 |# 

(provide
 ; slots
 queue-items set-queue-items!
 the-rank-p the-random-gen)
 #| 
   auto provide:
   classes: LIFO-queue FIFO-queue 
     RANKED-queue RANDOM-queue
   generics: queue enqueue! dequeue! 
     head-queue empty-queue?
     enumerate-queue
     reset-queue! rLess?
 |#

(require se3-bib/sim/simBase/sim-utility-base-package)

(defclass* queue () 
  (items :reader queue-items
         :writer set-queue-items!
         :initvalue (sm-makelist); mutable list with sentinels 
         ;:type mlist?
         :documentation "The items in the queue"
         )
  :autopred #t ; auto generate predicate queue?
  :printer  #t 
  :documentation    
  "the top class of the hierarchie of queues"
  )

(defclass* LIFO-queue (queue) 
  ; all slots are inherited
  :autopred #t 
  :automaker #t 
  :printer  #t 
  :documentation 
  "Queues with a last-in-first-out strategy"
  )

(defclass* FIFO-queue (queue) 
  :documentation 
  "Queues with a last-in-first-out strategy"
  :autopred #t 
  :automaker #t 
  :printer  #t 
  )

(defclass* RANKED-queue (queue) 
  (rank-p :reader the-rank-p
          :initarg :rank-p
          :type <function>
          :documentation 
          "a procedure to compute the rank of an item")
  :documentation "Queues that are ordered     
according to a rank of the items"  
  :autopred #t 
  :automaker #t 
  :printer  #t 
  )

(defclass* RANDOM-queue (queue) 
  (rand-gen :reader the-random-gen;randomGen,randGen
            :initvalue (lambda (x) (random x))
            :initarg :rand-gen
            :type <function>
            :documentation 
            "a procedure to generate a random position in the queue"
            ; <top>-> real >= 0
            )
  :documentation "Queues that are ordered randomly"  
  :autopred #t 
  :automaker #t 
  :printer  #t 
  )

(defgeneric* enqueue! ((qu queue) item) 
  ; enqueue!:  queue any -> queue
  ; Arg. item: keine Spezialisierung
  ; abstract, should be provided by the subclasses
  :documentation "Push an item onto the queue."
  )

(defgeneric* dequeue! ((qu queue))
  ; dequeue!: queue -> any
  :documentation    
  "Remove an item from the front of the queue.
 Return the item."
  )

(defgeneric* enqueue-items! ((qu queue) (items <list>)) 
  ; enqueue!:  queue <list> -> queue
  :documentation "Push all items of a list onto the queue."
  )

(defgeneric* head-queue ((qu queue))
  :documentation 
  "Return the item at the front of  the queue." 
  )

(defgeneric* empty-queue? ((qu queue))
  :documentation "Is the queue empty?"
  )

(defgeneric* enumerate-queue ((qu queue))
  :documentation  
  "Return a list of  the queue-items"
  )

(defgeneric* reset-queue! ((qu queue))
  :documentation    
  "Reset the queue, remove all items"
  )

(defgeneric* rLess? ((qu queue))
  ;rLess?: queue -> (<top><top> -> boolean)
  :documentation 
  "Generate a less?-predicate according to rankp")
;)

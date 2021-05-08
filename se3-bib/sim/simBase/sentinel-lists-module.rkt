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

#|
 Listen mit einem Wächter am Ende
 für einfache push und pop-Operationen
|#
(provide sm-push!
         sm-pop!
         sm-makelist;
         sm-first-element
         sm-insert-at-pos!
         sm-insert-at-back!
         sm-insert-Ranked!
         sm-empty?
         sm-length
         sm-first-element ; the first element that is not a sentinel
         sm->list)

(require scheme/mpair);  (all-except scheme/mpair mappend)) ; use mutable lists

(define (mcadr mxs)
  (mcar (mcdr mxs)))

(define (mcddr mxs)
  (mcdr (mcdr mxs)))

(define (sm-first-element mxs)
  (mcadr mxs)); skip sentinel

(define-struct sentinel 
  (firstCons lastCons theLength) 
  #:mutable)

(define (sm-makelist)
  ; create sentinels at front and end,
  ; initialize the reference to the last cons-cell:
  ; the sentinel points to the last cons
  ; and keeps track of the length of the mlist.
  (let* ((the-sentinel (make-sentinel '() '() 0 ))
         (new-list (mlist the-sentinel the-sentinel)))  
    (set-sentinel-firstCons! the-sentinel new-list)
    (set-sentinel-lastCons! the-sentinel new-list)
    new-list
    ))

(define (theSentinel mxs)
  (mcar mxs))

(define (sm-length mxs)
  (sentinel-theLength (theSentinel mxs)))

(define (sm-empty? mxs)
  (zero? (sentinel-theLength (theSentinel mxs))))

(define (sm->list mxs)
  ; copy a sentinel-list to a proper list and remove the sentinels
  (filter (lambda (x)
            (not (sentinel? x)))
          (mlist->list mxs)))

(define (insert-item-behind-cons! mxs item senti)
  ; inserts item as second element into mxs. 
  ; mxs must have at least one element.
  ; updates the sentinal senti.
  ; Returns the modified list.
  (when (null? mxs)
    (error "mxs must not be empty")) ; illegal argument
  (let ([newcons (mcons item 
                        (mcdr mxs))])
    (set-mcdr! mxs newcons)
    (set-sentinel-theLength! 
     senti (+ 1 (sentinel-theLength senti)))
    (when (eq? (mcadr newcons) senti) ; neues letztes Element
      (set-sentinel-lastCons! senti newcons)
      mxs)))

(define (sm-push! mxs item)
  ; adds a new cons-cell as second element.  
  ; mxs must have at least one element.
  ; Returns the modified list.
  (insert-item-behind-cons! mxs item (mcar mxs))
  mxs
  )

(define (sm-pop! mxs)
  ; removes the mcons-cell behind the sentinel and returns its value. 
  ; the mlist must have at least three Elements: 
  ; the two sentinels and one element to pop.
  (when (or 
         (null? mxs) 
         (null? (mcdr mxs))
         (null? (mcdr(mcdr mxs))))
    (error "mxs must have at least three elements"))
  
  (let ([result (mcadr mxs)])
    (set-mcdr! mxs (mcdr (mcdr mxs))); remove the second cons 
    (set-sentinel-theLength! 
     (theSentinel mxs) (- (sm-length mxs) 1))
    (when (sm-empty? mxs)
      (set-sentinel-lastCons! (theSentinel mxs)  mxs)); empty list
    result; return the element popped
    ))

(define (sm-insert-at-back! mxs item)
  (insert-item-behind-cons!
   (sentinel-lastCons (theSentinel mxs)) 
   item (theSentinel mxs))
  )

;(insert-item-behind-cons! mxs item senti)
(define (sm-insert-at-pos-senti! mxs item senti pos)
  ; insert the item into the sm-list mxs 
  ; at the position pos, zero based
  (cond [(or (null? (mcddr mxs));mxs is empty
             (zero? pos))
         (insert-item-behind-cons! mxs item senti)]
        [else 
         (sm-insert-at-pos-senti! 
          (mcdr mxs) item senti (- pos 1))]))

(define ( sm-insert-at-pos! mxs item pos)
  (sm-insert-at-pos-senti! mxs item (theSentinel mxs) pos)
  mxs)

(define (sm-insert-Ranked-senti! mxs item senti lessp?)
  ;mxs mutable list
  (if (or (null? (mcddr mxs)) ;mxs is empty (mlist item))
          (lessp? item (mcadr mxs))); insertion location found
      (insert-item-behind-cons! mxs item senti)         
      (sm-insert-Ranked-senti! (mcdr mxs) item senti lessp?)))

(define (sm-insert-Ranked! mxs item lessp?)
  ;mxs mutable list with sentinels
  (sm-insert-Ranked-senti! mxs item (theSentinel mxs) lessp?)
  mxs)


(define testml 
  (sm-push! (sm-push! (sm-push! (sm-makelist) 1) 2) 3))


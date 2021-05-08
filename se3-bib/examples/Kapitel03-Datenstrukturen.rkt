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

(require 
 (except-in se3-bib/tools-module 
            one-of random-elt writeln space)
 rackunit)

(define (fak n) ; 
  (cond [(not (integer? n)) (error "not natural n")]
        [(< n 1) (error "n must be > 0")]
        [(= n 1) 1] 
        [else 
         (* n (fak (- n 1) ))]))

;;Example: (exact->inexact (fak 100))

(check = (fak 1) 1 "test: (fak 1) != 1")
(check = (fak 4) 24 "test: (fak 4) != 24")

(check > (fak 200) (fak 100) )
(check > (exact->inexact (fak 200)) 
       (exact->inexact (fak 100)) )

(define (leap y)
  (cond  [(= 0 (remainder y 100))
          (= 0 (remainder y 400))]
         ; durch 400 teilbar
         [else 
          (= (remainder y 4) 0)]))  

(check eq? (leap 2000) #t "test: (leap 2000) != #t");
(check eq? (leap 1000) #f "test: (leap 1000) != #f")
(check eq? (leap 1963) #f "test: (leap 1963) != #f")
(check eq? (leap 1964) #t "test: (leap 1964) != #t")

(define (leap2 y)
  (or 
   (and (= (remainder y 4) 0)  
        (not (= (remainder y 100) 0)))
   (= (remainder y 400) 0)))

(check eq? (leap2 2000) #t "test: (leap2 2000) != #t")
(check eq? (leap2 1000) #f "test: (leap2 1000) != #f")
(check eq? (leap2 1963) #f "test: (leap2 1963) != #f")
(check eq? (leap2 1964) #t "test: (leap2 1964) != #t")

(define *computer-scientists*
  '( (Charles Babbage)
     (Alonzo Church)
     (Freiherr Gottfried Wilhelm von Leibniz)
     (Rear Admiral Grace Murray Hopper)
     (Lady Ada Countess of Lovelace)
     (John McCarthy)
     (John von Neumann)
     (Alan Turing)
     (Blaise Pascal)))

(define (last-name name)
  ;;; Select the last name of a name 
  ;;; represented as a list."
  (car (reverse name))) 

(check eq?  ;langer Name

       (last-name '(Rear Admiral Grace
Murray Hopper))

       'Hopper 
       "test: (last-name '(Rear Admiral Grace
Murray Hopper)) != 'Hopper")

(check eq?  ;langer Name

       (last-name '(Eratosthenes))

       'Eratosthenes 
       "test: (last-name '(Eratosthenes)) != Eratosthenes")


(define (first-name name)
  ;;; select the first name from a name 
  ;;; represented as a list.
  (car name))

#|
(check eq?
  ;without titles

  (first-name '(John McCarthy))
  'John)
(check
  ;without titles

  (first-name '(John McCarthy))
  'John)
(check
  ;with titles
  (first-name '(Freiherr Gottfried Wilhlm von Leibnitz))
  'Freiherr)
|#

(define *titles*
  '(Freiherr Rear Admiral Lady von Herr Frau 
             Mr. Mrs. Miss Sir Madam Dr. Prof. ))

(define (first-name2 name)
  ;; select the first name from a name 
  (if (member (car name) *titles*)
      (first-name2 (cdr name))
      (car name)))
#|
(check-expect
  ;with titles

  (first-name2 '(Lady Ada Lovelace))
  'Ada)
(check-expect
  ;kurzer Name

  (first-name2 '(Erathostenes))
  'Erathostenes)

;assoc
(check-expect
  (assoc 'Alan *computer-scientists*) 
  '(Alan Turing))
(check-expect
  (last-name (assoc 'Alan
*computer-scientists*)) 
  'Turing)
|#

(define (list-ref xs n)
  ;;; element n of list xs, zero indexed
  (cond
    ((<= (length xs) n)
     (error "list-ref: Index" n 
            "out of range:" xs))
    ((zero? n) (car xs))
    (else (list-ref (cdr xs) (- n 1)))))

;(check-expect (list-ref '(1 2 3) 1) 2)


; Grammar1
(define (sentence)
  (append (noun-phrase) (verb-phrase)))
(define (noun-phrase) 
  (append (Article) (Noun)))
(define (verb-phrase)
  (append (Verb) (noun-phrase)))
(define (Article)
  (one-of '(the a)))
(define (Noun)
  (one-of '(man ball woman table)))
(define (Verb) 
  (one-of '(hit took saw liked)))
(define (one-of set)
  ;;; Pick one element of set, 
  ;;; and make a list of it.
  (list (random-elt set)))
(define (random-elt choices)
  ;;; Choose an element from a list 
  ;;; at random.
  (list-ref choices 
            (random (length choices))))
(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

;(sentence)(sentence)

(define writeln 
  (lambda args
    (for-each display args)
    (newline)))

#| grammar2

(define (Adj*)
  (if (= (random 2) 0)
      '()
      (append (Adj) (Adj*))))

(define (PP*)
  (if (random-elt '(#t #f))
      (append (PP) (PP*))    '()))

(define (noun-phrase) 
  (append (Article) (Adj*) (Noun) (PP*)))

(define (PP) (append (Prep) (noun-phrase)))

(define (Adj) 
  (one-of 
   '(big little blue green adiabatic)))

(define (Prep) (one-of '(to in by with on)))

(sentence)(sentence)
|#

#| member, memq, memv

(check-expect
 (member '(Alan Turing) *computer-scientists*)
 '((Alan Turing) (Blaise Pascal)))

(check-expect
  (member '(Madonna) *computer-scientists*)
#f)

(check-expect
  (memq '(Alan Turing) *computer-scientists*)
  #f)
|#

(define (my-length xs)
  (if (null? xs) 0
      (+ 1 (my-length (cdr xs)))))

;(check = (my-length '(1 2)) 2)

(define (member? item xs)
  (if (null? xs) #f
      (or (equal? item (car xs))
          (member? item (cdr xs)))))
#|
(check-expect  (member? 2 '(5 4 7 2 4)) #t)
(check-expect (member? 2 '(5 4))  #f)
|#

;characters
#|
(check-expect
  (integer->char (char->integer #\A))  
   #\A)
(check-expect 
  (integer->char (+ 1 (char->integer #\A)))
  #\B)
|#

;message passing
(define (my-cons head tail)
  (lambda (message)
    (case message
      ((h) head)
      ((t) tail))))  

(define (my-car xs) (xs 'h))
(define (my-cdr xs) (xs 't))
(define *xs*  (my-cons 1 2)) 
(check eq? (my-car *xs*) 1 "test: (my-car *xs*) != 1")
(check eq? (my-cdr *xs*) 2 "test: (my-cdr *xs*) != 2")


;;;; =============================================================================================
;;;; convert numbers to text, see Bird and Wadler 88 for a Miranda Version
;;;; =============================================================================================

(define units  '("one" "two" "three" "four" "five" 
             "six" "seven" "eight" "nine"))
(define teens  '("ten" "eleven" "twelve" "thirteen" 
           "fourteen" "fifteen" "sixteen" 
           "seventeen" "eighteen" "nineteen"))
(define tens  '("twenty" "thirty" "forty" "fifty" 
             "sixty" "seventy" "eighty" "ninety"))
(define (digit n pos) 
  (let ((n-ziffern (remainder n (expt 10 pos))))
       (quotient n-ziffern (expt 10 (- pos 1)))))

(define (convert2 n)
  (combine2 (digit n 2)
            (digit n 1)))

(define (combine2 d-tens d-units)
  (cond ((= 0 d-tens) (list-ref units (- d-units 1)))
        ((= 1 d-tens) (list-ref teens d-units))
	((= 0 d-units)(list-ref tens (-  d-tens 2)))
	(else (string-append 
	       (list-ref tens (-  d-tens 2))
               "-"
	       (list-ref units (- d-units 1))))))

(define (convert3 n)
  (combine3 (digit n 3) 
	    (remainder n 100)))

(define (combine3 d-hundreds d-belowhundred)
  (cond ((= 0 d-hundreds)(convert2 d-belowhundred))
	((= 0 d-belowhundred)
              (string-append (list-ref units (- d-hundreds 1))
                             " hundred"))
	(else (string-append (list-ref units (- d-hundreds 1))
                             " hundred and "
			     (convert2 d-belowhundred)))))
(define (number->cleartext n)
        (if (< n 1000000)
	    (combine6 
	     (quotient n 1000)
	     (remainder n 1000))
            "n zu gross!")
  )

(define (link h) 
  (if (< h 100) " and " " "))

(define (combine6 thousands hundreds)
  (cond ((= 0 thousands) (convert3 hundreds))
	((= 0 hundreds) 
	 (string-append (convert3 thousands)
			" thousand"))
	(else 
	 (string-append (convert3 thousands)
			" thousand" (link hundreds)
			(convert3 hundreds)))))

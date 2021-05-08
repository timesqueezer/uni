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

(provide ljustify cjustify 
         rjustify
         number->cleartext
         list-of-symbols->string 
         string->list-of-symbols
         number->cleartext
         )

(require se3-bib/tools-module)

;;;; ======================================================================
;;;;  justify text to fit into n spaces, 
;;;;  see Bird and Wadler 88 for a Miranda Version
;;;; =======================================================================

(define (ljustify n s)
  (let ([len (string-length s)])
    (if (>= n len)
        ; string to short, pad with blanks
        (string-append 
         s
         (space (- n len)))
        ; string to long, trim at the end
        (substring s 0 n)
        )))

(define (rjustify n s)
  (let ([len (string-length s)])
    (if (>= n len)
        ; string to short, pad with blanks
        (string-append 
         (space (- n len))
         s)
        ; string to long, trim at the start
        (substring s (- len n) len)
        )))

(define (cjustify n s)
  (let* ([len (string-length s)]
         [left (quotient (- n len) 2)]
         [right (- n left len)])
    (if (>= n len)
        (string-append 
         (space left)  s (space right))
        (substring s 0 n) )))


;;;; ======================================================================
;;;; convert numbers to text, see Bird and Wadler 88 for a Miranda Version
;;;; ======================================================================

(define units  '("one" "two" "three" "four" "five" 
                       "six" "seven" "eight" "nine"))
(define teens  '("ten" "eleven" "twelve" "thirteen" 
                       "fourteen" "fifteen" "sixteen" 
                       "seventeen" "eighteen" "nineteen"))
(define tens  '("twenty" "thirty" "forty" "fifty" 
                         "sixty" "seventy" "eighty" "ninety"))
(define (digit n pos) 
  (let ([n-ziffern 
         (remainder n (expt 10 pos))])
    (quotient n-ziffern (expt 10 (- pos 1)))))

(define (convert2 n)
  (combine2 (digit n 2)
            (digit n 1)))

(define (combine2 d-tens d-units)
  (cond [(= 0 d-tens) 
         (list-ref units  (- d-units 1))]
        [(= 1 d-tens) 
         (list-ref teens d-units )]
        [(= 0 d-units)
         (list-ref tens (-  d-tens 2))]
        [else 
         (string-append 
          (list-ref tens (-  d-tens 2))
          "-"
          (list-ref units (- d-units 1)))]))

(define (convert3 n)
  (combine3 (digit n 3) 
            (remainder n 100)))

(define (combine3 d-hundreds d-belowhundred)
  ; combine3: number number --> string
  (cond [(= 0 d-hundreds)
         (convert2 d-belowhundred)]
        [(= 0 d-belowhundred)
         (string-append 
          (list-ref units (- d-hundreds 1))
          " hundred")]
        [else (string-append 
               (list-ref units (- d-hundreds 1))
               " hundred and "
               (convert2 d-belowhundred))]))

(define (number->cleartext n)
  (if (< n 1000000)
      (combine6 
       (quotient n 1000)
       (remainder n 1000))
      ; number to large, just show the digits
      (number->string n)))

(define (link h) 
  (if (< h 100) " and " " "))

(define (combine6 thousands hundreds)
  (cond [(= 0 thousands)(convert3 hundreds)]
        [(= 0 hundreds) 
         (string-append (convert3 thousands)
                        " thousand")]
        [else 
         (string-append (convert3 thousands)
                        " thousand" (link hundreds)
                        (convert3 hundreds))]))

;;; =====================================================================
;;; tokenizer
;;; =====================================================================

(define (list-of-symbols->string symbols)
  (apply string-append
         (map (curryr string-append " ")
              (map symbol->string symbols))))

(define (copy-to-string-object-bug-fix s) 
  ; bug fix: open-input-string does not accept the string objects 
  ; returned by the gui
  (list->string (string->list s)))

(define (next-item port)
  ; read symbols from the port and collect them into a list
  (let (( item (read port)))
    (if (eof-object? item)
        '()
        (cons item (next-item port)))))

(define (string->list-of-symbols string-of-symbols)
  (let ((port 
         (open-input-string 
          (copy-to-string-object-bug-fix string-of-symbols))))
    (next-item port)))



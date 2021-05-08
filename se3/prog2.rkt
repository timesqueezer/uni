#lang racket


;;Aufgabe 1

(define wuff 'Flocki)
(define Hund wuff)
(define Wolf 'wuff)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Zaphod)
        (PersonC PersonA))
    PersonC))

(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))


(define xs4 '(damn hey bro))

wuff
;evaluiert zu 'Flocki weil es als dieses definiert ist

Hund
;evaluiert auch zu 'Flocki weil Hund als wuff definiert ist
;und wuff als 'Flocki definiert wurde

Wolf
;evaluiert zu 'wuff weil dies ein Symbol ist

(quote Hund)
;evaluiert zu 'Hund weil die die beide Synonime sind

(eval Wolf)
;zuerst zeigt Wolf auf wuff
;dann wuff wird auf 'Flocki evaluiert.

(eval Hund)
;gibt einen Fehler zurück, weil dies kein Symbol ist


(eval 'Wolf)
;evaluiert das Symbol Wolf was auf 'wuff verweist

(welcherNameGiltWo 'lily 'potter)
;gibt lily züruck, weil let PersonC als parameter zurückkomt,
;was als PersonA ('lily) definiert ist

(cdddr xs1)
;gibt die Liste (wuff Hund) zurück, weil cdddr die Elemente einer Liste
;nach dem dritten Element zurückgibt

(cdr xs2)
;gibt '(Flocki) zurück da die Liste ohne das erste Element zurückgegeben wird
;und Hund zu '(Flocki) evaluiert.

(cdr xs3)
;gibt '(Flocki) zurück

(sqrt 1/4)
;gibt 1/2 zurück was die Wurzel von 1/4 ist

(eval'(welcherNameGiltWo 'Wolf 'Hund))
;gibt 'Wolf zurück weil das Symbol '(welcherNameGiltWo 'Wolf 'Hund) evaluiert wird

(eval (welcherNameGiltWo 'Hund 'Wolf))
;gibt 'Flocki zurück weil 'Hund als Ergebniss der Funktion auf 'Flocki verweist. (????)


;;Aufgabe 2.1 Fakultät

(define (fak x)
  (cond [(= x 0) 1]
        [(< x 0) (error "natural number needed")]
        [(> x 0) (* (fak (sub1 x))x)]))


;;Aufgabe 2.2 Potenzen von Rationalzahlen

(define (power r n)
 (if(= n 0)
    1
  (if(odd? n)
    (*(power r (sub1 n)) r)
    (sqr(power r (/ n 2))))))


;; Aufgabe 2.3 Die Eulerzahl e

(define (e-aprox)
  (/
    (foldl + 0
      (for/list
        ([i (in-naturals 1)])
        #:break (< (/ i (fak (sub1 i))) (expt 10 -1000))
        (/ i (fak (sub1 i)))
      )
    ) 2
  )
)

(* (e-aprox) (expt 10 1001))

;;Aufgabe 3 Typprädikaten

(define (type-of object)
  (cond [(boolean? object) "boolean"]
        [(pair? object) "pair"]
        [(list? object) "list"]
        [(symbol? object) "symbol"]
        [(number? object) "number"]
        [(char? object) "char"]
        [(string? object) "string"]
        [(vector? object) "vector"]
        [(procedure? object) "procedure"]
        [else "nothing here"]))


(type-of (* 2 3 4))
;gibt number züruck weil die operation eine nummer liefert

(type-of (not 42))
;gibt boolean zurück weil eine einfache not-condition überpruft wird

(type-of '(eins zwei drei))
;gibt pair zurück

(type-of '())
;gibt list zurück, weil das objekt als die leere liste interpretiert wird

(define (id z) z)
(type-of (id sin))
;gibt procedure zurück

(type-of (string-ref "SE-3" 3))
;gibt char zurück weil string-ref als prozedur ein char liefert (wenn die bedienung erfüllt ist!)
;sonst bekommt man ein index is out of range fehlermeldung

(type-of (lambda (x) x))
;gibt procedure zurück, weil lambda eine prozedur erzeugt
;in diesem fall identität

(type-of type-of)
;gibt procedure zurück weil type-of als prozedur (durch ein define) in der code
;beschrieben wurde

(type-of (type-of type-of))
;gibt string zurück
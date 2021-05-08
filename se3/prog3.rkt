#lang racket
(require racket/gui/base)
(require se3-bib/flaggen-module)

; 6549385 Lorenzo Luciano
; 6930992 Franz Krekeler
; 6946325 Matz Radloff


;; Aufgabe 1.1
;; Gegeben ist eine Liste die aus Paaren von char, string besteht.
;; Dabei kann man später mit der Procedure "assoc" das erste Element eines Paares finden, was ein Schlüssel wäre.
;; Wir können also mit Hilfe eines Schlüssel das korrespondierende Element erhalten.
(define Buchstabiertafel
  '( (#\A "Alpha")
     (#\B "Bravo")
     (#\C "Charlie")
     (#\D "Delta")
     (#\E "Echo")
     (#\F "Foxtrot")
     (#\G "Golf")
     (#\H "Hotel")
     (#\I "India")
     (#\J "Juliet")
     (#\K "Kilo")
     (#\L "Lima")
     (#\M "Mike")
     (#\N "November")
     (#\O "Oscar")
     (#\P "Papa")
     (#\Q "Quebec")
     (#\R "Romeo")
     (#\S "Sierra")
     (#\T "Tango")
     (#\U "Uniform")
     (#\V "Viktor")
     (#\W "Whiskey")
     (#\X "X-ray")
     (#\Y "Yankee")
     (#\Z "Zulu")
     (#\0 "Nadazero")
     (#\1 "Unaone")
     (#\2 "Bissotwo")
     (#\3 "Terrathree")
     (#\4 "Kartefour")
     (#\5 "Pantafive")
     (#\6 "Soxisix")
     (#\7 "Setteseven")
     (#\8 "Oktoeight")
     (#\9 "Novenine")
     (#\, "Decimal")
     (#\. "Stop")))

;; Aufgabe 1.2
;; Die Funktion sucht das erste Element der Buchstabiertabelle Liste dessen "car"
;; ist gleich "c", wir bekommen dann das Zweite Element durch ein "cdr". In Prinzip ein char "c" wird auf seine Schlüssel abgebildet.
(define (char->key c)
  (cdr (assoc c Buchstabiertafel)))

;; Aufgabe 1.3
;; Hilfsfunktion: bildet die Kleinbuchstaben auf die entsprechenden Großbuchstaben ab
;; mit Hilfe der ASCII Code.
(define (lower->upper c)
  (char->key
   (if (char<=? #\a c #\z)
       (integer->char (- (char->integer c) 32))
       c)))

;; Die Funktion nimmt ein String und wird auf die Liste der Buchstabierschlüssel agebildet.
;; Aufruf der rekursiven Hilfsfunktion string->list
(define (buchstabieren text)
  (list-char (string->list text) lower->upper)
  )

;; Hilfsfunktion: Aufbau einer neuen Liste aus den substituierten Elementen. fn ist die Funktion, die den ersetzenden Wert zurückgibt
(define (list-char x fn)
  (if (= (length x) 1)
      (fn (car x))
      (append (fn (car x))
              (list-char (cdr x) fn))
      )
  )


;; Aufgabe 2.1 + Aufgabe 2.2
;; Statt der "assoc"-Methode nutzen wir eine Funktion, die einen Buchstaben direkt in den korrekten Identifier umwandelt.
;; Da die Flaggen-Identifier einzelne Buchstaben bzw. in der Form "Z{zahl}" sind, wandeln wir den Parameter vom Typ char in einen string um,
;; damit er mit string->symbol in ein Symbol umgewandelt werden kann, was dann evaluiert wird. Wenn char numerisch ist, fügen wir "Z" an, um den korrekten Identifier zu erhalten

(define (char->flag char)
  (cons (cond
    [(char-numeric? char) (eval (string->symbol (string-append "Z" (string char))))]
    [else (eval (string->symbol (string char)))]
  ) '())
)

;; Aufgabe 2.3
;; Wie in 1.3 nutzen wir die Funktion list-char um die Zeichen des Textes auf Flaggen abzubilden

(define (text->flag text)
  (list-char (string->list text) char->flag)
  )

;; Aufgabe 3.1
;; mittels string-append wird der korrekte Dateiname zurückgegeben
(define (char->morse_code_filename char)
  (string-append "Morse-" (string char) ".wav")
)

;; Aufgabe 3.2
;; das Ergebnis von char->morse_code_filename wird an play-sound als Parameter übergeben
(define (char->morse_code_sound char)
  (cons (play-sound (char->morse_code_filename char) #f)
       '())
)

;; Aufgabe 3.3
;; mithilfe der list-char Methode wird char->morse_code_sound für jedes Zeichen des strings text aufgerufen
(define (text->morse_code text)
  (list-char (string->list text) char->morse_code_sound)
)
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

(require se3-bib/prolog/prologInScheme)

;;;; iright: Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; Section 11.4 (The Zebra Problem)
(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest)) :-
    (iright ?left ?right ?rest))

; Das Rätsel aus: Harry Potter und der Stein der Weisen:
; Welche der sieben Flaschen enthalten Zaubertrank und welche Gift??
; Datenstruktur für eine Flasche:
; eine Liste der Form:
;( ?name ?inhalt)

; die sieben Rätselflaschen
; eine Liste mit Tupeln für jede der sieben Flaschen

; genau drei Giftflaschen
(<- (giftflaschen ?flaschen) :-
    (member (Gift1 Gift) ?flaschen)
    (member (Gift2 Gift) ?flaschen)
    (member (Gift3 Gift) ?flaschen))

; genau zwei Flaschen mit Nesselwein
(<- (nesselwein ?flaschen) :-
    (member (Wein1 Wein) ?flaschen)
    (member (Wein2 Wein) ?flaschen))

(<- (traenke ?flaschen) :-
    (member (TrankVoraus TrankVoraus)
            ?flaschen)
    (member (TrankZurueck TrankZurueck)
            ?flaschen))
(<- (riese ( ? ?inhalt)) :- (!= ?inhalt Gift))
(<- (zwerg ( ? ?inhalt)) :- (!= ?inhalt Gift))

(<- (verschieden ( ? ?inhalt1)( ? ?inhalt2)) 
    :- (not = ?inhalt1 ?inhalt2))
(<- (zwilling ( ? ?inhalt)( ? ?inhalt)))
(<- (keinFreundVoraus ( ? ?inhalt)) :- 
    (!= ?inhalt TrankVoraus))

(<- (raetsel ?flaschen) :-
    ; lege die Anzahl und Art der Flaschen fest
    (= ?flaschen (?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7))
    (giftflaschen ?flaschen)
    (nesselwein ?flaschen)
    (traenke ?flaschen)
    ; Einschränkungen aus dem Rätsel:    
    ;1. links vom Nesselwein steht Gift
    (iright (? Gift) (Wein1 Wein) ?flaschen)
    (iright (? Gift) (Wein2 Wein) ?flaschen)     
    ;2.  verschieden an den Enden
    (verschieden ?f1 ?f7)   
    ;2. kein Freund, wenn man weiter will
    (keinFreundVoraus ?f1)
    (keinFreundVoraus ?f7) 
    ; 3. Zwerg und Riese ungiftig
    (riese ?f7) 
    (zwerg ?f3);
    ; zwei links, zwei rechts Zwilling
    (zwilling ?f3 ?f5)       
    )

; eine Lösung
; (?- (raetsel (?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7)))

; alle Lösungen
; (?- (findall ?bs (raetsel ?bs) ?bss) (length ?bss ?num))
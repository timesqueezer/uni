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
;( ?name ?inhalt ?groesse)

; die sieben Rätselflaschen
; eine Liste mit Tripeln für jede der sieben Flaschen

; es gibt sieben unterschiedliche Groessen
(<- (groesse Klitzeklein))
(<- (groesse SehrKlein))
(<- (groesse MittelGross))
(<- (groesse MittelGross))
(<- (groesse MittelGross))
(<- (groesse SehrGross))
(<- (groesse Riesig))

; genau drei Giftflaschen
(<- (giftflaschen ?flaschen) :-
    (member (Gift1 Gift ?) ?flaschen)
    (member (Gift2 Gift ?) ?flaschen)
    (member (Gift3 Gift ?) ?flaschen)
    )

; genau zwei Flaschen mit Nesselwein
(<- (nesselwein ?flaschen) :-
    (member (Wein1 Wein ?) ?flaschen)
    (member (Wein2 Wein ?) ?flaschen)
    )

(<- (traenke ?flaschen) :-
    (member (TrankVoraus TrankVoraus  ?)
            ?flaschen)
    (member (TrankZurueck TrankZurueck  ?)
            ?flaschen)
    )
(<- (ungiftig Wein))
(<- (ungiftig TrankVoraus))
(<- (ungiftig TrankZurueck))

(<- (riese ( ? ?inhalt SehrGross)) :- (ungiftig ?inhalt))
(<- (riese ( ? ?inhalt Riesig)) :- (ungiftig ?inhalt))
(<- (zwerg ( ? ?inhalt SehrKlein)) :- (ungiftig ?inhalt))
(<- (zwerg ( ? ?inhalt Klitzeklein)) :- (ungiftig ?inhalt))

(<- (verschieden ( ? ?inhalt1 ?)( ? ?inhalt2 ?)) 
    :- (not = ?inhalt1 ?inhalt2))
(<- (zwilling ( ? ?inhalt ?)( ? ?inhalt ?)))
(<- (keinFreundVoraus ( ? ?inhalt ?)) :- (member ?inhalt (Gift Wein TrankZurueck)))
  ;  (!= ?inhalt TrankVoraus))

(<- (raetsel ?flaschen) :-
    ; lege die Anzahl und Art der Flaschen fest
    (= ?flaschen (?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7))
    (= ?flaschen 
       ((? ? MittelGross1)
        (? ? MittelGross2)
        (? ? Riesig)
        (? ? MittelGross3)
        (? ? SehrKlein)
        (? ? Klitzeklein)
        (? ? SehrGross)))       
    (giftflaschen ?flaschen)
    (nesselwein ?flaschen)
    (traenke ?flaschen)
    ; Einschränkungen aus dem Rätsel:    
    ;1. links vom Nesselwein steht Gift
    (iright (? Gift ?) (Wein1 Wein ?) ?flaschen)
    (iright (? Gift ?) (Wein2 Wein ?) ?flaschen)     
    ;2.  verschieden an den Enden
    (verschieden ?f1 ?f7)   
    ;2. kein Freund, wen man weitergeht
    (keinFreundVoraus ?f1)
   (keinFreundVoraus ?f7)

    ; 3. Zwerg und Riese ungiftig
    ; zwei links, zwei rechts Zwilling
    (zwilling ?f3 ?f5) 
    (zwerg ?f6)
    (riese ?f7)
    (riese ?f3)
       
    )

; (?- (raetsel (?f1 ?f2 ?f3 ?f4 ?f5 ?f6 ?f7)))
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

; (parents mother father child))

; Die Eltern von Peter
(<- (parents Vera Viktor Peter))

; Die Großltern von Peter
(<- (parents Maja Martin Vera))
(<- (parents Mona Mike Viktor))

; Die Urgroßltern von Peter
(<- (parents Nora Norbert Maja))
(<- (parents Nina Nick Martin))
(<- (parents Nana Stefan Mona))
(<- (parents Nena Alfons Mike))

; Petras und Peters Kinder

(<- (parents Petra Peter Wilma))
(<- (parents Petra Peter Werner))
(<- (parents Petra Peter Winnie))
(<- (parents Petra Peter Walter))
(<- (parents Petra Peter Wilfried))

;Projektionen
(<- (mother ?mother ?child)
    :- (parents ?mother ? ?child))
(<- (father ?father ?child)
    :- (parents ? ?father ?child))

(<- (parent ?mother ?child)
    :- (parents ?mother ? ?child))
(<- (parent ?father ?child)
    :- (parents ? ?father ?child))

(<- (granny ?gran ?grandchild)
    :- (parent ?gran ?parent)
    (parent ?parent ?grandchild))

(<- (predecessor ?predc ?person) 
    :- (parent ?predc ?person))

(<- (predecessor ?predc ?person) 
    :- (parent ?predc ?parent)
    (predecessor ?parent ?person))

(<- (siblingA ?x ?y) :- 
    (parents ?m ?f ?x)
    (parents ?m ?f ?y)
    )
(<- (sibling ?x ?y) :- 
    (parents ?m ?f ?x)
    (parents ?m ?f ?y)
    (!= ?x ?y) ; oder (not = ?x ?y)
    )

; Die Gesschwister von Walter:
; (?- (sibling ?x Walter))
  
; Die Liste aller Vorfahren
(<- (allAncestors ?person ?ancs) :- 
 (findall ?predc (predecessor ?predc ?person) ?ancs))

; (?- (count ?numb (predecessor ?predc Peter))); Anzahl der Vorfahren von Peter

; (?- (count ?numSiblings (sibling ? Walter))); Anzahl der Geschwister von Walter

; (?-  ( ! sibling ?x Walter)) ; cut, only one answer
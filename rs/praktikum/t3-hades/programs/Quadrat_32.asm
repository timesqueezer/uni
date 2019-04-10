;====================================
; Ihr Hauptprogramm
;====================================
Start:
      jsr LDR5
      .defw	1234
      jsr nh2      
      mov	r9, r6
      mov	r10, r7
      halt
;=================================================
LDR5:
      ldw    r5, (r15)
      addi   r15, 2
      jmp    r15
;=================================================
; Unterprogramm zur Berechnung des Quadrats
;================================================ 
nh2:
      movi   r6, 0
      movi   r7, 0
      movi   r8, 0
      movi   r2, 0
      cmpne r8, r8 ; initialize carry
nh2loop:
      cmpe   r8, r5
      bt     nh2ende
      addc   r7, r8
      addc   r6, r2
      addc   r7, r8
      addc   r6, r2
      addi   r8, 1
      br     nh2loop
nh2ende:
     cmpne r8, r8
     addc   r7, r5
     addc   r6, r2
     jmp    r15
     
     

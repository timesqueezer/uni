;====================================
; Ihr Hauptprogramm
;====================================
Start:
      movi	r5, 5
      jsr nh2      
      mov	r9, r7
      movi	r5, 8
      lsli	r5, 2
      addi	r5, 8
      addi	r5, 15
      jsr nh2
      mov	r10, r7
      lsli	r5, 1
      subi	r5, 9
      jsr nh2
      mov	r11, r7
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
      movi   r7, 0
      movi   r8, 0
nh2loop:
      cmpe   r8, r5
      bt     nh2ende
      addu   r7, r8
      addu   r7, r8
      addi   r8, 1
      br     nh2loop
nh2ende:
     addu   r7, r5
     jmp    r15
     
     

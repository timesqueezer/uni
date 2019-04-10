Start:
    movi    R0, 0   ; initialize stackpointer
    movi    R1, 8   ; n = 3
    movi    R2, 1   ; i = 1
    movi    R3, 3   ; j = 3
    movi    R4, 1   ; 1 to compare
    jsr     towers
    halt
towers:
    .push R15
    cmpe   R4, R1  ; if (n == 1)
    bf      else    ; ifn goto else
    .prdez  R2
    .prdez  R3
    .prnewline
    .pop    R15
    jmp     R15
else:
    movi    R5, 6   ; k = 6
    subu     R5, R2  ; k -= i
    subu     R5, R3  ; k -= j
    .push   R1
    .push   R2
    .push   R3
    .push   R5
    subi    R1, 1   ; n -= 1
    mov     R3, R5  ; j = k
    jsr     towers
    .pop    R5
    .pop    R3
    .pop    R2
    .pop    R1
    .push   R1
    .push   R2
    .push   R3
    .push   R5
    movi    R1, 1
    jsr     towers
    .pop    R5
    .pop    R3
    .pop    R2
    .pop    R1
    .push   R1
    .push   R2
    .push   R3
    .push   R5
    subi    R1, 1
    mov     R2, R5
    jsr     towers
    .pop    R5
    .pop    R3
    .pop    R2
    .pop    R1
    .pop    R15
    jmp     R15

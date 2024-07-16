    xref sintab_plasma
    xref costab_plasma
    xdef _zxplasma_draw
    ; a0: pointer to coppechunky copperlist
    ; a1: pointer to palette (128 entries atm)
    ; a2: delta x
    ; a3: delta y
    ; d0: start color
    ; d1: height
    ; d2: start x
    ; d3: start y
    ; all other registers are preserved
_zxplasma_draw:
    ; allocate registers
    move.l      d1,d7                   ; d7 - y counter
    move.l      d0,a4                   ; a4 - start color
    lea         costab_plasma,a5
    lea         sintab_plasma,a6
    move.w      #$1E,d4                 ; d4 - mask
    move.l      d2,d5                   ; d5 - start x

    ; y loop
.y_loop:
    move.l      a4,d1                   ; d1 = sc
    move.l      d3,d0                   ; 
    lsr.w       #8,d0                   ; d0 = y >> 8
    add.b       (a5,d0.w),d1            ; d1 = sc + cos[y]

    ; unrolled 31 pixels copperchunky writer
    move.l      d5,d2                   ; get start x
    rept 31
    move.l      d2,d0                   ; d0 = x
    lsr.w       #8,d0                   ; d0 = x >> 8
    move.b      (a6,d0.w),d0            ; d0 = sin[x]
    add.b       d1,d0                   ; d0 = sc + cos[y] + sin[x]
    ;lsr.b       #3,d0                   ;      calculate...
    ;and.w       d4,d0                   ;      ...index in palette
    move.w      (a1,d0.w),(REPTN*4)(a0) ; *copPtr = pal[d0]
    add.w       a2,d2                   ; d2 = x += dx
    endr

    add.w       a3,d3                   ; d4 = y += dy
    adda.l      #128,a0                 ; a0 = copPtr for next line
    dbra        d7,.y_loop

    rts









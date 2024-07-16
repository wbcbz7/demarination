    xref stars_buf
    xref stars_persp_table
    xdef _stars_draw_stars

    ; a0: bitplane 0 pointer (+64 - bpl1, pitch = 128)
    ; d0: total stars count-1 (?)
    ; d1: star position increment
_stars_draw_stars:
    move.l      D0,D7
    move.l      D1,D6
    move.w      #$7FFE,d5
    lea         stars_buf,A1
    lea         stars_persp_table,A2
    lea         stars_persp_table+256*64*2,A3
.starloop:
    ; load star data
    ; d0: [x z] [y 0]
    move.l      (a1),d0
    move.l      d0,d2       ; d2 - temp Y part
    ; fetch X value from perspective table
    and.w       d5,d0  ; clear top overflow bits
    move.w      (a2,d0.w),d1
    ; switch to Y, fetch Y
    lsr.l       #8,d2       ; d0 = [z y 0 0]
    and.w       d5,d2  ; clear low bit
    move.w      (a3,d2.w),d0

    ; clipping
    btst        #15,d0
    bne         .skip_draw

    ; draw the star
    ; d0 - y, d1 - x
    moveq       #-$80,d4
    ror.b       d1,d4
    lsr.w       #3,d1
    add.w       d0,d1
    
    ; bpl1
    btst        #6,d2
    beq         .skip_bpl1
    or.b        d4,64(a0,d1.w)
    ; plot at bpl0 (always)
    btst        #5,d2
    beq         .skip_bpl0
    or.b        d4,(a0,d1.w)
.skip_bpl0:

.skip_draw:
    ; advance the star
    add.l       d6,(a1)
.skip_reset:
    ; store star position
    adda        #4,a1
    dbra        d7,.starloop

    ; done
    rts

    ; always plot at bpl0
.skip_bpl1:
    or.b        d4,(a0,d1.w)
    ; advance the star
    add.l       d6,(a1)
    ; store star position
    adda        #4,a1
    dbra        d7,.starloop

    ; done
    rts
    
    lsl.w       #7,d0
    lsl.w       #8,d0
    ror.w       #1,d0
    add.w       a0,d0
    addi.w      #-40,d0





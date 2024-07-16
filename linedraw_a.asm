; https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw3/
; Subroutine linedraw
; If both points are equal, then no line is drawn.
; Input:
;   d0: x1 x-coordinate of first point
;   d1: y1 y-coordinate of first point
;   d2: x2 x-coordinate of second point
;   d3: y2 y-coordinate of second point
;   d4: screen pitch in bytes
;   a0: pointer to the bitplane

    xdef _linedraw_a
    xdef _linedraw_a_p64
    xdef blt_scratch

_linedraw_a:                      ; subroutine linedraw
    lea     blt_scratch,a5        ; load scratch area
    cmp.w	d0,d2                 ; compare x1 and x2
    bne.s	ld_not1pix            ; if x1 != x2 goto ld_not1pix
    cmp.w	d1,d3                 ; compare y1 and y2
    bne.s	ld_not1pix            ; if y1 != y2 goto ld_not1pix 
    rts                           ; return from subroutine
ld_not1pix:                       ; label, both points of line is not the same pixel
    lea.l   octant_areafill(pc),a1         ; a1 - octant address
    lea.l   $dff000,a2            ; a2 - custom chip registers
    move.l  d4,d6                 ; d6 - screen pitch
    moveq	#0,d7                 ; clear octant index d7
    sub.w	d0,d2                 ; store dx=x2-x1 in d2
    bge.s	ld_xok                ; if dx>=0 goto ld_xok
    neg.w	d2                    ; store -dx in d2
    addq.w	#2,d7                 ; add 2 to octant index d7 (dx < 0)
ld_xok:                        ; label, dx is OK
    sub.w	d1,d3              ; store dy=y2-y1 in d3
    bge.s	ld_yok             ; if dy>=0 goto ld_yok
    neg.w	d3                 ; store -dy in d3
    addq.w	#4,d7              ; add 4 to octant index d7 (dy < 0)
ld_yok:                        ; label, dy is OK
    cmp.w	d3,d2              ; compare dy and dx
    bgt.s	ld_xyok            ; if dx > dy goto ld_xyok 
    ; bne.s	ld_not45           ; FIX: remove code
    ; add.w	#16,d7             ; FIX: remove code
ld_not45:                      ; Label line is not 45 degrees
    exg	d2,d3                  ; exchange dx and dy so that dx is largest
    addq.w	#8,d7              ; add 8 to octant index d7 (dy > dx)
ld_xyok:                       ; label, dx and dy is OK
    add.w	d3,d3              ; 2dy to d3 
    move.w	d3,d4              ; 2dy to d4
    cmp.w	d3,d2              ; FIX: compare d3=2dy with d2=dx
    bgt.s	ld_sign_ok         ; FIX: if dx > 2dy goto ld_sign_ok
    add.w	#16,d7             ; FIX: no sign is needed 2dy - dx >= 0    
ld_sign_ok:
    sub.w	d2,d4              ; 2dy-dx to d4
    add.w	d3,d3              ; 4dy to d3
    move.w	d3,a3              ; 4dy to a3
    add.w	d2,d2              ; 2dx in d2
    add.w	d2,d2              ; 4dx in d2
    sub.w	d2,d3              ; 4dy - 4dx in d3
    mulu	d6,d1              ; convert y1 coordinate to byte offset
    move.l	a0,a4              ; Screen pointer to a4
    add.w	d1,a4              ; Screen + y1 to a4
    move.w	d0,d1              ; x1 to d1
    lsr.w	#3,d1              ; convert x1 coordinate to byte offset (x1/8)
    add.w	d1,a4              ; Screen + y1 + x1 to a4.
    andi.w	#$f,d0             ; d0 sets BLTCON0 / BLTCON1 Keep the first four bits of x1
    ror.w	#4,d0              ; Within a word rotate right 4 bits.
    add.w	#$b4a,d0           ; FIX: Add values to three lowest nibbles 
    swap	d0                 ; Swap the words in d0
    move.w	(a1,d7.w),d0       ; move octant value at d7 into d0
    lsl.w	#4,d2              ; d2 sets BLTSIZE bit 15-6 holds dx  
    add.w	#$40,d2            ; FIX: bit 15-6 now holds dx + 1    
    addq.w	#2,d2              ; bit 5-0 holds 2
ld_wldraw:
    btst	#6,$2(a2)          ; DMACONR test Blitter DMA enable
    bne.s	ld_wldraw          ; if not set then goto ld_wldraw
    move.l	d0,$40(a2)         ; BLTCON0 / BLTCON1
    move.w	d3,$64(a2)         ; BLTAMOD
    move.w	a3,$62(a2)         ; BLTBMOD
    move.w	d4,$52(a2)         ; BLTAPTL
    move.l	a4,$48(a2)         ; BLTCPTH / BLTCPTL
    move.l	a5,$54(a2)         ; BLTDPTH / BLTDPTL
    move.w	d2,$58(a2)         ; BLTSIZE
    rts

octant:
    dc.w	$0051,$0055,$0059,$005d
    dc.w	$0041,$0049,$0045,$004d
    dc.w	$0011,$0015,$0019,$001d
    dc.w	$0001,$0009,$0005,$000d

octant_areafill:
    dc.w	$0053,$0057,$005b,$005f
    dc.w	$0043,$004b,$0047,$004f
    dc.w	$0013,$0017,$001b,$001f
    dc.w	$0003,$000b,$0007,$000f

    ; fixed 64 byte pitch version
    ; assumes Y1 <= Y2!
    ; Input:
    ;   d0: x1 x-coordinate of first point
    ;   d1: y1 y-coordinate of first point
    ;   d2: x2 x-coordinate of second point
    ;   d3: y2 y-coordinate of second point
    ;   a0: pointer to the bitplane
    ;   clobbers d0-d3, a3, a4, a5, a6, a7
_linedraw_a_p64:                      ; subroutine linedraw
    cmp.w	d0,d2                 ; compare x1 and x2
    bne.s	ld64_not1pix            ; if x1 != x2 goto ld_not1pix
    cmp.w	d1,d3                 ; compare y1 and y2
    bne.s	ld64_not1pix            ; if y1 != y2 goto ld_not1pix 
    rts                           ; return from subroutine
ld64_not1pix:                       ; label, both points of line is not the same pixel
    lea     blt_scratch,a5        ; load scratch area
    lea.l   octant_areafill(pc),a3         ; a3 - octant address
    lea.l   $dff000,a6            ; a6 - custom chip registers
    moveq	#0,d7                 ; clear octant index d7
    sub.w	d0,d2                 ; store dx=x2-x1 in d2
    bge.s	ld64_xok                ; if dx>=0 goto ld_xok
    neg.w	d2                    ; store -dx in d2
    addq.w	#2,d7                 ; add 2 to octant index d7 (dx < 0)
ld64_xok:                        ; label, dx is OK
    sub.w	d1,d3              ; store dy=y2-y1 in d3
;    bge.s	ld64_yok             ; if dy>=0 goto ld_yok
;    neg.w	d3                 ; store -dy in d3
;    addq.w	#4,d7              ; add 4 to octant index d7 (dy < 0)
ld64_yok:                        ; label, dy is OK
    cmp.w	d3,d2              ; compare dy and dx
    bgt.s	ld64_xyok            ; if dx > dy goto ld_xyok 
    exg	d2,d3                  ; exchange dx and dy so that dx is largest
    addq.w	#8,d7              ; add 8 to octant index d7 (dy > dx)
ld64_xyok:                       ; label, dx and dy is OK
    add.w	d3,d3              ; 2dy to d3 
    move.w	d3,d4              ; 2dy to d4
    cmp.w	d3,d2              ; FIX: compare d3=2dy with d2=dx
    bgt.s	ld64_sign_ok         ; FIX: if dx > 2dy goto ld_sign_ok
    add.w	#16,d7             ; FIX: no sign is needed 2dy - dx >= 0    
ld64_sign_ok:
    sub.w	d2,d4              ; 2dy-dx to d4
    add.w	d3,d3              ; 4dy to d3
    move.w	d3,d5              ; 4dy to d5
    add.w	d2,d2              ; 2dx in d2
    add.w	d2,d2              ; 4dx in d2
    sub.w	d2,d3              ; 4dy - 4dx in d3
    lsl.w   #6,d1              ; convert y1 coordinate to byte offset
    move.l	a0,a4              ; Screen pointer to a4
    add.w	d1,a4              ; Screen + y1 to a4
    move.w	d0,d1              ; x1 to d1
    lsr.w	#3,d1              ; convert x1 coordinate to byte offset (x1/8)
    add.w	d1,a4              ; Screen + y1 + x1 to a4.
    andi.w	#$f,d0             ; d0 sets BLTCON0 / BLTCON1 Keep the first four bits of x1
    ror.w	#4,d0              ; Within a word rotate right 4 bits.
    add.w	#$b4a,d0           ; FIX: Add values to three lowest nibbles 
    swap	d0                 ; Swap the words in d0
    move.w	(a3,d7.w),d0       ; move octant value at d7 into d0
    lsl.w	#4,d2              ; d2 sets BLTSIZE bit 15-6 holds dx  
    add.w	#$42,d2            ; FIX: bit 15-6 now holds dx + 1    
;    addq.w	#2,d2              ; bit 5-0 holds 2
ld64_wldraw:
    btst	#6,$2(a6)          ; DMACONR test Blitter DMA enable
    bne.s	ld64_wldraw          ; if not set then goto ld_wldraw
    move.l	d0,$40(a6)         ; BLTCON0 / BLTCON1
    move.w	d3,$64(a6)         ; BLTAMOD
    move.w	d5,$62(a6)         ; BLTBMOD
    move.w	d4,$52(a6)         ; BLTAPTL
    move.l	a4,$48(a6)         ; BLTCPTH / BLTCPTL
    move.l	a5,$54(a6)         ; BLTDPTH / BLTDPTL
    move.w	d2,$58(a6)         ; BLTSIZE
    rts

    move.w	d0,d1              ; x1 to d1
    lsr.w	#3,d1              ; convert x1 coordinate to byte offset (x1/8)
    add.w	d1,a4              ; Screen + y1 + x1 to a4.
    andi.w	#$f,d0             ; d0 sets BLTCON0 / BLTCON1 Keep the first four bits of x1
    ror.w	#4,d0              ; Within a word rotate right 4 bits.
    add.w	#$b4a,d0           ; FIX: Add values to three lowest nibbles 

    add d0,d0
    add d0,d0
    move.w (a0,d0.w),d1
    move.w 2(a0,d0.w),d2


    xdef _animdraw_a
    ; draw animation
    ; in:  a0 - screen, a1 - animation data
    ; out: a1 - current animation pointer, or 0 if end of anim
    ; assumes blitter is set up for line drawing
_animdraw_a:

.frame_loop:
    ; get number of frames or end of frame marker
    move.w  (a1)+,d6
    cmp.w   #$FFFE,d6
    bhs     .end_of_frame
    ; d6 - number of vertices in frame
    move.l  a1,a2        ; save vtx0 pointer in a2
    sub.w   #2,d6
    bcs     .frame_loop  ; no vertices - skip shape
.vtx_loop:
    ; render 0..n-1
    move.w  0(a1),d0
    move.w  2(a1),d1
    move.w  4(a1),d2
    move.w  6(a1),d3
    cmp.w   d1,d3       ; y1>y2?
    bcc     .no_swap
    exg     d0,d2
    exg     d1,d3
.no_swap:
    jsr     _linedraw_a_p64
    adda    #4,a1
    dbra    d6,.vtx_loop

    ; render last line
    move.w  0(a1),d0
    move.w  2(a1),d1
    move.w  0(a2),d2
    move.w  2(a2),d3
    cmp.w   d1,d3       ; y1>y2?
    bcc     .no_swap_last
    exg     d0,d2
    exg     d1,d3
.no_swap_last:
    jsr     _linedraw_a_p64
    adda    #4,a1
    bra     .frame_loop

.end_of_frame:
    cmp.w    #$FFFE,d6
    bne      .end_of_anim
    rts

.end_of_anim:
    lea     0,a1
    rts

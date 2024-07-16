#include "support/gcc8_c_support.h"
#include "main.h"
#include "spritegen.h"
#include <proto/graphics.h>
#include <hardware/dmabits.h>
#include <hardware/custom.h>

void sprite_cut(USHORT* dst, USHORT *bpl0, USHORT *bpl1, USHORT mod0, USHORT mod1, USHORT height, USHORT x, USHORT y) {
    USHORT *dst0 = dst + 2;
    USHORT *dst1 = dst + 3;
    USHORT end_y = y + height;

    // copy bpl0 
    WaitBlit();
    USHORT  bltsize = (height << HSIZEBITS) | (16/16);
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltamod = mod0;
    custom->bltdmod = 2;        // sprite graphics is 2bpl interleaved 
    custom->bltafwm = 0xFFFF;
    custom->bltalwm = 0xFFFF;
    custom->bltapt  = bpl0;
    custom->bltdpt  = dst0;
    custom->bltsize = bltsize;

    // set control word
    *(dst + 0) = ((y & 0xFF) << 8)     | ((x >> 1) & 0xFF);
    *(dst + 1) = ((end_y & 0xFF) << 8) | (x & 1) | ((y & 0x100) >> 6) | ((end_y & 0x100) >> 7);

    // copy bpl1
    WaitBlit();
    custom->bltamod = mod1;
    custom->bltapt  = bpl1;
    custom->bltdpt  = dst1;
    custom->bltsize = bltsize;

    // mark end of sprite
    *(ULONG*)(dst + 2 + (height<<1)) = 0;
}
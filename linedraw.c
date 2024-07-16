#include "support/gcc8_c_support.h"
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/graphics.h>
#include <graphics/gfxbase.h>
#include <graphics/view.h>
#include <exec/execbase.h>
#include <graphics/gfxmacros.h>
#include <hardware/custom.h>
#include <hardware/dmabits.h>
#include <hardware/intbits.h>

#include "main.h"
#include "linedraw.h"

// scratch buffer to skip 1st pixel
WORD blt_scratch[256] __attribute__((section (".MEMF_CHIP")));

void blt_line_setup(int pitch) {
    custom->bltafwm = custom->bltalwm = 0xFFFF;
    custom->bltadat = 0x8000;
    custom->bltbdat = 0xFFFF;
    custom->bltcmod = custom->bltdmod = pitch;
}

void blt_draw_line(UBYTE *bpl, int pitch, int x1, int y1, int x2, int y2) {
    register volatile const void* _a0 ASM("a0") = bpl;
    register                int   _d0 ASM("d0") = x1;
    register                int   _d1 ASM("d1") = y1;
    register                int   _d2 ASM("d2") = x2;
    register                int   _d3 ASM("d3") = y2;
    register                int   _d4 ASM("d4") = pitch;

    __asm volatile (
        "movem.l %%d0-%%d7/%%a1-%%a6,-(%%sp)\n"
        "jsr _linedraw_a\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a1-%%a6"
    : "+rf" (_d0), "+rf" (_d1), "+rf" (_d2), "+rf" (_d3), "+rf" (_d4), "+rf"(_a0)
    :
    : "cc", "memory");
    return;
}

void blt_draw_line_pitch64(UBYTE *bpl, int x1, int y1, int x2, int y2) {
    register volatile const void* _a0 ASM("a0") = bpl;
    register                int   _d0 ASM("d0") = x1;
    register                int   _d1 ASM("d1") = y1;
    register                int   _d2 ASM("d2") = x2;
    register                int   _d3 ASM("d3") = y2;

    __asm volatile (
        "movem.l %%d2-%%d7/%%a1-%%a6,-(%%sp)\n"
        "jsr _linedraw_a_p64\n"
        "movem.l (%%sp)+,%%d2-%%d7/%%a1-%%a6"
    : "+rf" (_d0), "+rf" (_d1), "+rf" (_d2), "+rf" (_d3), "+rf"(_a0)
    :
    : "cc", "memory");
    return;
}

#define imul16(a, b) (((a) * (b)) >> 8L)
#define idiv16(a, b) (((a) << 8L) / (b))

// clipper
int polyclip(struct vec2w *out, struct vec2w *in, SHORT in_count,
             int x1, int x2, int y1, int y2) {
    volatile struct vec2w *src, *dst, *dst0;

    // process x1
    src = in, dst = dst0 = out;
    for (int i = 0; i < in_count; i++) {
        int j = (i + 1); if (j >= in_count) j -= in_count;
        struct vec2w *vi = src + i;
        struct vec2w *vj = src + j;
        
        if (vi->x >= (x1)) {
            *dst++ = *vi;
        }

        if (((vi->x >  (x1)) && (vj->x < (x1))) || 
            ((vj->x >= (x1)) && (vi->x < (x1)))) {
            LONG k = idiv16((LONG)(x1 - vi->x), (LONG)(vj->x - vi->x));
            dst->x = x1;
            dst->y = vi->y + imul16(k, (LONG)(vj->y - vi->y));
            dst++;
        }
    }
    in_count = dst - dst0;

    // process x2
    src = out, dst = dst0 = in;
    for (int i = 0; i < in_count; i++) {
        int j = (i + 1); if (j >= in_count) j -= in_count;
        struct vec2w *vi = src + i;
        struct vec2w *vj = src + j;
        
        if (vi->x < (x2)) {
            *dst++ = *vi;
        }

        if (((vi->x <  (x2)) && (vj->x >= (x2))) || 
            ((vj->x <  (x2)) && (vi->x >= (x2)))) {
            LONG k = idiv16((LONG)(x2 - vi->x), (LONG)(vj->x - vi->x));
            dst->x = x2;
            dst->y = vi->y + imul16(k, (LONG)(vj->y - vi->y));
            dst++;
        }
    }
    in_count = dst - dst0;

    // process y1
    src = in, dst = dst0 = out;
    for (int i = 0; i < in_count; i++) {
        int j = (i + 1); if (j >= in_count) j -= in_count;
        struct vec2w *vi = src + i;
        struct vec2w *vj = src + j;
        
        if (vi->y >= (y1)) {
            *dst++ = *vi;
        }

        if (((vi->y >  (y1)) && (vj->y < (y1))) || 
            ((vj->y >= (y1)) && (vi->y < (y1)))) {
            LONG k = idiv16((LONG)(y1 - vi->y), (LONG)(vj->y - vi->y));
            dst->y = y1;
            dst->x = vi->x + imul16(k, (LONG)(vj->x - vi->x));
            dst++;
        }
    }
    in_count = dst - dst0;

    // process y2
    src = out, dst = dst0 = in;
    for (int i = 0; i < in_count; i++) {
        int j = (i + 1); if (j >= in_count) j -= in_count;
        struct vec2w *vi = src + i;
        struct vec2w *vj = src + j;
        
        if (vi->y < (y2)) {
            *dst++ = *vi;
        }

        if (((vi->y <  (y2)) && (vj->y >= (y2))) || 
            ((vj->y <  (y2)) && (vi->y >= (y2)))) {
            LONG k = idiv16((LONG)(y2 - vi->y), (LONG)(vj->y - vi->y));
            dst->y = y2;
            dst->x = vi->x + imul16(k, (LONG)(vj->x - vi->x));
            dst++;
        }
    }
    in_count = dst - dst0;
    //memcpy(out, in, sizeof(struct vec2x) * in_count);

    return in_count;
}

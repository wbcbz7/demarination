#include "../support/gcc8_c_support.h"
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

// includes
#include "../main.h"
#include "../copper.h"
#include "../resources.h"
#include "../tables.h"
#include "copperchunky.h"

#define X_RES           256
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH         256
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT           5
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT)

#define BPL_MODULO_BYTE     (X_PITCH_BPL_BYTE - (X_RES/8))
#define BPL_MODULO_WORD     (X_PITCH_BPL_WORD - (X_RES/16))

#define Y_RES               192
#define Y_PITCH             192
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT)

#define X_FRAME 248
#define Y_FRAME 192

#define X_GRID (X_FRAME/8)
#define Y_GRID (Y_FRAME/4)

// -------------------------
// resources
INCBIN_CHIP(copperchunky, "gfx/copperchunky.bpl")
INCBIN(copperchunky_palette, "gfx/copperchunky.pal")

static USHORT speccypal_fade[8][16] = {
    // full gray
    {0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC},
    // fade to regular palette
    {0xC0C,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xCCC,0xC0C,0xC0C,0xC0C,0xC0C,0xCCC,0xCCC,0xCCC,0xCCC},
    {0x00C,0xC0C,0xC0C,0xC0C,0xC0C,0xCCC,0xCCC,0xCCC,0x00C,0xC0F,0xF0C,0xC0C,0x0CC,0x0CC,0xCC0,0xCCC},
    // regular palette
    {0x000,0x00C,0xC00,0xC0C,0x0C0,0x0CC,0xCC0,0xCCC,0x000,0x00F,0xF00,0xF0F,0x0F0,0x0FF,0xFF0,0xFFF},
    // fade to black
    {0x000,0x00C,0xC00,0xC0C,0x0C0,0x0CC,0xCC0,0xCCC,0x000,0x00F,0xF00,0xF0F,0x0F0,0x0FF,0xFF0,0xFFF},
    {0x000,0x00C,0xC00,0x00C,0x0C0,0x00C,0xC00,0xC0C,0x000,0x00C,0xC00,0xC0C,0x0C0,0x0CC,0xCC0,0xCCC},
    {0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x00C,0x000,0x00C,0xC00,0x00C,0x0C0,0x00C,0xC00,0xC0C},
    {0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000,0x000},
};

// updated plasma tables
signed char costab_plasma[256], sintab_plasma[256];

// palette table
static USHORT plasma_palette[8][128];

// copperlist
static USHORT* copper_main;
static USHORT* copper_pal;
static USHORT* copper_bplstart;
static volatile USHORT* coplist_color0;
static USHORT* copper_scroll[2];

// define this to allocate copperlist in bitplaane pool memory
#define UNSAFE_COPPERLIST_ALLOC

#ifdef UNSAFE_COPPERLIST_ALLOC
#define COPPERLIST_OFFSET (40 * 1024)
#endif

int copperchunky_free()
{
    FreeMem(copper_main, 256);
#ifndef UNSAFE_COPPERLIST_ALLOC
    FreeMem(copper_scroll[0], 6800);
    FreeMem(copper_scroll[1], 6800);
#endif
    return 0;
}

// called during demo init
int copperchunky_init() {
    // fill sin/cos tables
    for (int i = 0; i < 256; i++) {
        costab_plasma[i] = costab_256[i] & 0xFE;
        sintab_plasma[i] = sintab_256[i] & 0xFE;
    }

    // fill palette table
    for (int c = 0; c < 8; c++) {
        for (int i = 0; i < 128; i++) {
            plasma_palette[c][i] = speccypal_fade[c][i >> 3];
        }
    }

    // allocate copperlist memory
    copper_main         = (USHORT*)AllocMem(256,  MEMF_CHIP | MEMF_CLEAR);
#ifdef UNSAFE_COPPERLIST_ALLOC
    copper_scroll[0]    = (USHORT*)(bpl_pool + COPPERLIST_OFFSET);
    copper_scroll[1]    = (USHORT*)(bpl_pool + COPPERLIST_OFFSET + 6800);
#else
    copper_scroll[0]    = (USHORT*)AllocMem(6800, MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[1]    = (USHORT*)AllocMem(6800, MEMF_CHIP | MEMF_CLEAR);
#endif

    if (!copper_main || !copper_scroll[0] || !copper_scroll[1]) return 1;

    // initialize copper list
    USHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 1024, 0);

    // init data fetch/display window
    copPtr = setLowres256Wide(copPtr, Y_RES);
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*single pf*/|(1<<9)/*color*/|(5<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0;

    // set bitplane modulo
	*copPtr++=offsetof(struct Custom, bpl1mod);
	*copPtr++=(USHORT)(-X_RES_BYTE);
	*copPtr++=offsetof(struct Custom, bpl2mod); 
	*copPtr++=(USHORT)(-X_RES_BYTE);

    // set pf0 bitplane pointers
    copper_bplstart = copPtr + 1;
    for (int i = 0; i < BPL_COUNT; i++) {
        ULONG bpl_ptr = (ULONG)copperchunky + (i * X_RES_BYTE);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 0;
        *copPtr++ = (APTR)(bpl_ptr >> 16);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 2;
        *copPtr++ = (APTR)(bpl_ptr & 0xFFFF);
    }
    // color 0 point
    coplist_color0 = copPtr;
    COPPERLIST_ADD_MOVE(copPtr, color[0], 0xCCC);
    // switch to 2nd copperlist
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);
    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

#if 1
    // fill copperlist    
    for (int i = 0; i < 2; i++) {
        copPtr = copper_scroll[i];

        // fill scroll DMA
        // TODO: add pf1/sprite scroll
        int palfix = 1;
        for (int i = 0; i < Y_RES; i += 4) {
#if 0       // enable if copperlist never touches line 255
            // add PAL fix after line 255
            if ((44+((256-Y_RES)>>1)+i-1 > 255) && palfix) {
                *copPtr++=0xffdf;
                *copPtr++=0xfffe;
                palfix = 0;
            }
#endif
            COPPERLIST_ADD_WAIT(copPtr, (64+X_RES)>>2, 44+((256-Y_RES)>>1)+i-1, -1);
            for (int c = 1; c < 32; c++) {
                COPPERLIST_ADD_MOVE(copPtr, color[c], 0xCCC);
            }
        }

        // terminate copperlist
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }
#endif

    return 0;
}

static void plasma_draw_asm(USHORT* copPtr, USHORT *palette, USHORT dx, USHORT dy,
                            USHORT sc,      USHORT height,   USHORT sx, USHORT sy) {
    register volatile const void* _a0 ASM("a0") = copPtr;
    register volatile const void* _a1 ASM("a1") = palette;
    register                int   _a2 ASM("a2") = dx;
    register                int   _a3 ASM("a3") = dy;
    register                int   _d0 ASM("d0") = sc;
    register                int   _d1 ASM("d1") = height;
    register                int   _d2 ASM("d2") = sx;
    register                int   _d3 ASM("d3") = sy;

    __asm volatile (
        "movem.l %%d0-%%d7/%%a0-%%a6,-(%%sp)\n"
        "jsr _zxplasma_draw\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a0-%%a6"
    : "+rf" (_d0), "+rf" (_d1), "+rf" (_d2), "+rf" (_d3),
      "+rf" (_a0), "+rf" (_a1), "+rf" (_a2), "+rf" (_a3)
    :
    : "cc", "memory");
    return;
}

// the main show!
int copperchunky_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    // disable screen
    WaitVbl();
    // disable screen
    custom->bplcon0 = 0;
    // disable copper DMA
    custom->dmacon  = DMAF_COPPER;

    int fc_start = frameCounter;
    volatile int cop_idx = 1;

    // TODO: this code seems to have issues on real amiga, especially
    // if copper is reinitialized while blitter is running
    // upd: no longer touching copjmp1, should work better
    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_scroll[0];
    custom->dmacon = DMAF_BLITTER;//disable blitter dma for copjmp bug
    //custom->copjmp1 = 0x7fff; //start coppper
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    fc_start = frameCounter;

    SHORT sc = 0;
    SHORT sx = 0<<8, dx = 1<<8;
    SHORT sy = 0<<8, dy = 1<<8;

    int palidx = 0;

    while((isRunning) && (frameCounter < 1*4*64)) {
        // update copperlist
        custom->cop2lc = (ULONG)copper_scroll[cop_idx];
        cop_idx ^= 1;

        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        // draw plasma :D
        USHORT *copPtr = copper_scroll[cop_idx] + 3;
        
        plasma_draw_asm(copPtr, plasma_palette[palidx], dx, dy, sc, Y_GRID, sx, sy);

        sc = (costab_256[(frame_counter<<1) & 255]) & ~1;
        sx = (frame_counter << 2) + (costab_256[(frame_counter<<1) & 255] << 3);
        sy = (frame_counter << 2) + (sintab_256[((frame_counter<<2)+(frame_counter<<1)) & 255] << 3);

        dx = (1 << 7) + (sintab_256[frame_counter & 255] << 1) + (sintab_256[(frame_counter<<1) & 255] << 1);
        dy = (2 << 8) + (costab_256[((frame_counter)+(frame_counter>>1)) & 255] << 1) + (sintab_256[(frame_counter >> 1) & 255] << 0);

        // set next color
        *(coplist_color0+1) = speccypal_fade[palidx][7];

        if ((frame_counter <  4*16) && (palidx < 3)) palidx = (frame_counter) >> 2;
        if ((frame_counter >= 4*56) && (palidx < 7)) palidx = 3 + ((frame_counter - 4*56) >> 2);
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

}





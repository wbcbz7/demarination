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
#include "../palerp.h"
#include "endscreen.h"

#define X_RES               320
#define X_RES_WORD          (X_RES/16)
#define X_RES_BYTE          (X_RES/8)

#define X_PITCH             320
#define X_PITCH_WORD        (X_PITCH/16)
#define X_PITCH_BYTE        (X_PITCH/8)

#define BPL_COUNT           3
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT)

#define BPL_MODULO_BYTE     ((X_PITCH - X_RES)/8)
#define BPL_MODULO_WORD     ((X_PITCH - X_RES)/16)

#define Y_RES               164
#define Y_PITCH             Y_RES
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT)

// ------------------------

// speccy part
#define X_RES_ZX            256
#define X_RES_ZX_WORD       (X_RES_ZX/16)
#define X_RES_ZX_BYTE       (X_RES_ZX/8)

#define X_PITCH_ZX          256
#define X_PITCH_ZX_WORD     (X_PITCH_ZX/16)
#define X_PITCH_ZX_BYTE     (X_PITCH_ZX/8)

#define Y_RES_ZX            192

// -------------------------
// resources
INCBIN_CHIP(endscreen_gfx, "gfx/endscreen_gfx.bpl")
INCBIN(endscreen_pal, "gfx/endscreen_gfx.pal")

INCBIN_CHIP(speccyend_gfx, "gfx/speccyend_gfx.bpl")

// copperlist
static USHORT* copper_main;
static USHORT* coplist_palette;    
static USHORT* copper_zx;
static USHORT* copper_zx_pal;

// palette
static USHORT endscreen_pal_fade[17+17][8];

static USHORT endscreen_pal_fadein_set [8];
static USHORT endscreen_pal_fadeout_set[8];

int endscreen_free()
{
    FreeMem(copper_main, 256);
    FreeMem(copper_zx, 256);
    return 0;
}

// called during demo init
int endscreen_init() {
    // allocate copperlist memory
    copper_main = (USHORT*)AllocMem(256, MEMF_CHIP | MEMF_CLEAR);
    copper_zx   = (USHORT*)AllocMem(256, MEMF_CHIP | MEMF_CLEAR);

    if ((copper_main==0) || (copper_zx==0)) return 1;

    for (int i = 0; i < 8; i++) {
        endscreen_pal_fadein_set [i] = 0xFFF;
        endscreen_pal_fadeout_set[i] = 0x000;
    }

    // generate palette interpolation
    pal_lerp((USHORT*)endscreen_pal_fade[0],  endscreen_pal_fadein_set, endscreen_pal,  8, 16);
    pal_lerp((USHORT*)endscreen_pal_fade[17], endscreen_pal, endscreen_pal_fadeout_set, 8, 16);

    // fill copperlist
    SHORT *copPtr = copper_main;

    // init data fetch/display window
    copPtr = setLowres320Wide(copPtr, Y_RES);
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*single pf*/|(1<<9)/*color*/|(BPL_COUNT<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0;

    // set bitplane modulo - don't forget about scrolling
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)(BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE);
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)(BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE);
        
    // set pf0 bitplane pointers
    for (int i = 0; i < BPL_COUNT; i++) {
        ULONG bpl_ptr = (ULONG)endscreen_gfx + (i * X_PITCH_BYTE);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 0;
        *copPtr++ = (APTR)(bpl_ptr >> 16);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 2;
        *copPtr++ = (APTR)(bpl_ptr & 0xFFFF);
    }
    // set colors
    coplist_palette = copPtr;
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i], endscreen_pal_fade[0][i]);
    }

    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    // set speccy copperlist
    copPtr = copper_zx;

    // init data fetch/display window
    copPtr = setLowres256Wide(copPtr, Y_RES_ZX);
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*single pf*/|(1<<9)/*color*/|(1<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0;
    COPPERLIST_ADD_MOVE(copPtr, bpl1mod, 0);
    COPPERLIST_ADD_MOVE(copPtr, color[0], 0x000);
    copper_zx_pal = copPtr;
    COPPERLIST_ADD_MOVE(copPtr, color[1], 0xCCC);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], bpl_pool);
    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;


    return 0;
}

static USHORT zxpal_fade[4] = {0xCCC, 0xC0C, 0x00C, 0x000, 0x000};

// the main show!
int endscreen_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    int fc_start = frameCounter;

    // clear bitplane pool
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltamod = 0;
    custom->bltdpt  = (APTR)bpl_pool;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES_ZX) << HSIZEBITS) | (X_PITCH_ZX_WORD);

    // blit message
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltapt  = speccyend_gfx;
    custom->bltdpt  = (UBYTE*)bpl_pool + ((Y_RES_ZX-7) * (X_PITCH_ZX_BYTE));
    custom->bltamod = 0;
    custom->bltdmod = X_PITCH_ZX_BYTE - (80/8);
    custom->bltsize = (7 << HSIZEBITS) | (80/16);
    WaitBlit();

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    fc_start = frameCounter;
    int palidx = 0;

#if 1
    while((isRunning) && (frameCounter < 26*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        if ((frame_counter <  4*16) && (palidx < 17)) palidx = (frame_counter) >> 1;
        if ((frame_counter >= 1*64*4+4*48) && (palidx < 17+17)) palidx = 17 + ((frame_counter - (1*64*4+4*48)) >> 1);

        // set palette
        USHORT* copPtr = coplist_palette;
        for (int i = 0; i < 8; i++)
            COPPERLIST_ADD_MOVE(copPtr, color[i], endscreen_pal_fade[palidx][i]);
    }

    // stop music
    playMusic = 0;

    for (int i = 0; (isRunning) && (i < 30); i++) WaitVbl();
#endif

    // show speccy gag
    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_zx;
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    palidx = 0;
    for (int i = 0; (isRunning) && (i < 4*60); i++) {
        WaitVbl();
        if ((i > 4*60 - 32) && (palidx < 4)) {
            palidx = (i - (4*60 - 32)) >> 3;
            *(copper_zx_pal+1) = zxpal_fade[palidx];
        }
    }
    *(copper_zx_pal+1) = 0x000;

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}
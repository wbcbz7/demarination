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
#include "picwobble.h"

#define X_RES               320
#define X_RES_WORD          (X_RES/16)
#define X_RES_BYTE          (X_RES/8)

#define X_PITCH             336
#define X_PITCH_WORD        (X_PITCH/16)
#define X_PITCH_BYTE        (X_PITCH/8)

#define BPL_COUNT           4
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT)

#define BPL_MODULO_BYTE     ((X_PITCH - X_RES)/8)
#define BPL_MODULO_WORD     ((X_PITCH - X_RES)/16)

#define Y_RES               144
#define Y_PITCH             144
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT)

// -------------------------
// resources
INCBIN_CHIP(wobble_pic, "gfx/mid_gfx.bpl")
INCBIN(wobble_pal, "gfx/mid_gfx.pal")

// copperlist
static USHORT* copper_main;    
static USHORT* copper_wobble[2];
static USHORT* coplist_palette;

// palette
static USHORT wobble_pal_fade[17+17][32];

static USHORT wobble_pal_fadein_set [32];
static USHORT wobble_pal_fadeout_set[32];

int picwobble_free()
{
    FreeMem(copper_main, 256);
    FreeMem(copper_wobble[0], 2048);
    FreeMem(copper_wobble[1], 2048);
    return 0;
}

// called during demo init
int picwobble_init() {
    // allocate copperlist memory
    copper_main         = (USHORT*)AllocMem(256,  MEMF_CHIP | MEMF_CLEAR);
    copper_wobble[0]    = (USHORT*)AllocMem(2048, MEMF_CHIP | MEMF_CLEAR);
    copper_wobble[1]    = (USHORT*)AllocMem(2048, MEMF_CHIP | MEMF_CLEAR);

    if ((copper_main==0) || (copper_wobble[0]==0) || (copper_wobble[1]==0)) return 1;

    for (int i = 0; i < 32; i++) {
        wobble_pal_fadein_set [i] = 0xFFF;
        wobble_pal_fadeout_set[i] = 0xFFC;
    }

    // generate palette interpolation
    pal_lerp((USHORT*)wobble_pal_fade[0],  wobble_pal_fadein_set, wobble_pal,  32, 16);
    pal_lerp((USHORT*)wobble_pal_fade[17], wobble_pal, wobble_pal_fadeout_set, 32, 16);

    // fill copperlist
    SHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 1024, 0);

    // init data fetch/display window
    copPtr = setLowres320Wide(copPtr, Y_RES);
    *copPtr++ = offsetof(struct Custom, ddfstrt);
    *copPtr++ = 0x0030;
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*single pf*/|(1<<9)/*color*/|(BPL_COUNT<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0;//(0<<6)|(4<<0)|(4<<3);

    // set bitplane modulo - don't forget about scrolling
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)(BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE - 2);
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)(BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE - 2);
        
    // set pf0 bitplane pointers
    for (int i = 0; i < BPL_COUNT; i++) {
        ULONG bpl_ptr = (ULONG)wobble_pic + (i * X_PITCH_BYTE);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 0;
        *copPtr++ = (APTR)(bpl_ptr >> 16);
        *copPtr++ = offsetof(struct Custom, bplpt[i]) + 2;
        *copPtr++ = (APTR)(bpl_ptr & 0xFFFF);
    }
    // set colors
    coplist_palette = copPtr;
    COPPERLIST_ADD_MOVE(copPtr, color[0], *((USHORT*)wobble_pal_fadein_set + 15));
    for (int i = 1; i < 16; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i], *((USHORT*)wobble_pal_fadein_set + i));
    }

    // jump to 2nd copperlist
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);

    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    // init wobble copperlist
    for (int cop = 0; cop < 2; cop++) {
        copPtr = copper_wobble[cop];
        
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)-1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], *((USHORT*)wobble_pal_fadein_set + 15));
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)-0, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], *((USHORT*)wobble_pal_fadein_set + 0));

        for (int i = 0; i < Y_RES; i++) {
            COPPERLIST_ADD_WAIT(copPtr, 0, 44+((256-Y_RES)>>1)+i-1,-1);
            COPPERLIST_ADD_MOVE(copPtr, bplcon1, 0);
        }
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)+Y_RES, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], *((USHORT*)wobble_pal_fadein_set + 15));
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)+Y_RES+1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], *((USHORT*)wobble_pal_fadein_set + 15));
        // terminate copperlist
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    return 0;
}

// the main show!
int picwobble_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    int fc_start = frameCounter;

    SHORT cop_ptr = 1;

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_wobble[cop_ptr ^ 1];
    custom->dmacon = DMAF_BLITTER;//disable blitter dma for copjmp bug
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    fc_start = frameCounter;

    SHORT y_fc = 0, y_y, y_dy = (16 << 8), y_mul = 8;

    int palidx = 0;

    while((isRunning) && (frameCounter < 12*4*64 - 4*6)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        y_y = y_fc;

        // render wobble
        USHORT *copPtr = copper_wobble[cop_ptr];
        *(copPtr+3)   = wobble_pal_fade[palidx][15];
        *(copPtr+4+3) = wobble_pal_fade[palidx][0];
        copPtr += 4+4+2+1;
        for (int i = 0; i < Y_RES; i++) {
            int xx = (y_mul*(127 - costab_256[(y_y >> 8) & 255]) >> 8) & 15;
            *(copPtr + 0) = (xx << 4) | xx;
            copPtr += 4;
            y_y += y_dy;
        }
        *(copPtr+0)   = wobble_pal_fade[palidx][15];
        *(copPtr+4+0) = wobble_pal_fade[palidx][15];

        // wait for semi-visible screen
        WaitLineOrAbove(0x10);

        // set copperlist
        custom->cop2lc = (ULONG)copper_wobble[cop_ptr];
        cop_ptr ^= 1;

        // animate 
        if (frame_counter < 4*64+4*32) {
            y_fc += (y_dy << 1);
            if ((frame_counter & 15) == 0) {if (y_mul > 0) y_mul--; else y_mul = 0;}
            if (y_dy > 0) y_dy -= 0x20; else y_dy = 0;
        }

        if ((frame_counter <  1*4*64) && (palidx < 17)) palidx = (frame_counter) >> 2;
        if ((frame_counter >= 1*4*64 + 4*48 - 4*4) && (palidx < 17+17)) palidx = 17 + ((frame_counter - 1*4*64 - 4*48 + 4*4) >> 1);

        // set palette
        copPtr = coplist_palette;
        COPPERLIST_ADD_MOVE(copPtr, color[0], wobble_pal_fade[palidx][15]);
        for (int i = 1; i < 15; i++)
            COPPERLIST_ADD_MOVE(copPtr, color[i], wobble_pal_fade[palidx][i]);
    }

    // disable sprite DMA

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}
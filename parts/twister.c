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
#include "../palerp.h"
#include "../tables.h"
#include "../drawtext.h"
#include "../spritegen.h"
#include "twister.h"

#define X_RES           320
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH         1024
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT           3
#define X_PITCH_BPL         (X_PITCH)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE)

#define BPL_MODULO_BYTE     (X_PITCH_BPL_BYTE - (X_RES/8))
#define BPL_MODULO_WORD     (X_PITCH_BPL_WORD - (X_RES/16))

#define Y_RES               256
#define Y_PITCH             (256+2)
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT)

#define Y_PITCH_BACK        (256+64)

// -------------------------
// resources
INCBIN_CHIP(twister, "gfx/twister.bpl")
INCBIN(twister_palette, "gfx/twister.pal")

#define GREETS_SPR_COUNT    2
#define GREETS_SPR_WIDTH    80
#define GREETS_SPR_HEIGHT   Y_RES

// greets sprite
static USHORT greets_spr[GREETS_SPR_COUNT][(GREETS_SPR_WIDTH/16)][2+GREETS_SPR_HEIGHT*2+2] __attribute__((section (".MEMF_CHIP")));

static USHORT pal_default[16] = {
    // twister (temp)
    0,0,0,0,0,0,0,0,
    // ......backdrop.....................sprites.............
    0x111, 0x111, 0x222, 0x222, 0x000, 0xAAB, 0xAAB, 0xDDE
};

static USHORT pal_fadein[16];
static USHORT pal_fadeout[16];
static USHORT pal_fade[17+17][16];

static USHORT xtab   [256];
static USHORT xmodtab[256];
static signed char sintab_sh2[256];

// bitplane pointers
static UBYTE* bplbase;
static UBYTE* bpl_twister;
static UBYTE* bpl_scroll;

// copperlist
static USHORT* copper_main;
static USHORT* copper_pal;
static USHORT* copper_bplstart;
static USHORT* copper_bgscroll;
static USHORT* copper_modulo;
static USHORT* copper_scroll[2];
static USHORT* copper_spritept;

int twister_free()
{
    FreeMem(copper_main, 512);
    FreeMem(copper_scroll[0], 4096);
    FreeMem(copper_scroll[1], 4096);
    return 0;
}

static char *greets_text[] = {
    // first 19 groups
    "marquee design",
    "smfx",
    "joker",
    "desire",
    "darklite",
    "spreadpoint",
    "darkage",
    "oxygene",
    "ngc",
    "nah-kolor",
    "andromeda",
    "attentionwhore",
    "spectrox",
    "alcatraz",
    "abyss",
    "tek",
    "lemon # melon",
    "disaster area",
    "inercia",

    // second 19 groups
    "demarche",
    "stardust",
    "caroline",
    "shiru",
    "7dump",
    "sands",
    "excess team",
    "v-nom",
    "fishbone",
    "fenomen",
    "mayhem",
    "outsiders",
    "enhancers",
    "nihirashi",
    "jin x",
    "f0x",
    "rmda",
    "serzhsoft",
    "@amigaru",
};

static void twister_greets_generate() {
    // init text writer
    text_init(A_OR_C);
    text_build_pitch_lookup(X_RES_BYTE, X_RES_BYTE);

    // draw text with given spacing
    int t = 0;
    for (int s = 0; s < GREETS_SPR_COUNT; s++) {
        int y = 7;
        UBYTE *dst = bpl_scroll + (X_RES_BYTE*y);
            
        // clear text buffer
        WaitBlit();
        custom->bltcon0 = A_TO_D | DEST;
        custom->bltcon1 = 0;
        custom->bltadat = 0;
        custom->bltamod = 0;
        custom->bltdpt  = (APTR)bpl_scroll;
        custom->bltdmod = 0;
        custom->bltafwm = custom->bltalwm = 0xffff;
        custom->bltsize = ((256) << HSIZEBITS) | (X_RES_WORD);
        WaitBlit();

        // draw text
        for (int j = 0; j < 19; j++) {
            text_draw(dst, &font_small, greets_text[t+j], 0, y);
            y += 13;
        }

        // cut to sprite
        USHORT* src = (USHORT*)bpl_scroll;
        for (int i = 0; i < (GREETS_SPR_WIDTH/16); i++) {
            sprite_cut(
                &greets_spr[s][i][0], src, src, (X_RES-16)/8, (X_RES-16)/8, GREETS_SPR_HEIGHT,
                128+(i<<4), 44
            );
            src++;
        }
        
        t += 19;
    }

}

static const int palofs[] = {16, 16+4, 16+12};

// called during demo init
int twister_init() {
    // allocate copperlist memory
    copper_main         = (USHORT*)AllocMem(512,   MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[0]    = (USHORT*)AllocMem(4096,  MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[1]    = (USHORT*)AllocMem(4096,  MEMF_CHIP | MEMF_CLEAR);

    if ((copper_main==0) || (copper_scroll[0]==0) || (copper_scroll[1]==0)) return 1;

    // init bpl pointers 
    bplbase = bpl_pool;

    bpl_twister = bplbase;
    bpl_scroll  = bplbase + BPLSCREEN_SIZE + X_PITCH_BYTE;

    // -------------------
    for (int i = 0; i < 8; i++) {
        pal_default[i] = *((USHORT*)twister_palette + i);
    }
    for (int i = 0; i < 16; i++) {
        pal_fadein[i] = 0x112;
    }
    for (int i = 0; i < 16; i++) {
        pal_fadeout[i] = 0x224;
    }

    // do fades
    pal_lerp(&pal_fade[0],  pal_fadein,  pal_default, 16, 16);
    pal_lerp(&pal_fade[17], pal_default, pal_fadeout, 16, 16);

    // initialize xtab
    for (int i = 0; i < 256; i++) {
        xtab[i]    = (((i - 0) & 0xFFF0) >> 3);
        xmodtab[i] = 15 - (i & 15);
        sintab_sh2[i] = sintab_256[i]>>1;
    }

    return 0;
}

static void twister_set_sprites(int set, int x) {
    // global for now (per-line gets later + it's a little bit tricky :)
    USHORT ystart = 44, yend = 44+GREETS_SPR_HEIGHT;
    x += 128;
    for (int i = 0; i < (GREETS_SPR_WIDTH/16); i++) {
        greets_spr[set][i][0] = ((ystart & 0xFF) << 8) | ((x >> 1) & 0xFF);
        greets_spr[set][i][1] = ((yend   & 0xFF) << 8) | (x & 1) | ((ystart & 0x100) >> 6) | ((yend & 0x100) >> 7);
        x += 16;
    }
}

static __attribute__ ((noinline)) void twister_fill_copperlist(USHORT *copPtr, SHORT frame, USHORT y_y, USHORT y_dy, USHORT y_sy, USHORT y_dsy, USHORT x_y, USHORT x_dy, USHORT height, USHORT mask) {
    frame &= 1;
    
    // update copperlist
    SHORT mod = 0, mod0 = 0;
    SHORT x = 0, y = 0;
    USHORT i = height;

    y = ((y_y >> 8) + (sintab_sh2[(y_sy >> 8) & 255]))&255;
    x = (64 + sintab_sh2[(x_y >> 8) & 255]);
    mod0 = (y << 7) + xtab[x];

    for (int i = 0; i < BPL_COUNT; i++) {
        ULONG bpl_ptr = (ULONG)bpl_twister + (i * X_RES_BYTE) + mod0;
        *(copPtr+1) = (APTR)(bpl_ptr >> 16);
        *(copPtr+3) = (APTR)(bpl_ptr & 0xFFFF);
        copPtr += 4;
    }
    // set BPLCON1
    *(copPtr+1) = 15 - (x & 15);
    copPtr += 2+1;
    x_y += x_dy;
    y_y += y_dy;
    y_sy += y_dsy;
    
    // set height part
    do {
        y = ((y_y >> 8) + (sintab_sh2[(y_sy >> 8) & 255]))&255;
        x = (64 + sintab_sh2[(x_y >> 8) & 255]);
        if ((i + frame) & mask) x = 128 - x;
        mod = (y << 7) + xtab[x];
        *(copPtr+0) = (mod - mod0) - X_RES_BYTE;
        *(copPtr+4) = xmodtab[x];
        mod0 = mod; copPtr += 6;
        x_y += x_dy;
        y_y += y_dy;
        y_sy += y_dsy;
    } while (--i);

    // jump to the blank line for all lines below the height
    i = Y_RES - height;
    if (i != 0) {
        x = 0;
        y = 256;
        mod = (y << 7) + xtab[x];
        *(copPtr+0) = (mod - mod0) - X_RES_BYTE;
        *(copPtr+4) = xmodtab[x];
        copPtr += 6;
        i--;
        if (i != 0) do {
            *(copPtr+0) = -X_RES_BYTE;
            *(copPtr+4) = xmodtab[x];
            copPtr += 6;
        } while (--i);
    }
}

// the main show!
int twister_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;

    // write text
    twister_greets_generate();

    // clear buffers
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bpl_twister;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_PITCH) << HSIZEBITS) | (X_PITCH_BPL_WORD);

    // unpack twister graphics
    for (int bpl = 0; bpl < 3; bpl++) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltcon1 = 0;
        custom->bltapt  = twister + (64/8)*bpl;
        custom->bltdpt  = bpl_twister + (X_RES_BYTE)*bpl + (240/8);
        custom->bltamod = (64/8)*(3-1);
        custom->bltdmod = X_PITCH_BYTE - (64/8);
        custom->bltafwm = custom->bltalwm = 0xffff;
        custom->bltsize = ((Y_RES) << HSIZEBITS) | (64/16);
    }

    // clear scroll bitplane
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0x0000;
    custom->bltdpt  = bpl_scroll;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_PITCH_BACK+256) << HSIZEBITS) | (X_RES_WORD);

    // fill scroll bitplane with shit
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0xFFFF;
    custom->bltdpt  = bpl_scroll;
    custom->bltdmod = 0;
    custom->bltsize = ((24) << HSIZEBITS) | (X_RES_WORD);

    // floodfill the rest
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltapt  = bpl_scroll;
    custom->bltdpt  = bpl_scroll + (64*X_RES_BYTE);
    custom->bltamod = 0;
    custom->bltdmod = 0;
    custom->bltsize = ((Y_RES) << HSIZEBITS) | (X_RES_WORD);
    WaitBlit();

    // init copperlist
    // initialize copper list
    USHORT *copPtr = copper_main;

    // init data fetch/display window
    copPtr = setLowres320Wide(copPtr, Y_RES);
    //*copPtr++ = offsetof(struct Custom, ddfstrt);
    //*copPtr++ = 0x0030;
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (1<<10)/*dual pf*/|(1<<9)/*color*/|(5<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = (4<<3)|(4<<0);

    // set bitplane modulo
    copper_modulo = copPtr;
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)(BPL_MODULO_BYTE);
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)(0);

    // set odd bitplane pointers (TODO: add scrollers here)
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[4], temp_2ndpf);
    copper_bgscroll = copPtr;
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[5], temp_2ndpf);

    // set sprites
    copper_spritept = copPtr;
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[0], &greets_spr[0][0]);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[1], &greets_spr[0][1]);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[2], &greets_spr[0][2]);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[3], &greets_spr[0][3]);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[4], &temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[5], &temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[6], &greets_spr[0][4]);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[7], &temp_2ndpf);

    // set palette
    copper_pal = copPtr;
    for (int i = 0; i < 12; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i], pal_fade[0][i]);
    }
    // set sprite palette
    for (int j = 0; j < ((GREETS_SPR_WIDTH+31)/32); j++) {
        for (int i = 1; i < 4; i++) {
            COPPERLIST_ADD_MOVE(copPtr, color[palofs[j]+i], pal_fade[0][12+i]);
        }
    }

    // switch to 2nd copperlist
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);
    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    // fill copperlist    
    for (int i = 0; i < 2; i++) {
        copPtr = copper_scroll[i];

        copper_bplstart = copPtr + 1;
        for (int i = 0; i < BPL_COUNT; i++) {
            ULONG bpl_ptr = (ULONG)bpl_twister + (i * X_RES_BYTE);
            *copPtr++ = offsetof(struct Custom, bplpt[0]) + (i << 3) + 0;
            *copPtr++ = (APTR)(bpl_ptr >> 16);
            *copPtr++ = offsetof(struct Custom, bplpt[0]) + (i << 3) + 2;
            *copPtr++ = (APTR)(bpl_ptr & 0xFFFF);
        }
        COPPERLIST_ADD_MOVE(copPtr, bplcon1, 0);        // fill this later

        // fill scroll DMA
        // TODO: add pf1/sprite scroll
        for (int i = 0; i < Y_RES; i++) {
            COPPERLIST_ADD_MOVE(copPtr, bpl1mod, X_PITCH_BYTE);
            // add PAL fix after line 255
            if (44+i == 255) {
                *copPtr++=0xffdf;
                *copPtr++=0xfffe;
            } else COPPERLIST_ADD_WAIT(copPtr, 0, 44+i+1, -1);
            COPPERLIST_ADD_MOVE(copPtr, bplcon1, 0);        // fill this later
        }

        // terminate copperlist
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    int fc_start = frameCounter;
    int cop_idx = 0;

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_scroll[1];
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER | DMAF_SPRITE;

    fc_start = frameCounter;

    USHORT y_fc = 0, y_dfc = 0, y_ddfc = 0;
    USHORT x_fc = 0, x_dfc = 0;
    USHORT y_y  = 0, y_dy = 0, y_ddy = 0;
    USHORT x_y  = 0, x_dy  = 0;
    USHORT y_sy = 0, y_dsy = (1 << 8);

    LONG   y_bd[2]   = {(256+64)<<8, (256+64)<<8}, y_dbd[2] = {(2<<8), (3<<8)};
    SHORT  y_lock[2] = {0, 0};
    USHORT tw_height = 2;

    volatile SHORT xspr = -(80<<8), dxspr = (2<<8);

    int palidx = 0, spridx = 0;
    int twister_mask = 0;

    while((isRunning) && (frameCounter < 22*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        USHORT* copPtr = copper_scroll[cop_idx];
        twister_fill_copperlist(copPtr, frameCounter, y_fc, y_dy, y_sy, y_dsy, x_fc, x_dy, tw_height, twister_mask);

        // advance variables
        y_fc += y_dfc;
        y_dfc = y_ddfc; if (y_ddfc < (7 << 8)) y_ddfc += 1 << 6;
        if (frame_counter > 4*48) y_dfc += sintab_256[((frame_counter - 4*48) << 2) & 255]; 
        //y_dfc = (2 << 8) + sintab_256[(frame_counter << 1) & 255]; 
        y_dy = y_ddy; if (y_ddy < (1 << 8)) y_ddy += (1 << 2);
        y_dy += sintab_256[(frame_counter << 1) & 255]; 
        y_dsy = (sintab_256[(frame_counter) & 255]);
        if (frame_counter > 4*64) {
            x_fc += x_dfc;
            x_dy  = (1 << 7) - costab_256[(frame_counter - 4*64) & 255]; 
            x_dfc = sintab_256[((frame_counter - 4*64) >> 0) & 255]; 
        }
        if (frame_counter < 4*64) {
            if (tw_height < Y_RES) tw_height += 2; else tw_height = Y_RES;
        }
        if (frame_counter >= 3*4*64) twister_mask = 1;

        // set backdrop pointers
        copPtr = copper_bgscroll;
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], bpl_scroll + ((y_bd[0]>>8)*(X_RES_BYTE)));
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], bpl_scroll + ((y_bd[1]>>8)*(X_RES_BYTE)));
        for (int i = 0; i < 2; i++) {
            y_bd[i] -= y_dbd[i];
            if (y_bd[i] < 0) y_lock[i] = 1;
            if (y_lock[i]) y_bd[i] &= ((64<<8)-1);
        }

        // set palette
        copPtr = copper_pal;
        for (int i = 0; i < 12; i++) {
            COPPERLIST_ADD_MOVE(copPtr, color[i], pal_fade[palidx][i]);
        }
        // set sprite palette
        for (int j = 0; j < ((GREETS_SPR_WIDTH+31)/32); j++) {
            for (int i = 1; i < 4; i++) {
                COPPERLIST_ADD_MOVE(copPtr, color[palofs[j]+i], pal_fade[palidx][12+i]);
            }
        }

        if (frame_counter == 2*4*64) {
            // set next set of sprites
            spridx++;
            copPtr = copper_spritept;
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[0], &greets_spr[spridx][0]);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[1], &greets_spr[spridx][1]);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[2], &greets_spr[spridx][2]);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[3], &greets_spr[spridx][3]);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[4], &temp_2ndpf);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[5], &temp_2ndpf);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[6], &greets_spr[spridx][4]);
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[7], &temp_2ndpf);
            xspr = -(80<<8); dxspr = (2<<8);
        }

        // animate sprites
        xspr += dxspr;
        twister_set_sprites(spridx, (xspr>>8) + (sintab_256[(frame_counter<<1) & 255]>>5));
        if ((frame_counter & (2*4*64-1)) <= 4*64) {
            if ((dxspr>0)&&(xspr<(4<<8)))
                dxspr-=6;
            else
                dxspr=0;
        }
        else if ((frame_counter & (2*4*64-1)) >= 1*4*64+4*32) {
            if ((xspr>-(80<<8)))
                dxspr-=6;
            else
                dxspr=0;
        }

        // wait for semi-visible screen
        WaitLineOrAbove(0x10);

        // update copperlist
        custom->cop2lc = (ULONG)copper_scroll[cop_idx];
        cop_idx ^= 1;

        if ((frame_counter <  1*4*64) && (palidx < 17)) palidx = frame_counter>>1;
        if ((frame_counter >= 3*4*64 + 4*56) && (palidx < 17+17)) palidx = 17 + ((frame_counter - 3*4*64 - 4*56) >> 1);
    }

    // kill sprites
    custom->dmacon = DMAF_SPRITE;
    copPtr = copper_spritept;
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &temp_2ndpf);
    }

    // fudge copperlist to prevent glitch in next part
    copPtr = copper_bgscroll;
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[5], temp_2ndpf);
    copPtr = copper_bplstart;
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[4], temp_2ndpf);
    copPtr = copper_modulo;
    COPPERLIST_ADD_MOVE(copPtr, bpl1mod, -X_RES_BYTE-2);
    COPPERLIST_ADD_MOVE(copPtr, bpl2mod, -X_RES_BYTE-2);

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

}






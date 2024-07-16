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
#include "../spritegen.h"
#include "../palerp.h"
#include "linewobble.h"

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
#define Y_PITCH             256
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT)

#define BPL_COUNT_BACKDROP  2
#define BACKDROP_WIDTH      128
#define BACKDROP_WIDTH_BYTE (BACKDROP_WIDTH/8)
#define BACKDROP_WIDTH_WORD (BACKDROP_WIDTH/16)
#define BACKDROP_HEIGHT     64

#define X_PITCH_BACK        X_RES
#define X_PITCH_BACK_WORD    (X_PITCH_BACK/16)
#define X_PITCH_BACK_BYTE    (X_PITCH_BACK/8)

#define X_PITCH_BACK_BPL    (X_PITCH_BACK*BPL_COUNT_BACKDROP)
#define X_PITCH_BACK_BPL_WORD    (X_PITCH_BACK_BPL/16)
#define X_PITCH_BACK_BPL_BYTE    (X_PITCH_BACK_BPL/8)

#define Y_PITCH_BACKDROP    (Y_RES+BACKDROP_HEIGHT)

#define SPRITE_WIDTH  64
#define SPRITE_HEIGHT 64

// backdrop
static USHORT *bpl_backdrop;

INCBIN_CHIP(linewobble_backdrop, "gfx/linewobble_backdrop.BPL")
INCBIN_CHIP(linewobble_sprite_gfx, "gfx/linewobble_sprite.BPL") 
INCBIN(linewobble_sprite_pal, "gfx/linewobble_sprite.pal") 

// mf sprite
static USHORT linewobble_sprite[(SPRITE_WIDTH/16)][SPRITE_HEIGHT*2+2+2] __attribute__((section (".MEMF_CHIP")));

// frame buffer
static USHORT *line_gfx_buf;
static USHORT *line_gfx;

// copperlist
static USHORT* copper_main;
static USHORT* copper_backdrop_scroll;
static USHORT* copper_scroll[2];
static USHORT* copper_bplpt[2];
static USHORT* copper_spritept[2];
static USHORT* copper_spritectrl[2];
static USHORT* copper_palette;

// palette
static USHORT pal[16] = {
// border ...................linewobble.................. ......backdrop......  ......sprite......
   0x000, 0xC23, 0xF53, 0xF83, 0xFB4, 0xFD8, 0xFFC, 0xFFC, 0x101, 0x111, 0x222, 0x9AA, 0x668, 0x113
};

static USHORT pal_fadein[16]  = {0xFFC};
static USHORT pal_fadeout[16] = {0};

static USHORT pal_fade[17+17][16];

struct line_info_t {
    SHORT pos;
    SHORT color;
};

#define arrayof(n) (sizeof(n) / sizeof(n[0]))

static struct line_info_t lines[] = {
    {-128, 1},
    { -96, 2},
    { -72, 3},
    { -48, 4},
    { -24, 5},
    {  -9, 6},
    {   9, 5},
    {  24, 4},
    {  48, 3},
    {  72, 2},
    {  96, 1},
    { 128, 0},
};

// SLOW! :)
static void pxor(USHORT* ptr, USHORT x, USHORT y, USHORT color) {
    USHORT *p = ptr + (y << 6) + (x >> 4);
    SHORT pixel = 0x8000 >> (x & 15);

    if (color & 1) *(p+0*X_RES_WORD) ^= pixel;
    if (color & 2) *(p+1*X_RES_WORD) ^= pixel;
    if (color & 4) *(p+2*X_RES_WORD) ^= pixel;
}

static void draw_line(USHORT *dst, SHORT pitch, SHORT y, SHORT dudx, SHORT hw) {
    struct line_info_t lold = {0, 0};
    for (int i = 0; i < arrayof(lines); i++) {
        SHORT pos = (lines[i].pos * dudx) >> 8;
        if (pos < -hw) pos = -hw;
        if (pos >= hw) pos =  hw-1;
        pxor(dst, hw + pos, y, lold.color ^ lines[i].color);
        lold = lines[i];
    }
}

// calculate line image
static void calc_line(USHORT *dst) {
    USHORT dudx = 0, ddudx = (512/Y_RES);
    for (int y = 0; y < Y_RES; y++) {
        draw_line(dst, Y_RES, y, dudx, X_RES>>1);
        dudx += ddudx;
        if ((y > 128) && !(y & 3)) ddudx += 1;
        if ((y > 190)) ddudx += 1;
    }
}

#define USE_BITPLANE_POOL

int linewobble_free() {
    // free memory
#ifndef USE_BITPLANE_POOL
    FreeMem(line_gfx_buf, X_PITCH_BYTE*(Y_RES+2));
#endif
    FreeMem(copper_main, 256);
    FreeMem(copper_scroll[0], 4096);
    FreeMem(copper_scroll[1], 4096);
}

int linewobble_init() {
    // init backdrop buffer
    bpl_backdrop  = bpl_pool;

    // allocate frame buffer memory
#ifdef USE_BITPLANE_POOL
    line_gfx_buf = (USHORT*)(bpl_pool + X_PITCH_BACK_BPL_BYTE*(Y_PITCH_BACKDROP+16)*2);
#else
    line_gfx_buf = (USHORT*)AllocMem(X_PITCH_BYTE*(Y_RES+2), MEMF_CHIP | MEMF_CLEAR);
#endif

    // allocate copper list
    copper_main         = (USHORT*)AllocMem(256,  MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[0]    = (USHORT*)AllocMem(4096, MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[1]    = (USHORT*)AllocMem(4096, MEMF_CHIP | MEMF_CLEAR);

    if ((line_gfx_buf==0) || (copper_main==0) || (copper_scroll[0]==0) || (copper_scroll[1]==0)) return 1;
    line_gfx = line_gfx_buf + X_PITCH_BYTE;

    // init fades
    for (int i = 0; i < 16; i++) {
        pal_fadein[i] =  0xFFC;
        pal_fadeout[i] = 0x112;
    }
    pal_lerp(&pal_fade[0][0], pal_fadein, pal, 16, 16);
    pal_lerp(&pal_fade[17][0], pal, pal_fadeout, 16, 16);

    // initialize copper list
    USHORT *copPtr = copper_main;

    // convert sprite
    USHORT* src = (USHORT*)linewobble_sprite_gfx;
    for (int i = 0; i < (64/16); i++) {
        sprite_cut(
            &linewobble_sprite[i][0], src, src+(64/16), (64*2-16)/8, (64*2-16)/8, 64,
            128+(i<<4)+((X_RES-64)>>1), -20
        );
        src++;
    }

    return 0;
}

static __attribute__ ((noinline)) void set_sprite(SHORT idx, SHORT x, SHORT y, SHORT height) {
    USHORT ctrl0, ctrl1;
    USHORT ofs = 0;     // offset in sprite graphics
    SHORT  end_y;

    // get visible x/y
    x += 128; y += 44;

    // clip sprite
    if (y+height < 44) {
        // fully clipped, disable
        ofs = 2+(height<<1);        // point to 0,0 word
        ctrl0 = ctrl1 = 0;          // and disable
    }
    else {
        if (y < 44) {
            // partial top clip
            ofs     = 2+((-y + 44)<<1);
            height += y-44;
            y = 44;
        } else {
            ofs = 2;
        }
        end_y = y + height;
        ctrl0 = ((y & 0xFF) << 8)     | ((x >> 1) & 0xFF);
        ctrl1 = ((end_y & 0xFF) << 8) | (x & 1) | ((y & 0x100) >> 6) | ((end_y & 0x100) >> 7);
    }

    USHORT *sprpt = copper_spritept[idx]  + 1;
    USHORT *ctrl = copper_spritectrl[idx] + 1;        // TODO: add clipping
    for (int i = 0; i < (SPRITE_WIDTH/16); i++) {
        ULONG pt = (ULONG)&linewobble_sprite[i][ofs];
        // set control word
        *(sprpt + 0) = (pt >> 16);
        *(sprpt + 2) = (pt & 0xFFFF);
        *(ctrl  + 0) = ctrl0;
        *(ctrl  + 2) = ctrl1;
        ctrl += 4; sprpt += 4;

        ctrl0 += 8;
    }
}

static void twister_fill_copperlist(USHORT *copPtr, USHORT height, USHORT y_y, USHORT y_dy, USHORT y_sy, USHORT y_dsy, SHORT skip_palfix) {
    // update copperlist
    SHORT mod = 0, mod0 = (((y_y >> 8) + ((127 - costab_256[(y_sy >> 8) & 255]) >> 5))&255)<<7;
    SHORT y = 0;

    SHORT i;
    if (height < 44) {
        i = height;
    } else {
        i = height-44;
        if (i > 0) do {
            y = ((y_y >> 8) + ((127 - costab_256[(y_sy >> 8) & 255]) >> 5))&255;
            mod = (y << 7);
            *(copPtr+0) = (mod - mod0) - X_RES_BYTE;
            mod0 = mod; copPtr += 4;
            y_y  += y_dy;
            y_sy += y_dsy;
        } while (--i);

        copPtr += 2;
        i = 44;
    }
    // do after pal fix
    if (i > 0) do {
        y = ((y_y >> 8) + ((127 - costab_256[(y_sy >> 8) & 255]) >> 5))&255;
        mod = (y << 7);
        *(copPtr+0) = (mod - mod0) - X_RES_BYTE;
        mod0 = mod; copPtr += 4;
        y_y  += y_dy;
        y_sy += y_dsy;
    } while (--i);
}

// the main show!
int linewobble_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    // initialize main copperlist
    USHORT* copPtr = copper_main;

    // init data fetch/display window
    copPtr = setLowres320Wide(copPtr, Y_RES);

    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (1<<10)/*dual pf*/|(1<<9)/*color*/|(5<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = (4<<3)|(4<<0);

    // set backdrop bpl positions
    copper_backdrop_scroll = copPtr;
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], bpl_backdrop);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], bpl_backdrop + 1*X_PITCH_BACK_BYTE);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[5], bpl_backdrop + 2*X_PITCH_BACK_BYTE);

    // set bitplane modulo
	*copPtr++=offsetof(struct Custom, bpl1mod); // odd planes   1,3,5
	*copPtr++=(USHORT)(BPL_MODULO_BYTE);
	*copPtr++=offsetof(struct Custom, bpl2mod); // even  planes 2,4,6
	*copPtr++=(USHORT)(X_PITCH_BACK_BPL_BYTE-X_RES_BYTE);

    // set palette
    copper_palette = copPtr;
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i], pal_fadein[i]);
    }
    // set backdrop palette
    COPPERLIST_ADD_MOVE(copPtr, color[8+1], pal_fadein[8]);
    COPPERLIST_ADD_MOVE(copPtr, color[8+2], pal_fadein[9]);
    COPPERLIST_ADD_MOVE(copPtr, color[8+3], pal_fadein[10]);
    // set sprite palette
    for (int i = 0; i < (64/32); i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[16+(i<<2)+1], pal_fadein[11]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+(i<<2)+2], pal_fadein[12]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+(i<<2)+3], pal_fadein[13]);    
    }

    // jump to 2nd copperlist
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);

    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    // fill 2nd copperlist
    for (int cop = 0; cop < 2; cop++) {
        copPtr = copper_scroll[cop];

        copper_spritept[cop] = copPtr;
        for (int i = 0; i < (64/16); i++) {
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &linewobble_sprite[i][0]);
        }
        for (int i = (64/16); i < 8; i++) {
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &temp_2ndpf);
        }
        COPPERLIST_ADD_WAIT(copPtr, (64)>>2, 25, -1);
        copper_spritectrl[cop] = copPtr;
        for (int i = 0; i < (64/16); i++) {
            COPPERLIST_ADD_MOVE(copPtr, spr[i].pos, 0);
            COPPERLIST_ADD_MOVE(copPtr, spr[i].ctl, 0);
        }

        copper_bplpt[cop] = copPtr;
        // set pf0 bitplane pointers to last line 
        for (int i = 0; i < BPL_COUNT; i++) {
            ULONG bpl_ptr = (ULONG)line_gfx + (i * X_RES_BYTE) + ((Y_RES-1)*(X_PITCH_BYTE));
            *copPtr++ = offsetof(struct Custom, bplpt[i<<1]) + 0;
            *copPtr++ = (APTR)(bpl_ptr >> 16);
            *copPtr++ = offsetof(struct Custom, bplpt[i<<1]) + 2;
            *copPtr++ = (APTR)(bpl_ptr & 0xFFFF);
        }

        // set bitplane scroll positions
        for (int y = 0; y < Y_RES; y++) {
            COPPERLIST_ADD_WAIT(copPtr, (64+X_RES-32)>>2, 44+y, -1);
            COPPERLIST_ADD_MOVE(copPtr, bpl1mod, -X_RES_BYTE);
            // add PAL fix after line 255
            if (44+y == 255) {
                *copPtr++=0xffdf;
                *copPtr++=0xfffe;
            }
        }

        // terminate copperlist
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    int fc_start = frameCounter;
    int cop_idx = 1;

    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;

    // clear line graphics buffer
    // enable blitter dma if is not enabled already
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = line_gfx_buf;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES) << HSIZEBITS) | (X_PITCH_WORD);
    WaitBlit();

    // calculate line graphics!
    calc_line(line_gfx);

    // area fill
    for (int bpl = 0; bpl < BPL_COUNT; bpl++) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
		custom->bltcon1 = FILL_XOR | BC1F_DESC;
		custom->bltapt  = custom->bltdpt = (APTR)line_gfx + (bpl*X_RES_BYTE) + (X_RES_BYTE-2) + ((Y_RES-1) * X_PITCH_BYTE);
        custom->bltamod = custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;
		custom->bltsize = ((Y_RES-1) << HSIZEBITS) | (X_RES_WORD);
    }
    WaitBlit();

    // copy backdrop graphics (quick and dirty lol)
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltapt  = (UBYTE*)linewobble_backdrop+(3*BACKDROP_WIDTH_BYTE/4);
    custom->bltdpt  = (UBYTE*)bpl_backdrop;
    custom->bltamod = 3*BACKDROP_WIDTH_BYTE/4;
    custom->bltdmod = X_PITCH_BACK_BYTE - (BACKDROP_WIDTH_BYTE/4);
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((BACKDROP_HEIGHT * BPL_COUNT) << HSIZEBITS) | (BACKDROP_WIDTH_WORD/4);
    for (int x = BACKDROP_WIDTH_BYTE/4; x < X_RES_BYTE - BACKDROP_WIDTH_BYTE/4; x += BACKDROP_WIDTH_BYTE) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltcon1 = 0;
        custom->bltapt  = linewobble_backdrop;
        custom->bltdpt  = (UBYTE*)bpl_backdrop + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_BACK_BYTE - BACKDROP_WIDTH_BYTE;
        custom->bltafwm = custom->bltalwm = 0xffff;
        custom->bltsize = ((BACKDROP_HEIGHT * BPL_COUNT) << HSIZEBITS) | (BACKDROP_WIDTH_WORD);
    }
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltapt  = linewobble_backdrop;
    custom->bltdpt  = (UBYTE*)bpl_backdrop + (X_RES_BYTE - BACKDROP_WIDTH_BYTE/4);
    custom->bltamod = 3*BACKDROP_WIDTH_BYTE/4;
    custom->bltdmod = X_PITCH_BACK_BYTE - (BACKDROP_WIDTH_BYTE/4);
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((BACKDROP_HEIGHT * BPL_COUNT) << HSIZEBITS) | (BACKDROP_WIDTH_WORD/4);

    // feedback blit vertically
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltapt  = (UBYTE*)bpl_backdrop;
    custom->bltdpt  = (UBYTE*)bpl_backdrop + X_PITCH_BACK_BPL_BYTE*BACKDROP_HEIGHT;
    custom->bltamod = 0;
    custom->bltdmod = 0;
    custom->bltsize = ((Y_RES * BPL_COUNT) << HSIZEBITS) | (X_PITCH_BACK_WORD);
    WaitBlit();

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_scroll[0];
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER | DMAF_SPRITE;

    // catch up
    while ((!isRunning) && (frameCounter < 12*4*64)) {}

    fc_start = frameCounter;

    int y_fc = (Y_RES-1)<<8;
    int y_ofc = 0, y_cofc = ((Y_RES/2) << 8L);
    int y_y = 0;
    int y_dy = 0, y_ddy = 0;
    int y_sfc = 0;
    int y_sy = 0, y_dsy = 0;

    int y_ffc  = 0;
    int y_dffc = 0;

    SHORT palidx = 0;

    int backdrop_scroll_y = 0, dbsy = 0x300;
    copPtr = copper_scroll[cop_idx];

    while((isRunning) && (frameCounter < 14*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        y_y = y_fc - y_ofc - y_ffc + y_cofc - ((Y_RES/2) << 8L);
        y_sy = y_sfc;
        copPtr = copper_bplpt[cop_idx];
        
        // process start
        // calculate start of wobble
        LONG y_start = 0;
        LONG y_start_sprite;
        if (y_y < 0) {
            y_start = -(y_y>>8);
            y_start_sprite = y_start - 45;
            y_y = 0<<8;
        } else {
            y_start_sprite = -(y_y>>8) - 45;
        }
        if (y_start > Y_RES) y_start = Y_RES;
        if (y_start_sprite > Y_RES+20) y_start_sprite = Y_RES;
        // set pf0 bitplane pointers
        for (int i = 0; i < BPL_COUNT; i++) {
            ULONG bpl_ptr = (ULONG)line_gfx + (i * X_RES_BYTE) + ((y_y >> 8) << 7);
            *(copPtr+1) = (APTR)(bpl_ptr >> 16);
            *(copPtr+3) = (APTR)(bpl_ptr & 0xFFFF);
            copPtr += 4;
        }

        // fill start with dummy modulo
        SHORT skip_palfix = 0;
        copPtr = copper_bplpt[cop_idx] + 3*4 + 3;
        for (int y = 0; y < y_start; y++) {
            *copPtr = -X_RES_BYTE;
            copPtr += 4;
            if (y == (255-44)) {
                copPtr += 2;
            }
        }
        // then show static stuff
        // the rest is filled with wobble
        twister_fill_copperlist(copPtr, Y_RES - y_start, y_y, y_dy, y_sy, y_dsy, skip_palfix);

        y_ffc += y_dffc;

        // advance
        if (frame_counter < 128) {
            y_fc -= 0x100;
        } else {
            y_fc = (253*y_fc) >> 8;
            y_cofc = (254*y_cofc) >> 8;
            y_ofc = sintab_256[((frame_counter-128) << 2) & 255] << 4;
        }
        if ((frame_counter > 1*50) && (frame_counter < 7*50)) {
            y_dy = ((frame_counter-1*50) >> 1);
        }

        if ((frame_counter > 4*50) && (frame_counter < 12*50)) {
            y_dsy = sintab_256[((frame_counter-4*50) << 0)&255]<<1;
            y_dsy += (frame_counter-4*50)<<2;
            y_sfc += (frame_counter-4*50)<<3;
        } else if (frame_counter >= 12*50) {
            y_dsy = (250*y_dsy) >> 8;
            //y_sfc = (250*y_sfc) >> 8;
        }

        if (frame_counter > 1*64*4+4*32) {
            y_dffc += (1 << 3);
            dbsy += (1 << 3) + (1 << 1);
        }

        WaitLineOrAbove(0x10);

        // set backdrop scroll position
        UBYTE* bplpos = (UBYTE*)bpl_backdrop + ((backdrop_scroll_y>>8)*X_PITCH_BACK_BPL_BYTE);
        COPPERLIST_SET_MOVE_LONG(copper_backdrop_scroll+0, bplpt[1], bplpos);
        COPPERLIST_SET_MOVE_LONG(copper_backdrop_scroll+4, bplpt[3], bplpos + 1*X_PITCH_BACK_BYTE);
        COPPERLIST_SET_MOVE_LONG(copper_backdrop_scroll+8, bplpt[5], bplpos + 2*X_PITCH_BACK_BYTE);
        backdrop_scroll_y -= dbsy; 
        backdrop_scroll_y &= ((BACKDROP_HEIGHT<<8)-1);

        // update sprite position
        SHORT shake_x = 0, shake_y;
        SHORT shake_vel = (frame_counter > 4*64) ? ((frame_counter-4*64)>>5) : 0;
        shake_x = ((((SHORT)rand() & 15) - 8) * shake_vel) >> 4;
        shake_y = ((((SHORT)rand() & 15) - 8) * shake_vel) >> 4;
        set_sprite(cop_idx, shake_x+((X_RES-SPRITE_WIDTH)>>1), y_start_sprite+shake_y, SPRITE_HEIGHT);

        // set palette
        copPtr = copper_palette;
        for (int i = 0; i < 14; i++) {
            *(copPtr+1) = pal_fade[palidx][i];
            copPtr += 2;
        }
        for (int i = 0; i < 3; i++) {
            *(copPtr+1) = pal_fade[palidx][11+i];
            copPtr += 2;
        }

        if ((frame_counter <= 4*32) && (palidx < 16)) palidx = frame_counter >> 3;
        if ((frame_counter >= 1*4*64 + 4*56) && (palidx < 17+16)) palidx = 16+((frame_counter - 1*4*64 - 4*56) >> 1);

        custom->cop2lc = (ULONG)copper_scroll[cop_idx];
        cop_idx ^= 1;

    }

    // kill sprites
    custom->dmacon = DMAF_SPRITE;
    copPtr = copper_spritept;
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &temp_2ndpf);
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;
}


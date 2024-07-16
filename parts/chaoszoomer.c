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
#include "../drawtext.h"
#include "chaoszoomer.h"

// chaoszoomer
#define X_RES           320
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH         384
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT_FX    1
#define X_PITCH_BPL           (X_PITCH*BPL_COUNT_FX)
#define X_PITCH_BPL_WORD      (X_PITCH_WORD*BPL_COUNT_FX)
#define X_PITCH_BPL_BYTE      (X_PITCH_BYTE*BPL_COUNT_FX)

#define BPL_MODULO_BYTE     ((X_PITCH - X_RES)/8)
#define BPL_MODULO_WORD     ((X_PITCH - X_RES)/16)

#define Y_RES               224
#define Y_PITCH             320
#define BPL_SIZE            (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE      (BPL_SIZE*BPL_COUNT_FX)

// text scroller
#define X_RES_TEXT          320
#define X_RES_TEXT_BYTE     (X_RES_TEXT/8)
#define X_RES_TEXT_WORD     (X_RES_TEXT/16)

#define X_PITCH_TEXT        384
#define X_PITCH_TEXT_BYTE     (X_PITCH_TEXT/8)
#define X_PITCH_TEXT_WORD     (X_PITCH_TEXT/16)

#define BPL_COUNT_TEXT          1
#define X_PITCH_TEXT_BPL        (X_PITCH_TEXT*BPL_COUNT_TEXT)
#define X_PITCH_TEXT_BPL_BYTE   (X_PITCH_TEXT_BYTE*BPL_COUNT_TEXT)
#define X_PITCH_TEXT_BPL_WORD   (X_PITCH_TEXT_WORD*BPL_COUNT_TEXT)

#define Y_PITCH_TEXT            1023
#define BPL_SIZE_TEXT           (X_PITCH_TEXT_BYTE*Y_PITCH_TEXT)
#define BPLSCREEN_SIZE_TEXT     (BPL_SIZE_TEXT*BPL_COUNT_TEXT)

// -------------------------
// resources

// copperlist
static USHORT* copper_main; 
static USHORT* copper_textscroll[2];   
static USHORT  cop_indent[2];  
// copperlist pointers
static USHORT* coplist_bpldata;
static USHORT* coplist_xshift;
static USHORT* coplist_palette;

// bitplane pointers
static UBYTE *bplbase;
static UBYTE *bplring[2], *bplring_render[2];

// text pointers
static UBYTE *bplring_text;
// prerendered text buffer
static UBYTE *bplring_textbuf;

static char *text[] = {
    "-------------------------",
    " welcome to the",
    " MULTIMATOGRAF 2024",
    " 29 - 30 april 2024",
    "-------------------------",
    "feel the natural",
    "oldskool scene spirit!",
    "- huge screen -",
    "- massive sound -",
    "- 15 awesome compos -",
    "- remote entries welcome! -",
};

struct chunk_table_t {
    SHORT   x;      // *2 (for table indexing)
    SHORT   y;      // *pitch
    SHORT   dst;    // ready destination address
};

#define MAX_CHUNKTABS 11

// chunk table
static struct {
    SHORT  rotate,  zoom;
    USHORT frame; 
} chunk_desc[MAX_CHUNKTABS] = {
    {0x100, -0x100, 4*56},
    {0x100, -0x140, 4*60},
    {0x100, -0x180, 4*64},
    {0x100, -0x200, 4*64*1 + 4*52},
    {0x120, -0x220, 4*64*1 + 4*56},
    {0x140, -0x240, 4*64*1 + 4*60},
    {0x160, -0x260, 4*64*2},
    {0x180, -0x280, 4*64*3 + 4*16},
    {0x140, -0x200, 4*64*3 + 4*20},
    {0x120, -0x180, 4*64*3 + 4*24},
    {0x100, -0x100, -1},
};
static struct chunk_table_t chunktab[MAX_CHUNKTABS+1][128];
static USHORT max_chunks, current_chunktab = 0;

// bltcon0 precalc table
static USHORT bltcon0_tab[16];

// shift table
static SHORT shift_tab_x[32], shift_tab_y[32];

void calc_tables() {
    for (SHORT i = 0; i < 16; i++) {
        bltcon0_tab[i] = (A_TO_D | SRCA | DEST) + (((16 - i) & 15) << ASHIFTSHIFT);
    }

    for (SHORT i = 0; i < 32; i++) {
        SHORT shift = 0;
        // generate offsets
        for (int j = 0; j < 5; j++) {
            shift <<= 1;
            shift |= (i >> j) & 1;
        }
        shift -= 32;
        
        shift_tab_x[i] = shift;
        shift_tab_y[i] = shift * X_PITCH_BPL_BYTE;
    }

}

// init chunk table
void chunktab_init(struct chunk_table_t *chunktab, SHORT zoom_factor, SHORT rotate_factor) {
    // do top to bottom, right to left
    const SHORT chunksize = 32;
    max_chunks = 0;

    // get screen size in chunks
    int xc = ((X_RES/2) + chunksize-1)/(chunksize), yc = ((Y_RES/2) + chunksize-1)/(chunksize);

    for (SHORT x = xc-0; x >= -xc+0; x--) {
        for (SHORT y = -yc-0; y <= yc+0; y++) {           
            //1x1
            SHORT src_x = x * chunksize + (((-y) * zoom_factor) >> 8) + (( (x) * rotate_factor) >> 8) + (X_PITCH/2);
            SHORT src_y = y * chunksize + (((x)  * zoom_factor) >> 8) - (((-y) * rotate_factor) >> 8) + (Y_PITCH/2);
            SHORT dst_x = x * chunksize + (X_PITCH/2) - 16;
            SHORT dst_y = y * chunksize + (Y_PITCH/2);
            chunktab[max_chunks].x   = (src_x);
            chunktab[max_chunks].y   = (src_y * X_PITCH_BPL_BYTE);
            chunktab[max_chunks].dst = (dst_x >> 3) + (dst_y * X_PITCH_BPL_BYTE);
            max_chunks++;
        }
    }
}

void draw_chunks(UBYTE *dst, UBYTE *src, struct chunk_table_t *chunktab, SHORT xshift, SHORT yshift) {
    struct chunk_table_t *chunk = chunktab;
    USHORT chunkpos = max_chunks;
    const USHORT bltsize = ((32 * BPL_COUNT_FX * 1) << HSIZEBITS) | (48/16);
    
    // pre-init blit
    WaitBlit();
    custom->bltcon1 = 0;
    custom->bltamod = ((X_PITCH - 48) / 8);
    custom->bltdmod = ((X_PITCH - 48) / 8);
    custom->bltafwm = 0xFFFF;
    custom->bltalwm = 0xFFFF;
    do {
        APTR    bltapt  = (APTR)src + chunk->y + yshift + ((chunk->x + xshift - 1) >> 3);
        APTR    bltdpt  = (APTR)dst + chunk->dst;
        USHORT  bltcon0 = bltcon0_tab[((chunk->x + xshift) & 15)];
        WaitBlit();
        custom->bltcon0 = bltcon0;
        custom->bltadat = 0;
        custom->bltapt  = bltapt;
        custom->bltdpt  = bltdpt;
        custom->bltsize = bltsize;
        chunk++;
    } while (--chunkpos);
}

// palette (4bpl?)
static USHORT chaoszoomer_pal[4] = {
    // main layer
    0x0124, 0x036A, 0xEEF, 0xFFF,
};
static USHORT chaoszoomer_pal_fade[17][4];
static USHORT chaoszoomer_pal_fadeout[4] = {0xFFF, 0xFFF, 0xFFF, 0xFFF};

int chaoszoomer_free()
{
    FreeMem(copper_main, 256);
    FreeMem(copper_textscroll[0], 1024);
    FreeMem(copper_textscroll[1], 1024);
    return 0;
}


// called during demo init
int chaoszoomer_init() {
    // allocate copperlist memory
    copper_main             = (USHORT*)AllocMem(256,  MEMF_CHIP | MEMF_CLEAR);
    copper_textscroll[0]    = (USHORT*)AllocMem(1024, MEMF_CHIP | MEMF_CLEAR);
    copper_textscroll[1]    = (USHORT*)AllocMem(1024, MEMF_CHIP | MEMF_CLEAR);

    if ((copper_main==0) || (copper_textscroll[0]==0) || (copper_textscroll[1]==0)) return 1;

    // init bpl pointers 
    bplbase = bpl_pool;
    bplring[0] = bplbase + ((Y_PITCH-Y_RES)/2)*X_PITCH_BPL_BYTE + ((X_PITCH_BYTE-X_RES_BYTE)/2);
    bplring[1] = bplring[0] + BPLSCREEN_SIZE*1;

    bplring_render[0] = bplbase;
    bplring_render[1] = bplring_render[0] + BPLSCREEN_SIZE*1;

    bplring_text = bplbase + BPLSCREEN_SIZE*2;

    // init chunk table
    for (int i = 0; i < MAX_CHUNKTABS; i++) {
        chunktab_init(chunktab[i], chunk_desc[i].rotate, chunk_desc[i].zoom);
    }
    calc_tables();

    // calcualte fade
    pal_lerp(chaoszoomer_pal_fade[0], chaoszoomer_pal, chaoszoomer_pal_fadeout, 4, 16);

    // init text
    text_init(A_OR_C);
    text_build_pitch_lookup(X_PITCH_TEXT_BYTE, X_PITCH_TEXT_BYTE);

    // fill copperlist
    USHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 1024, 0);

    // init data fetch/display window
    copPtr = setLowres320Wide(copPtr, Y_RES);
    *copPtr++ = offsetof(struct Custom, ddfstrt);
    *copPtr++ = 0x0030;
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (1<<10)/*dual pf*/|(1<<9)/*color*/|((BPL_COUNT_FX+BPL_COUNT_TEXT)<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    coplist_xshift = copPtr;
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0<<6;

    ///set bitplane modulo
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
    *copPtr++=(USHORT)((BPL_COUNT_TEXT*X_PITCH_TEXT_BYTE - X_RES_BYTE) - 2);
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
    *copPtr++=(USHORT)((BPL_COUNT_FX*X_PITCH_BYTE - X_RES_BYTE) - 2);

	// set bitplane pointers
    coplist_bpldata = copPtr;	
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], bplring_text - 2);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], bplring[0] - 2);

    // set colors here
    coplist_palette = copPtr;
    COPPERLIST_ADD_MOVE(copPtr, color[0],   chaoszoomer_pal[0]);
    COPPERLIST_ADD_MOVE(copPtr, color[8+1], chaoszoomer_pal[1]);
    COPPERLIST_ADD_MOVE(copPtr, color[0+1], chaoszoomer_pal[2]);

    // jump to copper2
    *copPtr++ = offsetof(struct Custom, copjmp2);
    *copPtr++ = 0x7fff;

    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    // init 2nd copperlist
    for (int c = 0; c < 2; c++) {
        copPtr = copper_textscroll[c];
        // copper top indent (scrolled)
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)-1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], 0xFFF);
        cop_indent[0] = copPtr - copper_textscroll[c];
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)-1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], chaoszoomer_pal[0]);
        // pal fix
        *copPtr++=0xffdf;
        *copPtr++=0xfffe;
        // copper bottom indent (scrolled)
        cop_indent[1] = copPtr - copper_textscroll[c];
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)+Y_RES+1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], 0xFFF);
        COPPERLIST_ADD_WAIT(copPtr, 4, 44+((256-Y_RES)>>1)+Y_RES+1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], chaoszoomer_pal[0]);


        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    return 0;
}

static WORD xorbrush[] __attribute__((section (".MEMF_CHIP"))) = {
    0xC000, 0x0000,
    0xC000, 0x0000,
};
static WORD xorbrush4[] __attribute__((section (".MEMF_CHIP"))) = {
    0x6000, 0x0000,
    0xF000, 0x0000,
    0xF000, 0x0000,
    0x6000, 0x0000
};
static WORD xorbrush6[] __attribute__((section (".MEMF_CHIP"))) = {
    0x7800, 0x0000,
    0xFC00, 0x0000,
    0xFC00, 0x0000,
    0xFC00, 0x0000,
    0xFC00, 0x0000,
    0x7800, 0x0000
};

void chaoszoomer_draw_trail(UBYTE *dst, WORD* brush, int x, int y, int rop, int w) {
    // center
    y -= (w>>1); x -= (w>>1);

    // pre-init blit
    APTR    bltapt  = (APTR)brush;
    APTR    bltdpt  = (APTR)dst + (y * X_PITCH_BPL_BYTE) + (x >> 3);
    USHORT  bltcon0 = rop | SRCA | SRCC | DEST | ((x & 15) << ASHIFTSHIFT);
    USHORT  bltsize = ((w * BPL_COUNT_FX * 1) << HSIZEBITS) | (32/16);
    WaitBlit();
    custom->bltcon1 = 0;
    custom->bltamod = 0;
    custom->bltcmod = ((X_PITCH - 32) / 8);
    custom->bltdmod = ((X_PITCH - 32) / 8);
    custom->bltafwm = 0xFFFF;
    custom->bltalwm = 0xFFFF;
    custom->bltcon0 = bltcon0;
    custom->bltapt  = bltapt;
    custom->bltcpt  =
    custom->bltdpt  = bltdpt;
    custom->bltsize = bltsize;
};

#define A_NAND_C (NABC|NANBC)

struct brush_info_t {
    struct {
        WORD* brush;
        SHORT size;
    } trail, center;
};
static struct brush_info_t brushinfo[] = {
    {{xorbrush,  2}, {xorbrush,  2}},
    {{xorbrush4, 4}, {xorbrush6, 6}},
};

// the main show!
int chaoszoomer_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;

    // clear effect buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltamod = 0;
    custom->bltdpt  = (APTR)bplbase;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_PITCH * BPL_COUNT_FX * 1) << HSIZEBITS) | (X_PITCH_WORD);

    // clear text buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltamod = 0;
    custom->bltdpt  = (APTR)bplring_text;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_PITCH_TEXT * BPL_COUNT_TEXT) << HSIZEBITS) | (X_PITCH_TEXT_WORD);

    // start blitter to copy image to framebuffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST | (8 << ASHIFTSHIFT);
    custom->bltcon1 = 0;
    custom->bltapt  = (APTR)multik_logo_80x80;
    custom->bltamod = (80/8);   // skip logo's bpl1
    custom->bltdpt  = (APTR)bplbase + (X_PITCH_BPL_BYTE*(((Y_PITCH - 80)>>1))) + ((X_PITCH_BYTE - (80/8)) >> 1);
    custom->bltdmod = (BPL_COUNT_FX*X_PITCH_BYTE - (80 / 8));
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((80 * 1 * 1) << HSIZEBITS) | (80 / 16);

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = copper_textscroll[1];
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    // draw text 
    {
        SHORT y = Y_RES + ((Y_RES - 5*25)>>1);
        SHORT text_y = 0;
        for (int i = 0; i < 11; i++) {
            SHORT xx = (X_RES - text_get_length(&font_main, text[i])) >> 1;
            text_draw(bplring_text, &font_main, text[i], xx, y + text_y);
            text_y += 25;
            if (i == 4) {
                y += Y_RES + Y_RES/2 + 35;
                text_y = 0;
            }
        }
    }

    int current_x = 0;

    SHORT shift_x, shift_y, shift_x_prev = 0, shift_y_prev = 0;

    int palidx = 0, trails = 0, copidx = 0;
    int indent_x = 0;

    int fc_start = frameCounter;

    // disable sprite DMA
    custom->dmacon = DMAF_SPRITE;

    // clear sprite pointers
    for (int i = 0; i < 8; i++) {
        custom->sprpt[i] = temp_2ndpf;
    }

    // indent innerloop
    while ((isRunning) && (frameCounter < 6*4*64)) {
    //while ((isRunning) && (frameCounter < 1*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        USHORT *copPtr = copper_textscroll[copidx];
        COPPERLIST_SET_WAIT(copPtr + cop_indent[0], ((104+indent_x)>>2),       44+((256-Y_RES)>>1)-1, -1);
        COPPERLIST_SET_WAIT(copPtr + cop_indent[1], ((128+X_RES-indent_x)>>2), 44+((256-Y_RES)>>1)+Y_RES, -1);
        if (indent_x < X_RES+48) indent_x += 4; else {
            // fix glitch on top
            COPPERLIST_SET_WAIT(copPtr + cop_indent[0], 4, 44+((256-Y_RES)>>1), -1);
        }

        // set copperlist
        custom->cop2lc = copper_textscroll[copidx];
        copidx ^= 1;
    };
    fc_start = frameCounter;

    LONG text_scroll_y = 0, text_scroll_dy = (3 << 8)-(1<<6)-(1<<5)-(1<<4);
    while((isRunning)  && (frameCounter < 10*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        shift_x = shift_tab_x[frame_counter & 31];
        shift_y = shift_tab_y[frame_counter & 31];

        // draw chaoszoomer chunks
        draw_chunks(bplring_render[1], bplring_render[0], chunktab + current_chunktab, shift_x - shift_x_prev, shift_y - shift_y_prev);

        // draw trails
        if (frame_counter > 4*32) {
            int brushidx = (frame_counter >> 9);
            if (frame_counter < 3*4*64 + 4*48) {
                if ((frame_counter < 1*4*64) && (trails < 8)) trails = ((frame_counter - 4*16) >> 4);
                if ((frame_counter > 3*4*64 + 4*16) && (trails > 0)) trails = 8 - (((frame_counter - (3*4*64 + 4*16))) >> 2);
                for (int i = 0; i < trails; i++) {
                    int x = sintab_256[(((frame_counter << 1) + (i << 5))) & 255] >> 2;
                    int y = sintab_256[(((frame_counter << 1) + (i << 6) + (i << 4))) & 255] >> 2;
                    chaoszoomer_draw_trail(bplring_render[1], brushinfo[brushidx].trail.brush, x + (X_PITCH/2) - shift_x, y + (Y_PITCH/2) - shift_x, A_XOR_C, brushinfo[brushidx].trail.size);
                }
            }
            chaoszoomer_draw_trail(bplring_render[1], brushinfo[brushidx].center.brush, (X_PITCH/2) - shift_x, (Y_PITCH/2) - shift_x, A_NAND_C, brushinfo[brushidx].center.size);
        } 
        
        // advance chunk table
        if (frame_counter >= chunk_desc[current_chunktab].frame) current_chunktab++;

        shift_x_prev = shift_x;
        shift_y_prev = shift_y;

        // wait for semi-visible screen
        WaitLineOrAbove(0x10);

        // advance bitplane ring buffer
        UBYTE* bplhead = bplring[0];
        bplring[0] = bplring[1];
        bplring[1] = bplhead;

        bplhead = bplring_render[0];
        bplring_render[0] = bplring_render[1];
        bplring_render[1] = bplhead;

        SHORT shift_x_mod = shift_x;

        // do sine wobble for text
        LONG  text_sin_x = (text_scroll_dy*sintab_256[(frame_counter<<1) & 255])>>12;
        LONG  text_sin_y = (text_scroll_dy*sintab_256[((frame_counter<<1)+(frame_counter<<0)) & 255])>>4L;

        // set bitplane pointers
        COPPERLIST_SET_MOVE_LONG(coplist_bpldata,   bplpt[0], bplring_text + (((text_scroll_y+text_sin_y)>>8L) * X_PITCH_TEXT_BPL_BYTE) + (text_sin_x>>3));
        COPPERLIST_SET_MOVE_LONG(coplist_bpldata+4, bplpt[1], bplring[0] + ((-shift_x_mod >> 4) << 1) - shift_y);
        *coplist_xshift = ((15 - (-shift_x_mod & 15)) << 4) | (15 - (text_sin_x & 15));

        // set palette
        if ((frame_counter >= 3*4*64 + 4*48) && (palidx < 16)) palidx = ((frame_counter - 3*4*64 - 4*48) >> 2);
        USHORT *copPtr = coplist_palette;
        *(copPtr+1) = chaoszoomer_pal_fade[palidx][0];
        *(copPtr+3) = chaoszoomer_pal_fade[palidx][1];
        *(copPtr+5) = chaoszoomer_pal_fade[palidx][2];

        // set more palette :)
        copPtr = copper_textscroll[copidx];
        *(copPtr + cop_indent[0] + 3) = chaoszoomer_pal_fade[palidx][0];
        *(copPtr + cop_indent[1] + 7) = chaoszoomer_pal_fade[palidx][0];

        // set copperlist
        custom->cop2lc = copper_textscroll[copidx];
        copidx ^= 1;

        // scroll text 
        if (text_scroll_y < (((Y_RES*3) + (Y_RES/2))) << 8) text_scroll_y += text_scroll_dy;
        if (frame_counter < 4*32) {if (text_scroll_dy > 0) text_scroll_dy -= (1 << 2);} else 
        if (frame_counter < 1*4*64+4*32) text_scroll_dy = 0; else
        if (frame_counter < 2*4*64+4*0) text_scroll_dy += (1 << 2)+(1<<1); else 
        if (frame_counter < 2*4*64+4*32) {if (text_scroll_dy > 0) text_scroll_dy -= (1 << 2)+(1<<1);} else 
        if (frame_counter < 3*4*64+4*32) text_scroll_dy = 0; else text_scroll_dy += (1 << 3);
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}

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
#include "stars.h"

#define X_RES           320
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)
#define X_PITCH         512
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT_STARS     2
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT_STARS)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT_STARS)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT_STARS)
#define Y_RES           256
#define Y_PITCH         288
#define BPL_SIZE       (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE (BPL_SIZE*BPL_COUNT_STARS)
#define BUF_COUNT       3

#define X_RES_LOW       256
#define X_RES_LOW_BYTE  (X_RES_LOW/8)
#define X_RES_LOW_WORD  (X_RES_LOW/16)

#define Y_RES_LOW       192

// logo stuff - 320 bytes it sufficient i think :)
#define BPL_COUNT_LOGO          2
#define X_PITCH_LOGO            320
#define X_PITCH_LOGO_WORD       (X_PITCH_LOGO/16)
#define X_PITCH_LOGO_BYTE       (X_PITCH_LOGO/8)
#define X_PITCH_LOGO_BPL        (X_PITCH_LOGO*BPL_COUNT_LOGO)
#define X_PITCH_LOGO_BPL_WORD   (X_PITCH_LOGO_WORD*BPL_COUNT_LOGO)
#define X_PITCH_LOGO_BPL_BYTE   (X_PITCH_LOGO_BYTE*BPL_COUNT_LOGO)
#define Y_PITCH_LOGO            256

// perspective correction table (TODO - PRECOMPILE THIS!)
signed short stars_persp_table[2*256*64];

// bitplane pointers
static UBYTE* bplbase;
static UBYTE* bplring[3];
static UBYTE* bplring_view[3];

// logo storage
static UBYTE* bpl_logo;

// incbined logo
INCBIN_CHIP(demarination_logo, "gfx/demarination_logo.bpl");

// maximum stars count
enum {
    STARS_COUNT = 448,
};

// the stars themself
typedef union {
    struct {
        signed char x, z, y, dummy;
    };
    unsigned long val;
} dot_t;

dot_t stars_buf[STARS_COUNT];
static BYTE current_cop1 = 0;

// copperlist
static USHORT* copper_main[2];
// copperlist pointers
static USHORT* copper_colors[16];
static USHORT* coplist_bpldata[2];
static USHORT* coplist_colors[2];
static USHORT* coplist_sprpt[2];
static USHORT* coplist_modulo[2];

static USHORT* copper2l;

// mf sprite
static USHORT* multik_sprite[(80/16)][80*2+2+2] __attribute__((section (".MEMF_CHIP")));

// base colors
static USHORT stars_colors[16] = {
    // starfield.................logo_bpl...........logo_spr.....
    0x000, 0x444, 0x999, 0xEEE, 0xFFF, 0x333, 0x111, 0x400, 0xF00
};
static USHORT stars_colors_fadein[16] = {
    0xFFF, 0xFFF, 0xFFF, 0xFFF, 0xFFF, 0xFFF, 0xFFF, 0xFFF, 0xFFF
};
static USHORT stars_colors_fadeout[16] = {
    0x124, 0x124, 0x124, 0x124, 0x124, 0x124, 0x124, 0x124, 0x036A
};

// color fades
static USHORT stars_pal_fade[34][16];

// speccy "tweaked" palette
static USHORT stars_colors_zx_fadein[8][4] = {
    // fade in
    {0x000, 0x000, 0x000, 0x000},
    {0x000, 0x00C, 0x00C, 0x00C},
    {0x000, 0xC0C, 0xC0C, 0xC0C},
    // running
    {0x000, 0xCCC, 0xCCC, 0xCCC},
    // fade to white
    {0x00C, 0xCCC, 0xCCC, 0xCCC},
    {0xC0C, 0xCCC, 0xCCC, 0xCCC},
    {0xCCC, 0xFFF, 0xFFF, 0xFFF},
    // WHITE
    {0xFFF, 0xFFF, 0xFFF, 0xFFF},
};

static USHORT stars_bplofs[2] = {
    (X_PITCH_BYTE * ((Y_RES - Y_RES_LOW)>>1)) + ((X_RES_BYTE - X_RES_LOW_BYTE)>>1),
    0
};

static USHORT stars_modulo[2] = {
    (BPL_COUNT_STARS*X_PITCH_BYTE - X_RES_LOW_BYTE),
    (BPL_COUNT_STARS*X_PITCH_BYTE - X_RES_BYTE),
};

int stars_free()
{
    FreeMem(copper_main[0], 512);
    FreeMem(copper_main[1], 512);
    return 0;
}

// called during demo init
int stars_init() {
    // calc persp table
    for (int z = 0; z < 64; z++) {
        signed long sz = (128 * 24) / (65 - z);
        for (int x = -128; x < 128; x++) {
            signed long s = (x * sz) >> 7;
            //signed long s = (x * 24) / (65 - z);
            signed long xx = s + X_RES/2;
            signed long yy = s + Y_RES/2;
            if ((xx < 0) || (xx >= X_RES)) {
                xx = X_RES;
            }
            if ((yy < 0) || (yy >= Y_RES)) {
                yy = 0x8000 >> 7;
            }
            stars_persp_table[0      + (z << 8) + (x + 128)] = xx;
            stars_persp_table[256*64 + ((x + 128) << 6) + z] = (((unsigned long)yy) << 7);

        }
        custom->color[0] = (USHORT)z;
    }

    // generate stars
    for (int i = 0; i < STARS_COUNT; i++) {
        stars_buf[i].val = rand() ^ (rand() >> 16);
        custom->color[0] = (USHORT)i;
    }

    // allocate copperlist memory
    copper_main[0] = (USHORT*)AllocMem(512, MEMF_CHIP | MEMF_CLEAR);
    copper_main[1] = (USHORT*)AllocMem(512, MEMF_CHIP | MEMF_CLEAR);
    
    if ((copper_main[0]==0) || (copper_main[1]==0)) return 1;

    // init bpl pointers 
    bplbase = bpl_pool;
    bplring[0] = bplbase;
    bplring[1] = bplbase + BPLSCREEN_SIZE*1;
    bplring[2] = bplbase + BPLSCREEN_SIZE*2;
    bpl_logo   = bplbase + BPLSCREEN_SIZE*3;

    bplring_view[0] = bplbase         + ((X_PITCH_BYTE - X_RES_BYTE)/2) + (X_PITCH_BYTE*((Y_PITCH - Y_RES)/2));
    bplring_view[1] = bplring_view[0] + BPLSCREEN_SIZE*1;
    bplring_view[2] = bplring_view[0] + BPLSCREEN_SIZE*2;

    for (int i = 0; i < 2; i++) {
        // init main copperlist
        USHORT *copPtr = copper_main[i];

        // init data fetch/display window
        copPtr = i == 0 ? setLowres256Wide(copPtr, Y_RES_LOW) : setLowres320Wide(copPtr, Y_RES);
        //enable bitplanes    
        *copPtr++ = offsetof(struct Custom, bplcon0);
        *copPtr++ = (1<<10)/*dual pf*/|(1<<9)/*color*/|((4)<<12)/*num bitplanes*/;
        *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
        *copPtr++ = 0;
        *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
        *copPtr++ = (4<<3)|(0<<0);//0x24;            //Sprites have priority over playfields

        //set bitplane modulo
        coplist_modulo[i] = copPtr;
        *copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
        *copPtr++ = (i == 0) ? -X_RES_LOW_BYTE : -X_RES_BYTE;
        *copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
        *copPtr++ = (USHORT)stars_modulo[i];

        // set bitplane pointers
        coplist_bpldata[i] = copPtr;
        const UBYTE* planes[2];
        for(int a=0;a<2;a++)
            planes[a]=((UBYTE*)bplring_view[0] + (X_PITCH_BYTE * a) + stars_bplofs[i]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], planes[0]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], planes[1]);
        
        // set 1st playfield as null
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], temp_2ndpf);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], temp_2ndpf);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[4], temp_2ndpf);

        coplist_colors[i] = copPtr;
        COPPERLIST_ADD_MOVE(copPtr, color[0], i == 0 ? stars_colors_zx_fadein[0][0] : stars_colors[0]);
        for (int c = 1; c < 4; c++) {
            COPPERLIST_ADD_MOVE(copPtr, color[8+c], i == 0 ? stars_colors_zx_fadein[0][c] : stars_colors[c]);
        }
        COPPERLIST_ADD_MOVE(copPtr, color[0+1], 0x000);
        COPPERLIST_ADD_MOVE(copPtr, color[0+2], 0x000);
        COPPERLIST_ADD_MOVE(copPtr, color[0+3], 0x000);
        COPPERLIST_ADD_MOVE(copPtr, color[16+0+2], stars_pal_fade[0][4]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+0+3], stars_pal_fade[0][5]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+4+2], stars_pal_fade[0][4]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+4+3], stars_pal_fade[0][5]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+8+2], stars_pal_fade[0][4]);
        COPPERLIST_ADD_MOVE(copPtr, color[16+8+3], stars_pal_fade[0][5]);

        // add sprites
        coplist_sprpt[i] = copPtr;
        for (int i = 0; i < (80/16); i++)
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], multik_sprite[i]);
        for (int i = (80/16); i < 8; i++) {
            COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &temp_2ndpf);
        }

        // terminate the copperlist
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    // generate palette interpolation
    pal_lerp((USHORT*)stars_pal_fade[0],  stars_colors_fadein, stars_colors,  16, 16);
    pal_lerp((USHORT*)stars_pal_fade[17], stars_colors, stars_colors_fadeout, 16, 16);

    // generate sprite
    USHORT* src = (USHORT*)multik_logo_80x80;
    for (int i = 0; i < (80/16); i++) {
        sprite_cut(
            multik_sprite[i], src, src+(80/16), (160-16)/8, (160-16)/8, 80,
            128+(i<<4)+((X_RES-80)>>1), 44+((Y_RES-80)>>1)
        );
        src++;
    }

    // init sprites

    return 0;
}

static void stars_draw_asm(USHORT* bpl, long count, long offset) {
    register volatile const void* _a0 ASM("a0") = bpl;
    register                int   _d0 ASM("d0") = count;
    register                int   _d1 ASM("d1") = offset;

    __asm volatile (
        "movem.l %%d0-%%d7/%%a1-%%a6,-(%%sp)\n"
        "jsr _stars_draw_stars\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a1-%%a6"
    : "+rf" (_d0), "+rf" (_d1), "+rf"(_a0)
    :
    : "cc", "memory");
    return;
}

// generate random star
void gen_new_star(int frame_counter, int scroll, int mask) {
    int x, y, z;
    //if (frame_counter & 1) {
        int phase = (frame_counter<<1) + (sintab_256[(frame_counter>>3) & 255] << 6);
        x = (sintab_256[phase & 255]>>2) + 0  + 128;
        y = (costab_256[phase & 255]>>2) - 16 + 128;
        z = (-frame_counter) & mask;
    /*} else {
        int phase = (-frame_counter<<2);
        x = (sintab_256[phase & 255]>>2) + 0  + 128;
        y = (costab_256[phase & 255]>>2) - 16 + 128;
        z = (frame_counter >> 1) & 63;
    }*/
    
    
    stars_buf[frame_counter & 255].val = (((y << 14) + (z << 8) + x) << 1) + scroll;
}

// the main show!
int stars_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;

    // clear current buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bplring_view[0];
    custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES * BPL_COUNT_STARS) << HSIZEBITS) | (X_RES_WORD);

    // clear logo buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bpl_logo;
    custom->bltdmod = X_PITCH_LOGO_BYTE - X_RES_BYTE;
    custom->bltafwm = custom->bltalwm = 0xffff;
    //custom->bltsize = ((Y_PITCH_LOGO * BPL_COUNT_LOGO) << HSIZEBITS) | (X_RES_WORD);
    custom->bltsize = (((132+85+100)*BPL_COUNT_LOGO) << HSIZEBITS) | (X_RES_WORD);

    // blit logo to buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltcon1 = 0;
    custom->bltapt  = demarination_logo;
    custom->bltdpt  = bpl_logo + 8 + ((132 + 85) * X_PITCH_LOGO_BPL_BYTE);
    custom->bltamod = 0;
    custom->bltdmod = X_PITCH_LOGO_BYTE - (192/8);
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((35 * BPL_COUNT_LOGO) << HSIZEBITS) | ((192/16));
    WaitBlit();

    custom->dmacon = DMAF_COPPER;
    custom->cop1lc = (ULONG)copper_main[current_cop1];
    //custom->copjmp1 = 0x7fff; //start coppper
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    int prev_x = 0, prev_y = 0, prev_z = 0;
    int scroll_x = 0, scroll_y = 0, scroll_z = 0;
    SHORT logo_scroll_y = (85<<8), dys = (0<<8);

    int fcstart = frameCounter;
    LONG scrollpos = 0, scrollpos_old = 0;

    SHORT swing = 0;
    LONG zz = 0, zspeed = 0xC0;

    SHORT palidx = 0, spr_palidx = 0, stars_count = STARS_COUNT >> 1;
    while((isRunning)  && (frameCounter < 6*4*64 - 4*32)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);
        //custom->color[0] = 0x0007;

        if (frameCounter == 4*64*4) {
            // LAME way to kickstart sprite DMA
            custom->dmacon = DMAF_SETCLR | DMAF_SPRITE;
            // TODO: show pf2

        }

        int frame_counter = frameCounter - fcstart;

        // clear
		WaitBlit();
		custom->bltcon0 = A_TO_D | DEST;
		custom->bltcon1 = 0;
		custom->bltadat = 0;
		custom->bltdpt  = bplring_view[2];
		custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = ((Y_RES * BPL_COUNT_STARS) << HSIZEBITS) | (X_RES_WORD);

        // draw stars? :)
        stars_draw_asm(
            (USHORT*)bplring_view[1],
            stars_count,
            scrollpos - scrollpos_old
        );
        scrollpos_old = scrollpos;

        //custom->color[0] = 0x0000;

        // get new scrolling position
        SHORT x = (swing * sintab_256[(frame_counter << 1) & 255]) >> 8;
        SHORT y = (swing * costab_256[(frame_counter + (frame_counter>>3)) & 255] >> 8);
        SHORT z = (zz >> 8) & 63;
        scrollpos = ((y << 14L) + (z << 8L) + x) << 1;

        // redraw random star
        if (frameCounter >= 3*64*4 + 0*4) gen_new_star(frame_counter, scrollpos_old, (frameCounter >= 4*64*4) ? 127 : 63);

        if (frameCounter > 2*64*4 + 32*4) {
            if (swing < 160) {
                if ((frameCounter & 1) == 0) swing++;
            }
        }

        // z speed + palette
        zz += zspeed;
        if (frameCounter < 2*64*4) {
            zspeed++;
            USHORT *copPtr = coplist_colors[0];
            for (int i = 0; i < 4; i++)
                *(copPtr+1+(i<<1)) = stars_colors_zx_fadein[palidx][i];
            
            if ((frame_counter <  4*16) && (palidx < 3)) palidx = (frame_counter) >> 4;
            if ((frame_counter >= 4*56) && (palidx < 7)) palidx = 3 + ((frame_counter - 4*56) >> 2);
        } else {
            zspeed = 1 << 8;
            current_cop1 = 1;
            USHORT *copPtr;
            if (frameCounter == 2*64*4) {
                palidx = 0;
            }
            copPtr = coplist_colors[1];
            for (int i = 0; i < 4; i++)
                *(copPtr+1+(i<<1)) = stars_pal_fade[palidx][i];
            for (int i = 0; i < 3; i++)
                *(copPtr+1+(4<<1)+(i<<1)) = stars_pal_fade[spr_palidx][4+i];
            for (int i = 0; i < 3; i++) {
                *(copPtr+(4<<1)+(3<<1)+(i<<2)+1) = stars_pal_fade[spr_palidx][4+3+0];
                *(copPtr+(4<<1)+(3<<1)+(i<<2)+3) = stars_pal_fade[spr_palidx][4+3+1];
            }
            if (palidx < 16)     palidx = (frameCounter - 2*64*4) >> 1;
            if ((frameCounter >= 5*4*64) && (palidx < 17+16))  {
                spr_palidx = palidx = 16 + ((frameCounter - 5*4*64) >> 2);
                dys += (1 << 3);
            }
            if ((frameCounter >= 5*4*64) && (stars_count > 0)) stars_count -= 2; else stars_count = STARS_COUNT;
        }
        if (frameCounter >= 4*64*4) {
            if (spr_palidx < 16) spr_palidx = (frameCounter - 4*64*4) >> 1;
        }

      	// advance bitplane ring buffer
        UBYTE* bplhead = bplring[0];
        bplring[0] = bplring[1];
        bplring[1] = bplring[2];
        bplring[2] = bplhead;

        bplhead = bplring_view[0];
        bplring_view[0] = bplring_view[1];
        bplring_view[1] = bplring_view[2];
        bplring_view[2] = bplhead;

        // set bitplane pointers
        USHORT* copPtr = coplist_bpldata[current_cop1];
        const UBYTE* planes[2];
        for(int a=0;a<2;a++)
            planes[a]=((UBYTE*)bplring_view[0] + (X_PITCH_BYTE * a) + stars_bplofs[current_cop1]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], planes[0]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], planes[1]);

        if (frameCounter > 4*64*4-1) {  // sprite+bf0 desync fix :p
            UBYTE *logoptr = bpl_logo + ((logo_scroll_y>>8)*X_PITCH_LOGO_BPL_BYTE);
            COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], logoptr);
            COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], logoptr + X_PITCH_LOGO_BYTE);
            copPtr = coplist_modulo[1];
            *(copPtr+1) = (BPL_COUNT_LOGO*X_RES_BYTE) - X_RES_BYTE;
        }

        if (logo_scroll_y >= 0xFF) logo_scroll_y -= dys;
        custom->dmacon = DMAF_COPPER;   // stop copper
        custom->cop1lc = (ULONG)copper_main[current_cop1];
        custom->dmacon = DMAF_SETCLR | DMAF_COPPER;
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}

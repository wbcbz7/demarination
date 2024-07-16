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
#include "../drawtext.h"
#include "parallax.h"

#define X_RES           320
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH_PF0         1280
#define X_PITCH_PF0_WORD    (X_PITCH_PF0/16)
#define X_PITCH_PF0_BYTE    (X_PITCH_PF0/8)

#define X_PITCH_PF1         768
#define X_PITCH_PF1_WORD    (X_PITCH_PF1/16)
#define X_PITCH_PF1_BYTE    (X_PITCH_PF1/8)

#define X_PITCH_PF1_CAR         336
#define X_PITCH_PF1_CAR_WORD    (X_PITCH_PF1_CAR/16)
#define X_PITCH_PF1_CAR_BYTE    (X_PITCH_PF1_CAR/8)

#define BPL_COUNT_PF0           3
#define X_PITCH_PF0_BPL         (X_PITCH_PF0*BPL_COUNT_PF0)
#define X_PITCH_PF0_BPL_WORD    (X_PITCH_PF0_WORD*BPL_COUNT_PF0)
#define X_PITCH_PF0_BPL_BYTE    (X_PITCH_PF0_BYTE*BPL_COUNT_PF0)
#define BPL_COUNT_PF1           3
#define X_PITCH_PF1_BPL         (X_PITCH_PF1)
#define X_PITCH_PF1_BPL_WORD    (X_PITCH_PF1_WORD)
#define X_PITCH_PF1_BPL_BYTE    (X_PITCH_PF1_BYTE)

#define BPL_MODULO1_BYTE        ((X_PITCH_PF0_BPL - X_RES)/8)
#define BPL_MODULO1_WORD        ((X_PITCH_PF0_BPL - X_RES)/16)
#define BPL_MODULO2_BYTE        ((X_PITCH_PF1 - X_RES)/8)
#define BPL_MODULO2_WORD        ((X_PITCH_PF1 - X_RES)/16)
#define BPL_MODULO2_CAR_BYTE    ((X_PITCH_PF1_CAR*BPL_COUNT_PF0 - X_RES)/8)
#define BPL_MODULO2_CAR_WORD    ((X_PITCH_PF1_CAR*BPL_COUNT_PF0 - X_RES)/16)

#define Y_RES                   256
#define Y_RES_PF0               244
#define Y_RES_PF1_L0            38
#define Y_RES_PF1_L1            28
#define Y_RES_PF1_L2            27
#define Y_RES_PF1_L3            63
#define Y_RES_PF1_CAR           42
#define PF1_CAR_COUNT           3
#define PF0_BPL_SIZE            (X_PITCH_PF0_BYTE*Y_RES_PF0)
#define PF1_L0_BPL_SIZE         (X_PITCH_PF1_BYTE*Y_RES_PF1_L0)
#define PF1_L1_BPL_SIZE         (X_PITCH_PF1_BYTE*Y_RES_PF1_L1*16)
#define PF1_L2_BPL_SIZE         (X_PITCH_PF1_BYTE*Y_RES_PF1_L2)
#define PF1_L3_BPL_SIZE         (X_PITCH_PF1_BYTE*Y_RES_PF1_L3)
#define PF1_CAR_BPL_SIZE        (X_PITCH_PF1_CAR_BYTE*BPL_COUNT_PF0*Y_RES_PF1_CAR*PF1_CAR_COUNT)
#define Y_RASTER_START          (44+((256-Y_RES)>>1))

#define PF0_DATA_X_RES          1024
#define PF0_DATA_X_RES_BYTE     (PF0_DATA_X_RES/8)
#define PF0_DATA_X_RES_WORD     (PF0_DATA_X_RES/16)

#define PF1_L0_DATA_X_RES          256
#define PF1_L0_DATA_X_RES_BYTE     (PF1_L0_DATA_X_RES/8)
#define PF1_L0_DATA_X_RES_WORD     (PF1_L0_DATA_X_RES/16)

#define PF1_L1_DATA_X_RES          256
#define PF1_L1_DATA_X_RES_BYTE     (PF1_L1_DATA_X_RES/8)
#define PF1_L1_DATA_X_RES_WORD     (PF1_L1_DATA_X_RES/16)

#define PF1_L2_DATA_X_RES          256
#define PF1_L2_DATA_X_RES_BYTE     (PF1_L2_DATA_X_RES/8)
#define PF1_L2_DATA_X_RES_WORD     (PF1_L2_DATA_X_RES/16)

#define PF1_L3_DATA_X_RES          256
#define PF1_L3_DATA_X_RES_BYTE     (PF1_L3_DATA_X_RES/8)
#define PF1_L3_DATA_X_RES_WORD     (PF1_L3_DATA_X_RES/16)

#define E_WERK_WIDTH    256
#define E_WERK_HEIGHT   154
#define E_WERK_X        0
#define E_WERK_Y        (180-E_WERK_HEIGHT)

#define SALUT_WIDTH     256
#define SALUT_HEIGHT    145
#define SALUT_X         (X_PITCH_PF0-SALUT_WIDTH)
#define SALUT_Y         (180-SALUT_HEIGHT)

#define ROAD_WIDTH      128
#define ROAD_HEIGHT     56
#define ROAD_Y          180

#define TROTUAR_WIDTH   256
#define TROTUAR_HEIGHT  132
#define TROTUAR_Y       (ROAD_Y-TROTUAR_HEIGHT)

#define CAR_WIDTH       112
#define CAR_HEIGHT      42
#define CAR_X           0
#define CAR_Y           0

// y is relative to current position
#define CAR2_WIDTH      96
#define CAR2_HEIGHT     39
#define CAR2_X          0
#define CAR2_Y          (71-39)

#define PICKUP_WIDTH    96
#define PICKUP_HEIGHT   56
#define PICKUP_X        0
#define PICKUP_Y        (56-39)

#define GRZVK_WIDTH     96
#define GRZVK_HEIGHT    71
#define GRZVK_X         0
#define GRZVK_Y         0

// -------------------------
// resources
INCBIN_CHIP(ewerk_gfx, "gfx/ewerk.bpl")
INCBIN_CHIP(salut_gfx, "gfx/salut.bpl")
INCBIN_CHIP(road_gfx, "gfx/road.bpl")
INCBIN_CHIP(trotuar_gfx, "gfx/trotuar.bpl")
INCBIN(pf0_pal, "gfx/ewerk.pal")

INCBIN_CHIP(car_gfx, "gfx/car.bpl")
INCBIN(pf1_car_pal, "gfx/car.pal")

INCBIN_CHIP(parallax_pf1_layer0, "gfx/parallax_pf1_layer0.bpl")
INCBIN(parallax_pf1_layer0_pal, "gfx/parallax_pf1_layer0.pal")

INCBIN_CHIP(parallax_pf1_layer1, "gfx/parallax_pf1_layer1.bpl")
INCBIN(parallax_pf1_layer1_pal, "gfx/parallax_pf1_layer1.pal")

INCBIN_CHIP(parallax_pf1_layer2, "gfx/parallax_pf1_layer2.bpl")
INCBIN(parallax_pf1_layer2_pal, "gfx/parallax_pf1_layer2.pal")

INCBIN_CHIP(parallax_pf1_layer3, "gfx/parallax_pf1_layer3.bpl")
INCBIN(parallax_pf1_layer3_pal, "gfx/parallax_pf1_layer3.pal")

INCBIN_CHIP(parallax_car2_gfx, "gfx/car2.bpl")
INCBIN(parallax_car2_pal, "gfx/car2.pal")

INCBIN_CHIP(parallax_pickup_gfx, "gfx/pickup.bpl")
INCBIN(parallax_pickup_pal, "gfx/pickup.pal")

INCBIN_CHIP(parallax_grzvk_gfx, "gfx/grzvk.bpl")
INCBIN(parallax_grzvk_pal, "gfx/grzvk.pal")

// copperlist
static USHORT* copper_main;
static USHORT* copper_scroll[2];
static USHORT* copper_sprite;

// bitplane pointers
static UBYTE *bplbase;
// playfield 0
static UBYTE *bpl_pf0;
// pf1, main layer (768x100)
static UBYTE *bpl_pf1_layer0;
// pf1, sub layer 1  (768x32, 16 preshifts)
static UBYTE *bpl_pf1_layer1;
// pf1, sub layer 2  (768x32, 16 preshifts)
static UBYTE *bpl_pf1_layer2;
// pf1, sub layer 2  (768x32, 16 preshifts)
static UBYTE *bpl_pf1_layer3;
// pf1, car (320x48x3)
static UBYTE *bpl_pf1_car;
// blank lines (512*Y_RES 1bpl)
static UBYTE *bpl_blank;
// filled lines (512*Y_RES 1bpl)
static UBYTE *bpl_filled;

// pf1 palette
static USHORT pf2_pal[8] = {
    0x000, 0x77A, 0x889, 0x889, 0x444, 0x444, 0x444, 0x444 
};

// bg fade palette
static USHORT bg_fade[] = {
    0x112, 0x112, 0x213, 0x214, 0x225, 0x336, 0x447, 0x558
};

// bg fade pos
static SHORT bg_fade_pos[] = {
    0,      -70,   -55,   -42,   -30,   -20,   -12,    -5
};

struct sprite_desc_t {
    USHORT  *ptr;
    SHORT   ox, oy, w, h;     // x/y and width/heigh
};

// sprites (chipmem eater)
static USHORT car2_spr[(CAR2_WIDTH/16)][2+CAR2_HEIGHT*2+2]    __attribute__((section (".MEMF_CHIP")));
static USHORT pickup_spr[(PICKUP_WIDTH/16)][2+PICKUP_HEIGHT*2+2] __attribute__((section (".MEMF_CHIP")));
static USHORT grzvk_spr[(GRZVK_WIDTH/16)][2+GRZVK_HEIGHT*2+2] __attribute__((section (".MEMF_CHIP")));

// game mode?
static USHORT game_mode = 0;

// vecluate collision

static __attribute__ ((noinline)) int get_collision(int xa1, int xa2, int ya1, int ya2, int xb1, int xb2, int yb1, int yb2) {
    return ((xa1 <= xb2) && (xa2 >= xb1) && (ya1 <= yb2) && (ya2 >= yb1)); 
}

// get joystick Y coordinate
static SHORT joy_get_y() {
    USHORT joy1dat = custom->joy1dat;
    USHORT up = (((joy1dat >> 1) ^ (joy1dat >> 0)) & 1);
    USHORT dn = (((joy1dat >> 9) ^ (joy1dat >> 8)) & 1);
    return (up ? 1 : (dn ? -1 : 0));    
}

static USHORT* parallax_set_sprite_coplist(int sprite) {
    USHORT *copPtr = copper_sprite;
    USHORT *palptr;
    switch (sprite) {
        case 0:
            // car2
            for (int x = 0; x < (CAR2_WIDTH/16); x++) {
                COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[x], &car2_spr[x][0]);
            }
            palptr = (USHORT*)parallax_car2_pal;
            break;
        case 2:
            // null
            for (int x = 0; x < (GRZVK_WIDTH/16); x++) {
                COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[x], temp_2ndpf);
            }
            palptr = (USHORT*)parallax_grzvk_pal;
            break;
        default:
            // pickup
            for (int x = 0; x < (PICKUP_WIDTH/16); x++) {
                COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[x], &pickup_spr[x][0]);
            }
            palptr = (USHORT*)parallax_pickup_pal;
            break;
    }
    // kil lreamining sprites
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[6], temp_2ndpf);
    COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[7], temp_2ndpf);

    // sprite palette
    for (int j = 0; j < ((CAR2_WIDTH+31)/32); j++) {
        for (int i = 1; i < 4; i++)
            COPPERLIST_ADD_MOVE(copPtr, color[16+i+(j<<2)], palptr[i]);
    }
    return copPtr;
}

static void parallax_set_sprites(int sprite, int x, int y) {
    // global for now (per-line gets later + it's a little bit tricky :)
    USHORT ystart = 44+y, yend = 0;
    x += 128;
    if (sprite == 0) { 
        yend = ystart + CAR2_HEIGHT;
        for (int i = 0; i < (CAR2_WIDTH/16); i++) {
            car2_spr[i][0] = ((ystart & 0xFF) << 8) | ((x >> 1) & 0xFF);
            car2_spr[i][1] = ((yend   & 0xFF) << 8) | (x & 1) | ((ystart & 0x100) >> 6) | ((yend & 0x100) >> 7);
            x += 16;
        }
    } else {
        yend = ystart + PICKUP_HEIGHT;
        for (int i = 0; i < (PICKUP_WIDTH/16); i++) {
            pickup_spr[i][0] = ((ystart & 0xFF) << 8) | ((x >> 1) & 0xFF);
            pickup_spr[i][1] = ((yend   & 0xFF) << 8) | (x & 1) | ((ystart & 0x100) >> 6) | ((yend & 0x100) >> 7);
            x += 16;
        }
    }
}

static char scores[6] = "00000\0";
static void draw_scores(SHORT score, SHORT x, SHORT y) {
    for (int i = 0; i < 5; i++) {
        scores[5-i] = (score % 10) + '0';
        score /= 10;
    }
    text_draw(bpl_pf0, &font_main, scores, x, y);
}

// called during demo init
int parallax_init() {
    // allocate copperlist memory
    copper_main         = (USHORT*)AllocMem(256,   MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[0]    = (USHORT*)AllocMem(2048,  MEMF_CHIP | MEMF_CLEAR);
    copper_scroll[1]    = (USHORT*)AllocMem(2048,  MEMF_CHIP | MEMF_CLEAR);

    if ((copper_main == 0) || (copper_scroll[0] == 0) || (copper_scroll[1] == 0)) return 1;

    // allocate bitplane memory
    bplbase         = bpl_pool;
    bpl_pf0         = bplbase;
    bpl_pf1_layer0  = bpl_pf0        + PF0_BPL_SIZE*BPL_COUNT_PF0;
    bpl_pf1_layer1  = bpl_pf1_layer0 + PF1_L0_BPL_SIZE;
    bpl_pf1_layer2  = bpl_pf1_layer1 + PF1_L1_BPL_SIZE;
    bpl_pf1_layer3  = bpl_pf1_layer2 + PF1_L2_BPL_SIZE;
    bpl_pf1_car     = bpl_pf1_layer3 + PF1_L3_BPL_SIZE;
    bpl_blank       = bpl_pf1_car    + PF1_CAR_BPL_SIZE;
    bpl_filled      = bpl_blank      + X_PITCH_PF1_BYTE*Y_RES;

    // cut sprites
    USHORT* src = (USHORT*)parallax_pickup_gfx;
    for (int i = 0; i < (PICKUP_WIDTH/16); i++) {
        sprite_cut(
            &pickup_spr[i][0], src, src+(PICKUP_WIDTH/16), (PICKUP_WIDTH*2-16)/8, (PICKUP_WIDTH*2-16)/8, PICKUP_HEIGHT,
            128, Y_RES
        );
        src++;
    }
    src = (USHORT*)parallax_car2_gfx;
    for (int i = 0; i < (CAR2_WIDTH/16); i++) {
        sprite_cut(
            &car2_spr[i][0], src, src+(CAR2_WIDTH/16), (CAR2_WIDTH*2-16)/8, (CAR2_WIDTH*2-16)/8, CAR2_HEIGHT,
            128, Y_RES
        );
        src++;
    }

    return 0;
}

int parallax_free()
{
    FreeMem(copper_main, 256);
    FreeMem(copper_scroll[0], 2048);
    FreeMem(copper_scroll[1], 2048);
    return 0;
}

static SHORT emit_palfix;

static __attribute__ ((noinline)) USHORT* emitWait(USHORT *copPtr, SHORT x, SHORT y, USHORT mask) {
    USHORT* cop = copPtr;
    if (y < 20) return cop;
    if ((y > 255) && (emit_palfix)) {
        emit_palfix = 0;
        *cop++=0xffdf;
        *cop++=0xfffe;
    }
    COPPERLIST_ADD_WAIT(cop, x, y&0xFF, mask);
    return cop;
}

static ULONG x_sc_pf0 = 0, x_sc_pf1_l0 = 0, x_sc_pf1_l1 = 0, x_sc_pf1_l2 = 0, x_sc_pf1_l3 = 0;
static  LONG dx_sc_pf0 = 0, dx_sc_pf1_l0 = 0, dx_sc_pf1_l1 = 0, dx_sc_pf1_l2 = 0, dx_sc_pf1_l3 = 0;
static const LONG mdx_sc_pf0 = (5 << 8), mdx_sc_pf1_l0 = (3 << 8), mdx_sc_pf1_l1 = (2 << 8), mdx_sc_pf1_l2 = (1 << 8), mdx_sc_pf1_l3 = (4 << 8);

static ULONG score, score_best;

static LONG x_car  = (X_RES-CAR_WIDTH-56)<<8, x_car_sin;
static LONG dx_car = 0;

static SHORT height_vstr[2] = {CAR2_HEIGHT, PICKUP_HEIGHT};
static SHORT lane_ofs[2] = {0, 22};
static LONG  x_vstr = -(CAR2_WIDTH<<8), dx_vstr = (6<<8), mdx_vstr = (6<<8);
static SHORT idx_vstr = 1, lane_vstr = 0;  // 0 - car2, 1 - pickup
static SHORT delay_vstr = 80;
static SHORT lane_car;

// debug switch
//#define NO_Y_SCROLL

static void init_vstr() {
    if (game_mode) {
        x_vstr  = -(CAR2_WIDTH<<10);
        x_car   = (X_RES-CAR_WIDTH-0)<<8;
    } else {
        x_vstr = -(CAR2_WIDTH<<8);
        x_car   = (X_RES-CAR_WIDTH-56)<<8;
    }
    lane_car = lane_ofs[1];
    
}

static void spawn_new_car() {
    dx_vstr = mdx_vstr;
    x_vstr = X_RES<<8;
    idx_vstr = rand() & 1;
    lane_vstr = rand() & game_mode;
    delay_vstr = ((rand()^(rand()>>16)) & 127) + 32;
}

#ifdef NO_Y_SCROLL
static LONG y_pos = (0<<8);
#else 
static LONG y_pos = (340<<8);
#endif
static LONG dy_pos = 0;
static LONG oy_pos = 27;

static void parallax_build_screen(SHORT cop_idx, SHORT frame_counter) {
    USHORT *copPtr = copper_scroll[cop_idx];
    UBYTE *bplptr;

#ifndef NO_Y_SCROLL
    if (frame_counter < 4*21+1) {
        dy_pos -= (1 << 4);
    } else if (frame_counter < 4*44) {
        if (y_pos > 0) dy_pos += (1<<5)+1;
    } else if ((game_mode == 0) && (frame_counter >= 3*64*4+4*46)) {
        dy_pos += (1 << 5)+(1<<4);
    } else dy_pos = 0;
#endif

    if ((frame_counter > 4*32) && (frame_counter <= 3*64*4+4*16)) {
        if (dx_sc_pf0 <= mdx_sc_pf0)       dx_sc_pf0    += (3 << 1); else dx_sc_pf0 = mdx_sc_pf0;
        if (dx_sc_pf1_l0 <= mdx_sc_pf1_l0) dx_sc_pf1_l0 += (2 << 1); else dx_sc_pf1_l0 = mdx_sc_pf1_l0;
        if (dx_sc_pf1_l1 <= mdx_sc_pf1_l1) dx_sc_pf1_l1 += (1 << 1); else dx_sc_pf1_l1 = mdx_sc_pf1_l1;
        if (dx_sc_pf1_l2 <= mdx_sc_pf1_l2) dx_sc_pf1_l2 += (1 << 0); else dx_sc_pf1_l2 = mdx_sc_pf1_l2;
        if (dx_sc_pf1_l3 <= mdx_sc_pf1_l3) dx_sc_pf1_l3 += (2 << 1)+(1<<0); else dx_sc_pf1_l3 = mdx_sc_pf1_l3;
    } else if (game_mode == 0) {
        if (dx_sc_pf0 > 0)       dx_sc_pf0 -= (3 << 2); else dx_sc_pf0 = 0;
        if (dx_sc_pf1_l0 > 0) dx_sc_pf1_l0 -= (2 << 2); else dx_sc_pf1_l0 = 0;
        if (dx_sc_pf1_l1 > 0) dx_sc_pf1_l1 -= (1 << 2)+1; else dx_sc_pf1_l1 = 0;
        if (dx_sc_pf1_l2 > 0) dx_sc_pf1_l2 -= (1 << 1)+1; else dx_sc_pf1_l2 = 0;
        if (dx_sc_pf1_l3 > 0) dx_sc_pf1_l3 -= (2 << 2)+(1<<1); else dx_sc_pf1_l3 = 0;
    }

    // do syncro
    y_pos += dy_pos;
    if (y_pos < 0) {
        y_pos = 0;
        if (dy_pos < 0) dy_pos = 0;
    }

    // enable palfix
    emit_palfix = 1;

    // at first, point all bitplanes to blank line
    for (int bpl = 0; bpl < 6; bpl++) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[bpl], bpl_blank + 2);
    }
    // and set modulo to the same line
    COPPERLIST_ADD_MOVE(copPtr, bpl1mod, -X_RES_BYTE-2);
    COPPERLIST_ADD_MOVE(copPtr, bpl2mod, -X_RES_BYTE-2);
    // set PF1 priority over PF2
    COPPERLIST_ADD_MOVE(copPtr, bplcon2, (0<<6)|(4<<3)|(4<<0));

    SHORT oy_sin = 0;
    if ((frame_counter > 4*18) && (frame_counter < 3*64*4+4*14)) oy_sin = sintab_256[((frame_counter - 4*18)<<1) & 255] >> 6;

    volatile LONG yyy = (y_pos >> 8) + oy_pos + oy_sin;

    for (int fd = 1; fd < 8; fd++) {
        SHORT yy = Y_RASTER_START+yyy+bg_fade_pos[fd];

        copPtr = emitWait(copPtr, 3, yy-2, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], bg_fade[fd]);
        copPtr = emitWait(copPtr, 3, yy-1, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], bg_fade[fd-1]);
        copPtr = emitWait(copPtr, 3, yy, -1);
        COPPERLIST_ADD_MOVE(copPtr, color[0], bg_fade[fd]);
    }

    // -------- MAIN SCROLL POSITION ----------
    // get integer scroll 
    USHORT xp0 = x_sc_pf0>>8, xp1l0 = x_sc_pf1_l0>>8, xp1l1 = x_sc_pf1_l1>>8, xp1l2 = x_sc_pf1_l2>>8, xp1l3 = x_sc_pf1_l3>>8;
    const SHORT y_pf0 = 0, y_pf1_l2 = 1+0, y_pf1_l1 = 1+22, y_pf1_l0 = 1+54, y_pf1_l3 = 1+94, y_pf1_car = 0+161+lane_car;// + (16 + (sintab_256[(frame_counter<<2)&255]>>3)); 
    USHORT xc = (x_car>>8) + (sintab_256[(x_car_sin>>9) & 255] >> 4);

    USHORT car_idx = "\x0\x2\x1\x2"[(xp0>>4)&3];

    // resolve fine scroll to fix pf1 layer1/2
    SHORT pf1_l0_x_fine = (xp1l0 & 15);
    SHORT pf1_l1_x_fine = (xp1l1) & 15;
    SHORT pf1_l2_x_fine = (xp1l2 - xp1l1) & 15;

    // init PF0
    bplptr = bpl_pf0 + ((xp0) >> 3);
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf0, -1);
    // set pf0 bitplanes
    for (int bpl = 0; bpl < 6; bpl += 2) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0+bpl], bplptr);
        bplptr += X_PITCH_PF0_BYTE;
    }
    COPPERLIST_ADD_MOVE(copPtr, bplcon1, ((15 - pf1_l1_x_fine) << 4) | ((15 - (xp0 & 15))));
    COPPERLIST_ADD_MOVE(copPtr, bpl1mod, BPL_MODULO1_BYTE-2);

    // init PF1 L1
    // (with x=0, we have time to update 5 bitplane pointers)
    bplptr = bpl_pf1_layer1 + ((xp1l2-pf1_l2_x_fine) >> 3) + (((15 - pf1_l2_x_fine)&15) * X_PITCH_PF1_BYTE * Y_RES_PF1_L1) + 2;
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l2, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+0], bplptr);
    COPPERLIST_ADD_MOVE(copPtr, bpl2mod, BPL_MODULO2_BYTE-2);

    bplptr = bpl_pf1_layer2 + ((xp1l1) >> 3) + 2;
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l1, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+2], bplptr);

    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l2+Y_RES_PF1_L2-1, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+0], bpl_filled);

    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l1+Y_RES_PF1_L1-1, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+2], bpl_filled);
    COPPERLIST_ADD_MOVE(copPtr, bplcon1, ((15 - pf1_l0_x_fine) << 4) | ((15 - (xp0 & 15))));

    bplptr = bpl_pf1_layer0 + ((xp1l0) >> 3) + 2;
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l0, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+4], bplptr);

    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l0+Y_RES_PF1_L0-1, -1);
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+4], bpl_filled);

    // after all parallax layers of PF1 are done, disable PF1
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_l3, -1);
    bplptr = bpl_pf1_layer3 + ((xp1l3) >> 3) + 2;
    COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+4], bplptr);
    COPPERLIST_ADD_MOVE(copPtr, bplcon1, ((15 - xp1l3) << 4) | ((15 - (xp0 & 15))));
    COPPERLIST_ADD_MOVE(copPtr, color[10], pf2_pal[7]);
    COPPERLIST_ADD_MOVE(copPtr, color[11], pf2_pal[7]);
    COPPERLIST_ADD_MOVE(copPtr, color[14], 0x232);
    COPPERLIST_ADD_MOVE(copPtr, color[15], 0x232);

#if 1
    // set car stuff
    // preset palette
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_car-1, -1);
    for (int c = 1; c < 8; c++) {
        COPPERLIST_ADD_MOVE(copPtr, color[8+c], *((USHORT*)pf1_car_pal + c));
    }

    bplptr = bpl_pf1_car + X_PITCH_PF1_CAR_BYTE*CAR_HEIGHT*BPL_COUNT_PF0*car_idx + (xc >> 3);
    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_car, -1);
    // set pf1 bitplanes
    for (int bpl = 0; bpl < 6; bpl += 2) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+bpl], bplptr);
        bplptr += X_PITCH_PF1_CAR_BYTE;
    }
    COPPERLIST_ADD_MOVE(copPtr, bplcon1, ((15 - (xc & 15)) << 4) | ((15 - (xp0 & 15))));
    COPPERLIST_ADD_MOVE(copPtr, bpl2mod, BPL_MODULO2_CAR_BYTE-2);
    COPPERLIST_ADD_MOVE(copPtr, bplcon2, (1<<6)|(4<<0)|(lane_car < lane_vstr ? (4<<3):(0<<3)));

    copPtr = emitWait(copPtr, 0, Y_RASTER_START+yyy+y_pf1_car+Y_RES_PF1_CAR, -1);
    // set pf1 bitplanes
    for (int bpl = 0; bpl < 6; bpl += 2) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1+bpl], bpl_blank);
    }
    COPPERLIST_ADD_MOVE(copPtr, bpl2mod, -X_RES_BYTE-2);
#endif

    // and terminate copperlist for now (fancy stuff later)
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    SHORT y_vstr = Y_RASTER_START+yyy + 160 + lane_ofs[lane_vstr] - height_vstr[idx_vstr];
    if ((y_vstr < 0) || (y_vstr > Y_RES)) y_vstr = Y_RES;

    // set opposite lane sprite
    parallax_set_sprites(idx_vstr, (x_vstr>>8)>X_RES ? X_RES : (x_vstr>>8), y_vstr);
    parallax_set_sprite_coplist(idx_vstr);

    // update scrolls
    x_car         += dx_car;
    x_car_sin     += dx_sc_pf0;
    score         += dx_sc_pf0;
    x_sc_pf0      += dx_sc_pf0;
    x_sc_pf1_l0   += dx_sc_pf1_l0;
    x_sc_pf1_l1   += dx_sc_pf1_l1;
    x_sc_pf1_l2   += dx_sc_pf1_l2;
    x_sc_pf1_l3   += dx_sc_pf1_l3;

    if (x_vstr < -(CAR2_WIDTH<<8)) {
        dx_vstr = 0;
        if (--delay_vstr == 0) {
            spawn_new_car();
        }
    } else {
        x_vstr        -= dx_vstr+dx_sc_pf0;
    }

    if (((frame_counter > 1*4*64) && (frame_counter < 2*4*64 + 4*64)) || (game_mode == 1)) {
        if (x_sc_pf0 > ((E_WERK_X + E_WERK_WIDTH + 256) << 8)) x_sc_pf0 -= (256<<8);
    }
    x_sc_pf1_l0 &= ((PF1_L0_DATA_X_RES<<8)-1);
    x_sc_pf1_l1 &= ((PF1_L1_DATA_X_RES<<8)-1);
    x_sc_pf1_l2 &= ((PF1_L1_DATA_X_RES<<8)-1);
    x_sc_pf1_l3 &= ((PF1_L1_DATA_X_RES<<8)-1);
}

// the main show!
int parallax_run(int hiddenMode) {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    game_mode = hiddenMode ? 1 : 0;
    init_vstr();

    // disable screen
    WaitVbl();

    // now the hardest part :D
    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;

    // clear blank buffer (used for empty lines)
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bpl_blank;
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES) << HSIZEBITS) | (X_PITCH_PF0_WORD);

    // then fill filled buffer
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltadat = 0xFFFF;
    custom->bltdpt  = bpl_filled;
    custom->bltdmod = 0;
    custom->bltsize = ((Y_RES) << HSIZEBITS) | (X_PITCH_PF0_WORD);

    // clear PF0
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltadat = 0x0000;
    custom->bltdpt  = bpl_pf0;
    custom->bltdmod = 0;
    custom->bltsize = ((Y_RES_PF0 * BPL_COUNT_PF0) << HSIZEBITS) | (X_PITCH_PF0_WORD);

    // blit e-werk
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltapt  = ewerk_gfx;
    custom->bltdpt  = (UBYTE*)bpl_pf0 + (SALUT_Y * (X_PITCH_PF0_BPL_BYTE)) + (E_WERK_X>>3);
    custom->bltamod = 0;
    custom->bltdmod = X_PITCH_PF0_BYTE - (E_WERK_WIDTH/8);
    custom->bltsize = ((E_WERK_HEIGHT * BPL_COUNT_PF0) << HSIZEBITS) | (E_WERK_WIDTH/16);

    // blit salut
    WaitBlit();
    custom->bltcon0 = A_TO_D | SRCA | DEST;
    custom->bltapt  = salut_gfx;
    custom->bltdpt  = (UBYTE*)bpl_pf0 + (SALUT_Y * (X_PITCH_PF0_BPL_BYTE)) + (SALUT_X>>3);
    custom->bltamod = 0;
    custom->bltdmod = X_PITCH_PF0_BYTE - (SALUT_WIDTH/8);
    custom->bltsize = ((SALUT_HEIGHT * BPL_COUNT_PF0) << HSIZEBITS) | (SALUT_WIDTH/16);

    // blit road
    for (volatile int x = 0; x < X_PITCH_PF0_BYTE; x += (ROAD_WIDTH/8)) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = road_gfx;
        custom->bltdpt  = (UBYTE*)bpl_pf0 + (ROAD_Y * (X_PITCH_PF0_BPL_BYTE)) + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF0_BYTE - (ROAD_WIDTH/8);
        custom->bltsize = ((ROAD_HEIGHT * BPL_COUNT_PF0) << HSIZEBITS) | (ROAD_WIDTH/16);
    }

    // blit trotuar
    for (volatile int x = ((E_WERK_X + E_WERK_WIDTH)/8); x < (SALUT_X/8); x += (TROTUAR_WIDTH/8)) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = trotuar_gfx;
        custom->bltdpt  = (UBYTE*)bpl_pf0 + (TROTUAR_Y * (X_PITCH_PF0_BPL_BYTE)) + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF0_BYTE - (TROTUAR_WIDTH/8);
        custom->bltsize = ((TROTUAR_HEIGHT * BPL_COUNT_PF0) << HSIZEBITS) | (TROTUAR_WIDTH/16);
    }

#if 1
    // blit pf1 l0 graphics
    for (int x = 0; x < X_PITCH_PF1_BYTE; x += PF1_L0_DATA_X_RES_BYTE) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = parallax_pf1_layer0;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer0 + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF1_BYTE - PF1_L0_DATA_X_RES_BYTE;
        custom->bltsize = ((Y_RES_PF1_L0) << HSIZEBITS) | (PF1_L0_DATA_X_RES_WORD);
    }
#endif

#if 1
    // blit PF1 l1 graphics
    for (int x = 0; x < X_PITCH_PF1_BYTE; x += PF1_L1_DATA_X_RES_BYTE) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = parallax_pf1_layer1;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer1 + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF1_BYTE - PF1_L1_DATA_X_RES_BYTE;
        custom->bltsize = ((Y_RES_PF1_L1) << HSIZEBITS) | (PF1_L1_DATA_X_RES_WORD);
    }
#endif
#if 1
    // generate shifts
    for (int x = 1; x < 16; x++) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST | ((x) << ASHIFTSHIFT);
        custom->bltapt  = (UBYTE*)bpl_pf1_layer1;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer1 + (x * X_PITCH_PF1_BYTE * Y_RES_PF1_L1);
        custom->bltamod = 0;
        custom->bltdmod = 0;
        custom->bltsize = ((Y_RES_PF1_L1) << HSIZEBITS) | (X_PITCH_PF1_BPL_WORD);
    }
#endif

#if 1
    // blit PF1 l2 graphics
    for (int x = 0; x < X_PITCH_PF1_BYTE; x += PF1_L2_DATA_X_RES_BYTE) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = parallax_pf1_layer2;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer2 + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF1_BYTE - PF1_L2_DATA_X_RES_BYTE;
        custom->bltsize = ((Y_RES_PF1_L2) << HSIZEBITS) | (PF1_L2_DATA_X_RES_WORD);
    }
#endif
#if 0
    // generate shifts
    for (int x = 1; x < 16; x++) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST | ((x) << ASHIFTSHIFT);
        custom->bltcon1 = 0;
        custom->bltapt  = (UBYTE*)bpl_pf1_layer2;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer2 + (x * X_PITCH_PF1_BYTE * Y_RES_PF1_L2);
        custom->bltamod = 0;
        custom->bltdmod = 0;
        custom->bltsize = ((Y_RES_PF1_L2) << HSIZEBITS) | (X_PITCH_PF1_BPL_WORD);
    }
#endif
#if 1
    // blit PF1 l3 graphics
    for (int x = 0; x < X_PITCH_PF1_BYTE; x += PF1_L3_DATA_X_RES_BYTE) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = parallax_pf1_layer3;
        custom->bltdpt  = (UBYTE*)bpl_pf1_layer3 + x;
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF1_BYTE - PF1_L3_DATA_X_RES_BYTE;
        custom->bltsize = ((Y_RES_PF1_L3) << HSIZEBITS) | (PF1_L3_DATA_X_RES_WORD);
    }
#endif

    // clear buffer for car
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltadat = 0x0000;
    custom->bltdpt  = bpl_pf1_car;
    custom->bltdmod = 0;
    custom->bltsize = ((Y_RES_PF1_CAR*BPL_COUNT_PF1*PF1_CAR_COUNT) << HSIZEBITS) | (X_PITCH_PF1_CAR_WORD);

    // blit car
    for (int y = 0; y < PF1_CAR_COUNT*CAR_HEIGHT; y += CAR_HEIGHT) {
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
        custom->bltapt  = (UBYTE*)car_gfx + ((y*CAR_WIDTH*BPL_COUNT_PF0)/8);
        custom->bltdpt  = (UBYTE*)bpl_pf1_car + (y * X_PITCH_PF1_CAR_BYTE * BPL_COUNT_PF0) + ((X_RES-CAR_WIDTH)/8);
        custom->bltamod = 0;
        custom->bltdmod = X_PITCH_PF1_CAR_BYTE - (CAR_WIDTH/8);
        custom->bltsize = ((CAR_HEIGHT * BPL_COUNT_PF0) << HSIZEBITS) | (CAR_WIDTH/16);
    }

    int fc_start = frameCounter;

    // fill copperlist
    USHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 256, 0);

    // init data fetch/display window, account for scroll
    //copPtr = setLowres320Wide(copPtr, Y_RES);
    //*copPtr++ = offsetof(struct Custom, ddfstrt);
    //*copPtr++ = 0x0030;
    COPPERLIST_ADD_MOVE(copPtr, diwstrt, 0x2C81)
    COPPERLIST_ADD_MOVE(copPtr, diwstop, 0x2CC1)
    COPPERLIST_ADD_MOVE(copPtr, ddfstrt, 0x0030)
    COPPERLIST_ADD_MOVE(copPtr, ddfstop, 0x00D0)
    // enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (1<<10)/*dual pf*/|(1<<9)/*color*/|((BPL_COUNT_PF0+BPL_COUNT_PF1)<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = (0<<6);//(0<<6)|(4<<0)|(4<<3);

    // set bitplane modulo - don't forget about scrolling
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)(BPL_MODULO1_BYTE - 2);
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)(BPL_MODULO2_BYTE - 2);

    // set colors
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i],   *((USHORT*)pf0_pal + i));
    }
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE(copPtr, color[i+8], *((USHORT*)pf2_pal + i));
    }
    // sprite
    copper_sprite = copPtr;
    copPtr = parallax_set_sprite_coplist(2);

    // jump to 2nd copperlist
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);

    // terminate copperlist
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    for (int cop = 0; cop < 2; cop++) {
        parallax_build_screen(cop, 0);
    }

    SHORT cop_idx = 1;

    if (game_mode) {
        text_init(A_OR_C);
        text_build_pitch_lookup(X_PITCH_PF0_BPL_BYTE, X_PITCH_PF0_BPL_BYTE);
    }

    custom->dmacon = DMAF_COPPER;   // stop copper
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_scroll[cop_idx ^ 1];
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER | DMAF_SPRITE;

    fc_start = frameCounter;

    while((isRunning) && ((game_mode == 1) || (frameCounter < 18*4*64))) {
    //while((isRunning) && (frameCounter < 4*4*64)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);

        int frame_counter = frameCounter - fc_start;

        if (game_mode) {
            WaitLineOrAbove(120);
            // clear old text
            WaitBlit();
            custom->bltcon0 = A_TO_D | DEST;
            custom->bltcon1 = 0;
            custom->bltadat = 0x0000;
            custom->bltdpt  = bpl_pf0 + (x_sc_pf0 >> (8+3));
            custom->bltamod = 0;
            custom->bltdmod = (X_PITCH_PF0_BPL_BYTE - ((X_RES+16)/8));
            custom->bltafwm = custom->bltalwm = 0xffff;
            custom->bltsize = ((16) << HSIZEBITS) | ((X_RES+16)/16);
            WaitBlit();

            // print new text
            draw_scores(score>>12,      (x_sc_pf0>>8), 15);
            draw_scores(score_best>>12, (x_sc_pf0>>8) + 96, 15);

            // reset hi scores by RMB
            if (MouseRight()) score_best = 0;

            // advance position
            SHORT joy_y = joy_get_y();
            lane_car += joy_y<<1;
            if (lane_car < lane_ofs[0]) lane_car = lane_ofs[0];
            if (lane_car > lane_ofs[1]) lane_car = lane_ofs[1];

            // calculate collisions
            volatile LONG real_xcar = (X_RES-CAR_WIDTH)-(x_car>>8);
            volatile LONG real_xvstr = (x_vstr>>8);
            if (get_collision(
                real_xcar+32,  real_xcar  + CAR_WIDTH-32,  lane_car-7, lane_car+7,
                real_xvstr+32, real_xvstr + CAR2_WIDTH-32, lane_ofs[lane_vstr]-7, lane_ofs[lane_vstr]+7
            )) {
                custom->color[0] = 0xF00;  
                if (score >= score_best) score_best = score;
                score = 0;

                // RESTART EVERYTHING!
                fc_start = frameCounter - 4*32;
                dx_sc_pf0 = 0;
                dx_sc_pf1_l0 = 0;
                dx_sc_pf1_l1 = 0;
                dx_sc_pf1_l2 = 0;
                dx_sc_pf1_l3 = 0;

                x_sc_pf0 = 0;
                x_sc_pf1_l0 = 0;
                x_sc_pf1_l1 = 0;
                x_sc_pf1_l2 = 0;
                x_sc_pf1_l3 = 0;

                lane_car = lane_ofs[1];

                spawn_new_car();
            }
        }

        parallax_build_screen(cop_idx, frame_counter);

        // apply new copperlist
        WaitLineOrAbove(0x10);
        custom->cop2lc = (ULONG)copper_scroll[cop_idx];
        cop_idx ^= 1;
    }

    // kill sprites
    custom->dmacon = DMAF_SPRITE;
    copPtr = copper_sprite;
    for (int i = 0; i < 8; i++) {
        COPPERLIST_ADD_MOVE_LONG(copPtr, sprpt[i], &temp_2ndpf);
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}
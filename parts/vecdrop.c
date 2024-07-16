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
#include "../linedraw.h"
#include "vecdrop.h"

#define X_RES           256
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH         512
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT       1
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT)

#define Y_RES           192
#define Y_PITCH         224
#define BPL_SIZE       (X_PITCH_BYTE*Y_PITCH)
#define BPLSCREEN_SIZE (BPL_SIZE*BPL_COUNT)

// bitplane pointers
static UBYTE* bplbase;
static UBYTE* bplring[2];           // 0 - displayed, 1 - drawn
static UBYTE* bplring_view[2];   // 0 - displayed, 1 - drawn

// copperlist
static USHORT* copper_main;
static USHORT* coplist_bpldata;
static USHORT* coplist_colors;

int vecdrop_free()
{
    FreeMem(copper_main, 512);
    return 0;
}


// called during demo init
int vecdrop_init() {
    // allocate copperlist memory
    copper_main = (USHORT*)AllocMem(512, MEMF_CHIP | MEMF_CLEAR);
    if (copper_main==0) return 1;

    // init bpl pointers 
    bplbase = bpl_pool;
    bplring[0] = bplbase;
    bplring[1] = bplbase + BPLSCREEN_SIZE*1;

    bplring_view[0] = bplbase         + ((X_PITCH_BYTE - X_RES_BYTE)/2) + (X_PITCH_BYTE*((Y_PITCH - Y_RES)/2));
    bplring_view[1] = bplring_view[0] + BPLSCREEN_SIZE*1;

    return 0;
}

static int anim_lastpos = 0;
static int animpos = 0;
static int animstop = 0;

enum {
    TOTAL_VERTICES = 4,
};
static struct vec4x shape[TOTAL_VERTICES] = {
    {-(X_RES/2), -(Y_RES/2), 0, 0},
    { (X_RES/2), -(Y_RES/2), 0, 0},
    { (X_RES/2),  (Y_RES/2), 0, 0},
    {-(X_RES/2),  (Y_RES/2), 0, 0}
};
static struct vec4x shape_p [TOTAL_VERTICES];
static struct vec2x shape_pt[TOTAL_VERTICES*2];
static struct vec2x shape_pc[TOTAL_VERTICES*2];

#if 1
static struct vec4x origin1 = {-2400, 0 , 16 << 7};
static struct vec4x origin2 = {0, 0 , 1 << 7};
static struct vec4x camrot1 = {(-4*128/2)<<8, 0 , (5*128/2)<<8};
static struct vec4x camrot2 = {0, 0, (1*128/2)<<8};
#else
static struct vec4x origin1 = {0, 0, 0xC0};
static struct vec4x origin2 = {0, 0, 0xC0};
static struct vec4x camrot1 = {0, 0, (2*128/2)<<8};
static struct vec4x camrot2 = {0, 0, (1*128/2)<<8};
#endif

static void vecdrop_draw(int frame_counter) {
    // setup blitter for line mode
    blt_line_setup(X_PITCH_BPL_BYTE);

    // limit maximum frame counter
    int f = (frame_counter & 127) + 1;

    // calculate offsets
    struct vec4x origin, camrot;

#if 1
    LONG f2 = (f*f) >> 4;
    origin.x = origin1.x + (((origin2.x - origin1.x)*(f))  >> 7);
    origin.y = origin1.y + (((origin2.y - origin1.y)*(f))  >> 7);
    origin.z = origin1.z + (((origin2.z - origin1.z)*(f2)) >> 10);
#else
    origin.x = origin.y = 0; origin.z = 256;
#endif

    //camrot.x = frame_counter;
    //camrot.z = frame_counter;
    camrot.x = (camrot2.x + ((camrot1.x - camrot2.x)*(f))>>7) >> 8;
    camrot.y = (camrot2.y + ((camrot1.y - camrot2.y)*(f))>>7) >> 8;
    camrot.z = (camrot2.z + ((camrot1.z - camrot2.z)*(f))>>7) >> 8;

    int xx = costab_256[camrot.z & 255];
    int yx = (costab_256[camrot.x & 255] * sintab_256[camrot.z & 255]) >> 7;
    int zx = (sintab_256[camrot.x & 255] * sintab_256[camrot.z & 255]) >> 7;

    int xy = -sintab_256[camrot.z & 255];
    int yy = (costab_256[camrot.x & 255] * costab_256[camrot.z & 255]) >> 7;
    int zy = (sintab_256[camrot.x & 255] * costab_256[camrot.z & 255]) >> 7;

    int xz = 0;
    int yz = -costab_256[camrot.x & 255];
    int zz = costab_256[camrot.x & 255];

    // transform
    for (int i = 0; i < TOTAL_VERTICES; i++) {
        // rotate/translate
        shape_p[i].x = ((shape[i].x*xx + shape[i].y*xy + shape[i].z*xz) >> 7) + origin.x;
        shape_p[i].y = ((shape[i].x*yx + shape[i].y*yy + shape[i].z*yz) >> 7) + origin.y;
        shape_p[i].z = ((shape[i].x*zx + shape[i].y*zy + shape[i].z*zz) >> 7) + origin.z;

        // project
        shape_pt[i].x = ((shape_p[i].x << 7L) / shape_p[i].z) + (X_PITCH>>1);
        shape_pt[i].y = ((shape_p[i].y << 7L) / shape_p[i].z) + (Y_PITCH>>1);
    }

    int vertices = 4-1;

    if (vertices >= 2) {
        for (int i = 0; i < vertices; i++) {
            if (shape_pt[i].y <= shape_pt[i+1].y) {
                blt_draw_line_pitch64(bplring[1], shape_pt[i].x, shape_pt[i].y, shape_pt[i+1].x, shape_pt[i+1].y);
            } else {
                blt_draw_line_pitch64(bplring[1], shape_pt[i+1].x, shape_pt[i+1].y, shape_pt[i].x, shape_pt[i].y);
            }
        }
        if (shape_pt[vertices].y <= shape_pt[0].y) {
            blt_draw_line_pitch64(bplring[1], shape_pt[vertices].x, shape_pt[vertices].y, shape_pt[0].x, shape_pt[0].y);
        } else {
            blt_draw_line_pitch64(bplring[1], shape_pt[0].x, shape_pt[0].y, shape_pt[vertices].x, shape_pt[vertices].y);
        }
    }
}

// the main show!
int vecdrop_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    int fc_start = frameCounter;

    // clear top 3 buffers with blitter, third with CPU (ignore modulo lol)
    WaitBlit();
    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0xFFFF;       // clear with color 1
    custom->bltdpt  = bplring[0];
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = (((Y_PITCH * BPL_COUNT * 2) - 1) << HSIZEBITS) | (X_PITCH_WORD);
    WaitBlit();
    
    SHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 1024, 0);

    // init data fetch/display window
    //copPtr = screenScanDefault(copPtr);
    copPtr = setLowres256Wide(copPtr, Y_RES);
    //enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*dual pf*/|(1<<9)/*color*/|((1)<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0<<6;//0x24;            //Sprites have priority over playfields

    //set bitplane modulo
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)((BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE));
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)((BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE));

    // set bitplane pointers
    coplist_bpldata = copPtr;
	const UBYTE* planes[1];
    planes[0]=((UBYTE*)bplring_view[0]);
    copPtr = copSetPlanes(0, coplist_bpldata, planes, 1);

    coplist_colors = copPtr;
    COPPERLIST_ADD_MOVE(copPtr,color[ 0], 0x000);
    COPPERLIST_ADD_MOVE(copPtr,color[ 1], 0x000);

    // terminate
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;
    
    custom->dmacon = DMAF_COPPER;
    custom->cop1lc = (ULONG)copper_main;
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

#if 1
    // animate boot
    playMusic = 0;
    for (int i = 0; i < 50; i++) WaitVbl();
    COPPERLIST_ADD_MOVE(coplist_colors,color[ 0], 0xC00);
    for (int i = 0; i < 70; i++) WaitVbl();
    COPPERLIST_ADD_MOVE(coplist_colors,color[ 0], 0x00C);
    for (int i = 0; i < 60; i++) WaitVbl();
    COPPERLIST_ADD_MOVE(coplist_colors,color[ 0], 0xCCC);
    for (int i = 0; i < 70; i++) WaitVbl();
    playMusic = 1;
#endif

    while(isRunning && (frameCounter < 32*4)) {
        // vbl
        volatile short fc = frameCounter;
        while (fc == frameCounter);
        //custom->color[0] = 0x0007;

        // clear
		WaitBlit();
		custom->bltcon0 = A_TO_D | DEST;
		custom->bltcon1 = 0;
		custom->bltadat = 0;
		custom->bltdpt  = bplring[1];
		custom->bltdmod = 0;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = ((Y_PITCH * BPL_COUNT) << HSIZEBITS) | (X_PITCH_WORD);
        //custom->color[0] = 0x0070;

        int frame_counter = frameCounter - fc_start;
        WaitBlit();
        vecdrop_draw(frame_counter);

        // area fill
        //custom->color[0] = 0x0700;
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
		custom->bltcon1 = FILL_OR | BC1F_DESC;
		custom->bltapt  = custom->bltdpt = bplring[1] + (X_PITCH_BYTE-2) + ((Y_PITCH-1) * X_PITCH_BYTE);
        custom->bltamod = custom->bltdmod = 0;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = (((Y_PITCH) * BPL_COUNT) << HSIZEBITS) | (X_PITCH_WORD);
        //custom->color[0] = 0x0070;

        // invert (YES!)
		WaitBlit();
		custom->bltcon0 = NABC | NANBC | NABNC | NANBNC | SRCA | DEST;
		custom->bltcon1 = 0;
		custom->bltapt  = custom->bltdpt  = bplring_view[1];
		custom->bltamod = custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = ((Y_RES * BPL_COUNT) << HSIZEBITS) | (X_RES_WORD);

        WaitLineOrAbove(0x10);

        // advance bitplane ring buffer
        UBYTE* bplhead = bplring[0];
        bplring[0] = bplring[1];
        bplring[1] = bplhead;

        bplhead = bplring_view[0];
        bplring_view[0] = bplring_view[1];
        bplring_view[1] = bplhead;

        // set bitplane pointers
        const UBYTE* planes[1];
        planes[0]=((UBYTE*)bplring_view[0]);
        copSetPlanes(0, coplist_bpldata, planes, 1);
        
        // terminate
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}

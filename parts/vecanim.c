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
#include "../linedraw.h"
#include "vecanim.h"

#define X_RES           320
#define X_RES_WORD      (X_RES/16)
#define X_RES_BYTE      (X_RES/8)

#define X_PITCH         512
#define X_PITCH_WORD    (X_PITCH/16)
#define X_PITCH_BYTE    (X_PITCH/8)

#define BPL_COUNT       1
#define X_PITCH_BPL         (X_PITCH*BPL_COUNT)
#define X_PITCH_BPL_WORD    (X_PITCH_WORD*BPL_COUNT)
#define X_PITCH_BPL_BYTE    (X_PITCH_BYTE*BPL_COUNT)

#define Y_RES           256
#define Y_PITCH         256
#define BPL_SIZE       (X_PITCH_BYTE*Y_RES)
#define BPLSCREEN_SIZE (BPL_SIZE*BPL_COUNT)

// bitplane pointers
static UBYTE* bplbase;
static UBYTE* bplring[6];   // 0..4 - displayed, 5 - drawn

// copperlist
static USHORT* copper_main;
static USHORT* copper_bpldata[2];
static USHORT* coplist_palette;

static USHORT vecanim_palette_base[16] = {
    0x224, 0x225, 0xA11, 0xA12, 0xC22, 0xC23, 0xC33, 0xC33,
    0xE44, 0xE44, 0xF44, 0xF44, 0xF55, 0xF55, 0xF55, 0xF55,
};
static USHORT vecanim_palette_fade[16];

static USHORT vecanim_palette[17][16];

// forward declaration
void vecanim_render();

int vecanim_free()
{
    FreeMem(copper_main, 512);
    FreeMem(copper_bpldata[0], 128);
    FreeMem(copper_bpldata[1], 128);
    return 0;
}

// called during demo init
int vecanim_init() {
    // allocate copperlist memory
    copper_main = (USHORT*)AllocMem(512, MEMF_CHIP | MEMF_CLEAR);
    copper_bpldata[0] = (USHORT*)AllocMem(128, MEMF_CHIP | MEMF_CLEAR);
    copper_bpldata[1] = (USHORT*)AllocMem(128, MEMF_CHIP | MEMF_CLEAR);
    if (copper_main==0 || copper_bpldata[0]==0 || copper_bpldata[1]==0) return 1;

    // init bpl pointers 
    bplbase = bpl_pool;
    bplring[0] = bplbase;
    bplring[1] = bplbase + BPLSCREEN_SIZE*1;
    bplring[2] = bplbase + BPLSCREEN_SIZE*2;
    bplring[3] = bplbase + BPLSCREEN_SIZE*3;
    bplring[4] = bplbase + BPLSCREEN_SIZE*4;
    bplring[5] = bplbase + BPLSCREEN_SIZE*5;

    // render animation
#ifndef PLAY_FROM_FILE
    vecanim_render();
#endif

    // render palette fade
    for (int i = 0; i < 16; i++) vecanim_palette_fade[i] = 0xFFF;
    pal_lerp((USHORT*)vecanim_palette[0], vecanim_palette_base, vecanim_palette_fade,  16, 16);

    SHORT *copPtr = copper_main;

    // register graphics resources with WinUAE for nicer gfx debugger experience
    debug_register_copperlist(copper_main, "copper1", 1024, 0);

    // init data fetch/display window
    //copPtr = screenScanDefault(copPtr);
    copPtr = setLowres320Wide(copPtr, 256);
    //enable bitplanes    
    *copPtr++ = offsetof(struct Custom, bplcon0);
    *copPtr++ = (0<<10)/*dual pf*/|(1<<9)/*color*/|((4)<<12)/*num bitplanes*/;
    *copPtr++ = offsetof(struct Custom, bplcon1);    //scrolling
    *copPtr++ = 0;
    *copPtr++ = offsetof(struct Custom, bplcon2);    //playfied priority
    *copPtr++ = 0<<6;//0x24;            //Sprites have priority over playfields

    //set bitplane modulo
	*copPtr++=offsetof(struct Custom, bpl1mod); //odd planes   1,3,5
	*copPtr++=(USHORT)((BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE));
	*copPtr++=offsetof(struct Custom, bpl2mod); //even  planes 2,4
	*copPtr++=(USHORT)((BPL_COUNT*X_PITCH_BYTE - X_RES_BYTE));

    // set palette
    coplist_palette = copPtr;
    for (int i = 0; i < 16; i++) {
        COPPERLIST_ADD_MOVE(copPtr,color[i], vecanim_palette[0][i]);
    }

    // jump to copperlist 2
    COPPERLIST_ADD_MOVE(copPtr, copjmp2, 0x7FFF);

    // terminate
    *copPtr++ = 0xffff;
    *copPtr++ = 0xfffe;

    for (int i = 0; i < 2; i++) {
        copPtr = copper_bpldata[i];
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], bplbase);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], bplbase);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], bplbase);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], bplbase);
        
        // terminate
        *copPtr++ = 0xffff;
        *copPtr++ = 0xfffe;
    }

    return 0;
}

//#define PLAY_FROM_FILE

// rotation animation
#ifdef PLAY_FROM_FILE
INCBIN(vecanim_rot, "rotanim.bin");
#else
static USHORT vecanim_rot[120 * 1024];          // YES!
#endif

#define TOTAL_DOTS 4
static struct vec4x shape[TOTAL_DOTS] = {
    {-(6/2), -(6/2), 0, 0},
    { (6/2), -(6/2), 0, 0},
    { (6/2),  (6/2), 0, 0},
    {-(6/2),  (6/2), 0, 0}
};
// transformed buffers
struct vec4x shape_pt[TOTAL_DOTS*2];
// projected and clipped shape
struct vec2w shape_p2d[2][TOTAL_DOTS*2];

// render animation to the buffer (the hardest part ever :)
void vecanim_render() {
    // mf logo shape
    static BYTE multik[8] = {129, 195, 231, 255, 255, 255, 255, 126};
    static SHORT z1 = 0, z2 = (128>>2);
    static SHORT r1 = (4*128/2)<<4, r2 = (0*128/2)<<4;
    const int frames = 64*4;
    SHORT* anim = vecanim_rot;

    for (int f = 0; f <= frames; f++) {
        // show current frame rendered
        custom->color[0] = f;

        // do the stuff
        // fill transformed vertices
        int f2 = (f*f) >> 4;
        int z = z1 + (((z2 - z1)*(f2)) >> 3);
        int r = (r1 + (((r2 - r1)*(f))/frames)) >> 4;

        SHORT cr = costab_256[r & 255];
        SHORT sr = sintab_256[r & 255];

        LONG zz = (1 << 20) / (z < 22 ? 18 : z - 4);

        for (int y = 0; y < 8; y++) {
            for (int x = 0; x < 8; x++) {
                if ((multik[y] & (1 << x)) == 0) continue;
                
                // translate/rotate/project
                for (int i = 0; i < TOTAL_DOTS; i++) {
                    SHORT ox = shape[i].x + (x << 3) - 32 + 4;
                    SHORT oy = shape[i].y + (y << 3) - 32 + 4;

                    SHORT px = (ox*cr - oy*sr);
                    SHORT py = (ox*sr + oy*cr);

                    shape_p2d[0][i].x = ((px * zz) >> 13) + (X_RES/2);
                    shape_p2d[0][i].y = ((py * zz) >> 13) + (Y_RES/2);
                }

                // clip
                int final_vertices = 4;
                if (zz > 0x100) {
                    final_vertices = polyclip(
                        shape_p2d[1], shape_p2d[0], TOTAL_DOTS,
                        0, X_RES - 1,
                        0, Y_RES - 1
                    );
                }

                // draw to the buffer
                if (final_vertices >= 3) {
                    *anim++ = final_vertices;
                    for (int i = 0; i < final_vertices; i++) {
                        *anim++ = shape_p2d[0][i].x;
                        *anim++ = shape_p2d[0][i].y;
                    }
                }
                custom->color[0] = shape_p2d[0][0].x;
            }
        }
        *anim++ = 0xFFFE;
    }
    // mark end of anim
    *(anim-1) = 0xFFFF;
    *(anim) = 0xFFFF;
}

static volatile int anim_lastpos = 0;
static volatile int animpos = 0;
static volatile int animstop = 0;

static void __attribute__ ((noinline)) vecanim_draw(void *anim) {
    // setup blitter for line mode
    blt_line_setup(X_PITCH_BPL_BYTE);

    register volatile void* _a0 ASM("a0");
    register volatile void* _a1 ASM("a1");
    _a0 = bplring[5];
    _a1 = (USHORT*)anim + animpos;

    __asm volatile (
        "movem.l %%d0-%%d7/%%a2-%%a6,-(%%sp)\n"
        "jsr _animdraw_a\n"
        "movem.l (%%sp)+,%%d0-%%d7/%%a2-%%a6"
    : "+rf" (_a0), "+rf"(_a1)
    : 
    : "cc", "memory");

    // _a1 - current anim ptr
    if (_a1 == NULL) {

    } else {
        animpos = (USHORT*)_a1 - (USHORT*)anim;
    }
    return;
}

// the main show!
int vecanim_run() {
    // remove stray VBL handlers
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    int fc_start = frameCounter;
    int cop_idx = 0;

    // clear buffers
    WaitBlit();
    // enable blitter dma if is not enabled already
    custom->dmacon  = DMAF_SETCLR | DMAF_MASTER | DMAF_BLITTER;
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bplring[0];
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES * BPL_COUNT * 3) << HSIZEBITS) | (X_PITCH_WORD);
    WaitBlit();
    custom->bltcon0 = A_TO_D | DEST;
    custom->bltcon1 = 0;
    custom->bltadat = 0;
    custom->bltdpt  = bplring[3];
    custom->bltdmod = 0;
    custom->bltafwm = custom->bltalwm = 0xffff;
    custom->bltsize = ((Y_RES * BPL_COUNT * 3) << HSIZEBITS) | (X_PITCH_WORD);

    custom->dmacon = DMAF_COPPER;
    custom->cop1lc = (ULONG)copper_main;
    custom->cop2lc = (ULONG)copper_bpldata[cop_idx];
    custom->dmacon = DMAF_SETCLR | DMAF_MASTER | DMAF_RASTER | DMAF_COPPER | DMAF_BLITTER;

    int palidx = 0;

    while(isRunning && (frameCounter < 24*64*4)) {
        // vbl
        volatile short fc = frameCounter;
        while ((fc == frameCounter) || ((frameCounter & 1) == 1));       // 25fps lock
        //custom->color[0] = 0x0007;

        int frame_counter = frameCounter - fc_start;

        // clear
		WaitBlit();
		custom->bltcon0 = A_TO_D | DEST;
		custom->bltcon1 = 0;
		custom->bltadat = 0;
		custom->bltdpt  = bplring[5];
		custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = ((Y_RES * BPL_COUNT) << HSIZEBITS) | (X_RES_WORD);
        //custom->color[0] = 0x0070;

        WaitBlit();
        vecanim_draw((void*)vecanim_rot);

        // area fill
        //custom->color[0] = 0x0700;
        WaitBlit();
        custom->bltcon0 = A_TO_D | SRCA | DEST;
		custom->bltcon1 = FILL_OR | BC1F_DESC;
		custom->bltapt  = custom->bltdpt = bplring[5] + (X_RES_BYTE-2) + ((Y_RES-1) * X_PITCH_BYTE);
        custom->bltamod = custom->bltdmod = X_PITCH_BYTE - X_RES_BYTE;
		custom->bltafwm = custom->bltalwm = 0xffff;
		custom->bltsize = (((Y_RES-1) * BPL_COUNT) << HSIZEBITS) | (X_RES_WORD);
        //custom->color[0] = 0x0070;

        // advance bitplane ring buffer
        UBYTE* bplhead = bplring[0];
        bplring[0] = bplring[1];
        bplring[1] = bplring[2];
        bplring[2] = bplring[3];
        bplring[3] = bplring[4];
        bplring[4] = bplring[5];
        bplring[5] = bplhead;

        USHORT *copPtr = copper_bpldata[cop_idx];
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[0], (UBYTE*)(rand() & 0x7FFFF));
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[1], bplring[1]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[2], bplring[2]);
        COPPERLIST_ADD_MOVE_LONG(copPtr, bplpt[3], bplring[3]);
        
        if ((frame_counter >= 1*4*64 + 4*56) && (palidx < 16)) palidx = ((frame_counter - 1*4*64 - 4*56) >> 1);

        // set palette
        copPtr = coplist_palette;
        for (int i = 0; i < 16; i++)
            COPPERLIST_ADD_MOVE(copPtr, color[i], vecanim_palette[palidx][i]);

        // set new copperlist
        custom->cop2lc = copper_bpldata[cop_idx];
        cop_idx ^= 1;
        
    }

    // cleanup
    part_vbl = NULL;
    part_cop = NULL;
    part_blt = NULL;

    return isRunning;
}

#include "support/gcc8_c_support.h"
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
#include "main.h"
#include "copper.h"
#include "resources.h"
#include "tables.h"

// parts!
//#include "parts/text_test.h"
//#include "parts/sprite.h"

#include "parts/stars.h"
#include "parts/chaoszoomer.h"
#include "parts/twister.h"
#include "parts/picwobble.h"
#include "parts/copperchunky.h"
#include "parts/parallax.h"
#include "parts/vecanim.h"
#include "parts/linewobble.h"
#include "parts/vecdrop.h"
#include "parts/endscreen.h"

//config
#define MUSIC

struct ExecBase *SysBase;
volatile struct Custom *custom;
struct DosLibrary *DOSBase;
struct GfxBase *GfxBase;

//backup
static UWORD SystemInts;
static UWORD SystemDMA;
static UWORD SystemADKCON;
static volatile APTR VBR=0;
static APTR SystemIrq;
 
struct View *ActiView;

static APTR GetVBR(void) {
	APTR vbr = 0;
	UWORD getvbr[] = { 0x4e7a, 0x0801, 0x4e73 }; // MOVEC.L VBR,D0 RTE

	if (SysBase->AttnFlags & AFF_68010) 
		vbr = (APTR)Supervisor((ULONG (*)())getvbr);

	return vbr;
}

void SetInterruptHandler(APTR interrupt) {
	*(volatile APTR*)(((UBYTE*)VBR)+0x6c) = interrupt;
}

APTR GetInterruptHandler() {
	return *(volatile APTR*)(((UBYTE*)VBR)+0x6c);
}

//vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
//vsync begins at line 2 hpos 132 and ends at vpos 5 hpos 18 
void WaitVbl() {
	debug_start_idle();
	while (1) {
		volatile ULONG vpos=*(volatile ULONG*)0xDFF004;
		vpos&=0x1ff00;
		if (vpos!=(311<<8))
			break;
	}
	while (1) {
		volatile ULONG vpos=*(volatile ULONG*)0xDFF004;
		vpos&=0x1ff00;
		if (vpos==(311<<8))
			break;
	}
	debug_stop_idle();
}

void WaitLine(USHORT line) {
	while (1) {
		volatile ULONG vpos=*(volatile ULONG*)0xDFF004;
		if(((vpos >> 8) & 511) == line)
			break;
	}
}

void WaitLineOrAbove(USHORT line) {
    while (1) {
        volatile ULONG vpos=*(volatile ULONG*)0xDFF004;
        if(((vpos >> 8) & 511) >= line)
            break;
    }
}

void TakeSystem() {
    // init screen (rtg fix?)
    ActiView=GfxBase->ActiView; //store current view
    LoadView(0);
    WaitTOF();
    WaitTOF();

    // disable multitasking
    Forbid();
    //Save current interrupts and DMA settings so we can restore them upon exit. 
    SystemADKCON=custom->adkconr;
    SystemInts=custom->intenar;
    SystemDMA=custom->dmaconr;

    WaitVbl();
    WaitVbl();

    OwnBlitter();
    WaitBlit();    
    Disable();
    
    custom->intena=0x7fff;//disable all interrupts
    custom->intreq=0x7fff;//Clear any interrupts that were pending
    
    custom->dmacon=0x7fff;//Clear all DMA channels

    //set all colors black
    for(int a=0;a<32;a++)
        custom->color[a]=0;

    // AGA compatibility stuff
    custom->fmode   = 0;            // 1x fetchmode (OCS compat)
    custom->bplcon3 = 0x0C00;       // DPF: PF2 start at color 8
    custom->bplcon4 = 0x0011;

    WaitVbl();
    WaitVbl();

    VBR=GetVBR();
    SystemIrq=GetInterruptHandler(); //store interrupt register

    // disable sprites by pointing to the [0, 0] sprite word
    for (int spr = 0; spr < 8; spr++) {
        custom->sprpt[spr] = (ULONG)temp_2ndpf;
    }
}

void FreeSystem() { 
    WaitVbl();
    WaitBlit();
    custom->intena=0x7fff;//disable all interrupts
    custom->intreq=0x7fff;//Clear any interrupts that were pending
    custom->dmacon=0x7fff;//Clear all DMA channels

    //restore interrupts
    SetInterruptHandler(SystemIrq);

    /*Restore system copper list(s). */
    custom->cop1lc=(ULONG)GfxBase->copinit;
    custom->cop2lc=(ULONG)GfxBase->LOFlist;
    custom->copjmp1=0x7fff; //start coppper

    /*Restore all interrupts and DMA settings. */
    custom->intena=SystemInts|0x8000;
    custom->dmacon=SystemDMA|0x8000;
    custom->adkcon=SystemADKCON|0x8000;

    WaitBlit();    
    DisownBlitter();
    Enable();

    LoadView(ActiView);
    WaitTOF();
    WaitTOF();

    Permit();
}

// DEMO - INCBIN
volatile short frameCounter = 0;
volatile short isRunning    = 1;
volatile short playMusic    = 0;

// VERY SLOW but seems to be working RNG (good old xorshift)
ULONG rand() {
    static ULONG seed = 0xAA532051;
    seed ^= seed >> 6;
    seed ^= seed << 12;
    seed ^= seed >> 15;
    return seed;
}

#ifdef MUSIC
	// Demo - Module Player - ThePlayer 6.1a: https://www.pouet.net/prod.php?which=19922
	// The Player® 6.1A: Copyright © 1992-95 Jarno Paananen
	// P61.testmod - Module by Skylord/Sector 7 
	INCBIN(player, "player610.6.no_cia.bin")
	//INCBIN_CHIP(module, "testmod.p61")
	INCBIN_CHIP(module, "papapa_inv_longerfade.p61")
	//INCBIN_CHIP(module, "P61.papapa")

	int p61Init(const void* module) { // returns 0 if success, non-zero otherwise
		register volatile const void* _a0 ASM("a0") = module;
		register volatile const void* _a1 ASM("a1") = NULL;
		register volatile const void* _a2 ASM("a2") = NULL;
		register volatile const void* _a3 ASM("a3") = player;
		register                int   _d0 ASM("d0"); // return value
		__asm volatile (
			"movem.l %%d1-%%d7/%%a4-%%a6,-(%%sp)\n"
			"jsr 0(%%a3)\n"
			"movem.l (%%sp)+,%%d1-%%d7/%%a4-%%a6"
		: "=r" (_d0), "+rf"(_a0), "+rf"(_a1), "+rf"(_a2), "+rf"(_a3)
		:
		: "cc", "memory");
		return _d0;
	}

	void p61Music() {
		register volatile const void* _a3 ASM("a3") = player;
		register volatile const void* _a6 ASM("a6") = (void*)0xdff000;
		__asm volatile (
			"movem.l %%d0-%%d7/%%a0-%%a2/%%a4-%%a5,-(%%sp)\n"
			"jsr 4(%%a3)\n"
			"movem.l (%%sp)+,%%d0-%%d7/%%a0-%%a2/%%a4-%%a5"
		: "+rf"(_a3), "+rf"(_a6)
		:
		: "cc", "memory");
	}

	void p61End() {
		register volatile const void* _a3 ASM("a3") = player;
		register volatile const void* _a6 ASM("a6") = (void*)0xdff000;
		__asm volatile (
			"movem.l %%d0-%%d1/%%a0-%%a1,-(%%sp)\n"
			"jsr 8(%%a3)\n"
			"movem.l (%%sp)+,%%d0-%%d1/%%a0-%%a1"
		: "+rf"(_a3), "+rf"(_a6)
		:
		: "cc", "memory");
	}
#endif //MUSIC

// -----------------------
// VBL handler stuff

void (*part_vbl)() = NULL;
void (*part_cop)() = NULL;
void (*part_blt)() = NULL;

// global VBL/blitter end/copper handler
static __attribute__((interrupt)) void interruptHandler() {
#if 1
    short reason = custom->intreqr;
    if (reason & INTF_COPER) {
        // call part's copper handler
        if (part_cop != NULL) part_cop();
    }
    if (reason & INTF_BLIT) {
        // call part's blitter handler
        if (part_blt != NULL) part_blt();
    }
    if (reason & INTF_VERTB) {
#endif
        if (playMusic) {
            // DEMO - increment frameCounter
            frameCounter++;
#ifdef MUSIC
            // DEMO - ThePlayer
            p61Music();
#endif
        }

        // call part's VBL handler
        if (part_vbl != NULL) part_vbl();

        // check for LMB
        isRunning = !MouseLeft();

        custom->intreq=(1<<INTB_VERTB); //reset vbl req. twice for a4000 bug.
#if 1
    }
    // acknowledge all interrupts
    custom->intreq = reason & (INTF_COPER|INTF_VERTB|INTF_BLIT);
#endif
}

// set up a 320x??? lowres display, write to copperlist
USHORT* setLowres320Wide(USHORT* copListEnd, int height) {
    const USHORT x=129;
    const USHORT width=320;
    const USHORT y=44 + ((256 - height)>>1);
    const USHORT RES=8; //8=lowres,4=hires
    USHORT xstop = x+width;
    USHORT ystop = y+height;
    USHORT fw=(x>>1)-RES;

    *copListEnd++ = offsetof(struct Custom, ddfstrt);
    *copListEnd++ = fw;
    *copListEnd++ = offsetof(struct Custom, ddfstop);
    *copListEnd++ = fw+(((width>>4)-1)<<3);
    *copListEnd++ = offsetof(struct Custom, diwstrt);
    *copListEnd++ = x+(y<<8);
    *copListEnd++ = offsetof(struct Custom, diwstop);
    *copListEnd++ = (xstop-256)+((ystop-256)<<8);
    return copListEnd;
}
 
// set up a 256x??? lowres display, write to copperlist
USHORT* setLowres256Wide(USHORT* copListEnd, int height) {
    const USHORT x=129 + ((320 - 256)>>1);
    const USHORT width=256;
    const USHORT y=44 + ((256 - height)>>1);
    const USHORT RES=8; //8=lowres,4=hires
    USHORT xstop = x+width;
    USHORT ystop = y+height;
    USHORT fw=(x>>1)-RES;

    *copListEnd++ = offsetof(struct Custom, ddfstrt);
    *copListEnd++ = fw;
    *copListEnd++ = offsetof(struct Custom, ddfstop);
    *copListEnd++ = fw+(((width>>4)-1)<<3);
    *copListEnd++ = offsetof(struct Custom, diwstrt);
    *copListEnd++ = x+(y<<8);
    *copListEnd++ = offsetof(struct Custom, diwstop);
    *copListEnd++ = (xstop-256)+((ystop-256)<<8);
    return copListEnd;
}

// ------------------
// common resources

// beeeeeg bitplane buffer - shared between parts
// should be big enough for 320x256x5x2 = 100kb (exact! :)
UBYTE* bpl_pool;

INCBIN(console_text, "demarination.txt")

int no_chipmem() {
    Write(Output(), (APTR)"no chipmem!\n", 12);
    return 1;
}

int main() {
	SysBase = *((struct ExecBase**)4UL);
	custom = (struct Custom*)0xdff000;

	// We will use the graphics library only to locate and restore the system copper list once we are through.
	GfxBase = (struct GfxBase *)OpenLibrary((CONST_STRPTR)"graphics.library",0);
	if (!GfxBase)
		Exit(0);

	// used for printing
	DOSBase = (struct DosLibrary*)OpenLibrary((CONST_STRPTR)"dos.library", 0);
	if (!DOSBase)
		Exit(0);
    
    if (MouseLeft() && MouseRight()) {
        Write(Output(), "\ncrocus city hall - 22.03.2024 - never forget\n\n", 48);
        return 0;
    } else { 
        // write text
        Write(Output(), (APTR)console_text, (ULONG)&incbin_console_text_end-(ULONG)&incbin_console_text_start);
    }

	warpmode(1);

    // init all the freaking stuff
	// allocate bitplane data (interleaved 5bpl)
	bpl_pool = (UBYTE*)AllocMem(512*256*5*3/8, MEMF_CHIP | MEMF_CLEAR);      // 2 buffers
    if (bpl_pool == NULL) return no_chipmem(); // not enough chip memory! :sob:

	// TODO: precalc stuff here
#ifdef MUSIC
    if(p61Init(module) != 0)
        KPrintF("p61Init failed!\n");
#endif

    // hold joystick fire to run hidden part
    USHORT hidden_part = (!((*(volatile UBYTE*)0xbfe001)&128));
    if (hidden_part) {
        Write(Output(), "bingo", 5);
    }

    // init parts
    if (vecdrop_init()) return no_chipmem();
    if (linewobble_init()) return no_chipmem();
    if (copperchunky_init()) return no_chipmem();
    if (chaoszoomer_init()) return no_chipmem();
    if (picwobble_init()) return no_chipmem();
    if (endscreen_init()) return no_chipmem();
    if (parallax_init()) return no_chipmem();
    if (vecanim_init()) return no_chipmem();
    if (stars_init()) return no_chipmem();
    if (twister_init()) return no_chipmem();

    warpmode(0);

	// shut down system
	TakeSystem();
	WaitVbl();
    WaitVbl();
    WaitVbl();

    SetInterruptHandler((APTR)interruptHandler);

    custom->intena = INTF_SETCLR | INTF_INTEN | INTF_VERTB;
#ifdef MUSIC
    custom->intena = INTF_SETCLR | INTF_EXTER; // ThePlayer needs INTF_EXTER
#endif
    custom->intreq=(1<<INTB_VERTB);//reset vbl req

	playMusic = 1;
    // RUUUUUN
    if (!hidden_part) {
#if 1
        vecdrop_run();
        frameCounter = 0;           // reset
        copperchunky_run();
        stars_run();
        chaoszoomer_run();
        picwobble_run();
        linewobble_run();
#endif
        parallax_run(hidden_part);
        twister_run();
        vecanim_run();
        endscreen_run();
    } else parallax_run(hidden_part);

    // END
#ifdef MUSIC
    p61End();
#endif

    // free parts
    vecdrop_free();
    vecanim_free();
    linewobble_free();
    copperchunky_free();
    stars_free();
    chaoszoomer_free();
    picwobble_free();
    parallax_free();
    twister_free();
    endscreen_free();
    //backdrop_free();
    //fonttest_free();

    FreeSystem();

    FreeMem(bpl_pool, 512*256*5*3/8);

	CloseLibrary((struct Library*)DOSBase);
	CloseLibrary((struct Library*)GfxBase);
    return 0;
}

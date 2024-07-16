#include <proto/exec.h>
#include <hardware/custom.h>

extern struct ExecBase *SysBase;
extern volatile struct Custom *custom;
extern struct DosLibrary *DOSBase;
extern struct GfxBase *GfxBase;

// --------------------
// useful common routines

// interrupt handling stuff
void SetInterruptHandler(APTR interrupt);
APTR GetInterruptHandler();

// vbl/copper/blt handlers
extern void (*part_vbl)();
extern void (*part_cop)();
extern void (*part_blt)();

//vblank begins at vpos 312 hpos 1 and ends at vpos 25 hpos 1
//vsync begins at line 2 hpos 132 and ends at vpos 5 hpos 18 
void WaitVbl();

// wait for given line
void WaitLine(USHORT line);

// set up a 320x256 lowres display
USHORT* screenScanDefault(USHORT* copListEnd);

USHORT* setLowres256Wide(USHORT* copListEnd, int height);

// set up a 320x??? lowres display, write to copperlist
USHORT* setLowres320Wide(USHORT* copListEnd, int height);

// wait for blitter done
__attribute__((always_inline)) inline void WaitBlt() {
	UWORD tst=*(volatile UWORD*)&custom->dmaconr; //for compatiblity a1000
	(void)tst;
	while (*(volatile UWORD*)&custom->dmaconr&(1<<14)) {} //blitter busy wait
}

// grab the whole system
void TakeSystem();

// restore system
void FreeSystem();

__attribute__((always_inline)) inline short MouseLeft(){return !((*(volatile UBYTE*)0xbfe001)&64);}	
__attribute__((always_inline)) inline short MouseRight(){return !((*(volatile UWORD*)0xdff016)&(1<<10));}

// GLOBAL frame counter (because we can!)
extern volatile short frameCounter;

// is running?
extern volatile short  isRunning;

// play music?
extern volatile short playMusic;

// VERY SLOW but seems to be working RNG
ULONG rand();

// ----------------
// pront constant string to console
#define CON_PRINT(str) Write(Output(), (APTR)str, sizeof(str)-1);

// -----------------
// music routines (TODO: ADAPT FOR YOUR USED APPLICATION!!!)


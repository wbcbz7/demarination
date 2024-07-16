#include <proto/exec.h>

// temp storage for 2nd playfield
extern WORD temp_2ndpf[512];

extern const signed char sintab_256[];
extern const signed char costab_256[];

// multik logo, 80x80, interleaved 2bpl format (bpl0 - red squares, bpl1 - outline)
extern const void* multik_logo_80x80;

// main font
extern struct font_info_t font_main;
extern const void* font_main_gfx;
extern const void* font_main_desc;
extern const void* font_main_ofs;
// small font
extern struct font_info_t font_small;
extern const void* font_main_gfx;
extern const void* font_main_desc;
extern const void* font_main_ofs;


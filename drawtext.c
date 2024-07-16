#include "support/gcc8_c_support.h"
#include "main.h"
#include "drawtext.h"
#include <proto/graphics.h>
#include <hardware/dmabits.h>
#include <hardware/custom.h>

// static tables
static USHORT bltcon0_tab[16];
static SHORT  pitch_tab[256];
static SHORT  current_pitch, current_pitch_bpl;

void text_init(SHORT rop) {
    rop &= 0xFF;
    for (int i = 0; i < 16; i++) bltcon0_tab[i] = rop | SRCA | SRCC | DEST | (i << ASHIFTSHIFT);
}

void text_build_pitch_lookup(SHORT pitch, SHORT pitch_bpl) {
    SHORT *p = pitch_tab;
    for (int i = 0; i < 127; i++)  *p++ = (i * pitch_bpl);
    for (int i = -128; i < 0; i++) *p++ = (i * pitch_bpl);
    current_pitch     = pitch;
    current_pitch_bpl = pitch_bpl;
}

SHORT text_get_length(struct font_info_t *font, const char *str) {
    if (str == NULL) return 0;
    SHORT length = 0;
    while (*str != 0) {
        char ch = *str - font->startChar;
        struct font_descriptor_t *desc = font->desc + ch;
        length += desc->advance;
        str++;
    }
    return length;
}

void text_draw(UBYTE *dst, struct font_info_t *font, const char* str, SHORT x, SHORT y) {
    if((font == NULL) || (str == NULL)) return;

    // pre-init blitter
    WORD bltsize = (font->width + 2) >> 1;

    WaitBlit();
    custom->bltcon1 = 0;
    custom->bltafwm = 0xFFFF;
    custom->bltalwm = 0;
    custom->bltamod = font->modulo;
    custom->bltcmod = custom->bltdmod = current_pitch_bpl - (font->width + 2);

    UBYTE* baseptr = (UBYTE*)dst + (current_pitch_bpl * y);
    while (*str != 0) {
        char ch = *str - font->startChar;
        struct font_descriptor_t *desc = font->desc + ch;
        USHORT *ofs = font->ofs + ch;
        int xx = x + desc->xofs;

        if (desc->height > 0) {
            // draw character
            WaitBlit();
            custom->bltcon0 = bltcon0_tab[xx & 15];
            custom->bltapt  = font->gfx + font->ofs[ch];
            custom->bltcpt  =
            custom->bltdpt  = baseptr + (xx >> 3) - pitch_tab[(UBYTE)desc->yofs];
            custom->bltsize = (desc->height << HSIZEBITS) | bltsize;
        }
        x += desc->advance;
        str++;
    }
}

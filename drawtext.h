#pragma once

#include <proto/exec.h>
#include <hardware/custom.h>

// font descriptor table
struct __attribute__((packed)) font_descriptor_t {
    BYTE  xofs, yofs;    // in pixels
    UBYTE height;        // in pixels, 0 - null glyph
    UBYTE advance;
};

// font info table
struct font_info_t {
    UBYTE                    *gfx;      // font graphics
    struct font_descriptor_t *desc;     // descriptor
    USHORT                   *ofs;      // offset table for each char
    UBYTE                     startChar;
    UBYTE                     width;    // max width of each glyph in bytex
    UBYTE                     bpls;     // number of bitplanes
    BYTE                      modulo;   // font modulo
};

// init text writer
void text_init(SHORT rop);

// prepare pitch lookup table
void text_build_pitch_lookup(SHORT pitch, SHORT pitch_bpl);

// get text string length
SHORT text_get_length(struct font_info_t *font, const char *str);

// draw text at (x,y), no wrap checks
void text_draw(UBYTE *dst, struct font_info_t *font, const char* str, SHORT x, SHORT y);

#pragma once

#include <proto/exec.h>
#include <hardware/custom.h>
#include "vec.h"

void blt_draw_line(UBYTE *bpl, int pitch, int x1, int y1, int x2, int y2);
void blt_draw_line_pitch64(UBYTE *bpl, int x1, int y1, int x2, int y2);
void blt_line_setup(int pitch);

// clipper
int polyclip(struct vec2w *out, struct vec2w *in, SHORT in_count, int x1, int x2, int y1, int y2);

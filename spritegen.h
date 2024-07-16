#pragma once

#include <proto/exec.h>
#include <hardware/custom.h>

// cut a sprite from two bitplanes
void sprite_cut(USHORT* dst, USHORT *bpl0, USHORT *bpl1, USHORT mod0, USHORT mod1, USHORT height, USHORT x, USHORT y);

#pragma once

#include <proto/exec.h>
#include <hardware/custom.h>

// -----------------
// beeeeeg bitplane buffer - shared between parts
// should be big enough for 320x256x5x2 = 100kb (exact! :)
extern UBYTE* bpl_pool;

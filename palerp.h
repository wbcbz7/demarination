#pragma once

#include <proto/exec.h>

// generate palette mix (note: generates steps+1 sets, make sure you're not screwing memory!)
void pal_lerp(SHORT *dst, SHORT *col0, SHORT *col1, SHORT colors, SHORT steps);

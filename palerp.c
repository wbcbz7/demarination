#include "palerp.h"

void pal_lerp(SHORT *dst, SHORT *col0, SHORT *col1, SHORT colors, SHORT steps) {
    for (SHORT i = 0; i < steps+1; i++) {
        SHORT *c0 = col0;
        SHORT *c1 = col1;
        for (SHORT c = 0; c < colors; c++) {
            SHORT r = ((*c0 & 0xF00) + ((((*c1 & 0xF00) - (*c0 & 0xF00)) * i) / steps)) & 0xF00; 
            SHORT g = ((*c0 & 0x0F0) + ((((*c1 & 0x0F0) - (*c0 & 0x0F0)) * i) / steps)) & 0x0F0; 
            SHORT b = ((*c0 & 0x00F) + ((((*c1 & 0x00F) - (*c0 & 0x00F)) * i) / steps)) & 0x00F;
            *dst++ = r | g | b; c0++; c1++;
        }
    }
}

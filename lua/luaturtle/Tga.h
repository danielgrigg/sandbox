#ifndef TGA_H
#define TGA_H

#include <stdint.h>
#include <vector>

bool TgaWrite(const char *szFileName, uint32_t bitsPerPixel, 
    uint32_t width, uint32_t height, const uint8_t* pixels);
#endif

#ifndef TGA_H
#define TGA_H

#include <stdint.h>
#include <vector>

// assume BGR layout
bool gltWriteTGA(const char *szFileName, uint32_t bitsPerPixel,
    uint32_t width, uint32_t height, const uint8_t* pixels);

#endif

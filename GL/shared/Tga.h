#ifndef TGA_H
#define TGA_H

#include <stdint.h>
#include <vector>

// assume BGR layout
bool gltWriteTGA(const char *szFileName, uint32_t bitsPerPixel, 
    uint32_t width, uint32_t height, const uint8_t* pixels);

bool gltLoadTGA(const char *szFileName, 
    uint32_t *iWidth, 
    uint32_t *iHeight, 
    uint32_t *iComponents, 
    uint32_t *eFormat,
    std::vector<uint8_t> &pixels);
#endif

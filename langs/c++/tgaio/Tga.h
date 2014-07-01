#ifndef TGA_H
#define TGA_H

#include <stdint.h>
#include <vector>

bool gltWriteTGA(const char *szFileName, int width, int height, 
    const std::vector<int8_t> &pixels);

bool gltLoadTGA(const char *szFileName, 
    int32_t *iWidth, 
    int32_t *iHeight, 
    int32_t *iComponents, 
    uint32_t *eFormat,
    std::vector<int8_t> &pixels);
#endif

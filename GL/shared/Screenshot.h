#ifndef SCREENSHOT_H
#define SCREENSHOT_H

#include <string>

bool SaveScreen(const std::string &fileName, uint32_t readBuffer);
bool SaveScreen(const std::string &fileName);

bool SaveTexture2D(const std::string &fileName);
bool SaveTextureRect(const std::string &fileName);
#endif

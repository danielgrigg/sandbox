#ifndef SHAPE_FACTORY_H
#define SHAPE_FACTORY_H

#include "GeometryBatch.h"
#include <lexi/Color/LColor.hpp>


void makeIndexedTriangle(GeometryBatch &batch, float width,  bool normals, 
    bool uvs, const LColor *color);

void makeIndexedQuad(GeometryBatch &batch, float width, float height, 
    bool normals, bool uvs, const LColor *color);

void makeTriangle(GeometryBatch &batch, float width,  
    bool normals, bool uvs, const LColor *color);

void makePlane(GeometryBatch &batch, float width, int divisions, 
    bool normals, bool uvs, const LColor *color = NULL);
void makeQuad(GeometryBatch &batch, float width, float height, 
              bool normals, bool uvs, const LColor *color = NULL);
void makeCube(GeometryBatch &batch, float width, 
    bool normals, bool uvs, const LColor *color = NULL);

// A rect exists in screenspace, thus UVS are mapped to the width/height 
// and the vertices are  (0,0 => lower-left, w,h => upper-right).
void makeRect(GeometryBatch &batch, float width, float height, 
              bool normals, bool uvs, const LColor *color = NULL);
void makePyramid(GeometryBatch &batch, 
    bool normals, bool uvs, const LColor *color = NULL);
void makeFan(GeometryBatch &b, float radius, float height, int divisions,
    bool normals, bool uvs, const LColor *color = NULL);
void makeRing(GeometryBatch &b, float radius, float height, int divisions, 
    bool normals, bool uvs, const LColor *color = NULL);
void makeSphere(GeometryBatch &b, float radius, int divisions, 
    bool normals, bool uvs, const LColor *color = NULL);

#endif

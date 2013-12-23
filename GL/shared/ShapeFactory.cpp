#include "ShapeFactory.h"
#include "glforward.h"
#include <vector>

#include <iterator>

void makePlane(GeometryBatch &batch, 
    float width, int divisions, bool normals, bool uvs, const LColor *color)
{
  float dx = width / divisions, dy = 0, dz = -width / divisions;
  float x0 = -width / 2.0f, y0 = 0.0f, z0 = width / 2.0f;
  LPoint p0(x0, y0, z0);
  LVector dp(dx, dy, dz);

  batch.init(GL_TRIANGLE_STRIP, "plane");
  for (int strip = 0; strip < divisions; ++strip)
  {
    LVector index = LVector(0,0,strip);
    for (int x = 0; x < divisions+1; ++x)
    {
      index.setX(x);
      LVector index1 = index + LVector(0,0,1);
      LPoint pb = p0 + index * dp;
      LPoint pt = p0 + index1 * dp;

      if (x == 0 && strip > 0 && strip < divisions)
      {
        batch.ps.push_back(pt);
      }

      batch.ps.push_back(pt);
      batch.ps.push_back(pb);
      if (x == divisions && strip < divisions-1)
      {
        batch.ps.push_back(pb);
      }
    }

  }

  if (normals) { batch.ns.assign(batch.ps.size(), LNormal(0,1,0)); }
  if (uvs) { std::cout << "Plane UV not implemented\n"; }
  if (color) { batch.cs.assign(batch.ps.size(), *color); }

//    cout << "plane vertices\n";
//    ostream_iterator<LPoint> osi(cout, "\n");
//    copy(batch.ps.begin(), batch.ps.end(), osi);
}

void makeIndexedTriangle(GeometryBatch &batch, float width,  bool normals, 
    bool uvs, const LColor *color)
{
  batch.init(GL_TRIANGLES, "triangle");
  batch.ps.resize(3);
  batch.ps[0] = LPoint(-width,-width,0);
  batch.ps[1] = LPoint(width,-width,0);
  batch.ps[2] = LPoint(0,width,0);
  batch.elements.resize(3);
  batch.elements[0] = 0;
  batch.elements[1] = 1;
  batch.elements[2] = 2;
  if (normals)
  {
    batch.ns.assign(batch.ps.size(), LNormal(0,0,1));
  }
  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }
}



void makeTriangle(GeometryBatch &batch, float width,  bool normals, 
    bool uvs, const LColor *color)
{
  batch.init(GL_TRIANGLES, "triangle");
  batch.ps.resize(3);
  batch.ps[0] = LPoint(-width,-width,0);
  batch.ps[1] = LPoint(width,-width,0);
  batch.ps[2] = LPoint(0,width,0);
  if (normals)
  {
    batch.ns.assign(batch.ps.size(), LNormal(0,0,1));
  }
  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }
}

void makeIndexedQuad(GeometryBatch &batch, float width, float height, 
    bool normals, bool uvs, const LColor *color)
{
  batch.init(GL_TRIANGLES, "quad");
  batch.ps.resize(4);
  batch.ps[0] = LPoint(-width,-height,0);
  batch.ps[1] = LPoint(width,-height,0);
  batch.ps[2] = LPoint(-width,height,0);
  batch.ps[3] = LPoint(width,height,0);
  if (uvs)
  {
    batch.uvs[0].resize(4);
    batch.uvs[0][0] = UV(0, 0);
    batch.uvs[0][1] = UV(1, 0);
    batch.uvs[0][2] = UV(0, 1);
    batch.uvs[0][3] = UV(1, 1);
  }
  batch.elements.resize(6);
  batch.elements[0] = 0;
  batch.elements[1] = 1;
  batch.elements[2] = 2;
  batch.elements[3] = 2;
  batch.elements[4] = 1;
  batch.elements[5] = 3;

  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }
  if (normals)
  {
    batch.ns.assign(batch.ps.size(), LNormal(0,0,1));
  }
}



void makeQuad(GeometryBatch &batch, float width, float height, bool normals, bool uvs, const LColor *color)
{
  batch.init(GL_TRIANGLE_STRIP, "quad");
  batch.ps.resize(4);
  batch.ps[0] = LPoint(-width,-height,0);
  batch.ps[1] = LPoint(width,-height,0);
  batch.ps[2] = LPoint(-width,height,0);
  batch.ps[3] = LPoint(width,height,0);
  if (uvs)
  {
    batch.uvs[0].resize(4);
    batch.uvs[0][0] = UV(0, 0);
    batch.uvs[0][1] = UV(1, 0);
    batch.uvs[0][2] = UV(0, 1);
    batch.uvs[0][3] = UV(1, 1);
  }

  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }
  if (normals)
  {
    batch.ns.assign(batch.ps.size(), LNormal(0,0,1));
  }
}

void makeCube(GeometryBatch &b, float w, 
    bool normals, bool uvs, const LColor *color)
{

  b.init(GL_TRIANGLES, "cube");
  b.ps.resize(36);
  b.ns.resize(b.ps.size());
  b.uvs[0].resize(b.ps.size());
  b.ps[0] = LPoint(-w, -w, w);  b.ns[0] = LNormal(0,0,1); b.uvs[0][0] = UV(0,0);
  b.ps[1] = LPoint(w, -w, w);   b.ns[1] = LNormal(0,0,1); b.uvs[0][1] = UV(1,0);
  b.ps[2] = LPoint(-w, w, w);   b.ns[2] = LNormal(0,0,1); b.uvs[0][2] = UV(0,1);
  b.ps[3] = LPoint(-w, w, w);   b.ns[3] = LNormal(0,0,1); b.uvs[0][3] = UV(0,1);
  b.ps[4] = LPoint(w, -w, w);   b.ns[4] = LNormal(0,0,1); b.uvs[0][4] = UV(1,0);
  b.ps[5] = LPoint(w, w, w);    b.ns[5] = LNormal(0,0,1); b.uvs[0][5] = UV(1,1);

  b.ps[6] = LPoint(w, -w, w);   b.ns[6] =  LNormal(1,0,0); b.uvs[0][6] = UV(0,0); 
  b.ps[7] = LPoint(w, -w, -w);  b.ns[7] =  LNormal(1,0,0); b.uvs[0][7] = UV(1,0);
  b.ps[8] = LPoint(w, w, w);    b.ns[8] =  LNormal(1,0,0); b.uvs[0][8] = UV(0,1);
  b.ps[9] = LPoint(w, w, w);    b.ns[9] =  LNormal(1,0,0); b.uvs[0][9] = UV(0,1);
  b.ps[10] = LPoint(w, -w, -w); b.ns[10] = LNormal(1,0,0); b.uvs[0][10] = UV(1,0); 
  b.ps[11] = LPoint(w, w, -w);  b.ns[11] = LNormal(1,0,0); b.uvs[0][11] = UV(1,1);

  b.ps[12] = LPoint(w, -w, -w);  b.ns[12] = LNormal(0,0,-1); b.uvs[0][12] = UV(0,0);
  b.ps[13] = LPoint(-w, -w, -w); b.ns[13] = LNormal(0,0,-1); b.uvs[0][13] = UV(1,0);
  b.ps[14] = LPoint(w, w, -w);   b.ns[14] = LNormal(0,0,-1); b.uvs[0][14] = UV(0,1);
  b.ps[15] = LPoint(w, w, -w);   b.ns[15] = LNormal(0,0,-1); b.uvs[0][15] = UV(0,1);
  b.ps[16] = LPoint(-w, -w, -w); b.ns[16] = LNormal(0,0,-1); b.uvs[0][16] = UV(1,0);
  b.ps[17] = LPoint(-w, w, -w);  b.ns[17] = LNormal(0,0,-1); b.uvs[0][17] = UV(1,1);

  b.ps[18] = LPoint(-w, -w, -w); b.ns[18] = LNormal(0,0,-1); b.uvs[0][18] = UV(0,0);
  b.ps[19] = LPoint(-w, -w, w);  b.ns[19] = LNormal(0,0,-1); b.uvs[0][19] = UV(1,0);
  b.ps[20] = LPoint(-w, w, -w);  b.ns[20] = LNormal(0,0,-1); b.uvs[0][20] = UV(0,1);
  b.ps[21] = LPoint(-w, w, -w);  b.ns[21] = LNormal(0,0,-1); b.uvs[0][21] = UV(0,1);
  b.ps[22] = LPoint(-w, -w, w);  b.ns[22] = LNormal(0,0,-1); b.uvs[0][22] = UV(1,0);
  b.ps[23] = LPoint(-w, w, w);   b.ns[23] = LNormal(0,0,-1); b.uvs[0][23] = UV(1,1);

  b.ps[24] = LPoint(-w, w, w);  b.ns[24] = LNormal(0,1,0); b.uvs[0][24] = UV(0,0);
  b.ps[25] = LPoint(w, w, w);   b.ns[25] = LNormal(0,1,0); b.uvs[0][25] = UV(1,0);
  b.ps[26] = LPoint(-w, w, -w); b.ns[26] = LNormal(0,1,0); b.uvs[0][26] = UV(0,1);
  b.ps[27] = LPoint(-w, w, -w); b.ns[27] = LNormal(0,1,0); b.uvs[0][27] = UV(0,1);
  b.ps[28] = LPoint(w, w, w);   b.ns[28] = LNormal(0,1,0); b.uvs[0][28] = UV(1,0);
  b.ps[29] = LPoint(w, w, -w);  b.ns[29] = LNormal(0,1,0); b.uvs[0][29] = UV(1,1);

  b.ps[30] = LPoint(-w, -w, -w); b.ns[30] = LNormal(0,-1,0); b.uvs[0][30] = UV(0,0);
  b.ps[31] = LPoint(w, -w, -w);  b.ns[31] = LNormal(0,-1,0); b.uvs[0][31] = UV(1,0);
  b.ps[32] = LPoint(-w, -w, w);  b.ns[32] = LNormal(0,-1,0); b.uvs[0][32] = UV(0,1);
  b.ps[33] = LPoint(-w, -w, w);  b.ns[33] = LNormal(0,-1,0); b.uvs[0][33] = UV(0,1);
  b.ps[34] = LPoint(w, -w, -w);  b.ns[34] = LNormal(0,-1,0); b.uvs[0][34] = UV(1,0);
  b.ps[35] = LPoint(w, -w, w);   b.ns[35] = LNormal(0,-1,0); b.uvs[0][35] = UV(1,1);

  if (!normals) b.ns.clear();
  if (!uvs) b.uvs[0].clear();
  if (color)
  {
    b.cs.assign(b.ps.size(), *color);
  }
}

void makeRect(GeometryBatch &batch, float width, float height, bool normals, bool uvs, const LColor *color)
{
  batch.init(GL_TRIANGLE_FAN, "rect");
  batch.ps.resize(4);
  batch.ps[0] = LPoint(0,0,0);
  batch.ps[1] = LPoint(width, 0,0);
  batch.ps[2] = LPoint(width,height,0);
  batch.ps[3] = LPoint(0,height,0);
  if (uvs)
  {
    batch.uvs[0].resize(4);
    batch.uvs[0][0] = UV(0, 0);
    batch.uvs[0][1] = UV(width, 0);
    batch.uvs[0][2] = UV(width, height);
    batch.uvs[0][3] = UV(0, height);
  }
  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }
  if (normals)
  {
    batch.ns.assign(batch.ps.size(), LNormal(0,0,1));
  }
}


void makePyramid(GeometryBatch &batch, bool normals, bool uvs, const LColor *color)
{
  float h = 2.0f;
  batch.init(GL_TRIANGLES, "pyramid");
  batch.ps.resize(12);
  batch.ps[0] = LPoint(-h,-h,h);
  batch.ps[1] = LPoint(h,-h,h);
  batch.ps[2] = LPoint(0,h,0);
  batch.ps[3] = LPoint(h,-h,h);
  batch.ps[4] = LPoint(h,-h,-h);
  batch.ps[5] = LPoint(0,h,0);
  batch.ps[6] = LPoint(h,-h,-h);
  batch.ps[7] = LPoint(-h,-h,-h);
  batch.ps[8] = LPoint(0,h,0);
  batch.ps[9] = LPoint(-h,-h,-h);
  batch.ps[10] = LPoint(-h,-h,h);
  batch.ps[11] = LPoint(0,h,0);

  if (uvs)
  {
    batch.uvs[0].resize(12);
    batch.uvs[0][0] = UV(0, 0);
    batch.uvs[0][1] = UV(1, 0);
    batch.uvs[0][2] = UV(.5, 1);
    batch.uvs[0][3] = UV(0, 0);
    batch.uvs[0][4] = UV(1, 0);
    batch.uvs[0][5] = UV(.5, 1);
    batch.uvs[0][6] = UV(0, 0);
    batch.uvs[0][7] = UV(1, 0);
    batch.uvs[0][8] = UV(.5, 1);
    batch.uvs[0][9] = UV(0, 0);
    batch.uvs[0][10] = UV(1, 0);
    batch.uvs[0][11] = UV(.5, 1);
  }

  if (color)
  {
    batch.cs.assign(batch.ps.size(), *color);
  }

  if (normals)
  {
  }

  //  ostream_iterator<LPoint> osi(cout, "\n");
  //  copy(b.ps.begin(), b.ps.end(), osi);
  //  ostream_iterator<UV> osi(cout, "\n");
  //  copy(batch.uvs[0].begin(), batch.uvs[0].end(), osi);
}

void makeCurve(vector<LPoint> &p, float radius, float divisions)
{
  p.resize(divisions+1);
  for (int i = 0; i < p.size(); ++i)
  {
    float iNorm = (float)i / (float)divisions;
    float theta = 2.0f * c_LPI * iNorm;
    p[i] = LPoint(radius * cosf(theta), 0.0f, -radius * sinf(theta));
  }
}

void makeRing(GeometryBatch &b, float radius, float height, int divisions, 
    bool normals, bool uvs, const LColor *color)
{
  b.init(GL_TRIANGLE_STRIP, "ring");
  std::vector<LPoint> curve;
  makeCurve(curve, radius, divisions);
  b.ps.clear();
  for (int j = 0; j < curve.size(); ++j)
  {
    b.ps.push_back(curve[j] + LPoint(0,height,0));
    b.ps.push_back(curve[j] + LPoint(0,-height,0));
  }
  if (uvs)
  {
    for (int j = 0; j < curve.size(); ++j)
    {
      const float jNorm = (float)j / (float)divisions;
      b.uvs[0].push_back(UV(jNorm, 1.0f));
      b.uvs[0].push_back(UV(jNorm, 0.0f));
    }
  }

  if (normals)
  {
    for (int j = 0; j < curve.size(); ++j)
    {
      b.ns.push_back(normalize(LNormal(curve[j] - LPoint(0,0,0))));
      b.ns.push_back(normalize(LNormal(curve[j] - LPoint(0,0,0))));
    }

  }

  if (color)
  {
    b.cs.assign(b.ps.size(), *color);
  }
}

void makeFan(GeometryBatch &b, float radius, float height, int divisions,
    bool normals, bool uvs, const LColor *color)
{
  b.init(GL_TRIANGLE_FAN, "fan");
  vector<LPoint> curve;
  makeCurve(curve, radius, divisions);
  b.ps.clear();
  b.ps.push_back(LPoint(0, height, 0));
  for (int j = 0; j < curve.size(); ++j)
  {
    b.ps.push_back(curve[j] + LPoint(0,-height,0));
  }

  if (uvs)
  {
    b.uvs[0].push_back(UV(0.5f, 1.0f));
    for (int j = 0; j < curve.size(); ++j)
    {
      const float jNorm = (float)j / (float)divisions;
      b.uvs[0].push_back(UV(jNorm,0.0f));
    }

    if (color)
    {
      b.cs.assign(b.ps.size(), *color);
    }
  }
}

LPoint pointOnSphere(float radius, float elevation_r, float azimuth_r)
{
  LPoint p = LPoint(radius * cosf(elevation_r) * cosf(azimuth_r),
      radius * sinf(elevation_r),
      -radius * cosf(elevation_r) * sinf(azimuth_r));
  p.setX(fabs(p.x()) < 0.00001 ? 0 : p.x());
  p.setY(fabs(p.y()) < 0.00001 ? 0 : p.y());
  p.setZ(fabs(p.z()) < 0.00001 ? 0 : p.z());
  return p;
}

float normalizeElevation(float elevation_r)
{
  return (elevation_r + c_LPI*0.5f) / c_LPI;
}

float normalizeAzimuth(float azimuth_r)
{
  return azimuth_r / (2.0f * c_LPI);
}

void makeSphere(GeometryBatch &b, float radius, int divisions, 
    bool normals, bool uvs, const LColor *color)
{

  b.init(GL_TRIANGLES, "sphere");
  b.ps.clear();

  b.uvs[0].clear();
  //  0, 45, 90, 135, 180
  //  0  1   2    3   4    45
  for (int i = 1; i < divisions-1; ++i)
  {
    float e0 =  c_LPI * ((float)i / (float)divisions) - c_LPI / 2.0f;
    float e1 =  c_LPI * (float)(i+1) / (float)divisions - c_LPI / 2.0f;
    for (int j = 0; j < divisions; ++j)
    {
      float a0 = 2.0f * c_LPI * (float)j / (float)divisions;
      float a1 = 2.0f * c_LPI * (float)(j+1) / (float)divisions;

      //      cout << "(e=" << LDegrees(e0) << ",a=" << LDegrees(a0) << ") => " << pointOnSphere(radius, e0, a0) << "\n";

      b.ps.push_back(pointOnSphere(radius, e0, a0));
      b.ps.push_back(pointOnSphere(radius, e0, a1));
      b.ps.push_back(pointOnSphere(radius, e1, a0));
      b.ps.push_back(pointOnSphere(radius, e1, a0));
      b.ps.push_back(pointOnSphere(radius, e0, a1));
      b.ps.push_back(pointOnSphere(radius, e1, a1));

      if (uvs)
      {
        float a0n = normalizeAzimuth(a0);
        float a1n = normalizeAzimuth(a1);
        float e0n = normalizeElevation(e0);
        float e1n = normalizeElevation(e1);
        b.uvs[0].push_back(UV(a0n,e0n));
        b.uvs[0].push_back(UV(a1n,e0n));
        b.uvs[0].push_back(UV(a0n,e1n));
        b.uvs[0].push_back(UV(a0n,e1n));
        b.uvs[0].push_back(UV(a1n,e0n));
        b.uvs[0].push_back(UV(a1n,e1n));
      }
    }
  }

  if (normals)
  {
    for (int n = 0; n < b.ps.size(); ++n)
    {
      b.ns.push_back(LNormal(b.ps[n] - LPoint(0,0,0)));
    }
  }
  if (color)
  {
    b.cs.assign(b.ps.size(), *color);
  }

  //  ostream_iterator<LPoint> osi(cout, "\n");
  //  copy(b.ps.begin(), b.ps.end(), osi);
}

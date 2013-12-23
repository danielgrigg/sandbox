#version 120

varying vec3 vVaryingNormal;
varying vec3 vVaryingLightDirection;
varying float vTexCoord;
uniform sampler1D toonMap;

void main(void)
{
  gl_FragColor = texture1D(toonMap, vTexCoord);
}


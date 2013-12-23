#version 120

uniform samplerCube skyColorMap;
varying vec3 skyTexCoord;

void main(void)
{
  gl_FragColor = textureCube(skyColorMap, skyTexCoord);
}


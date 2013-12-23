#version 120

uniform sampler2DArray colorMaps;
varying vec3 texCoord;

void main(void)
{
  gl_FragColor = texture2DArray(colorMaps, texCoord.xyz);
}


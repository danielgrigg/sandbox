#version 120

uniform sampler2DRect fontFaceMap;
varying vec2 texCoord;

void main(void)
{
  float l = texture2DRect(fontFaceMap, texCoord).r;
  gl_FragColor = vec4(pow(l, 1.0/2.2));// texture2DRect(fontFaceMap, texCoord).rrrr;
}

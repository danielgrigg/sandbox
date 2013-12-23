#version 120

uniform sampler2DRect glyphMap;
varying vec2 texCoord;

void main(void)
{
  gl_FragColor = texture2DRect(glyphMap, texCoord);
}


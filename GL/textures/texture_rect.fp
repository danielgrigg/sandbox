#version 120

uniform sampler2DRect colorMap;
varying vec2 texCoord;

void main(void)
{
  gl_FragColor = texture2DRect(colorMap, texCoord * vec2(1.0, 1.0));
}


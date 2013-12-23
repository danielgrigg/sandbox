#version 120

uniform sampler2D starColorMap;
varying vec4 starColor;

void main(void)
{
  gl_FragColor = texture2D(starColorMap, gl_PointCoord) *starColor;
}


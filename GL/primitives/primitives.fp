#version 120

//varying vec4 vFragColor;
varying vec4 vVaryingColor;
void main(void)
{
  gl_FragColor = vVaryingColor;
 // vFragColor = vVaryingColor;
}


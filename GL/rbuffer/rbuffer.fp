#version 120

varying vec4 vVaryingColor;
void main(void)
{
//  gl_FragColor = vVaryingColor;
  gl_FragData[0] = vVaryingColor * vec4(1,1,1,1);
  gl_FragData[1] = vVaryingColor* vec4(1,0,0,1);;
  gl_FragData[2] = vVaryingColor* vec4(0,1,0,1);;
}


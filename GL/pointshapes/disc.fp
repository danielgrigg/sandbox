#version 120

varying vec4 vVaryingColor;
void main(void)
{
  vec2 p = gl_PointCoord * 2.0 - vec2(1.0);
  if (dot(p,p) > 1.0)
    discard;

  gl_FragColor = vec4(vVaryingColor.xyz, 1.0 - dot(p,p));
}


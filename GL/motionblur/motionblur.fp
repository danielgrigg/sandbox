#version 120

varying vec3 vVaryingNormal;
void main(void)
{
  vec3 C = vVaryingNormal * .5 + vec3(.5);
  gl_FragColor = vec4(C, 1);
}


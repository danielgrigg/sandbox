#version 120

varying vec4 vVaryingColor;
varying mat2 rotation;

void main(void)
{
  vec2 pos = gl_PointCoord - vec2(0.5);
  pos = rotation * pos + vec2(0.5);

  vec2 p = pos * 2.0f - vec2(1.0);
  float r2 = dot(p,p);
  float y2 = sin(atan(p.y, p.x) * 5.0);
  if (r2 > y2)
    discard;
  float y1 = 0.6 * y2;
  float z = 1.0 - smoothstep(y1, y2, r2);

  //gl_FragColor = vec4(vVaryingColor.xyz, z);
  vec3 c = mix(vec3(.3, .15, 0), vec3(0, 1, 0), sqrt(r2));
  gl_FragColor = vec4(c, z);
}


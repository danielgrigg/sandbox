#version 120
uniform vec3 Kd;
uniform vec3 Ka;
uniform vec3 Ks;
uniform float Ns;
varying vec3 vVaryingNormal;
varying vec3 vVaryingLightDirection;

void main(void)
{
  float diff = max(0.0, dot(normalize(vVaryingNormal), 
        normalize(vVaryingLightDirection)));

  vec3 color = diff * Kd;
  color += Ka;
  vec3 reflection = normalize(reflect(-normalize(vVaryingLightDirection), 
        normalize(vVaryingNormal)));
  float spec = max(0.0, dot(normalize(vVaryingNormal), reflection));
  if (diff != 0)
  {
    color += pow(spec, Ns) * Ks;
  }

//  color = 0.5 * normalize(vVaryingNormal) + vec3(.5);
  gl_FragColor = vec4(color, 1.0);

}

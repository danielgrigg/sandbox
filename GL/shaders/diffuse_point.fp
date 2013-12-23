#version 120

varying vec3 vVaryingNormal;
varying vec3 vVaryingLightDirection;

void main(void)
{
  vec4 diffuseColor = vec4(1,0,0,1);

  float diff = max(0.0, dot(normalize(vVaryingNormal), 
        normalize(vVaryingLightDirection)));

  gl_FragColor = diff * diffuseColor;
  gl_FragColor += vec4(0.03, 0.00, 0.00, 0);

  vec3 reflection = normalize(reflect(-normalize(vVaryingLightDirection), 
        normalize(vVaryingNormal)));
  float spec = max(0.0, dot(normalize(vVaryingNormal), reflection));
  if (diff != 0)
  {
    float specPower = pow(spec, 96.0);
    gl_FragColor += vec4(specPower, specPower, specPower, 0.0);
  }
  gl_FragColor = pow(gl_FragColor, vec4(1.0/2.2, 1.0/2.2, 1.0/2.2, 1.0));

  gl_FragColor = vec4(normalize(vVaryingNormal), 1);
}


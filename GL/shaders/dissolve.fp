#version 120

varying vec3 vVaryingNormal;
varying vec3 vVaryingLightDirection;
varying vec2 vVaryingTexCoord;
uniform sampler2D diffuseMap;
uniform float dissolve;
uniform float uTime;

void main(void)
{

  // Erode over time
  vec4 erosion = texture2D(diffuseMap, vVaryingTexCoord * vec2(3.0, 2.0));
  if (erosion.r < dissolve)
    discard;

  vec4 diffuseColor = vec4(1,1,0,1);

  float diff = max(0.0, dot(normalize(vVaryingNormal), 
        normalize(vVaryingLightDirection)));

  gl_FragColor = diff * diffuseColor;


  vec3 reflection = normalize(reflect(-normalize(vVaryingLightDirection), 
        normalize(vVaryingNormal)));
  float spec = max(0.0, dot(normalize(vVaryingNormal), reflection));
  if (diff != 0)
  {
    float specPower = pow(spec, 48.0);
    gl_FragColor += vec4(specPower, specPower, specPower, 0.0);
  }
}


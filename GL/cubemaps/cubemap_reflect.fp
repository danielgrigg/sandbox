#version 120

uniform samplerCube skyColorMap;
uniform mat4 invCamera;
varying vec3 eyeNormal;
varying vec3 eyeVertex;

void main(void)
{
  vec4 worldReflect = invCamera * vec4(reflect(eyeVertex, eyeNormal), 1.0);
  vec3 skyTexCoord = normalize(worldReflect.xyz);
  gl_FragColor = textureCube(skyColorMap, skyTexCoord);
//  gl_FragColor = vec4(skyTexCoord, 1);
}


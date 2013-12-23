#version 120

uniform sampler2D colorMap;
uniform sampler2D modulationMap;
//uniform sampler2D blendFactor;
varying vec2 texCoord;

void main(void)
{
  float blendFactor = 1.0;
  vec4 modColor = texture2D(modulationMap, texCoord * vec2(1.0, 1.0));
  vec4 blendModColor = (1.0 - blendFactor) * vec4(1,1,1,1) + blendFactor * modColor;
  gl_FragColor = texture2D(colorMap, texCoord * vec2(1.0, 1.0)) * blendModColor;
}


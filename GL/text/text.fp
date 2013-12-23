#version 120

uniform sampler2DRect fontFaceMap;
varying vec2 texCoord;

void main(void)
{

  gl_FragColor = texture2DRect(fontFaceMap, texCoord).rrrr;
//  gl_FragColor = vec4(texCoord.s, texCoord.t, 0,1);
}

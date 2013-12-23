#version 120

uniform sampler2DRect blurMap0;
uniform sampler2DRect blurMap1;
uniform sampler2DRect blurMap2;
uniform sampler2DRect blurMap3;
uniform sampler2DRect blurMap4;
uniform sampler2DRect blurMap5;
uniform sampler2DRect blurMap6;
uniform sampler2DRect blurMap7;
uniform sampler2DRect blurMap8;
uniform sampler2DRect blurMap9;
varying vec2 vUV;
void main(void)
{
  vec4 summed = texture2DRect(blurMap0, vUV) + 
    texture2DRect(blurMap1, vUV) + 
    texture2DRect(blurMap2, vUV) + 
    texture2DRect(blurMap3, vUV) + 
    texture2DRect(blurMap4, vUV) + 
    texture2DRect(blurMap5, vUV) +
    texture2DRect(blurMap6, vUV) + 
    texture2DRect(blurMap7, vUV) + 
    texture2DRect(blurMap8, vUV) + 
    texture2DRect(blurMap9, vUV);
  gl_FragColor = summed / 10.0;
}


//
//  Shader.vsh
//  esdemo
//
//  Created by Daniel Grigg on 11/09/10.
//  Copyright Daniel Grigg 2010. All rights reserved.
//

attribute vec4 position;
attribute vec4 color;

varying vec4 colorVarying;

uniform float translate;

void main()
{
    gl_Position = position;
    gl_Position.y += sin(translate) / 2.0;

    colorVarying = color;
}

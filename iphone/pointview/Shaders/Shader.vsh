//
//  Shader.vsh
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

attribute vec4 position;
attribute vec4 color;

varying vec4 colorVarying;

uniform mat4	locMVP;

void main()
{
    gl_Position = locMVP * position;
    colorVarying = color;
}

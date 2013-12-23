//
//  Shader.fsh
//  HelloWorldGL
//
//  Created by Daniel Grigg on 10/01/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}

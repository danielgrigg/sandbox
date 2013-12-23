//
//  Shader.fsh
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}

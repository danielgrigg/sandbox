//
//  Shader.fsh
//  esdemo
//
//  Created by Daniel Grigg on 11/09/10.
//  Copyright Daniel Grigg 2010. All rights reserved.
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}

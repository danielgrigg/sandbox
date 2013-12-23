//
//  ESRenderer.h
//  simplegl
//
//  Created by Daniel Grigg on 24/01/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>

#import <OpenGLES/EAGL.h>
#import <OpenGLES/EAGLDrawable.h>

@protocol ESRenderer <NSObject>

- (void)render;
- (BOOL)resizeFromLayer:(CAEAGLLayer *)layer;

@end

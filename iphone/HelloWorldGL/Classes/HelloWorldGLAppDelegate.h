//
//  HelloWorldGLAppDelegate.h
//  HelloWorldGL
//
//  Created by Daniel Grigg on 10/01/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>

@class HelloWorldGLViewController;

@interface HelloWorldGLAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    HelloWorldGLViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet HelloWorldGLViewController *viewController;

@end


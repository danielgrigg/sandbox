//
//  testlibsAppDelegate.m
//  testlibs
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#import "testlibsAppDelegate.h"

#import "EAGLView.h"

@implementation testlibsAppDelegate


@synthesize window;

@synthesize glView;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {

  // Override point for customization after application launch.
  [glView startAnimation];
    return YES;
}

- (void)applicationWillTerminate:(UIApplication *)application {

  // Save data if appropriate.
  [glView stopAnimation];
}

- (void)dealloc {

  [window release];
  [glView release];
    [super dealloc];
}

- (void)applicationDidBecomeActive:(UIApplication *)application {

  [glView startAnimation];
}


- (void)applicationWillResignActive:(UIApplication *)application {

  [glView stopAnimation];
}


@end

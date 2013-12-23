//
//  simpleglAppDelegate.m
//  simplegl
//
//  Created by Daniel Grigg on 24/01/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#import "simpleglAppDelegate.h"

#import "EAGLView.h"

@implementation simpleglAppDelegate


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

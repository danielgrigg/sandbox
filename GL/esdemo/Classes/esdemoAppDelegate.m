//
//  esdemoAppDelegate.m
//  esdemo
//
//  Created by Daniel Grigg on 11/09/10.
//  Copyright Daniel Grigg 2010. All rights reserved.
//

#import "esdemoAppDelegate.h"
#import "EAGLView.h"

@implementation esdemoAppDelegate

@synthesize window;
@synthesize glView;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    [glView startAnimation];
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    [glView stopAnimation];
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    [glView startAnimation];
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    [glView stopAnimation];
}

- (void)dealloc
{
    [window release];
    [glView release];

    [super dealloc];
}

@end

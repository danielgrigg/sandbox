//
//  whereamiAppDelegate.m
//  whereami
//
//  Created by Daniel Grigg on 21/03/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#import "whereamiAppDelegate.h"

@implementation whereamiAppDelegate


@synthesize window=_window;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
  locationManager = [[CLLocationManager alloc] init];
  
  // We're realizing the CLLocationManagerDelegate protocol so assign
  // ourself as the delegate (observer).  Note the delegate pattern 
  // requires delegates to NOT be retained, avoiding any retain cycles.
  [locationManager setDelegate:self];
  
  // The distance filter specifies the minimum distance travelled before
  // the delegate is notified.  A value of None ensures we're notified
  // whenever a new location is available.
  [locationManager setDistanceFilter:kCLDistanceFilterNone];
  
  // Aim for whatever the iPhone considers 'best' accuracy...
  [locationManager setDesiredAccuracy:kCLLocationAccuracyBest];
  
  // Start the location manager to get locations asap.
  [locationManager startUpdatingLocation];
  [locationManager startUpdatingHeading];
  
  [self.window makeKeyAndVisible];
  return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
  /*
   Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
   Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
   */
}

- (void)applicationDidEnterBackground:(UIApplication *)application
{
  /*
   Use this method to release shared resources, save user data, invalidate timers, and store enough application state information to restore your application to its current state in case it is terminated later. 
   If your application supports background execution, this method is called instead of applicationWillTerminate: when the user quits.
   */
}

- (void)applicationWillEnterForeground:(UIApplication *)application
{
  /*
   Called as part of the transition from the background to the inactive state; here you can undo many of the changes made on entering the background.
   */
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
  /*
   Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
   */
}

- (void)applicationWillTerminate:(UIApplication *)application
{
  /*
   Called when the application is about to terminate.
   Save data if appropriate.
   See also applicationDidEnterBackground:.
   */
}

- (void)dealloc
{
  // We're being deallocated, so remove any reference to us from our 
  // locationManager.
  [locationManager setDelegate:nil];
  
  [_window release];
  [super dealloc];
}

// Callback for Location updates
- (void) locationManager:(CLLocationManager *)manager 
     didUpdateToLocation:(CLLocation *)newLocation 
            fromLocation:(CLLocation *)oldLocation
{
#ifdef DEBUG
  NSLog(@"Location: %@", newLocation);
#endif
  [locationField setText:[newLocation description]];
}

- (void)locationManager:(CLLocationManager *)manager didUpdateHeading:(CLHeading *)newHeading
{
#ifdef DEBUG
  NSLog(@"Heading: %@", newHeading);
#endif
  [headingField setText:[newHeading description]];
}

- (void)locationManager:(CLLocationManager *)manager didFailWithError:(NSError *)error 
{
#ifdef DEBUG
  NSLog(@"Could not find location: %@", error);
#endif
  [locationField setText:[NSString stringWithFormat:@"Error %@", error]];
}

@end

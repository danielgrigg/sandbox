//
//  whereamiAppDelegate.h
//  whereami
//
//  Created by Daniel Grigg on 21/03/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreLocation/CoreLocation.h>

@interface whereamiAppDelegate : NSObject <UIApplicationDelegate, CLLocationManagerDelegate> {
  CLLocationManager *locationManager;
  
  IBOutlet UILabel *locationField;
  IBOutlet UILabel *headingField;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;

@end

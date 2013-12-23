//
//  pointviewAppDelegate.h
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>

@class pointviewViewController;

@interface pointviewAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    pointviewViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet pointviewViewController *viewController;

@end


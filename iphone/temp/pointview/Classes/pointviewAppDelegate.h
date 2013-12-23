//
//  pointviewAppDelegate.h
//  pointview
//
//  Created by Daniel Grigg on 6/02/11.
//  Copyright (c) 2011 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>

@class EAGLView;

@interface pointviewAppDelegate : NSObject <UIApplicationDelegate> {
  UIWindow *window;
  EAGLView *glView;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;

@property (nonatomic, retain) IBOutlet EAGLView *glView;

@end

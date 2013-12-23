//
//  esdemoAppDelegate.h
//  esdemo
//
//  Created by Daniel Grigg on 11/09/10.
//  Copyright Daniel Grigg 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@class EAGLView;

@interface esdemoAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    EAGLView *glView;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet EAGLView *glView;

@end


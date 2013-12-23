//
//  tabtestAppDelegate.h
//  tabtest
//
//  Created by Daniel Grigg on 29/03/12.
//  Copyright 2012 Daniel Grigg. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface tabtestAppDelegate : NSObject <UIApplicationDelegate> {
  UITabBarController* _tabs;
  
  IBOutlet UITextField *_txtInput;
	IBOutlet UILabel *_lblMessage;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet UITabBarController * tabs;
@property (nonatomic, retain) IBOutlet UITextField *txtInput;
@property (nonatomic, retain) IBOutlet UILabel *lblMessage;

@end

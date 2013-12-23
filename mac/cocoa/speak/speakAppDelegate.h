//
//  speakAppDelegate.h
//  speak
//
//  Created by Daniel Grigg on 3/12/10.
//  Copyright 2010 Daniel Grigg. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface speakAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end

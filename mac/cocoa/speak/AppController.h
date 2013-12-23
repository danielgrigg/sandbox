//
//  AppController.h
//  speak
//
//  Created by Daniel Grigg on 3/12/10.
//  Copyright 2010 Daniel Grigg. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface AppController : NSObject
{
  IBOutlet NSTextField *textField;
  NSSpeechSynthesizer *speechSynth;
}
- (IBAction)sayIt:(id)sender;
- (IBAction)stopIt:(id)sender;

@end

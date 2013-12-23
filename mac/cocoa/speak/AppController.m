//
//  AppController.m
//  speak
//
//  Created by Daniel Grigg on 3/12/10.
//  Copyright 2010 Daniel Grigg. All rights reserved.
//

#import "AppController.h"


@implementation AppController

- (id)init
{
  [super init];
  NSLog(@"init");

  speechSynth = [[NSSpeechSynthesizer alloc] initWithVoice:nil];
  return self;
}

- (IBAction)sayIt:(id)sender
{
  NSString *string = [textField stringValue];
  if ([string length] == 0) 
  {
    NSLog(@"string from %@ is of zero-length", textField);
    return;
  }
  [speechSynth startSpeakingString:string];
  NSLog(@"Have started to say: %@", string);
}

- (IBAction)stopIt:(id)sender
{
  NSLog(@"stopping");
  [speechSynth stopSpeaking];
}
@end

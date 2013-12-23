//
//  quizAppDelegate.h
//  quiz
//
//  Created by Daniel Grigg on 8/12/10.
//  Copyright Daniel Grigg 2010. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface quizAppDelegate : NSObject <UIApplicationDelegate>
{
  int currentQuestionIndex;
  NSMutableArray *questions;
  NSMutableArray *answers;
  
  IBOutlet UILabel *questionField;
  IBOutlet UILabel *answerField;
  UIWindow *window;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
- (IBAction)showQuestion:(id)sender;
- (IBAction)showAnswer:(id)sender;
@end


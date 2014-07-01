//
//  AppDelegate.m
//  xpcworld
//
//  Created by Daniel Grigg on 5/08/13.
//  Copyright (c) 2013 Sliplane Software. All rights reserved.
//

#import "AppDelegate.h"

#include <xpc/xpc.h>


static void xpc_world_peer_event_handler(xpc_connection_t peer, xpc_object_t event)
{
	xpc_type_t type = xpc_get_type(event);
	if (type == XPC_TYPE_ERROR) {
		if (event == XPC_ERROR_CONNECTION_INVALID) {
			// The client process on the other end of the connection has either
			// crashed or cancelled the connection. After receiving this error,
			// the connection is in an invalid state, and you do not need to
			// call xpc_connection_cancel(). Just tear down any associated state
			// here.
		} else if (event == XPC_ERROR_TERMINATION_IMMINENT) {
			// Handle per-connection termination cleanup.
		}
	} else {
		assert(type == XPC_TYPE_DICTIONARY);
		// Handle the message.
    
    NSLog(@"xpc_world_peer_event_handler message!\n");
	}
}


@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  xpc_connection_t conn = xpc_connection_create("com.sliplanesoftware.xpcnow", NULL);
  
  xpc_connection_set_event_handler(conn, ^(xpc_object_t event) {
    NSLog(@"xpc_connection event!\n");
		xpc_world_peer_event_handler(conn, event);
	});
  xpc_connection_resume(conn);

  for (int i = 0; i < 1000; ++i) {
    xpc_object_t message = xpc_dictionary_create(NULL, NULL, 0);
      xpc_dictionary_set_uint64(message, "X", 640);
    xpc_connection_send_message(conn, message);
    sleep(2);
  }
}

@end

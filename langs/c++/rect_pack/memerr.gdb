fb -[NSException raise]
fb -[NSAssertionHandler handleFailureInFunction:file:lineNumber:description:]
fb -[NSAssertionHandler handleFailureInMethod:object:file:lineNumber:description:]

#define NSZombies
# this will give you help messages.  Set to NO to turn them off.
set env MallocHelp=YES
# might also be set in launch arguments.
set env NSZombieEnabled=YES
set env NSDeallocateZombies=NO
set env MallocCheckHeapEach=100000
set env MallocCheckHeapStart=100000
set env MallocScribble=YES
set env MallocGuardEdges=YES
set env MallocCheckHeapAbort=1

set env CFZombie 5

fb -[_NSZombie init]
fb -[_NSZombie retainCount]
fb -[_NSZombie retain]
fb -[_NSZombie release]
fb -[_NSZombie autorelease]
fb -[_NSZombie methodSignatureForSelector:]
fb -[_NSZombie respondsToSelector:]
fb -[_NSZombie forwardInvocation:]
fb -[_NSZombie class]
fb -[_NSZombie dealloc]

fb szone_error

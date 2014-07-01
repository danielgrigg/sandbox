# Usage gdb core -x trace_breaks.gdb
# OR gdb core <pid> -x trace_breaks.gdb
# Future breakpoints on foo, bar
# If using an older gdb without 'future-break',
# you'll need to first 'set breakpoint pending on'
# then replace fb with b.
fb foo
# Commands to invoke on break
command
  p someVar
  continue
end
fb bar
command
  continue
end
# If we have signal handlers, we can break/continue
# on them to escape from gdb :)
fb handleSignal
command
  quit
end
# Prevent gdb catching SIGABRT
handle SIGABRT nostop
run


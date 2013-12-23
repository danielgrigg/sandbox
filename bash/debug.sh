#!/bin/bash

#DEBUG=1

function debug() { ((DEBUG)) && echo ">>> $*"; }

debug "Testing the debug function"

#!/bin/bash

# comma operator
echo /{,usr/}bin/ca*

echo `date` | tee ./out/x{1,2}

# extended brace expansion
echo {a..x}


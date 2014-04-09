#!/bin/sh

# Very simple script to download & build bullet2. 
# Better would be updating the bullet2 repo if it exists
# and only making the bootstrap target if there's updates.

# At the end of the script, you'll have:
#  /vendor/ios/include/bullet2 - all the bullet headers
#  /vendor/ios/lib/ - libraries for LinearMath, Collision and Dynamics.

cd vendor/downloads/
rm -rf bullet2
svn checkout http://bullet.googlecode.com/svn/trunk/ bullet2
cd ../ios/bullet2

make bootstrap && make && make install

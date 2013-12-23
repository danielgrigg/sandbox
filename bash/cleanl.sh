#!/bin/sh
ls -1 | grep -i "^$1" | 
while read line 
do 
  rm -rf "$line"
done

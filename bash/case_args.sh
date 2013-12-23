#!/bin/bash

case "$1" in
  ""    ) echo "empty";;
??[1-5]* ) echo "some small digits";;
*[0-9]* ) echo "a number";;
*       ) echo "umm, a $1";;
esac



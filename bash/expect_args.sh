#!/bin/bash
#
E_WRONG_ARGS=85
script_params="-a -h -m -z"
number_expected=4

if [ $# -ne $number_expected ] 
then
  echo "Usage: `basename $0` $script_params"
  exit $E_WRONG_ARGS
fi


#!/bin/sh

NAME=$1
EXT=${NAME##*.}
if [[ $NAME =~ ^(.*)\ (.*)\ ([0-9]{1,2})\ (.*)\ ([0-9]{1,2}) ]] ; then
  OUT_NAME="${BASH_REMATCH[1]}.S${BASH_REMATCH[3]}.E${BASH_REMATCH[5]}.${EXT}"
  echo "Moving ${NAME} to $OUT_NAME"
  mv ${NAME} ${OUT_NAME}
fi



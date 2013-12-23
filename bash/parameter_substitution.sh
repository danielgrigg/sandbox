echo `basename $PWD`
echo "${PWD##*/}"  # strip everything to last /
Z="123456"

echo ${Z#1} # remove shortest front pattern
echo ${Z##*3} # remove longest front pattern
echo ${Z%6}
echo ${Z%%4*}

echo ${PATH//:/,} # replace : with , globally



count=1
MAXCOUNT=10

while [[ "$count" -le $MAXCOUNT ]]
do
  number=$RANDOM
  echo $number
  let "count += 1"
done


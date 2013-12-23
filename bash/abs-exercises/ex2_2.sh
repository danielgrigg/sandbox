DATE=`date` 
WHO=`who`
UPTIME=`uptime`
LOG_FILE=/tmp/ex22.log

echo $DATE
echo $WHO
echo $UPTIME

echo $DATE > $LOG_FILE
echo $WHO >> $LOG_FILE
echo $UPTIME >> $LOG_FILE


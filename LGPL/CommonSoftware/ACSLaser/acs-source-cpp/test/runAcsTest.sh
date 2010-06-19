#!/bin/bash
if [ "$#" -ne 1 ]
then
	echo -e "\n\nUsage:\n\nrunAcsTest.sh <NUM_TO_SEND>\n\nwhere: NUM_TO_SEND is the number of alarms to send/verify in the test\n\n"
fi
NUM_TO_SEND=$1
loggingClient Logging >& logs.xml &
testDriverAcs $NUM_TO_SEND
sleep 5
numAlarmsFound=`grep -i alert logs.xml | grep -i alarm | wc -l`
kill -9 $!

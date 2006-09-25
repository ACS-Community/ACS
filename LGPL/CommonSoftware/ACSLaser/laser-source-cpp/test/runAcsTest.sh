#!/bin/bash
if [ "$#" -ne 1 ]
then
	echo -e "\n\nUsage:\n\nrunAcsTest.sh <NUM_TO_SEND>\n\nwhere: NUM_TO_SEND is the number of alarms to send/verify in the test\n\n"
fi
NUM_TO_SEND=$1
acsStopContainer bilboContainer
cp $ACS_CDB/CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml .
patch $ACS_CDB/CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml AlarmSystemConfiguration.xml.patch
cdbjDALClearCache
acsutilAwaitContainerStart -cpp bilboContainer
loggingClient LoggingChannel >& logs.xml &
sleep 10
testDriverAcs $NUM_TO_SEND
sleep 10
numAlarmsFound=`grep -i alert logs.xml | grep -i alarm | wc -l`
if [ "$numAlarmsFound" -eq "$NUM_TO_SEND" ]
then
	echo "SUCCESS: expected $NUM_TO_SEND alarms and detected $numAlarmsFound alarms"
else
	echo "FAILED: expected $NUM_TO_SEND alarms but only detected $numAlarmsFound alarms"
fi
kill -9 $!
cp AlarmSystemConfiguration.xml $ACS_CDB/CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml

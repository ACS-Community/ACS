#! /bin/bash

# Run the JUnit test, which will activate and use the test components
acsStartJava \
    -DACS.log.minlevel.namedloggers="jacorb@ContainerLoggingAlarmTest=5,5" \
    org.junit.runner.JUnitCore \
    alma.alarmContainerTest.client.ContainerLoggingAlarmTest

# xml logs, captured by the loggingClient that gets run by the prologue (acsutilTATPrologue -l)
XML_LOG_FILE=$ACS_TMP/ACS_INSTANCE.$ACS_INSTANCE/all_logs.xml
if [ -f "$XML_LOG_FILE" ]
then
	echo "OK: XML log file found."
else
	echo "ERROR: XML log file $XML_LOG_FILE not found."
fi

# alarm service logs
ALARM_SERVICE_LOG_FILE=$ACS_TMP/acsStart.log
if [ -f "$ALARM_SERVICE_LOG_FILE" ]
then
	echo "OK: Alarm service log file found."
else
	echo "ERROR: Alarm service log file $ALARM_SERVICE_LOG_FILE not found."
fi

# stdout logs of the unthrottled container (where we expect log queue overflow)
UNTHROTTLED_CONTAINER_LOG_FILE=$ACS_TMP/ACS_INSTANCE.$ACS_INSTANCE/unthrottledJavaContainer.out
if [ -f "$UNTHROTTLED_CONTAINER_LOG_FILE" ]
then
	echo "OK: Unthrottled container log file found."
else
	echo "ERROR: Unthrottled container log file $UNTHROTTLED_CONTAINER_LOG_FILE not found."
fi


# Wait a bit for all xml logs to be processed and for the alarm service to report the clearing of the throttle alarm.
sleep 5

# Check that throttle alarms from ContainerLoggingAlarmTest#testLogThrottleAlarm() arrived at the alarm service
echo "Expecting throttle alarm (triplet Logging:throttledJavaContainer:10) to be raised and cleared,"
echo "thus two alarms with active=true/false published by the alarm service:"
grep "publishing alarm change for Logging:throttledJavaContainer:10" $ALARM_SERVICE_LOG_FILE

# TODO: Check container output for throttle logs that correspond to the alarms:
# FINE [alma.acs.logging] Log throttle kicked in, suppressing logs. Alarm has been raised.
# FINE [alma.acs.logging] Log throttle disengaged. Alarm has been cleared.


# Compute the index numbers of lost "Info Test log" logs from component TestcompJavaUnthrottled, 
# based on missing index numbers in the XML logs. 
grep "^<Info.*SourceObject=\"TestcompJavaUnthrottled\".*Test log" $XML_LOG_FILE \
	| sed "s/.*CDATA\[Test log (INFO) \#\([0-9]*\)\].*/\1/" \
	| sort -n \
	> UnthrottledJavaInfoTestLogIndices

test -s UnthrottledJavaInfoTestLogIndices || echo "ERROR: No test logs found in the XML logs, which is bad because the test relies on both getting and losing some."

# We expect the test log index to run from 0 to 9.999. Add 10.000 to make sure we don't miss a log gap at the end.
echo "10000" >> UnthrottledJavaInfoTestLogIndices

cat UnthrottledJavaInfoTestLogIndices \
    | awk '{ if ($1 > prev+1) ret = (ret " " prev+1); prev=$1 } END { if(length(ret)>0) print substr(ret, 2) }' \
    > UnthrottledJavaMissingInfoTestLogIndices

test -s UnthrottledJavaMissingInfoTestLogIndices || echo "ERROR: No test logs missing in the XML logs, which is bad because the test relies on both getting and losing some."

# Check container stdout for the expected error messages
# The test loses both DEBUG and INFO logs, but with different error messages, 
# because DEBUG logs get dropped already when the queue is 70% full, while INFO logs only when the queue is completely full.
grep "log queue overflow: log record with message 'Test log (INFO) #[0-9]*' and possibly future log records will not be sent to the remote logging service." $UNTHROTTLED_CONTAINER_LOG_FILE \
	| sed "s/[^0-9]*\([0-9]*\).*/\1/" \
	| paste -s -d " " \
	> UnthrottledJavaDroppedInfoTestLogIndices

# The blocks of INFO logs missing from the captured XML logs should be exactly those 
# that the container reported as "log queue overflow:"
echo "Diff between missing blocks of logs reported by container and the logs missing in XML should be empty:"
diff UnthrottledJavaMissingInfoTestLogIndices UnthrottledJavaDroppedInfoTestLogIndices

# Now check the container stdout logs for the warning that Debug and below logs get dropped because the queue is full
# Note that this message does currently not appear in the XML logs, to not stress the logging system further. 
DEBUG_LOGS_DROPPED_MESSAGE_COUNT=`grep "looming log queue overflow .*Test log (DEBUG)" $UNTHROTTLED_CONTAINER_LOG_FILE | wc -l`
if [ $DEBUG_LOGS_DROPPED_MESSAGE_COUNT -eq 0 ]; then
	echo "ERROR: expected at least one 'looming log queue overflow...' message in $UNTHROTTLED_CONTAINER_LOG_FILE"
else
	echo "OK: Verified 'looming log queue overflow...' message caused by dropped DEBUG logs when queue capacity got scarce."
fi

# TODO: Verify that at least some of the "Test log (DEBUG)" are in the xml logs.

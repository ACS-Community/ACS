#!/bin/bash

. acsstartupPids
#---------------------------
PID=`getManagerPid`
echo "Manager process ID is: $PID"

#---------------------------
PID=`getNamingServicePid`
echo "Naming service process ID is: $PID"

#---------------------------
PID=`getNotifyServicePid`
echo "Notify service process ID is: $PID"

#---------------------------
PID=`getLoggingServicePid`
echo "Logging service process ID is: $PID"

#---------------------------
PID=`getIRPid`
echo "IFR process ID is: $PID"

#---------------------------
PID=`getLoggingNotifyServicePid`
echo "Logging notify service process ID is: $PID"

#---------------------------
PID=`getArchiveNotifyServicePid`
echo "Archive notify service process ID is: $PID"

#---------------------------
PID=`getLogPid`
echo "ACS Log Service process ID is: $PID"

#---------------------------
PID=`getCDBPid`
echo "ACS CDB process ID is: $PID"
#---------------------------


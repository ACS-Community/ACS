#!/bin/bash
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`
CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
echo "$CURR_TIME Stopping Notify Service '$1'"
acsNotifyService -k -b 0 -w -n $1 -x corbaloc::$HOST:$NS_PORT/NameService
CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
echo "$CURR_TIME Stopped Notify Service '$1'"

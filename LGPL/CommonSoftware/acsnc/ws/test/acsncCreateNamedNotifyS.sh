#!/bin/bash
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`
CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
echo "$CURR_TIME Restarting Notify Service '$1'"
acsNotifyService -k -s -w -n $1 -x corbaloc::$HOST:$NS_PORT/NameService
CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
echo "$CURR_TIME Restarted Notify Service '$1'"

#!/bin/bash
# $1: Name of the Notify Service
# $2: type of execution: START, STOP, RESTART
# $3: sleep seconds. It only takes effect when NS is restarted
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`

NS_NAME=$1
EXEC_TYPE=$2
SLEEP_SEC=$3

if [ "$EXEC_TYPE" == "START" ]; then
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Starting Notify Service '$NS_NAME'"
    acsNotifyService -s -w -n $NS_NAME -x corbaloc::$HOST:$NS_PORT/NameService
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Started Notify Service '$NS_NAME'"
elif [ "$EXEC_TYPE" == "STOP" ]; then
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Stopping Notify Service '$NS_NAME'"
	acsNotifyService -k -w -n $NS_NAME -x corbaloc::$HOST:$NS_PORT/NameService
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Stopped Notify Service '$NS_NAME'"
elif [ "$EXEC_TYPE" == "RESTART" ]; then
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Restarting Notify Service '$NS_NAME'"
    if [ "$SLEEP_SEC" == "0" ]; then
        acsNotifyService -k -s -w -n $NS_NAME -x corbaloc::$HOST:$NS_PORT/NameService
    else
        acsNotifyService -k -w -n $NS_NAME -x corbaloc::$HOST:$NS_PORT/NameService
        sleep $SLEEP_SEC
        acsNotifyService -s -w -n $NS_NAME -x corbaloc::$HOST:$NS_PORT/NameService
    fi
    CURR_TIME=`date +"%Y-%m-%dT%H:%M:%S.%N"`
    echo "$CURR_TIME Restarted Notify Service '$NS_NAME'"
fi


#!/bin/bash
ACS_CDB_ORIG=$ACS_CDB
export ACS_CDB=$PWD
FLAG=0
mkdir -p logs

NUM_THREADS_OPTION="5"

if [ -n "$1" ]; then
	NUM_THREADS_OPTION="$1"
	echo "NUM_THREADS_OPTION = \"$NUM_THREADS_OPTION\""
fi

acscontainerdaemon -n $NUM_THREADS_OPTION &> logs/syncContainers.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting container daemon"
    FLAG=1
else
    sleep 5
fi

acsStart -b $ACS_INSTANCE &> acsStart.log 
if [ $? -ne 0 ]; then
    echo "FAILED - starting ACS"
    FLAG=1
fi

sleep 1

acsdaemonStartContainer -t cpp -c slowComponentContainer -i $ACS_INSTANCE -s &> logs/slowComponentContainer.log
RET1=$?
if [ $RET1 -ne 0 ]; then
    echo "FAILED - starting slowComponentContainer"
    FLAG=1
fi

acsdaemonStartContainer -t cpp -c slowComponentContainer2 -i $ACS_INSTANCE --synchronous &> logs/slowComponentContainer2.log
RET2=$?
if [ $RET2 -ne 0 ]; then
    echo "FAILED - starting slowComponentContainer2"
    FLAG=1
fi

sleep 2

acsDaemonTestNumThreadsOption &> logs/acsDaemonTestNumThreadsOption.log
RET3=$?
CONTAINERS_STOP_FAILED=0
if [ $RET3 -ne 0 ]; then
	cat logs/acsDaemonTestNumThreadsOption.log
	if [ $NUM_THREADS_OPTION -gt 1 ]; then
	    echo "FAILED - acsDaemonTestNumThreadsOptions unexpected error"
		FLAG=1
	else
		echo "OK - acsDaemonTestNumThreadsOptions expected error"
	fi
else
	if [ $NUM_THREADS_OPTION -le 1 ]; then
		cat logs/acsDaemonTestNumThreadsOption.log
	    echo "FAILED - acsDaemonTestNumThreadsOptions unexpected success"
		FLAG=1
	else
		echo "OK - acsDaemonTestNumThreadsOptions expected success"
	fi
fi

acsStop -b $ACS_INSTANCE &> logs/acsStop.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping ACS"
    FLAG=1
fi
sleep 2 
CPID=$(ps aux|grep acscontainerdaemon| grep -v grep|awk '{ print $2 }')
kill $CPID 
sleep  5 
ps -p $CPID &>/dev/null
if [ $? -ne 1 ]; then
    echo "FAILED - shutting down acsContainerDaemon"
    FLAG=1
fi

# Restore ACS CDB
export ACS_CDB=$ACS_CDB_ORIG

if [ $FLAG -ne 0 ]; then
    echo "SOME TEST FAILED"
else
    echo "HEY MATE, EVERYTHING IS A OK"
fi


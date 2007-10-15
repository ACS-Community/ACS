#!/bin/bash
FLAG=0
acsservicesdaemon &> services.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting services daemon"
    FLAG=1
else
    SPID=$(ps aux|grep acsservicesdaemon| grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acscontainerdaemon &> containers.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting container daemon"
    FLAG=1
else
    CPID=$(ps aux|grep acscontainerdaemon| grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acsdaemonStartAcs -i 0 &> startACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ACS"
    FLAG=1
fi
acsdaemonStartContainer -t cpp -c bilboContainer -i 0 &> startContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting bilboContainer"
    FLAG=1
fi
sleep 30


acsdaemonStopContainer -c bilboContainer -i 0 >> stopContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping bilboContainer"
    FLAG=1
fi
sleep 10
acsdaemonStopAcs -i 0 >> stopACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping ACS"
    FLAG=1
fi
sleep 10 
kill $CPID 
sleep  5 
ps -p $CPID &>/dev/null
if [ $? -ne 1 ]; then
    echo "FAILED - shutting down acsContainerDaemon"
    FLAG=1
fi
kill $SPID
sleep  5 
ps -p $SPID &>/dev/null 
if [ $? -ne 1 ]; then
    echo "FAILED - shutting down acsServicesDaemon"
    FLAG=1
fi

if [ $FLAG -ne 0 ]; then
    echo "SOME TEST FAILED"
else
    echo "HEY MATE, EVERYTHING IS A OK"
fi

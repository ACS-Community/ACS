#!/bin/bash
acsservicesdaemon &> services.log &
if [ $? -ne 0 ]; then
    echo "FAILED - starting services daemon"
else
    SPID=$(ps aux|grep acsservicesdaemon| grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acscontainerdaemon &> containers.log &
if [ $? -ne 0 ]; then
    echo "FAILED - starting container daemon"
else
    CPID=$(ps aux|grep acscontainerdaemon| grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acsdaemonStartAcs -i 0 &> startACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ACS"
fi
acsdaemonStartContainer -t cpp -c bilboContainer -i 0 &> startContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting bilboContainer"
fi
sleep 30


acsdaemonStopContainer -c bilboContainer -i 0 >> stoptContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping bilboContainer"
fi
sleep 10
acsdaemonStopAcs -i 0 >> stopACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping ACS"
fi
sleep 10 
kill $CPID 
ps -p $CPID &>/dev/null
if [ $? -ne 1 ]; then
    echo "FAILED - shuting down acsContainerDaemon"
fi
kill $SPID
ps -p $SPID &>/dev/null 
if [ $? -ne 1 ]; then
    echo "FAILED - shuting down acsServicesDaemon"
fi

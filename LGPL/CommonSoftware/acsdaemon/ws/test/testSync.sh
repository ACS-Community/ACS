#!/bin/bash
FLAG=0
acsservicesdaemon &> syncServices.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting services daemon"
    FLAG=1
else
    SPID=$(ps auxww |grep acsservicesdaemon| grep $USER | grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acscontainerdaemon &> syncContainers.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting container daemon"
    FLAG=1
else
    CPID=$(ps auxww |grep acscontainerdaemon| grep $USER | grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acsdaemonStartAcs -i 0 &> syncStartACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ACS"
    FLAG=1
fi
acsdaemonStartContainer -t cpp -c bilboContainer -i 0 -s &> syncStartbilboContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting bilboContainer"
    FLAG=1
fi

acsdaemonStartContainer -t cpp -c ARCHIVE/ACC/cppContainer -i 0 --synchronous &> syncStartcppContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ARCHIVE/ACC/cppContainer"
    FLAG=1
fi

acsdaemonStartContainer -s -t java -c ARCHIVE/ACC/javaContainer -i 0 &> syncStartjavaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ARCHIVE/ACC/javaContainer"
    FLAG=1
fi

acsdaemonStartContainer -t py -s -c CONTROL/AMBSOCKETSERVER/pyContainer -i 0 &> syncStartpyContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting CONTROL/AMBSOCKETSERVER/pyContainer"
    FLAG=1
fi

acsdaemonStartContainer --synchronous -t py -m casaContainer -c CONTROL/AMBSOCKETSERVER/pyCasaContainer -i 0 &> syncStartpyCasaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting CONTROL/AMBSOCKETSERVER/pyCasaContainer"
    FLAG=1
fi

sleep 1

acsdaemonStatusAcs -i 0 &> syncAcsStatus.log
if [ $? -ne 0 ]; then
    echo "FAILED - getting ACS status"
    FLAG=1
fi

acsdaemonStopContainer -s -c bilboContainer -i 0 &> syncStopbilboContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping bilboContainer"
    FLAG=1
fi

acsdaemonStopContainer -c ARCHIVE/ACC/cppContainer --synchronous -i 0 >> syncStopcppContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping ARCHIVE/ACC/cppContainer"
    FLAG=1
fi

acsdaemonStopContainer -c ARCHIVE/ACC/javaContainer -i 0 -s >> syncStopjavaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping ARCHIVE/ACC/javaContainer"
    FLAG=1
fi

acsdaemonStopContainer -c CONTROL/AMBSOCKETSERVER/pyContainer -s -i 0 >> syncStoppyContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping CONTROL/AMBSOCKETSERVER/pyContainer"
    FLAG=1
fi

CASA_DUMMY_CONTAINER_PID=$(ps auxww |grep "CONTROL/AMBSOCKETSERVER/pyCasaContainer" | grep $USER | grep acsStartContainerWithCASA | grep -v grep|awk '{ print $2 }')
kill $CASA_DUMMY_CONTAINER_PID
sleep 1
ps -p $CASA_DUMMY_CONTAINER_PID &>/dev/null
if [ $? -ne 1 ]; then
    echo "FAILED - stopping CONTROL/AMBSOCKETSERVER/pyCasaContainer dummy process"
    FLAG=1
fi

acsdaemonStopAcs -i 0 >> syncStopACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - stopping ACS"
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

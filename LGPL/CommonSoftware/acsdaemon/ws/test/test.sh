#!/bin/bash
FLAG=0
acsservicesdaemon &> services.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting services daemon"
    FLAG=1
else
    SPID=$(ps auxww |grep acsservicesdaemon| grep $USER |grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acscontainerdaemon &> containers.log &
sleep 1
if [ $? -ne 0 ]; then
    echo "FAILED - starting container daemon"
    FLAG=1
else
    CPID=$(ps auxww |grep acscontainerdaemon| grep $USER | grep -v grep|awk '{ print $2 }')
    sleep 5
fi
acsdaemonStartAcs -i 0 &> startACS.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ACS"
    FLAG=1
fi
acsdaemonStartContainer -t cpp -c bilboContainer -i 0 &> startbilboContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting bilboContainer"
    FLAG=1
fi
sleep 10
acsdaemonStartContainer -t cpp -c ARCHIVE/ACC/cppContainer -i 0 &> startcppContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ARCHIVE/ACC/cppContainer"
    FLAG=1
fi
sleep 10
acsdaemonStartContainer -t java -c ARCHIVE/ACC/javaContainer -i 0 &> startjavaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting ARCHIVE/ACC/javaContainer"
    FLAG=1
fi
sleep 10
acsdaemonStartContainer -t py -c CONTROL/AMBSOCKETSERVER/pyContainer -i 0 &> startpyContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting CONTROL/AMBSOCKETSERVER/pyContainer"
    FLAG=1
fi
sleep 10
acsdaemonStartContainer -t py -m fortranContainer -c CONTROL/AMBSOCKETSERVER/pyCasaContainer -i 0 &> startpyCasaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - starting CONTROL/AMBSOCKETSERVER/pyContainer"
    FLAG=1
fi
sleep 30


acsdaemonStatusAcs -i 0 &> acsStatus.log
if [ $? -ne 0 ]; then
    echo "FAILED - getting ACS status"
    FLAG=1
fi
sleep 5

acsdaemonStopContainer -c bilboContainer -i 0 &> stopbilboContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping bilboContainer"
    FLAG=1
fi
sleep 10
acsdaemonStopContainer -c ARCHIVE/ACC/cppContainer -i 0 >> stopcppContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping ARCHIVE/ACC/cppContainer"
    FLAG=1
fi
sleep 10
acsdaemonStopContainer -c ARCHIVE/ACC/javaContainer -i 0 >> stopjavaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping ARCHIVE/ACC/javaContainer"
    FLAG=1
fi
sleep 10
acsdaemonStopContainer -c CONTROL/AMBSOCKETSERVER/pyContainer -i 0 >> stoppyContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping CONTROL/AMBSOCKETSERVER/pyContainer"
    FLAG=1
fi
sleep 10
acsdaemonStopContainer -c CONTROL/AMBSOCKETSERVER/pyCasaContainer -i 0 >> stoppyCasaContainer.log
if [ $? -ne 0 ]; then
    echo "FAILED - stoping CONTROL/AMBSOCKETSERVER/pyCasaContainer"
    FLAG=1
fi
sleep 10
CASA_DUMMY_CONTAINER_PID=$(ps auxww |grep "CONTROL/AMBSOCKETSERVER/pyCasaContainer" | grep $USER |grep acsStartContainerWithFortran | grep -v grep|awk '{ print $2 }')
kill $CASA_DUMMY_CONTAINER_PID
sleep 1
ps -p $CASA_DUMMY_CONTAINER_PID &>/dev/null
if [ $? -ne 1 ]; then
    echo "FAILED - stopping CONTROL/AMBSOCKETSERVER/pyCasaContainer dummy process"
    FLAG=1
fi
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

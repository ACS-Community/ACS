#!/bin/bash

#run the command
$* >& $ACS_TMP/$$.log

#sleep so things stabilize
sleep 10

#stop the container
acsStopContainer frodoContainer >&  /dev/null

#give the container a chance to flush logs
sleep 10

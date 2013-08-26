#!/bin/bash

#run the command
$*

#sleep so things stabilize
sleep 10

#stop the container
acsStopContainer bilboContainer >&  /dev/null

#give the container a chance to flush logs
sleep 10

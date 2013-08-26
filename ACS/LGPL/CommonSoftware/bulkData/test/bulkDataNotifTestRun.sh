#!/bin/bash
. acsstartupAcsPorts

export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

echo "Running $*"

$*

sleep 25

echo "Shutting down ContainerNotifTest"
acsStopContainer ContainerNotifTest >&  /dev/null
sleep 10
echo "All done!"



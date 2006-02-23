#!/bin/bash
. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

# Start the container
if [ "$WIND_BASE" != "" ] 
then
    vccResetLcu $LCU PPC604 > /dev/null
    maciTest $LCU ContainerStart $1 -m corbaloc::$HOST:`getManagerPort`/Manager
else  
    maciContainer $1 -ORBEndpoint iiop://$HOST:$2
fi

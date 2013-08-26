#!/bin/bash
# Start the container
. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

maciContainer $1 -ORBEndpoint iiop://$HOST:$2


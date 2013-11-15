#!/bin/bash
# Stop the manager
. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

acsManager -k -b $ACS_INSTANCE
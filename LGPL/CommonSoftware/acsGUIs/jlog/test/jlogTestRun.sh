#!/bin/bash
. acsstartupAcsPorts

export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

echo "Running $*"
if [ "$WIND_BASE" != "" ] 
then
    export MANAGER_REFERENCE=corbaloc::`getIP`:`getManagerPort`/Manager
    export DAL_REFERENCE=corbaloc::`getIP`:`getCDBPort`/CDB
    sleep 250
    $*
else 
    $*
fi

echo "All done!"

#!/bin/bash
# sleep was 120
echo "Running $*"

. acsstartupAcsPorts

export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`
export MANAGER_REFERENCE=corbaloc::`getIP`:`getManagerPort`/Manager

if [ "$WIND_BASE" != "" ] 
then
  sleep 150
  $*
else 
  $*
fi

sleep 5

echo "Shutting down Container"
acsStopContainer Container >&  /dev/null
sleep 15
echo "All done!"



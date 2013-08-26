#!/bin/ksh

. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`
export DAL_REFERENCE=corbaloc::$HOST:`getCDBPort`/CDB

rm -f iors.dat
if [ "$WIND_BASE" != "" ] 
then
  rm -f $VLTDATA/ENVIRONMENTS/$lcuTat/iors.dat
  vccResetLcu $LCU PPC604 > /dev/null
  export MANAGER_REFERENCE=corbaloc::$HOST:`getManagerPort`/Manager 
  baciTest $LCU startBaciTestServer 0
  rm -f $VLTDATA/ENVIRONMENTS/$lcuTat/iors.dat
 else   
  baciTestServer 0
fi

rm -f iors.dat

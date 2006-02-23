#!/bin/ksh
# Start the activator

. acsstartupAcsPorts

export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`
export MANAGER_REFERENCE=corbaloc::`getIP`:`getManagerPort`/Manager

# Not necessary any more since long
# export DAL_REFERENCE=corbaloc::`getIP`:`getCDBPort`/CDB


if [ "$WIND_BASE" != "" ] 
then
  vccResetLcu $LCU PPC604 > /dev/null
  acssampTest $LCU ContainerStart Container -ORBInitRef InterfaceRepository=corbaloc::`getIP`:`getIRPort`/InterfaceRepository
 else  
  acsStartContainer -cpp Container
fi


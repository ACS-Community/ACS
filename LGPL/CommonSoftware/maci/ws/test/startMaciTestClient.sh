#!/bin/bash

. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`
export MAX_TIMEOUT=600

maciTestClient $1 &

#Fail-safe mechanism in case maciTestClient hangs
MTC_PID=$!

i="0"
while [ "$i" ] 
do
  #check if we've waited too long.
  if [ "$i" = "$MAX_TIMEOUT" ]
  then
      echo "==> Error - max timeout exceeded"
      acsKillProc $MTC_PID
      break
  #pid has disappeared
  elif [ "`ps --no-headers -p $MTC_PID`" = "" ] 
  then
      break
  else 
      i=$(( $i + 1 ))
      sleep 1
  fi 
done 

# give time to the Manager and containers to shutdown
sleep 30

echo "================= Done ==============="


#! /bin/bash

#acsutilTATEpilogue
#sleep 1000
#source maciTATEnvironmentWrongCDB
export ACS_LOG_STDOUT=4
export ACS_CDB=$PWD/CDB-WRONG
acsStart -b 9 > acsStartWrong.log 2>&1
sleep 500


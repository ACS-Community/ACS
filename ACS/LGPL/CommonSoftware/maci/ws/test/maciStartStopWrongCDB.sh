#! /bin/bash

export ACS_LOG_STDOUT=4
export ACS_CDB=$PWD/CDB-WRONG

echo "Will start ACS with a faulty CDB" 
acsStart -b 9 > acsStartWrongCDB.log  2>&1
echo "ACS started as instance 9"

echo "Will extract the CDB error"
grep -A 4 "Failed to read curl" acsStartWrongCDB.log

sleep 50
rm acsStartWrongCDB.log

echo "Will stop ACS with the faulty CDB"
acsStop -b 9 > /dev/null 2>&1

sleep 50

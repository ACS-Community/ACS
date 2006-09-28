#! /bin/bash

cat  acsStartWrong.log | grep "XML Error" | grep "CURL='MACI/Components'"
acsStop -b 9 > /dev/null 2>&1
sleep 50
rm acsStartWrong.log
#acsutilTATPrologue

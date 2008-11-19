#! /bin/bash

# To be sure, delete all temporary and the recovery files before starting
if [ -e $ACS_TMP ]; then
  rm -rf $ACS_TMP &> /dev/null
fi
mkdir $ACS_TMP

#acsStart > acsStart.log 2>&1
#echo "ACS started."

noloadifr='--noloadifr'
acsutilTATPrologue -l $noloadifr
filesToLoad=`cat IDLFilesToLoad`
acsstartupLoadIFR  $filesToLoad



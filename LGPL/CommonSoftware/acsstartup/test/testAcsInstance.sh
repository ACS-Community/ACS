#! /bin/bash

. acsstartupAcsInstance

rm -rf $ACSDATA/tmp/ACS_INSTANCE.*

if [ "`acsList`" != "" ]
then
    echo "ERROR - other instance of ACS seem to be running. Cannot run this test."
    exit 1
fi

#------------------------------------------------------------------------------
echo "--checkBasePort----------------------------------------------------------"
BAD_STUFF=abc
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=-7
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=-10
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=10
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=77
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=a1a
if ! checkBasePort $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

echo "-------------------------------------------------------------------------"

GOOD_STUFF=0
if ! checkBasePort $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

GOOD_STUFF=5
if ! checkBasePort $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

GOOD_STUFF=9
if ! checkBasePort $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

#------------------------------------------------------------------------------
echo "--getBasePort------------------------------------------------------------"
if [ "`getBasePort`" != "0" ]
then
    echo "Bad"
else
    echo "Good"
fi

if [ "`getBasePort`" != "0" ]
then
    echo "Bad"
else
    echo "Good"
fi

if [ "`getBasePort 1`" != "1" ]
then
    echo "Bad"
else
    echo "Good"
fi

if ! getBasePort 11
then
    echo "Good"
else
    echo "Bad"
fi

mkdir -p $ACSDATA/tmp/ACS_INSTANCE.0
if [ "`getBasePort`" == "0" ]
then
    echo "Bad"
else
    echo "Good"
fi

mkdir -p $ACSDATA/tmp/ACS_INSTANCE.0
if [ "`getBasePort 0`" == "0" ]
then
    echo "Good"
else
    echo "Bad"
fi

mkdir -p $ACSDATA/tmp/ACS_INSTANCE.0
if [ "`getBasePort`" == "1" ]
then
    echo "Good"
else
    echo "Bad"
fi

if [ "`getBasePort 3`" == "3" ]
then
    echo "Good"
else
    echo "Bad"
fi

mkdir -p $ACSDATA/tmp/ACS_INSTANCE.3
if [ "`getBasePort 3`" == "3" ]
then
    echo "Good"
else
    echo "Bad"
fi


mkdir -p $ACSDATA/tmp/ACS_INSTANCE.0
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.1
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.2
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.3
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.4
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.5
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.6
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.7
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.8
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.9
if [ "`getBasePort`" == "" ]
then
    echo "Good"
else
    echo "Bad"
fi

rm -rf $ACSDATA/tmp/ACS_INSTANCE.*

#------------------------------------------------------------------------------
echo "--createInstanceDirectory------------------------------------------------"

BAD_STUFF=10
if ! createInstanceDirectory $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

BAD_STUFF=-1
if ! createInstanceDirectory $BAD_STUFF
then
    echo "Good"
else
    echo "Bad: $BAD_STUFF!"
fi

GOOD_STUFF=0
if ! createInstanceDirectory $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

GOOD_STUFF=0
if ! createInstanceDirectory $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi


chmod 000 $ACSDATA/tmp/ACS_INSTANCE.0
GOOD_STUFF=0
if ! createInstanceDirectory $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

chmod 755 -R $ACSDATA/tmp/ACS_INSTANCE.*
rm -rf $ACSDATA/tmp/ACS_INSTANCE.*

#------------------------------------------------------------------------------
echo "--pickBasePort-----------------------------------------------------------"
if [ "`pickBasePort`" != "0" ] #creates 0
then
    echo "Bad"
else
    echo "Good"
fi

if [ "`pickBasePort`" != "1" ] #creates 1
then
    echo "Bad"
else
    echo "Good"
fi

if [ "`pickBasePort 1`" != "1" ] #reuses 1
then
    echo "Bad"
else
    echo "Good"
fi

if ! pickBasePort 11
then
    echo "Good"
else
    echo "Bad"
fi

mkdir -p $ACSDATA/tmp/ACS_INSTANCE.0
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.1
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.2
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.3
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.4
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.5
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.6
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.7
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.8
mkdir -p $ACSDATA/tmp/ACS_INSTANCE.9
if [ "`pickBasePort`" == "" ]
then
    echo "Good"
else
    echo "Bad"
fi

rm -rf $ACSDATA/tmp/ACS_INSTANCE.*
echo "-------------------------------------------------------------------------"

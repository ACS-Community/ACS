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

echo "-------------------------------------------------------------------------"

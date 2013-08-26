#! /bin/bash
if [ "$OSYSTEM" = "$CYGWIN_VER" ]; then
HOST=`hostname`
else
HOST=`hostname -s`
fi

. acsstartupAcsInstance

rm -rf $ACSDATA/tmp/$HOST/ACS_INSTANCE.*

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


chmod 000 $ACS_TMP/ACS_INSTANCE.0
GOOD_STUFF=0
if ! createInstanceDirectory $GOOD_STUFF
then
    echo "Bad: $GOOD_STUFF!"
else
    echo "Good"
fi

chmod 755 -R $ACS_TMP/ACS_INSTANCE.*
rm -rf $ACS_TMP/ACS_INSTANCE.*

echo "-------------------------------------------------------------------------"

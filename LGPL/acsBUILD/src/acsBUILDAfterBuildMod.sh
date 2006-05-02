#! /bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: acsBUILDAfterBuildMod.sh,v 1.126 2006/05/02 10:48:06 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# psivera  2002-12-21 The copy of .acs is done in the case of no INTROOT also
# gchiozzi 2002-04-05 Cannot use chmod -R 755. This would make all files executable. Use go-w.
# psivera  2002-03-14 created
#

if [ "X$INTROOT" != X  -a  "X$ACSROOT" != X  -a  "$INTROOT" = "$ACSROOT" ]
then
    echo "         \$INTROOT = \$ACSROOT = $INTROOT => the file are by default writable by the group."
    echo "         Now, we change the default permissions in $ACSROOT. "
    echo "         => $INTROOT will be writable by $USER only. "
    START_DIR=$PWD
    cd $ACSROOT
    if [ $? != 0 ]
    then
	echo "Can't cd to $ACSROOT!"
	exit 1
    else
	chmod -R go-w .
    fi
    cd $START_DIR
else
    echo "	acsBUILDAfterBuildMod.sh: no chmod to do"
fi

#
# Copies ACS version files from the CVS ACS root directory into ACSROOT 
#
if [ "X$INTROOT" != X ]
then
   INSTROOT=$INTROOT
else
   INSTROOT=$ACSROOT
fi

if [ -f "ACS_VERSION" ]
then
    echo "ACS_VERSION is:"
    cat ACS_VERSION
    cp ACS_VERSION $INSTROOT
else
    echo "WARNING: No ACS_VERSION available"
    echo "WARNING: No ACS_VERSION available" > $INSTROOT/ACS_VERSION
fi
if [ -f "ACS_PATCH_LEVEL" ]
then
    echo "ACS_PATCH_LEVEL is:"
    cat ACS_PATCH_LEVEL
    cp ACS_PATCH_LEVEL $INSTROOT
else
    echo "WARNING: No ACS_PATCH_LEVEL available"
    echo "WARNING: No ACS_PATCH_LEVEL available" > $INSTROOT/ACS_PATCH_LEVEL
fi
if [ -f "ACS_TAG" ]
then
    echo "ACS_TAG is:"
    cat ACS_TAG
    cp ACS_TAG $INSTROOT
else
    echo "WARNING: No ACS_TAG available"
    echo "WARNING: No ACS_TAG available" > $INSTROOT/ACS_TAG
fi
exit 0


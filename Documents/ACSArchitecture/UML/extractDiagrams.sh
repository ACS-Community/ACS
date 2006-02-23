#! /bin/bash
#*******************************************************************************
# E.S.O. - ACS project
#
# "@(#) $Id: extractDiagrams.sh,v 1.3 2004/06/02 16:06:50 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# almamgr 2003-12-30 created
#

#************************************************************************
#   NAME
# 
#   SYNOPSIS
# 
#   DESCRIPTION
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#

# Pass the destination directory as the first argument
if [ "X$1" == "X" ]
then 
    export DEST_DIR="."
else
    export DEST_DIR=$1
fi
echo Destination directory is: $DEST_DIR
python extractDiagrams.py -d $DEST_DIR `find . -name "dgm*.htm" -print`

#
# ___oOo___


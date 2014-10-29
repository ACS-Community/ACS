#! /bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: acsBUILDCheckModuleTree.sh,v 1.125 2003/03/03 13:46:29 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# eallaert 2014-10-22 Removed man directories (ICT-3851) and several others created by acsMakefile
# gchiozzi 2003-02-22 Added also man directories
# gchiozzi 2003-02-10 Adde also lib/python/site-packages
# gchiozzi 2003-01-24 Created
#
# This script check the health of a module tree and adds directories when needed.
# This should never happen, but currently we have problems with CVS extraction
# Once fixed, this script should never log and problem

checkDir()
{
  if [ ! -d "$@" ]
  then
     echo "   ------> Restoring $@"
     mkdir -p "$@"
  fi
}

echo "## Checking $1"
checkDir $1/doc/idl/
checkDir $1/doc/html/

#! /bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: acsBUILDCheckModuleTree.sh,v 1.125 2003/03/03 13:46:29 gchiozzi Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
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
     mkdir "$@"
  fi
}

echo "## Checking $1"
checkDir $1/bin
checkDir $1/object
checkDir $1/lib
checkDir $1/lib/python
checkDir $1/lib/python/site-packages
checkDir $1/include
checkDir $1/idl
checkDir $1/doc/
checkDir $1/doc/idl/
checkDir $1/doc/html/
checkDir $1/man
checkDir $1/man/man1
checkDir $1/man/man2
checkDir $1/man/man3
checkDir $1/man/man4
checkDir $1/man/man5
checkDir $1/man/man6
checkDir $1/man/man7
checkDir $1/man/manl


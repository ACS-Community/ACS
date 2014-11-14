#!/bin/ksh
#*******************************************************************************
# E.S.O. - VLT project
#
# buildKit
#
# "@(#) $Id: acsBUILDPrepareKit.sh,v 1.125 2012/11/05 09:37:29 eallaert Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2003-01-09 Removed some echo messages
# gchiozzi 2002-12-11 created from VLT buildKit

#*******************************************************************************
# ALMA - Atacama Large Millimeter Array
# Copyright (c) ESO - European Southern Observatory, 2014
# (in the framework of the ALMA collaboration).
# All rights reserved.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#*******************************************************************************

#************************************************************************
#   NAME
#
#   SYNOPSIS
#
#   DESCRIPTION
#   Jacket script to call the Makefile of a set of modules and to
#   clean, do and install them. Development areas are cleaned at the end.
#
#
#   FILES
#
#------------------------------------------------------------------------
#

TITLE="Build ACSROOT directory structure"
BUILD_NAME="ACSROOT"
. acsBUILD/src/standardPrologue

#
# Build ACSROOT
ERROR=true

. acsBUILD/src/acsBUILDBeforeInstall.sh

#*****************************************************
# PREPARATION 
#*****************************************************

#
#   To do "acs" I need "doc", to do "doc" I need "acs"!
#     Firts the egg or first the hen?
#
#   To make it simple, I do acs first, ignoring the errors produced by the
#   man pages that cannot yet been generated (doDocManPages), and then 
#   doc is done. When both are ready, I'll start the real installation
#   from acs, then doc, then all the others.
#
echo "$SEPARATOR"
echo "preparing installation . . ."

#
# use the acsMakefile in the distribution
#MAKE="make -k MAKEDIR=$INSTALL_ROOT/vlt/include INTLIST=$INSTALL_ROOT/vlt MAKE_VERBOSE=on"
#!## Please note that LOCALDIR is used by vltMakefile
#!#MAKE="make -k MAKEDIR=$INSTALL_ROOT/Kit/vlt/include LOCALDIR=$INSTALL_ROOT/Kit/vlt"
MAKE="make -k MAKEDIR=$INSTALL_ROOT/Kit/acs/include INTLIST=$INSTALL_ROOT/Kit/acs"

#
# give a warning to error file readers!
echo "$SEPARATOR"
echo "WARNING: preparing installation                                               "  
echo "   >>>> At this stage is not yet possible to completely build vlt and doc.    "  
echo "   >>>> Some error messages are normal. I leave this output in case something "  
echo "   >>>> else goes wrong. Only if the installation of all utilities goes bad,  "  
echo "   >>>> please send this file to ESO, otherwise ignore it.                    "  
echo ""  
echo " .\c"

CURR_DIR=`pwd`
echo now in `pwd`
cd $INSTALL_ROOT/Kit/acs/src
echo now in `pwd`

# I need some acsMakeXxxxx utilities executable and in the path
#!#chmod +x vltMakeCleanDB
chmod +x acsMakeCopySources
#!#chmod +x vltMakeExecutableDependencies
#!#chmod +x vltMakeIndexFilesDependencies
#!#chmod +x vltMakeInstallErrorFiles
#!#chmod +x vltMakeInstallAlarmFiles
chmod +x acsMakeInstallFiles
#!#chmod +x vltMakeInstallLogFiles
#!#chmod +x vltMakeInstallTableFiles
#!#chmod +x vltMakeLibraryDependencies
chmod +x acsMakeLogInstallation
#!#chmod +x vltMakePanelDependencies
#!#chmod +x vltMakeScriptDependencies
chmod +x acsMakeTclLib
#!#chmod +x vltMakeTclLibDependencies
chmod +x acsMakeTclScript
#!#chmod +x vltMakeTclScriptDependencies

PATH=$INSTALL_ROOT/Kit/acs/src:${PATH}
export PATH
hash -r

echo " .\c"; $MAKE all     
echo " .\c"; $MAKE install 
cd $CURR_DIR

#
# force a rehash (everytime PATH is changed, the command table is rebuild)
PATH=${PATH}
hash -r

#
# create doc
# 
CURR_DIR=`pwd`
echo " .\c"; cd $INSTALL_ROOT/Kit/doc/src
echo " .\c"; $MAKE all     
echo " .\c"; $MAKE install 
cd $CURR_DIR


echo "   >>>> "  
echo "   >>>> end of preparation.   "  
echo "   >>>>      !!!!! From now on no errors should be displayed!!!!! "  
echo "$SEPARATOR"

echo now in `pwd`
. acsBUILD/src/standardEpilogue

#
#___oOo___

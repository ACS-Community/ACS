#!/bin/bash
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2009 
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
#
# "@(#) $Id: runMonitorArchiverUnitTest.sh,v 1.2 2011/02/03 17:07:22 tstaig Exp $"
#
# Makefile of ........
#
# who       when      what
# --------  --------  ----------------------------------------------
# pburgos  2009-04-05  created
#

export PYTHONPATH=../lib/python/site-packages:$PYTHONPATH

declare TEST_SUITE=1
declare TEST_LOG=/dev/stdout

if test $# -ge 1; then
  TEST_SUITE=$1
  if test $# -eq 2; then
    TEST_LOG=$2
  fi
fi


declare TEST_TMP=tmp
if [ -d $TEST_TMP ]; then
touch tmp/MonitorArchiverUnitTest.log
else
#echo "Error: '$check' does not exist!!"
#echo "EXITING"
#exit 1
mkdir $TEST_TMP
touch $TEST_TMP/MonitorArchiverUnitTest.log
fi

acsStartJava -Darchive.configFile="archiveConfig.properties.blobberTest" -DACS.log.minlevel.namedloggers="hibernateSQL=2,2:hibernate=2,2" org.testng.TestNG testng.xml &> $TEST_LOG
declare TEST_RESULT="`grep -R FAIL ../test/test-output/testng-results.xml`"

#RESULT=$?
#if [ "$RESULT" = "0" ]; then
#    printf "OK\n"
#else
#    printf "ERROR\n"
#fi

if [ "$TEST_RESULT" = "" ]; then
    printf "OK\n"
else
    printf "ERROR\n"
fi


# __pBa__

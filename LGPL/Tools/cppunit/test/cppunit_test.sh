#! /bin/bash
#*************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration),
# All rights reserved
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307  USA
#

#
# Runs the standard test coming with the tool.
# To do this goes in the directory where the iriginal test is
#
START_DIR=$PWD
CPPUNIT_VER=1.12.1

if [ -f /etc/redhat-release ]
    then
    RELEASE=`cat /etc/redhat-release`
    SLRELEASE="Scientific Linux SL Release 3.0.3 (SL)"
    if [ X"$RELEASE" = X"$SLRELEASE" ]
	then
	echo $RELEASE
	echo "Abort test: it would hang on $RELEASE !"
	exit 1
    fi
fi

cd ../src
if [ ! -d cppunit-$CPPUNIT_VER ]
then
    make all >../test/makeCppunitForTest.log 2>&1
fi

cd cppunit-$CPPUNIT_VER
make check 2>&1 | tee ../../test/cppunit_test.log

#
# Goes back to start directory
#
cd $START_DIR

#__oOo__

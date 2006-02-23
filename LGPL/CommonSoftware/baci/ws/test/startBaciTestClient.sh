#!/bin/ksh
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) European Southern Observatory, 2002
# Copyright by ESO (in the framework of the ALMA collaboration)
# and Cosylab 2002, All rights reserved
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

. acsstartupAcsPorts
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

if [ "$WIND_BASE" != "" ] 
then
    IOR_FILE=$VLTDATA/ENVIRONMENTS/$lcuTat/iors.dat
    max_iter=500
else 
    IOR_FILE=iors.dat
    max_iter=50
fi

i=0
 
while [ ! -f $IOR_FILE -a $i -lt $max_iter ]
  do
  sleep 2
  i=`expr $i + 1`
done

if [ $i -lt $max_iter ]
    then
    if [ "$WIND_BASE" != "" ]
	then
	cp -f $IOR_FILE .
    fi
    $1 $2
    i=0
    while [ -f $IOR_FILE -a $i -lt $max_iter ]
      do
      sleep 2
      i=`expr $i + 1`
    done
    sleep 2
else
   echo 'iors.dat file not created !'
fi
#fi

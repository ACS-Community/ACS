#!/bin/bash

#*******************************************************************************
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

echo "Running $*"
if [ "$WIND_BASE" != "" ] 
then
  i=0
  max_iter=100
  while [ ! -f $VLTDATA/ENVIRONMENTS/$lcuTat/$2.ior -a $i -lt $max_iter ]
  do
    sleep 2
    i=`expr $i + 1`
  done
  if [ $i -lt $max_iter ]
  then
    cp -f $VLTDATA/ENVIRONMENTS/$lcuTat/$2.ior .
    $*
    sleep 5
  else
    echo '$VLTDATA/ENVIRONMENTS/$lcuTat/$2.ior file not created !'
  fi
else
  $*
fi

echo "All done!"
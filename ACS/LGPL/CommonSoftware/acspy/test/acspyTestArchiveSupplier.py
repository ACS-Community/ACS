#!/usr/bin/env python
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
# (c) Associated Universities Inc., 2002 
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
# @(#) $Id: acspyTestArchiveSupplier.py,v 1.1 2006/11/24 07:55:57 cparedes Exp $
###############################################################################
'''
Tests archive supplier
'''
from Acspy.Nc.ArchiveSupplier import ArchiveSupplier
from sys  import argv
from time import sleep
###############################################################################


mySupplier = ArchiveSupplier()

mySupplier.publishEvent(3.14)
mySupplier.publishEvent(3.1)
mySupplier.publishEvent(1)
mySupplier.publishEvent(2)
mySupplier.publishEvent(3)
mySupplier.publishEvent(4)
mySupplier.publishEvent("a string")

doubleSeq = (3.14 , 2.1)
mySupplier.publishEvent(doubleSeq)

floatSeq = (3.1 , 8.1)
mySupplier.publishEvent(floatSeq)

longSeq = (1 , 7)
mySupplier.publishEvent(longSeq)

stringSeq = ("a" , "string")
mySupplier.publishEvent(stringSeq)

#shutdown everything cleanly
mySupplier.disconnect()


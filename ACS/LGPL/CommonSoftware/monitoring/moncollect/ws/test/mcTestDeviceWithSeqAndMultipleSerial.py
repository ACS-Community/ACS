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

from Acspy.Clients.SimpleClient import PySimpleClient
from sys                        import argv
from sys                        import exit
from sys                        import stdout      
from TMCDB                      import MonitorCollector
from TMCDB                      import propertySerailNumber
import MonitorErrImpl
import MonitorErr

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()

mc = simpleClient.getComponent(argv[1])

try:
    print "DeviceWithSeqAndMultipleSerial"
    psns =[propertySerailNumber('ROdoubleSeqPM', ['12124']),propertySerailNumber('RWdoubleSeqPM', ['3432535'])]    
    mc.registerMonitoredDeviceWithMultipleSerial('AMSSEQ1', psns)    
    mc.startMonitoring('AMSSEQ1')    
except MonitorErr.RegisteringDeviceProblemEx, _ex:
    ex = MonitorErrImpl.RegisteringDeviceProblemExImpl(exception=_ex)
    ex.Print();   

mc.stopMonitoring('AMSSEQ1')
mc.deregisterMonitoredDevice('AMSSEQ1')


#cleanly disconnect
simpleClient.releaseComponent(argv[1])
simpleClient.disconnect()
stdout.flush()



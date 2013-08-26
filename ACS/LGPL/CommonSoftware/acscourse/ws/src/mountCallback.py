#!/usr/bin/env python
from Acspy.Clients.SimpleClient import PySimpleClient     # Import the acspy.PySimpleClient class
from Acspy.Common.Callbacks     import CBdouble
import ACS                      # Import the Python CORBA stubs for BACI
from time                       import sleep

#------------------------------------------------------------------------------
simpleClient = PySimpleClient()

#Get the MOUNT1 Mount device
mount = simpleClient.getComponent("MOUNT2_LOOP")

#Get the actAz property
actAzProperty = mount._get_actAz()

#Create a callback monitor for the actAz Property
cbMon = CBdouble(name="actAz", archive=1)  
actMon = actAzProperty.create_monitor(cbMon._this(), ACS.CBDescIn(0L, 0L, 0L))

#Working method gets invoked once per second
actMon.set_timer_trigger(10000000)  

#Destroy the monitor after ten seconds
sleep(10)  
actMon.destroy()

print "The monitored values are: ", cbMon.values

# Release the component
simpleClient.releaseComponent("MOUNT2_LOOP")
simpleClient.disconnect()

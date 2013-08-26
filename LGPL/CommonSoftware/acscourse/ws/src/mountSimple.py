#!/usr/bin/env python
from Acspy.Clients.SimpleClient import PySimpleClient

remoteComponent="MOUNT2_LOOP"

#Make an instance of the PySimpleClient
simpleClient = PySimpleClient()  

#Get the MOUNT1 Mount device
mount  = simpleClient.getComponent(remoteComponent)  

#Get the actAz property
actAzProperty = mount._get_actAz()

#Get the current value of the property
(azm, compl) = actAzProperty.get_sync()  
print "MOUNT actual azimuth: ", azm

#Cleanly disconnect
simpleClient.releaseComponent(remoteComponent)
simpleClient.disconnect()

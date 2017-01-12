#!/usr/bin/env python

from Acspy.Clients.SimpleClient import PySimpleClient
client = PySimpleClient()
f1 = client.getDynamicComponent("FRIDGE001", 'IDL:alma/FRIDGE/FridgeControl:1.0', 'Acssim.Servants.Simulator', 'pyContainer')
f2 = client.getDynamicComponent("FRIDGE002", 'IDL:alma/FRIDGE/FridgeControl:1.0', 'Acssim.Servants.Simulator', 'pyContainer')
h3 = client.getDynamicComponent("HELLOWORLD001", 'IDL:alma/acsexmplHelloWorld/HelloWorld:1.0', 'Acssim.Servants.Simulator', 'pyContainer')
f1.doorStatus
f2.doorStatus
h3.displayMessage()

#!/usr/bin/env python

from Acspy.Clients.SimpleClient import PySimpleClient
client = PySimpleClient()
f1 = client.getDynamicComponent("FRIDGE001", 'IDL:alma/FRIDGE/FridgeControl:1.0', 'Acssim.Servants.Simulator', 'pyContainer')
f2 = client.getDynamicComponent("FRIDGE002", 'IDL:alma/FRIDGE/FridgeControl:1.0', 'Acssim.Servants.Simulator', 'pyContainer')

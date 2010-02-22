#!/usr/bin/env python

from Acspy.Clients.SimpleClient import PySimpleClient

simpleClient = PySimpleClient.getInstance()

simple = simpleClient.getComponent("DDS_SIMPLE_EXAMPLE")

for i in range(5):#1000):
	simple.sendMessage()
	print i

simpleClient.releaseComponent("DDS_SIMPLE_EXAMPLE")
simpleClient.disconnect()


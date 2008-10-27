#!/usr/bin/env python

from Acspy.Clients.SimpleClient import PySimpleClient

simpleClient = PySimpleClient.getInstance()

ncBench = simpleClient.getComponent("NC_SUPPLIER")

ncBench.runTest(1,1)

simpleClient.releaseComponent("NC_SUPPLIER")
simpleClient.disconnect()


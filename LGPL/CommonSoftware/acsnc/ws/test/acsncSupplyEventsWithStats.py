#!/usr/bin/env python

from Acspy.Clients.SimpleClient import PySimpleClient
import sys

client   = PySimpleClient()
supplier = client.getComponent('NC_Reliability')

supplier.sendEvents(int(sys.argv[1]))

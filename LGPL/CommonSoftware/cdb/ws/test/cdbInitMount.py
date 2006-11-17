#! /usr/bin/env python

from sys import argv
from time import sleep
from Acspy.Clients.SimpleClient import PySimpleClient
import ACS
import time, os

import acsexmplMount_idl

simpleClient = PySimpleClient()
cdb_mount = simpleClient.getComponent("MOUNT99") 

sleep(5)

simpleClient.releaseComponent("MOUNT99")



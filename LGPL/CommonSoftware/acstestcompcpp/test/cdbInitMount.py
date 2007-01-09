#! /usr/bin/env python

from sys import argv
from time import sleep
from Acspy.Clients.SimpleClient import PySimpleClient
import ACS
import time, os

simpleClient = PySimpleClient()
cdb_mount = simpleClient.getComponent("COMP99") 

sleep(10)

simpleClient.releaseComponent("COMP99")



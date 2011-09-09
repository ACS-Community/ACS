from Acspy.Clients.SimpleClient import PySimpleClient
from time import sleep

c = PySimpleClient.getInstance()
newr = c.getComponent("NEWCONFIG_RECEIVER")
oldr = c.getComponent("OLDCONFIG_RECEIVER")

newr.openReceiver()
oldr.openReceiver()
sleep(10);

c.releaseComponent("NEWCONFIG_RECEIVER")
c.releaseComponent("OLDCONFIG_RECEIVER")

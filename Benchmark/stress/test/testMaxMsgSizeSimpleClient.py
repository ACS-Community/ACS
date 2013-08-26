import sys
import time
from Acspy.Clients.SimpleClient import PySimpleClient

remoteComponentName="TESTMAXMSGSIZE"

# Loop and call the sendSequence method on the component
# making the size of the sequence sent larger w/ each iteration
# at some point, the ORB should fail because omniorb's max 
# msg size will have been exceeded.

# Make an instance of the PySimpleClient
simpleClient = PySimpleClient()  

while 1:
	try:
		characters = "1"

		# Get the test component by name
		remoteComponent = simpleClient.getComponent(remoteComponentName)  

		for i in range(0,22):
			print "Length being sent: ", len(characters)
			remoteComponent.sendSequence(characters)	
			characters = characters + characters

		simpleClient.releaseComponent(remoteComponentName)
	except:
		print "FAILED to send: ", len(characters)
		time.sleep(1)

simpleClient.disconnect()

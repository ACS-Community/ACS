###################################################################
#
#  MACI TEST SCRIPT
#
#  "@(#) $Id: maciTest.mts,v 1.84 2007/09/28 13:22:03 bjeram Exp $"
#
#  Following is a sequence of commands that the maciTestClient is
#  to execute. These commands test the Manager and Containers by
#  issuing requests to the manager.
#
#  For a list of available maciTestClient, run maciTestClient
#  without any command-line options.
#
#  To execute this "script" file, type:
#
#     maciTestClient @maciTest.mts
#
#  Some of the tests in this file fail. Those are outcommented and
#  marked with @@.
#
###################################################################

# Initialize the MACI Simple Client (connect to the Manager, ...)
init

# Do client side testing (resolveManagerReference, ...)
testClient:dummy

# Admininstrator test - to test notifications create it first
administrator:testAdmin

# Get a Component, which we know isn't activated yet, and don't attempt to
# initialize it.
getComponent:SimpleClient:0:MACI01

# Attemp to activate non-existing DO
getComponent:SimpleClient:1:dummyDO

# Attemp to activate fake DO (no DO configuration (characteristics, ...))
getComponent:SimpleClient:1:MACI_FAKE

# Get a Component, which we know isn't activated yet, but do initialize it.
getComponent:SimpleClient:1:MACI04

# Do server side testing
testServer:MACI04

# MACI07 is a constructable component
#getComponent:SimpleClient:1:MACI07


# Activate Components on different containers
getComponent:SimpleClient:1:MACI01
getComponent:SimpleClient:1:MACI02
getComponent:SimpleClient:1:MACI03


sleep:1000

# hierarchical component
getComponent:SimpleClient:1:MACI_HIER


#releaseComponent:SimpleClient:MACI07

releaseComponent:SimpleClient:MACI01
releaseComponent:SimpleClient:MACI04

getComponent:SimpleClient:1:MACI01
getComponent:SimpleClient:1:MACI04
getComponent:SimpleClient:1:MACI01

releaseComponent:SimpleClient:MACI01

getComponent:SimpleClient:1:MACI01
getComponent:SimpleClient:1:MACI04

releaseComponent:SimpleClient:MACI01
releaseComponent:SimpleClient:MACI04



getComponent:SimpleClient:1:MACI02
releaseComponent:SimpleClient:MACI02
getComponent:SimpleClient:1:MACI02
getComponent:SimpleClient:1:MACI05
releaseComponent:SimpleClient:MACI02
releaseComponent:SimpleClient:MACI05
getComponent:SimpleClient:1:MACI02
getComponent:SimpleClient:1:MACI05
releaseComponent:SimpleClient:1:MACI02
releaseComponent:SimpleClient:1:MACI05
getComponent:SimpleClient:1:MACI02
releaseComponent:SimpleClient:1:MACI02

# test for deprecated get_object
get_object:SimpleClient:1:MACI01

# unknown logout test
logout:dummy

# ping test
client:defunctionalClient:1
sleep:10000
# already logged out by manager
logout:defunctionalClient

client:transientPingClient:2
sleep:10000
#sleep:130000
## already logged out by manager
logout:transientPingClient

# test container shutdown, unavailable Components, container logout notification 
shutdown:testAdmin:Container01:512

# shutdown unknown container
shutdown:testAdmin:dummyContainer:512

sleep:3000

# info test
getContainerInfo:testAdmin:*
getClientInfo:testAdmin:*
getComponentInfo:testAdmin:*:*:0
getComponentInfo:testAdmin:*:*:1

# try to activate Component when not container is logged in
# already unavailable Component
getComponent:SimpleClient:1:MACI01
# on non-existing container 
getComponent:SimpleClient:1:MACI10

# Shutdown all containers
shutdown:testAdmin:*:512

sleep:8000

logout:testAdmin

reset


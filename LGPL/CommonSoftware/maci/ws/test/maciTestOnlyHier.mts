###################################################################
#
#  MACI TEST SCRIPT
#
#  "@(#) $Id: maciTestOnlyHier.mts,v 1.80 2003/09/17 19:52:30 msekoran Exp $"
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

# hierarchical Component
getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER

sleep:300

releaseComponent:SimpleClient:MACI_HIER

sleep:100

getComponent:SimpleClient:1:MACI_HIER

sleep:200

releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER

getComponent:SimpleClient:1:MACI_HIER
releaseComponent:SimpleClient:MACI_HIER


sleep:3000

reset

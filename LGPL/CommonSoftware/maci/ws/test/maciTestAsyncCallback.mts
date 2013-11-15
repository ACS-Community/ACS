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

# slow Component
getComponent:SimpleClient:1:MACI_SLOW
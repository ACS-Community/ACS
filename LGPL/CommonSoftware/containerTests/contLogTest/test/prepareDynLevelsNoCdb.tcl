#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareDynLevelsNoCdb.tcl,v 1.1 2007/12/14 16:58:50 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-12-13  original version (extracted from prepareDynLevels.tcl)

# Set relevant environment variables
set env(ACS_LOG_CENTRAL) 3
set env(ACS_LOG_STDOUT)  4

# Clear out the CDB-cache, so the next time the container starts the proper
# xml file gets read in again from $ACS_CDB
exec cdbjDALClearCache
sleep 2

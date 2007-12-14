#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareHardcodedLevelsNoCdb.tcl,v 1.1 2007/12/14 16:58:50 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-12-13  original version (split off from prepareHardCodedlevels.tcl)

# Unset relevant environment variables
unset -nocomplain -- env(ACS_LOG_CENTRAL)
unset -nocomplain -- env(ACS_LOG_STDOUT)

# Clear out the CDB-cache, so the next time the container starts the proper
# xml file gets read in again from $ACS_CDB
exec cdbjDALClearCache
sleep 2


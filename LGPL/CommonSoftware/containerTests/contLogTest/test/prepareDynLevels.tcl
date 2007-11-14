#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareDynLevels.tcl,v 1.2 2007/11/14 09:27:45 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# Set relevant environment variables
set env(ACS_LOG_CENTRAL) 3
# Next is temporary, for C++ !!!!!!!!!!!!!!!!!!!
#####set env(ACS_LOG_CENTRALIZE_LOGGER) 3
set env(ACS_LOG_STDOUT)  4

# Clear out the CDB-cache, so the next time the container starts the proper
# xml file gets read in again from $ACS_CDB
exec cdbjDALClearCache
sleep 2

# Use the Container-xml file with entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer frodoContainerN bilboContainer bilboContainerN} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withHighLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml]
}

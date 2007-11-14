#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareHardcodedLevels.tcl,v 1.2 2007/11/14 09:27:45 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# Unset relevant environment variables
unset -nocomplain -- env(ACS_LOG_CENTRAL)
unset -nocomplain -- env(ACS_LOG_STDOUT)

# Clear out the CDB-cache, so the next time the container starts the proper
# xml file gets read in again from $ACS_CDB
exec cdbjDALClearCache
sleep 2

# Use the Container-xml file without entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer frodoContainerN bilboContainer bilboContainerN} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withoutLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml] 
}

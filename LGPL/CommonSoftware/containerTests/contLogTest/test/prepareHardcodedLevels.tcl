#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareHardcodedLevels.tcl,v 1.1.1.1 2007/11/13 14:25:38 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# Unset relevant environment variables
unset -nocomplain -- env(ACS_LOG_CENTRAL)
unset -nocomplain -- env(ACS_LOG_STDOUT)

# Use the Container-xml file without entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer frodoContainerN bilboContainer bilboContainerN} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withoutLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml] 
}

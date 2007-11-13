#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareCdbLevels.tcl,v 1.1.1.1 2007/11/13 14:25:38 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# Unset relevant environment variables
unset -nocomplain -- env(ACS_LOG_CENTRAL)
unset -nocomplain -- env(ACS_LOG_STDOUT)

# Use the Container-xml file with entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer bilboContainer} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml]
}

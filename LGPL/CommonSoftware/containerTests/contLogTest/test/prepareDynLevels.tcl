#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareDynLevels.tcl,v 1.1.1.1 2007/11/13 14:25:38 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# Set relevant environment variables
set env(ACS_LOG_CENTRAL) 3
# Next is temporary, for C++ !!!!!!!!!!!!!!!!!!!
#####set env(ACS_LOG_CENTRALIZE_LOGGER) 3
set env(ACS_LOG_STDOUT)  4

# Use the Container-xml file with entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer bilboContainer} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withHighLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml]
}

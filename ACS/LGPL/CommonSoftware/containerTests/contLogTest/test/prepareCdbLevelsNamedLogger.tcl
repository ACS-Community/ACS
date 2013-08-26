#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareCdbLevelsNamedLogger.tcl,v 1.1 2007/12/14 16:58:50 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version

# First get the environment var settings etc. (identical to Hardcoded case)
source prepareHardcodedLevelsNoCdb.tcl

# Use the Container-xml file with entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer bilboContainer aragornContainer} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_namedLoggerWithLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml]
}

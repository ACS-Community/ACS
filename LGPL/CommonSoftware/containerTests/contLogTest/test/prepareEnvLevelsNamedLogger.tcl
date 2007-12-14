#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareEnvLevelsNamedLogger.tcl,v 1.1 2007/12/14 16:58:50 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version
# agrimstr  2007-11-14  Added tests for Python
# eallaert  2007-12-13  source CDB independent part from other script

source prepareEnvLevelsNoCdb.tcl

# Use the Container-xml file with entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer bilboContainer aragornContainer} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_namedLoggerWithHighLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml]
}

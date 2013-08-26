#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: prepareHardcodedLevels.tcl,v 1.4 2007/12/14 16:58:50 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-11-02  original version
# agrimstr  2007-11-14  Added tests for Python
# eallaert  2007-12-13  put CDB independent part into prepareHardcodedLevelsNoCdb.tcl

# First get the environment var settings etc.
source prepareHardcodedLevelsNoCdb.tcl

# Use the Container-xml file without entries for minLogLevel & minLogLevelLocal
foreach cont {frodoContainer bilboContainer aragornContainer} {
    file copy -force -- \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}_withoutLevels.xml] \
	[file join $env(ACS_CDB) CDB MACI Containers $cont ${cont}.xml] 
}

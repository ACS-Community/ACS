#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: setupTatEnvironment.tcl,v 1.1 2008/05/23 12:53:45 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2008-01-18  initial version

set ACS_TMP [file join [pwd] tmp]
set env(ACS_TMP) $ACS_TMP
set env(ACS_LOG_CENTRAL) 3
set env(ACS_LOG_STDOUT) 4
set env(MACI_RECOVERY_FILE_NAME) [file join $ACS_TMP maciRecovery]
set env(ACS_BACI_RECOVERY_FILE) [file join $ACS_TMP baciRecovery]
set env(ACS_LOG_FILE) [file join $ACS_TMP log_cache.dat] 
set env(ACS_CDB) [pwd] 
set env(JAVA_OPTIONS) -Djacorb.log.loggerFactory=alma.acs.logging.adapters.JacORBLoggerFactory

##set T_PWD [pwd]

#
# ___oOo___


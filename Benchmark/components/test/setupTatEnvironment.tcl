#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: setupTatEnvironment.tcl,v 1.1 2007/07/20 07:46:18 eallaert Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# 

set ACS_TMP [file join [pwd] tmp]
set env(ACS_TMP) $ACS_TMP
set env(ACS_LOG_STDOUT) 4
set env(MACI_RECOVERY_FILE_NAME) [file join $ACS_TMP maciRecovery]
set env(ACS_BACI_RECOVERY_FILE) [file join $ACS_TMP baciRecovery]
set env(ACS_LOG_FILE) [file join $ACS_TMP log_cache.dat] 

#
# ___oOo___

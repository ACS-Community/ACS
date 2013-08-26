global PID

set ACS_TMP $env(PWD)/tmp
set env(ACS_TMP) $ACS_TMP
set env(ACS_LOG_STDOUT) 2
set env(ACS_CDB) $env(PWD)
set env(MODPATH) 1
set env(ACS_STARTUP_TIMEOUT_MULTIPLIER) 10

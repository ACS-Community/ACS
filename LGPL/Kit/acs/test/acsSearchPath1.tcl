# -*- tcl -*-
source acsRestoreEnv.tcl

set ::env(INTROOT) /introot/myIntroot
set ::env(INTLIST) /introot/herIntroot:/introot/hisIntroot
set ::env(ACSROOT) /alma/ACS15/ACSSW
catch {unset ::env(MODPATH)}
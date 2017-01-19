# -*- tcl -*-
source acsRestoreEnv.tcl

file delete -force ../idl
file mkdir ../idl
file link -symbolic ../idl/mySymLink ../test/TestList.lite

#*******************************************************************************
#  ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: acssampTest.tcl,v 1.8 2005/05/10 09:08:23 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# oat      2003-08-13 copied from acsexmpl
# gchiozzi 2002-05-13 Replaced maciTest with acsexmplTest in help line
# bgustafs 2002-02-26  created
#


global argc argv

if { $argc != 5 } {
    puts "usage: acssampTest <lcu> <process name> <argument1> <argument2> <argument3>"
    exit 1
}
set LCU  [lindex $argv 0]
set Proc [lindex $argv 1]
set Arg  [lindex $argv 2]
set Arg1 [lindex $argv 3]
set Arg2 [lindex $argv 4]

# timeout was 220
if {[info exists env(LCU_WAIT)]} {
   set LCU_WAIT $env(LCU_WAIT)
} else {
   set LCU_WAIT 360
}

if {[info exists env(MANAGER_REFERENCE)]} {
    set MREF $env(MANAGER_REFERENCE)
} else {
    set MREF "corbaloc::$env(HOST):3000/Manager"
}

if {[info exists env(DAL_REFERENCE)]} {
    set DREF $env(DAL_REFERENCE)
} else {
    set DREF "corbaloc::$env(HOST):5000/CDB"
}

set ReturnCode 0
set timeout $LCU_WAIT

#LCU connect

spawn rlogin $LCU
expect -gl "*->"

# Set configuration
send "putenv \"MANAGER_REFERENCE=$MREF\"\r"
expect -gl "*->"
send "putenv \"DAL_REFERENCE=$DREF\"\r"
expect -gl "*->"

send "sp $Proc,\" $Arg $Arg1 $Arg2\"\r"
expect -gl "*->"

expect {
         "*Container stopped." { puts ""}
         timeout { puts "timeout ($LCU_WAIT) for $Proc on $LCU";
                   set ReturnCode 1}
     }


close
exit $ReturnCode

#
# ___oOo___

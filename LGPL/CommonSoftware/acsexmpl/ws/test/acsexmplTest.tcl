#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: acsexmplTest.tcl,v 1.84 2004/10/07 14:52:27 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi 2002-05-13 Replaced maciTest with acsexmplTest in help line
# bgustafs 2002-02-26  created
#


global argc argv

if { $argc != 3 } {
    puts "usage: acsexmplTest <lcu> <process name> <argument>"
    exit 1
}
set LCU  [lindex $argv 0]
set Proc [lindex $argv 1]
set Arg  [lindex $argv 2]

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
    set DREF "corbaloc::$env(HOST):3012/CDB"
}

set ReturnCode 0
set timeout $LCU_WAIT

#LCU connect

spawn rlogin $LCU
expect -gl "*->"

# send "< reloadScript \r"
# expect -gl "*->"
# after 5000

send "putenv \"MANAGER_REFERENCE=$MREF\"\r"
expect -gl "*->"
send "putenv \"DAL_REFERENCE=$DREF\"\r"
expect -gl "*->"

send "sp $Proc,\" $Arg\"\r"
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

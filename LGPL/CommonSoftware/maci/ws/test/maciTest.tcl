#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: maciTest.tcl,v 1.83 2005/02/18 15:12:21 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# bgustafs 2002-02-26  created
#


global argc argv

if { $argc < 3 } {
    puts "usage: maciTest <lcu> <process name> <arguments>"
    exit 1
}
set LCU  [lindex $argv 0]
set Proc [lindex $argv 1]
set Arg1  [lindex $argv 2]
set Arg2 [lindex $argv 3]
set Arg3 [lindex $argv 4]


if {[info exists env(LCU_WAIT)]} {
   set LCU_WAIT $env(LCU_WAIT)
} else {
   set LCU_WAIT 500
}


set ReturnCode 0
set timeout $LCU_WAIT

#LCU connect

spawn rlogin $LCU
expect -gl "*->"

#send "< reloadScript \r"
#expect -gl "*->"
#after 50000

if { $argc >= 3 } {
send "sp $Proc,\" $Arg1 $Arg2 $Arg3\"\r"
}

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

#*******************************************************************************
#  ALMA - Atacama Large Millimiter Array
#  (c) European Southern Observatory, 2002
#  Copyright by ESO (in the framework of the ALMA collaboration),
#  All rights reserved
#
#  This library is free software; you can redistribute it and/or
#  modify it under the terms of the GNU Lesser General Public
#  License as published by the Free Software Foundation; either
#  version 2.1 of the License, or (at your option) any later version.
#
#  This library is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public
#  License along with this library; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
#  MA 02111-1307  USA
#
# "@(#) $Id: acscomponentTestTcl.tcl,v 1.4 2004/10/27 12:19:18 bjeram Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# oat      2003-08-13 copied from acsexmpl
# gchiozzi 2002-05-13 Replaced maciTest with acsexmplTest in help line
# bgustafs 2002-02-26  created
#


global argc argv

if { $argc != 2 } {
    puts "usage: acssampTest <lcu> <process name>"
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
    set DREF "corbaloc::$env(HOST):5000/CDB"
}

set ReturnCode 0
set timeout $LCU_WAIT

#LCU connect

spawn rlogin $LCU
expect -gl "*->"

#send "< reloadScript \r"
#expect -gl "*->"
#after 5000

# Set configuration
send "putenv \"MANAGER_REFERENCE=$MREF\"\r"
expect -gl "*->"
send "putenv \"DAL_REFERENCE=$DREF\"\r"
expect -gl "*->"

send "sp $Proc \r"
expect -gl "*->"

expect {
         "*Activate stopped." { puts ""}
         timeout { puts "timeout ($LCU_WAIT) for $Proc on $LCU";
                   set ReturnCode 1}
     }


close
exit $ReturnCode

#
# ___oOo___

#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatEnvStatus.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  28/12/95  created
# psivera  2004-02-13 fixed tcl procheck warnings
#

#******************************************************************************
#   NAME
#  
#   tatEnvStatus - list status of 'tat' environments
# 
#   SYNOPSIS
#
#   tatEnvStatus
#
#   DESCRIPTION
#
#   Displays allocated WS/LCU environments whose .testSession file  
#   does not exist any more or whose .testSession does not match env. id.
#
#   WS/RWS environments checked are the local one. LCU environemnts
#   checked are those available on the network.
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   EXAMPLES
#
#   SEE ALSO
#   
#   tat
#
#   BUGS     
#
#------------------------------------------------------------------------
#

global env

if {[catch {set HOST $env(HOST)}]} {set HOST undefined}
if {[catch {set VLTDATA $env(VLTDATA)}]} {set VLTDATA undefined}
if {[catch {set LCUROOT $env(LCUROOT)}]} {set LCUROOT undefined}

proc tatCheckEnvLocks { envList envType } {

global LCUROOT 

    while {![lempty $envList]} {
	set envId [lvarpop envList]
	if { $envType == "WS" } {
	    set lockFile /tmp/$envId.lock
	} else {
	    set envId [file tail $envId]
	    set lockFile $LCUROOT/LOCK.$envId
	}
	if { [file exists $lockFile] } {
	    set host_sessionFile [split [exec cat $lockFile]]
	    set host [lindex $host_sessionFile 0]
	    set sessionFile [lindex $host_sessionFile 1]
	    if {[ catch { exec rcp $host:$sessionFile /tmp/. } ]} {
		puts "$lockFile: $host:$sessionFile not found"
	    } else {
		# if not found => out is empty
		set out [vccRemExec $host "grep $envId $sessionFile"]
		if { [ llength $out ] == 0 } {
		    puts "$host:$sessionFile : $lcu not found"
		}
	    }
	}
    }
}

set trace 0

foreach arg $argv {
    switch -regexp -- $arg {
	        -t          { set trace 1 }
		default	    { puts "usage: tatEnvStatus"; 
	                      exit 1;
	                    }
	    }
    }

if { $trace == 1 } {
    cmdtrace on
}



#
# WS environments
#
set envList {}
set pattern w$HOST
set p1 {[0-9][0-9]}
set pattern $pattern$p1

# use the vcc database to query 'tat' environments
if {[ catch vccInit err  ]} {
    error "tatEnvStatus: $err"
}
if {[catch { set envList [vccInfo GetEnvs -attic $pattern] } out ]} {
    error "tatEnvStatus: $out"
}

tatCheckEnvLocks $envList WS

#
# LCU environments
#

catch { set envList [glob $LCUROOT/l*] }
tatCheckEnvLocks $envList LCU

#
# ___oOo___

#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatGetRTAPEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/05/96  created
# psivera  2004-02-13 fixed tcl procheck warnings
#

#************************************************************************
#   NAME
#
#   tatGetRTAPEnv -
# 
#   SYNOPSIS
#
#   tatGetRTAPEnv <sessionFile>
# 
#   DESCRIPTION
#
#   Allocates a RTAP environment name from the predefined tat RTAP
#   environment names pool.
#
#   The file name <sessionFile> is written in the lock file of the
#   allocated environment.
#
#   FILES
#
#   $VLTDATA/ENVIRONMENTS/w<host><number>*
#   /tmp/w<host><number>.lock
#
#   ENVIRONMENT
#
#   RETURN VALUES
# 
#   0 OK
#   1 FAILURE
#
#   CAUTIONS
#
#   EXAMPLES
#
#   SEE ALSO
#
#   tatFreeLocalRTAPEnv
#
#   BUGS     
#
#------------------------------------------------------------------------
#

proc tatGetRTAPEnv { envName sessionFile HOST VLTDATA } {

global env

set envList {}
# to cope with VLT/NTT LAN specification host naming convention
# _and_ with Garching ad-hoc convention:
# if the host name starts with 'w', we assume LAN specification convention
# otherwise we assume ad-hoc convention
set fhc [string index $HOST 0]
if { $fhc == "w" } {
    # take the hostname first 6 caracters
    set pattern [string range $HOST 0 5]
} else {
    # add 'w' before the hostname first 5 characters
    set pattern w[string range $HOST 0 4] 
}
set p1 {[0-9]}
set pattern $pattern$p1

# use the vcc database to query 'tat' environments
if {[ catch { vccInit} err ]} {
    error "1 error: tatGetLocalRTAPEnv: $err"
}
if {[catch { set envList [vccInfo GetEnvs -attic $pattern] } out ]} {
    error "2 error: tatGetLocalRTAPEnv: $out"
}

set envFound 0
while {![lempty $envList]} {
    set arg [lvarpop envList]
    set lockFile /tmp/$arg.lock

    if { [catch {vccRemExec $HOST "ls $lockFile" }] } {
	# lock file does not exist

	vccRemExec $HOST "exec touch $lockFile; chmod og+w $lockFile"

	# test that local env. directory is writable
	# if not, try the next one free
	# (too difficult for the remote case ...)

	set localHost $env(HOST)
	if { $HOST == $localHost } {
	    set envDir $VLTDATA/ENVIRONMENTS/$arg
	    if { [catch { file delete -force -- $envDir} ] != 0} {
		tatPuts "Directory $envDir is not writable. Trying next one."
		catch { file delete -force -- $lockFile }
		continue
	    }
	    file mkdir $envDir
	    chmod og+w $envDir
	}

	vccRemExec $HOST "echo $localHost $sessionFile >> $lockFile"
	set envFound 1
	tatPuts "Allocated WS environment for $envName: $arg"
	return $arg
    }
}

error "No more WS environment available under $HOST:$VLTDATA/ENVIRONMENTS"

}

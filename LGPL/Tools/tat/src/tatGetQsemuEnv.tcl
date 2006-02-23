#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatGetQsemuEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# fcarbogn  29/06/98  created
# eallaert 2003-06-11 replaced several exec file-operations by Tcl "file" cmds
# psivera  2003-06-30 spr 20030137: check on the output of vccRemExec
# psivera  2004-02-13 fixed tcl procheck warnings
#

#************************************************************************
#   NAME
#
#   tatGetQsemuEnv -
# 
#   SYNOPSIS
#
#   tatGetQsemuEnv <sessionFile>
# 
#   DESCRIPTION
#
#   Allocates a CCS environment name from the predefined tat CCS
#   environment names pool.
#
#   The file name <sessionFile> is written in the lock file of the
#   allocated environment.
#
#   FILES
#
#   $VLTDATA/ENVIRONMENTS/w<number>qs*
#   /tmp/w<number>qs.lock
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
#   BUGS     
#
#------------------------------------------------------------------------
#

proc tatGetQsemuEnv { envName sessionFile HOST VLTDATA } {

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
set p1 {[a-l]}
set pattern $pattern$p1

# use the vcc database to query 'tat' environments
if {[ catch { vccInit} err ]} {
    error "tatGetQsemuEnv: $err"
}
if {[catch { set envList [vccInfo GetEnvs -attic $pattern] } out ]} {
    error "tatGetQsemuEnv: $out"
}

set envFound 0
while {![lempty $envList]} {
    set arg [lvarpop envList]
    set lockFile /tmp/$arg.lock

    # for the SPR 20030137 the following line:
    #if { [catch {vccRemExec $HOST "ls $lockFile" }] } 
    # is substituted with the following two lines: 
    catch {vccRemExec $HOST "ls $lockFile" } out
    if { [string match "*No such file or directory*" $out] || [string match "*not found*" $out]} {

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
	tatPuts "Allocated QS environment for $envName: $arg"
	return $arg
    }
}

error "No more QS environment available under $HOST:$VLTDATA/ENVIRONMENTS"

}

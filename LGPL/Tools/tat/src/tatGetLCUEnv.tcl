#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatGetLCUEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# eallaert 2003-06-11 replaced several exec file-operations by Tcl "file" cmds
# psivera  2004-02-13 fixed tcl procheck warnings
#

#************************************************************************
#   NAME
#
#   tatGetLCUEnv -
# 
#   SYNOPSIS
#
#   tatGetLCUEnv sessionFile LCUROOT VLTDATA
# 
#   DESCRIPTION
#
#   Allocates an LCU from LCUROOT
#
#   FILES
#
#   $LCUROOT/<lcu>
#   $LCUROOT/LOCK.<lcu>
#
#------------------------------------------------------------------------
#

proc tatGetLCUEnv { envName sessionFile LCUROOT VLTDATA HOST } {

global env

set envList {}
catch { set envList [glob $LCUROOT/l*] }

set envFound 0
while {![lempty $envList]} {
    set arg [lvarpop envList]
    set lcu [file tail $arg]
    set lockFile $LCUROOT/LOCK.$lcu
    if { ![file exists $lockFile] } {
	close [open $lockFile w]; chmod og+w $lockFile
        # skip non writable directories
        set envDir $VLTDATA/ENVIRONMENTS/$arg
	if { [catch { file delete -force -- $envDir} ] != 0} {
		tatPuts "Directory $envDir is not writable. Trying next one."
		catch { file delete -force -- $lockFile }
		continue
	}
	# skip non available LCU to avoid blocking a test suite.
	if {[ catch { vccInit } err ]} {
	    catch { file delete -force -- $lockFile }
	    error "tatGetLCUEnv.tcl: $err"

	}
	if {[ catch { set lcuHost [vccInfo GetByEnv $lcu hostName] } out ]} {
	    catch { file delete -force -- $lockFile }
	    error "tatGetLCUEnv.tcl: $out"
	}
	if { [catch { vccRemPing $lcuHost } ] } {
	       tatPuts "LCU $lcuHost not available. Trying next one."
	       catch { file delete -force -- $lockFile }
	       continue
	}
	set envFound 1
	# LCU pool is available on the network : host info. needed
        exec echo "$HOST $sessionFile" >> $lockFile
	tatPuts "Allocated LCU for $envName: $lcu"
	return [file tail $arg]
    }
}

error "No more LCU available under $LCUROOT"

}

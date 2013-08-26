#***********************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatRemExec.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when       what
# --------  --------   ----------------------------------------------
# pforstma  20/08/96   created
# psivera   2004-02-13 fixed tcl procheck warnings
#

#************************************************************************
#   NAME
#   tatRemExec - execute commands via rlogin on a LCU
#
#   SYNOPSIS
#   tatRemExec <script> <log>
# 
#   DESCRIPTION
#   Does an rlogin to the LCU specified by the environment variable LCU
#   (set by tat), executes VxWorks commands of <script>, and logs output
#   into <log>.
#
#   FILES
#   <script> input: VxWorks command script
#   <log>    output
#
#   RETURN VALUES
#   0 (success) or 1 (failure)
#
#   CAUTIONS
#   There is no specific timeout (VLT_VCCBOOTTIME does _not_ apply).
#   But, as for a WS test script, TEST_WAIT may be used. See tat(1).   
# 
#   BUGS     
#
#------------------------------------------------------------------------
#

global env

if { $argc != 2} {
    error "usage: tatRemExec <script> <log>"
} else {
    set script [lindex $argv 0]
    set log [lindex $argv 1]
}

set fo [open $log "w+"]

if {[ catch {set lcu $env(LCU)} ]} {
    puts $fo "LCU: not defined"
}

# no output from expect
log_user 0

# no timeout
set timeout -1

# login to LCU
spawn rlogin $lcu
expect -gl ->

#read commands

set cmdFile [open $script "r"]
while { ![eof $cmdFile] } {

    set cmd [gets $cmdFile]
    # execute command but not comments (=> timeout)
    if { ![regexp "^#.*" $cmd] } {

	exp_send "$cmd\r"
        expect {

	    *-> { 
		puts $fo $expect_out(buffer)
	    }

	    timeout {
		puts $fo "tatRemExec:timeout ($timeout s) expired for '$cmd' on $lcu"
		break;
	    }

	    eof {
		puts $fo "tatRemExpect: connection closed by remote host" 
		break;
	    }
	}
    } 
}

close $cmdFile
close $fo
close


#
# ___oOo___

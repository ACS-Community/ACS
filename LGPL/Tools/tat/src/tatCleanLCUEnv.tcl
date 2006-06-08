#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanLCUEnv.tcl,v 1.80 2006/06/08 15:05:30 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  22/07/98  Added  WSEnv to tatCleanLCUEnv parameters
# sfeyrin  2005-08-17 SPR 20050148: Check file owner before LOCK.lcu file deletion
#

#************************************************************************
#   NAME
#
#   tatCleanLCUEnv -
# 
#   SYNOPSIS
# 
#   DESCRIPTION
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
#   BUGS     
#
#------------------------------------------------------------------------
#

proc tatCleanLCUEnv { envName LCUROOT WSEnv} {

global env

set LCUEnv $env($envName)

# delete directory
# It is mandatory to specify the -w <wsEnv> switch to vccEnvDelete
# working for a given LCU in order to avoid vcc to connect to the
# host assigned in the vcc database: -w overrides this static
# information and tells vcc to execute on the workstation where
# -w <wsEnv> is defined.

tatPuts "Deleting target directory for $envName."
tatPuts "Executing vccEnvDelete -e $LCUEnv -w $WSEnv"
catch { exec vccEnvDelete -e $LCUEnv -w $WSEnv} 

# give LCU back in the pool

set lockFile $LCUROOT/LOCK.$LCUEnv

set user [exec whoami]

tatPuts "Unlocking environment $envName"
if { [file exists $lockFile] } {
#check the owner of the file before deleting (SPR 20050148)
    set owner [file attributes $lockFile -owner]
    if { $owner == $user } {
       catch { file delete -force -- $lockFile }
    } else {
       error "$lockFile can't be deleted: $user is not the owner"
    }
} else {
    error "$envName is not a 'tat' LCU environment"
}


}

#
# ___oOo___

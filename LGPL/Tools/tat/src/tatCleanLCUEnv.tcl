#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanLCUEnv.tcl,v 1.81 2011/10/11 13:21:16 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  22/07/98  Added  WSEnv to tatCleanLCUEnv parameters
# sfeyrin  2006-08-17 SPR 20050148: Check file owner before LOCK.lcu file deletion
# sfeyrin  2006-11-20 SPR 20050148: Check lockFile owner before deleting target directory
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

set lockFile $LCUROOT/LOCK.$LCUEnv
set owner [file attributes $lockFile -owner]
set user [exec whoami]

# delete directory
# It is mandatory to specify the -w <wsEnv> switch to vccEnvDelete
# working for a given LCU in order to avoid vcc to connect to the
# host assigned in the vcc database: -w overrides this static
# information and tells vcc to execute on the workstation where
# -w <wsEnv> is defined.

tatPuts "Deleting target directory for $envName."
#check the owner of the lockfile before deleting (SPR 20050148)
if { $owner != $user } {
   error "$user is not the owner of $lockFile"
}

tatPuts "Executing vccEnvDelete -e $LCUEnv -w $WSEnv"
catch { exec vccEnvDelete -e $LCUEnv -w $WSEnv} 

# give LCU back in the pool

tatPuts "Unlocking environment $envName"
if { [file exists $lockFile] } {
    catch { file delete -force -- $lockFile }
} else {
    error "$envName is not a 'tat' LCU environment"
}


}

#
# ___oOo___

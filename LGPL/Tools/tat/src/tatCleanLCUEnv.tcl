#*************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanLCUEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  22/07/98  Added  WSEnv to tatCleanLCUEnv parameters
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

tatPuts "Unlocking environment $envName"
if { [file exists $lockFile] } {
    catch { file delete -force -- $lockFile }
} else {
    error "$envName is not a 'tat' LCU environment"
}


}

#
# ___oOo___

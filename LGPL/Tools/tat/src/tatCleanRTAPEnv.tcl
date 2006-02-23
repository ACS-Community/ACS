#******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanRTAPEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  03/08/98  Eliminated puts "Stopping $envName environment failed: $out"
# fcarbogn  23/08/99  make use of -f option of vccEnvStop to wait for
#                     complete environment shutdown
# psivera  2004-02-13 fixed tcl procheck warnings
#

#************************************************************************
#   NAME
#
#   tatCleanRTAPEnv -
# 
#   SYNOPSIS
#
#   tatCleanRTAPEnv <envName> HOST VLTDATA
# 
#   DESCRIPTION
#   
#   envName is the name of the environment variable defining the
#   environment do be cleaned.
#
#   HOST is always the local host for WS/RWS.
#
#   Stops the RTAP environment and deletes the corresponding directory.
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

proc tatCleanRTAPEnv { envName HOST VLTDATA } {

global env 

#
# Do not call always error, otherwise allocated environments not released.
#

if {[catch {set RtapEnvName $env($envName)}]} {
    error "Environment variable $envName is not defined"
}

# stop  RTAP environment
# Because an uncaught error aborts the script, catch need to be used with
# vccEnvStop that can be failing because tatCleanRTAPEnv can be called
# also when the environment has not been jet started
tatPuts "Stopping environment $envName"
tatPuts "Executing vccEnvStop -e $RtapEnvName -f 300"
catch { exec vccEnvStop -e $RtapEnvName -f 300 }

# delete target directory: works even with the following error message:
# "chmod: can't change .licensed: Not owner"
tatPuts "Removing target directory for $envName"
tatPuts "Executing vccEnvDelete -e $RtapEnvName"
catch { exec vccEnvDelete -e $RtapEnvName }


# local or remove env. ?
if {[ catch { vccInit } out ]} {
    error "tatCleanRTAPEnv: $out"
}
if {[ catch { set envHost [vccInfo GetByEnv $RtapEnvName hostName] } out ]} {
    error "tatCleanRTAPEnv: $out"
}

# unlock environment

tatPuts "Unlocking environment $envName"
set lockFile /tmp/$RtapEnvName.lock
if { $envHost == $HOST } {
    file delete -force -- $lockFile 
}  else {
    vccRemExec $envHost "rm -f $lockFile"
}

}
#
# ___oOo___

#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanQsemuEnv.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  30/04/96  created
# fcarbogn  23/06/98  Upgrade to New CCS_LITE
# fcarbogn  03/08/98  Trapped error from vccEnvStop and vccEnvDelete
# fcarbogn  23/08/99  make use of -f option of vccEnvStop to wait for 
#		      complete environment shutdown 
# psivera  2004-02-13 fixed tcl procheck warnings
#
#************************************************************************
#   NAME
#
#   tatCleanQsemuEnv -
# 
#   SYNOPSIS
#
#   tatCleanQsemuEnv <envName>
# 
#   DESCRIPTION
#   
#   envName is the name of the environment variable defining the
#   environment do be cleaned.
#
#   Stops the qsemu environment and deletes the corresponding directory.
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

proc tatCleanQsemuEnv { envName LOGNAME VLTDATA HOST} { 

   global env

if {[catch {set envId $env($envName)}]} {
    set EnvId undefined 
}

# Because an uncaught error aborts the script, catch need to be used with
# vccEnvStop that can be failing because tatCleanQsemuEnv can be called
# also when the environment has not been jet started
tatPuts "Stopping environment $envName"
tatPuts "Executing vccEnvStop -e $envId -h $HOST -f 300"
catch {exec vccEnvStop -e $envId -h $HOST -f 300 }

tatPuts "Removing target directory for $envName"
tatPuts "Executing vccEnvDelete -e $envId -h $HOST"
catch { exec vccEnvDelete -e $envId -h $HOST }

# local or remove env. ?
if {[ catch { vccInit } out ]} {
    error "tatCleanQsemuEnv: $out"
}
if {[ catch { set envHost [vccInfo GetByEnv $envId hostName] } out ]} {
    error "tatCleanQsemuEnv: $out"
}
tatPuts "Unlocking environment $envName"
set lockFile /tmp/$envId.lock
if { $envHost == $env(HOST) } {
    file delete -force -- $lockFile
}  else {
    vccRemExec $envHost "rm -f $lockFile"
}


}

#
# ___oOo___

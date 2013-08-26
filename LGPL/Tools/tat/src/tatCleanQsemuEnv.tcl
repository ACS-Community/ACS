#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanQsemuEnv.tcl,v 1.80 2011/10/11 13:21:16 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  30/04/96  created
# fcarbogn  23/06/98  Upgrade to New CCS_LITE
# fcarbogn  03/08/98  Trapped error from vccEnvStop and vccEnvDelete
# fcarbogn  23/08/99  make use of -f option of vccEnvStop to wait for 
#		      complete environment shutdown 
# psivera  2004-02-13 fixed tcl procheck warnings
# sfeyrin  2006-11-20 SPR 20050148: Check lockFile owner before env stop
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

set lockFile /tmp/$envId.lock
set owner [file attributes $lockFile -owner]
set user [exec whoami]

# Because an uncaught error aborts the script, catch need to be used with
# vccEnvStop that can be failing because tatCleanQsemuEnv can be called
# also when the environment has not been jet started
tatPuts "Stopping environment $envName"

# check the owner of the lockfile before doing the vccEnvStop (SPR 20050148)
if { $owner != $user } {
   error "$user is not the owner of $lockFile"
}

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

if { $envHost == $env(HOST) } {
    file delete -force -- $lockFile
}  else {
    vccRemExec $envHost "rm -f $lockFile"
}


}

#
# ___oOo___

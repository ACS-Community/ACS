#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatMakeRTAPEnv.tcl,v 1.80 2011/10/11 13:21:16 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  06/11/97  addedd messagge in verbose mode for vccEnvCreate
# fcarbogn  10/08/99  bug fixing for make possible RWS from CCSLite Workstation
# psivera   09/01/03  SPR 20020703: for the host where the templates are, 
#                     the local host is used
# psivera  2004-02-13 fixed tcl procheck warnings
# sfeyrin  2010-11-02 SVN support: changed check of empty ENVIRONMENTS/$envName 
#

#************************************************************************
#   NAME
#
#   tatMakeRTAPEnv - 
# 
#   SYNOPSIS
#
#   tatMakeRTAPEnv <envName>
# 
#   DESCRIPTION
#
#   Creates an RTAP environment from scratch: 
#
#   - uses the db loader to create the DB structure
#   - starts the RTAP environment making a snapshot
#   - restarts the RTAP environment.
#  
#   envName is the name of the environment variable defining the
#   environment do be created.
#
#   FILES
#
#   <target_host>:$VLTDATA/ENVIRONMENTS/env($envName)
#
#   ENVIRONMENT
#
#   When starting a RTAP environment, waits for RTAP_WAIT seconds
#   (if RTAP_WAIT is not defined, default is 60 seconds).
#
#   SEE ALSO
#
#   tat - envs - vccCreateEnv, vccEnvInit, vccStartEnv.
#
#   BUGS     
#
#   CREDITS
#
#   envName/autostart (BGI).
#   vcc* (SSA).
#
#------------------------------------------------------------------------
#

proc tatMakeRTAPEnv { envName HOST VLTDATA } {

global env 

if {[catch {set RtapEnvName $env($envName)}]} {
    error "tatMakeRTAPEnv" "$envName is not defined"
}

# SPR 20020703: for the host where the templates are, the local host is used
    set TEMPL_HOST $HOST
    if {[info exists env(HOST)]} {
        set TEMPL_HOST $env(HOST)
    } 

# Be sure that the target directory does not exist:
# this is required by vccEnvCreate needs it.

# 
catch { exec vccEnvDelete -e $RtapEnvName }

# $HOST:pwd is needed for remote environment creation.
# vccEnvCreate fails if user directory is empty ([r]cp -r needs to
# use <dir>/* because target directory already exists): do not give
# use directory -s option if source directory is empty.
tatPuts "Creating target directory for $envName."
#set fdir [readdir [pwd]/ENVIRONMENTS/$envName]
# For SVN case, the directory should be considered as empty if only directory .svn is present
# On Linux,a "." at the beginning of a file's name doesn't match the pattern "*", so .svn excluded
set fdir [glob -nocomplain -directory [pwd]/ENVIRONMENTS/$envName *] 
if { [lempty $fdir] } {
    tatPuts "Executing vccEnvCreate -e $RtapEnvName"
    if {[ catch { exec vccEnvCreate -e $RtapEnvName } out ]} {
	error "Creating target directory for $envName: $out"
    }
} else {
    tatPuts "Executing vccEnvCreate -e $RtapEnvName -s $TEMPL_HOST:[pwd]/ENVIRONMENTS/$envName"
    if {[ catch { exec vccEnvCreate -e $RtapEnvName -s $TEMPL_HOST:[pwd]/ENVIRONMENTS/$envName } out ]} {
	error "Creating target directory for $envName: $out"
    }
}

tatPuts "Starting autoloading environment for $envName."
# fails if not catched (which Rtap proc. writes something on stderr ?)
if {[ catch { exec vccEnvInit -e $RtapEnvName } out ]} {


    error "Starting autoloading environment failed for $envName: $out"
}

tatPuts "Starting real environment for $envName."
if {[ catch { exec vccEnvStart -q -e $RtapEnvName } out ]} {
    error "Real environment RTAP starting failed for $envName: $out"
}

tatPuts "$envName environment successfully created."

}

#
# ___oOo___

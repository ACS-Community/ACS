#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatMakeLCUEnv.tcl,v 1.80 2011/10/11 13:21:16 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  11/07/95  created
# fcarbogn  29/10/97  Modified the  search testDirHost (SPR970261)
# fcarbogn  29/10/97  -m template option on vccEnvCreate (SPR970435) 
# fcarbogn  22/07/98  -m template option only if <src>/bootScript exist
# fcarbogn  06/08/98  updated the nfs host search after users home redefinition
# fcarbogn  17/08/98  modification to nfs host search  to consider "localhost"
# fcarbogn  02/06/99  Eliminated vcc timeout settings (SPR 990194)
# fcarbogn  23/07/99  Eliminated testDirHost search, currently done by vcc  
#		      (SPRs 960308 and 960055)
# psivera 2003-07-21  If Warnings are produced during "make Env" they 
#                     are printed to the standard output (SPR 20030324)
# psivera 2003-07-31  SPR 990447: handling of configuration files for logManager
# psivera  2004-02-13 fixed tcl procheck warnings
# sfeyrin  2009-02-05 added catch when trying the cat reboot.log 
# sfeyrin  2010-11-02 SVN support: changed check of empty ENVIRONMENTS/$envName 
#

#************************************************************************
#   NAME
#
#   tatMakeLCUEnv -
# 
#   SYNOPSIS
#
#   tatMakeLCUenv <lcuEnvName> <wsEnvName>
# 
#   DESCRIPTION
#
#   creates the LCU environment whose name is the value of the 
#   environment variable given as first parameter.
#
#   The environment dependent sections of the bootscript
#   are automatically generated, and the bootChange sequence on the LCU 
#   itself as well. The LCU is rebooted.
#
#   FILES
#
#   $VLTDATA/ENVIRONMENTS/$<lcuEnvName>
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
#   tat
#
#   BUGS     
#
#------------------------------------------------------------------------
#

proc tatMakeLCUEnv { envName WSEnvName HOST VLTDATA} {

global env

if {[catch {set LCUEnvName $env($envName)}]} {set LCUEnvName undefined}
if {[catch {set WSEnv $env($WSEnvName)}]} {
    error "tatMakeLCUEnv: $WSEnvName environment variable not defined."
}

if {[catch {set user $env(USER)}]} {
    error "tatMakeLCUEnv: USER variable not defined."
}

# It is mandatory to specify the -w <wsEnv> switch to all following vccEnvXXX
# working for a given LCU in order to avoid vcc to connect to the
# host assigned in the vcc database: -w overrides this static
# information and tells vcc to execute on the workstation where
# -w <wsEnv> is defined.

# delete former environment 
# (do not catch errors because access rights error messages are expected)

catch { exec vccEnvDelete -e $LCUEnvName -w $WSEnv} 

# create the new one

tatPuts "Creating target directory for $envName."

# vccEnvCreate fails if user directory is empty ([r]cp -r needs to
# use <dir>/* because target directory already exists): do not give
# use directory -s option if source directory is empty
# set fdir [readdir [pwd]/ENVIRONMENTS/$envName]
# For SVN case, the directory should be considered as empty if only directory .svn is present
# On Linux,a "." at the beginning of a file's name doesn't match the pattern "*", so .svn excluded
set fdir [glob -nocomplain -directory [pwd]/ENVIRONMENTS/$envName *] 
if { [lempty $fdir] } {
    tatPuts "Executing vccEnvCreate -e $LCUEnvName  -m minimum -w $WSEnv -h $HOST"
    if {[ catch { exec vccEnvCreate -e $LCUEnvName  -m minimum -w $WSEnv -h $HOST } out ]} {
	error "tatMakeLCUEnv: vccEnvCreate -e $LCUEnvName (...) failed: $out"
    }
} else {
    if { [file exists ./ENVIRONMENTS/$envName/bootScript] } {
        tatPuts "Executing vccEnvCreate -e $LCUEnvName  -s ./ENVIRONMENTS/$envName -m template -w $WSEnv -h $HOST"
        if {[ catch { exec vccEnvCreate -e $LCUEnvName -s ./ENVIRONMENTS/$envName -m template -w $WSEnv -h $HOST } out ]} {
	    error "tatMakeLCUEnv: vccEnvCreate -e $LCUEnvName -s (...) failed: $out"
	}
    } else {
	tatPuts "Executing vccEnvCreate -e $LCUEnvName  -s ./ENVIRONMENTS/$envName -w $WSEnv -h $HOST"
        if {[ catch { exec vccEnvCreate -e $LCUEnvName -s ./ENVIRONMENTS/$envName -w $WSEnv -h $HOST } out ]} {
            error "tatMakeLCUEnv: vccEnvCreate -e $LCUEnvName -s (...) failed: $out"
	}
    }
}

# SPR 990447 BEGIN
if { [file exists $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config] } {
    if {[ catch { exec  sed "s/$envName/$LCUEnvName/g" $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config > $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config.temp } otto ]} {
	error "tatMakeLCUEnv: parsing $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config failed: $otto"
    }
    file copy -force $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config.temp $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config
    file delete -force -- $env(VLTDATA)/ENVIRONMENTS/$WSEnv/logLCU.config.temp
}
# SPR 990447 END

foreach line [split $out \n] {
    # search for lines starting with "warning" (case is not relevant)
    if {[string match -nocase warning* $line] || [string match -nocase error* $line]} {
    # print them out (assuming these warnings are not continued on next line)
        tatPuts $line
    }
}


# execute the bootChange sequence
set out ""

tatPuts "Executing vccEnvInit -e $LCUEnvName -w $WSEnv"

if {[ catch { exec vccEnvInit -e $LCUEnvName -w $WSEnv} out ]} {
   error "tatMakeLCUEnv: vccEnvInit -e $LCUEnvName: $out"
}

foreach line [split $out \n] {
    # search for lines starting with "warning" (case is not relevant)
    if {[string match -nocase warning* $line] || [string match -nocase error* $line]} {
    # print them out (assuming these warnings are not continued on next line)
        tatPuts $line
    }
}

# reboot with vxWorks and execute bootscript.
set out ""

tatPuts "Executing vccEnvStart -v -e $LCUEnvName -w $WSEnv"

if {[ catch { exec vccEnvStart -v -e $LCUEnvName -w $WSEnv} out ]} {
  set oldOut $out
  set newOut ""
  catch {set newOut [exec cat $VLTDATA/ENVIRONMENTS/$LCUEnvName/.reboot.log]}
  set out "$oldOut $newOut"
  error "tatMakeLCUEnv failed vccEnvStart -v -e $LCUEnvName: $out"
}

foreach line [split $out \n] {
    # search for lines starting with "warning" (case is not relevant)
    if {[string match -nocase warning* $line] || [string match -nocase error* $line]} {
    # print them out (assuming these warnings are not continued on next line)
        tatPuts $line
    }
}

tatPuts "$envName environment successfully created."

}

#
# ___oOo___
 

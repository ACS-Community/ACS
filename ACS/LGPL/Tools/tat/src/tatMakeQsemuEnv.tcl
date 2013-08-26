#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatMakeQsemuEnv.tcl,v 1.80 2011/10/11 13:21:16 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  30/04/96  created
# fcarbogn  23/06/98  Upgrade to New CCS_LITE
# fcarbogn  31/07/98  Added the merging with user-defined template
# psivera   09/01/03  SPR 20020703: for the host where the templates are, 
#                     the local host is used
# psivera 2003-07-21  If Warnings are produced during "make Env" they
#                     are printed to the standard output (SPR 20030324)
# psivera 2003-07-31  SPR 20030339: handling of configuration files for logManager 
# psivera 2003-08-06  the previous bit has been moved to tat.tcl
# psivera 2003-08-08  SPR 990447 and 20030339: handling of logLCU.config moved here 
#                     from tat.tcl; the file is created/parsed just after vccEnvCreate.
# psivera 2003-09-10  added check on size of logLCU.config and on existence of LCU envs
# psivera  2004-02-13 fixed tcl procheck warnings
# sfeyrin  2010-11-02 SVN support: changed check of empty ENVIRONMENTS/$envName 
#

#************************************************************************
#   NAME
#
#   tatMakeQsemuEnv - 
# 
#   SYNOPSIS
#
#   tatMakeQsemuEnv <envName>
# 
#   DESCRIPTION
#
#   Creates an qsemu (CCS lite) environment from scratch: 
#  
#   envName is the name of the environment variable defining the
#   environment do be created.
#
#   FILES
#
#   ENVIRONMENT
#
#   SEE ALSO
#
#   tat.
#
#   BUGS     
#
#   CREDITS
#

#------------------------------------------------------------------------
#

proc tatMakeQsemuEnv { envName LOGNAME HOST } {

    global env
    
    if {[catch {set  envId $env($envName)}]} {
	set envId undefined 
    }

    catch { exec vccEnvDelete -e $envId -h $HOST}

# SPR 20020703: for the host where the templates are, the local host is used
    set TEMPL_HOST $HOST
    if {[info exists env(HOST)]} {
	set TEMPL_HOST $env(HOST)
    } 

# vccEnvCreate fails if user directory is empty ([r]cp -r needs to
# use <dir>/* because target directory already exists): do not give
# use directory -s option if source directory is empty.
    tatPuts "Creating target directory for $envName."
    #set fdir [readdir [pwd]/ENVIRONMENTS/$envName]
    # For SVN case, the directory should be considered as empty if only directory .svn is present
    # On Linux,a "." at the beginning of a file's name doesn't match the pattern "*", so .svn excluded
    set fdir [glob -nocomplain -directory [pwd]/ENVIRONMENTS/$envName *] 
    if { [lempty $fdir] } {
       tatPuts "Executing vccEnvCreate -e $envId -t QSEMU -h $HOST"
       if {[ catch { exec vccEnvCreate -e $envId -t QSEMU -h $HOST } out ]} {
           error "tatMakeQsemuEnv: vccEnvCreate -e $envId failed: $out"
       }

    } else {
        tatPuts "Executing vccEnvCreate -e $envId -s $TEMPL_HOST:[pwd]/ENVIRONMENTS/$envName"
        if {[ catch { exec vccEnvCreate -e $envId -s $TEMPL_HOST:[pwd]/ENVIRONMENTS/$envName } out ]} {
            error "tatMakeQsemuEnv: vccEnvCreate -e $envId failed: $out"
	}
    }

    foreach line [split $out \n] {
        # search for lines starting with "warning" (case is not relevant)
        if {[string match -nocase warning* $line] || [string match -nocase error* $line]} {
        # print them out (assuming these warnings are not continued on next line)
            tatPuts $line
        }
    }


    # SPR 990447, 20030339: BEGIN

    # The algorithm is executed only if there are environments to create: -> .testSession is not empty
    if { [ file exists .testSession ] && [ file size .testSession ] != 0 } {

    # find the first WS environment in the .testSession. The logLCU.config
    # will be generated using that WS environment. If there are not WS
    # envs to be created, the default one $RTAPENV is used.
    set fd [open .testSession r]
    set wsSymb ""
    set wsReal ""
    while { [gets $fd line] >= 0} {
        if {[string match "QS *" $line]} {
            set wsSymb [lindex $line 1]
            set wsReal [lindex $line 2]
            break
        }
    }
    close $fd

    # check whether there are LCU environments or not. If not, do not do anything
    # with the logLCU.config file
    set NOLCU 1
    set fd [open .testSession r]
    while { [gets $fd line] >= 0} {
        if {[string match "LCU *" $line]} {
	    set NOLCU 0
            break
        }
    }
    close $fd

    set logLCUfile $env(VLTDATA)/ENVIRONMENTS/$envId/logLCU.config

    # 1. first case: the file under the first WS environment does not exist:
    if { ![ file exists $logLCUfile ] || [file size $logLCUfile] == 0 } {  
        if { $NOLCU == 0 } {
        if { $wsReal == $envId } {

        close [open $logLCUfile w]
        set fileId [open $logLCUfile a]
        set fd [open .testSession r]
        while { [gets $fd line] >= 0} {
            if {[string match "LCU *" $line]} {
                set lcuSymb [lindex $line 1]
                set lcuReal [lindex $line 2]
                puts $fileId "$lcuReal $wsReal"
            }
        }
        close $fd
        close $fileId

	}
	}

    # 2. second case is: there is a WS envs to be created and it
    # contains a logLCU.config in the template directory. This file has been copied
    # in the destination directory under /vltdata/ENVIRONMENT/wsName
    # What we have to do is to parse the file and substitute the symbolic names
    # with real ones.
    } else {
        if { $NOLCU == 0 } {
	set wsSubs ""
	set wsSubs [ exec grep $envName $logLCUfile | awk " {print \$2} " | sort -u ]
	if { [ string compare $wsSubs $envName ] == 0 } {
            if { [ catch { exec  sed "s/$envName/$envId/g" $logLCUfile > $logLCUfile.temp } otto ] } {
                error "tat making environments: parsing of $logLCUfile failed: $otto"
            }
            file copy -force $logLCUfile.temp $logLCUfile
            file delete -force -- $logLCUfile.temp
            set fd [open .testSession r]
            while { [gets $fd line] >= 0} {
                if {[string match "LCU *" $line]} {
                    set lcuSymb [lindex $line 1]
                    set lcuReal [lindex $line 2]
                    set fd2 [open $logLCUfile r]
                    while { [gets $fd2 line2] >= 0} {
                        set safelcuSymb "$lcuSymb "
                        set lcuSymbLog [lindex $line2 0]
                        set safelcuSymbLog "$lcuSymbLog "
                        if { $safelcuSymbLog == $safelcuSymb } {
                            if {[ catch { exec  sed "s/$lcuSymb /$lcuReal /g" $logLCUfile > $logLCUfile.temp } otto ]} {
                                error "tat making environments: parsing of $logLCUfile failed: $otto"
                            }
                            file copy -force $logLCUfile.temp $logLCUfile
                            file delete -force -- $logLCUfile.temp
                            break
                        }
                    }
                    close $fd2
                }
            }
            close $fd
	} else {
	    puts "Warning: tat making environments: $envName not found in $logLCUfile; is that OK?"
	}
	}
    }

    } 
    # SPR 990447, 20030339: END

    set out ""
    tatPuts "Executing vccEnvInit -e $envId -t QSEMU -h $HOST"
    if {[ catch { exec vccEnvInit -e $envId -t QSEMU -h $HOST } out ]} {
        error "tatMakeQsemuEnv: vccEnvInit -e $envId failed: $out"
    }

    foreach line [split $out \n] {
        # search for lines starting with "warning" (case is not relevant)
        if {[string match -nocase warning* $line] || [string match -nocase error* $line]} {
        # print them out (assuming these warnings are not continued on next line)
            tatPuts $line
        }
    }

    set out ""
    tatPuts "Executing vccEnvStart -v -e $envId -t QSEMU -h $HOST"
    if {[ catch { exec vccEnvStart -v -e $envId -t QSEMU -h $HOST } out ]} {
        error "tatMakeQsemuEnv: vccEnvStart -v -e $envId failed: $out"
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

#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: compat.tcl,v 1.23 2001/06/20 11:14:28 vltsccm Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  01/03/95    created
# eallaert  08/08/95    adapted for JUL1995
# eallaert  09/08/95    added "progress" status of part 2 for ttys
# eallaert  08/12/95    more complete check of <version>
# eallaert  1998-09-22  flush stdout on tty after updating file
# eallaert  1999-02-17  added -verbose, -configFile
#                       use regular expr. instead of glob-style pattern matching
#

#************************************************************************
#   NAME
#   compat - check files for potential compatibility problems
# 
#   SYNOPSIS
#   compat ?-verbose? <version> ?<fileList>?
#   or
#   compat ?-verbose? -configFile <fileName> ?<fileList>?
# 
#   DESCRIPTION
#   compat will test all files within <fileList> for (potential)
#   incompatibilities with the release <version> and produce the corresponding
#   error and warning messages.
#
#   -verbose   : print verbose help whenever a potential incompatibility is
#                found. This verbose help must be an entry for this 
#                incompatibility in the configuration file, under the key
#                VERBOSE.
#   -configFile: flag that the next argument, <fileName>, is the name
#                of the configuration file. In this case, there will be
#                no attempt to find a <version> argument.
#   <fileName> : full pathname of the configuration file.
#   <version>  : release-string of new software, in form MMMYYYY. The name of 
#                the configuration file (containing the description of all
#                incompatilities) will be derived from <version>, unless the
#                -configFile flag was set (in which case <version> should
#                not be given).              
#   <fileList> : the file list or mask (standard wildcards allowed); if 
#                nothing is given on the commandline, input will be taken from
#                stdin
#
#   FILES
#   Makes use of a configuration file, whose name is either given explicitly
#   as <fileName> or is derived from <version> (compat.<version>.config).  In
#   the latter case it is searched for in the following order:
#     1. pwd
#     2. [pwd]/../config
#     3. [pwd]/config
#     4. $INTROOT/config
#     5. $VLTROOT/config
#
#   This file contains a number of entries, with each entry describing a single
#   incompatibilitiy.  Each entry is structured as a keyed list (see TclX)
#   The format of each entry is:
#      1. Opening brace
#      2. A number of sublists each containing a key and a value;
#      3. Closing brace
#   Being keyed lists, the exact order of the sublists is irrelevant.
#
#   The mandatory sublists (2. above) are the ones with the following keys:
#   TYPE : tells whether the incompatibility stems from the 'filename' or 
#          from the 'content' of the file
#   PATTERN : if the TYPE is 'filename', this is a glob-style pattern to match,
#             otherwise it is a regular expression to match. In the latter
#             case, there is no "backslash substitutions" supported, ie. a
#             <Tab> must be typed as such, not as '\t'. So watch out with these
#             special unvisible characters like <Tab> or <NewLine>: you
#             may not see them while editing, but they play a significant
#             role in regular expressions!
#   LEVEL : classification of incompatibility.  WARNING means potentially
#           incompatible, SEVERE means a hard incompatibility.
#   HELP : a short explanatory help text (up to ~50 chars)
#   
#   The optional keys are:
#   LABEL : will be used when information about this incompatibility is
#           printed, instead of the contents of the PATTERN key. Useful if
#           the latter contains funny characters (which it probably will if
#           you're using regular expressions). If not given, the PATTERN
#           key will be used as a label as well.
#   VERBOSE: a more verbose help which will be printed if compat is run with
#           the -verbose flag. If the VERBOSE key does not exist, the HELP
#           key will be used for these purposes. Remark that this help text
#           will be printed as entered within the VERBOSE key, including
#           the line-breaks and tabs as they have been entered for this key.
#
#   Comment or blank lines can appear anywhere outside the braces enclosing one
#   entry (1. and 3.). But as also comment lines (including the header) are
#   read via an 'lgets' command, care should be taken about balanced braces
#   and quotes in these comment-lines (e.g. a closing double quote must be
#   followed by whitespace or a newline, not by any other character).
#
#   ENVIRONMENT
#   possibly uses INTROOT and VLTROOT environment variables - see above
#
#   CAUTIONS
#   The config files only contain the incompatibilities with respect to the
#   previous software release.  If you want to test incompatibilities with
#   more consecutive versions, more passes will be required, each one with
#   the corresponding config file.
#
#   For some mysterious reason, use two consecutive <ctrl-D>s to terminate
#   interactive input of the <fileList>.
#
#   Watch out with the construction of regular expressions in config-files,
#   and the use of braces and quotes in their comment lines - see above. 
#
#   EXAMPLES
#   $ compat JUL1995 < fileList > compatList
#       will take a list of filenames contained in the file "fileList", check
#       them for compatibility-problems with respect to the JUL1995 release, 
#       and produce a report in the file "compatList".
#
#   $ ls | compat MAY1997 > compatList
#       will check the current directory for files with compatibility-problems
#       with respect to the MAY1997 release, and produce a report in the file 
#       "compatList".
#
#   $ compat NOV2000 `find .`
#       will check the current directory and all its subdirectories for files
#       with compatibility-problems with respect to the NOV2000 release, and
#       print a report to stdout.
#
#   $ find ~ -follow | compat OCT1998
#       will check the home directory and its subdirectories for files with 
#       compatibility-problems with respect to the OCT1997 release, and display
#       the progress on the screen.
#
#   $ find ~/seq -follow | compat -configFile TclTkUpgrade
#       will check the ~/seq directory and its subdirectories for files with 
#       compatibility-problems which are listed in the file TclTkUpgrade (on
#       the current directory), and display the progress on the screen.
#
#   SEE ALSO
#   The Tcl/Tk manpages on regexp(n), regsub(n)
#
#------------------------------------------------------------------------
#
if {$argc == 0} {
    puts stderr "\nWrong # args; must be \"$argv0 <version> ?<fileList>?\""
    puts stderr "\n              with <version> in format MMMYYYY\n"
    puts stderr "\nor alternatively:     \"$argv0 -configFile <fileName> ?<fileList>?\""
    puts stderr "\n              with <fileName> pointing to the config file\n"
    exit 1
}
# Get the version to check; this should also indicate what config file to use
# Parse command line options (could override default configFile)
while {![lempty $argv]} {
    set arg [lvarpop argv]
    switch -glob -- $arg {
	-configFile {set configFile [lvarpop argv]}
	-verbose {set verbose 1}
	default	{set argv [linsert $argv 0 $arg]; break}
    }
}

if {[info exists configFile]} {
    if {![file exists $configFile]} {
	puts stderr "\nCould not find configuration file \"$configFile\"\n"
	exit 1
    }
} else {
    # the filename of the configfile is not given; in this case the version
    # must be there
    set version  [lvarpop argv]
    if {[string length $version] != 7} {
	puts stderr "\nVersion \"$version\" must be in format MMMYYYY\n"
	exit 1
    }

    set monthList {JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC}
    set month [string range $version 0 2]
    if {![string match *${month}* $monthList]} {
	puts stderr "\nVersion \"$version\" must be in format MMMYYYY"
	puts stderr "First 3 chars of \"$version\" must be one of \"$monthList\"\n"
	exit 1
    }    

    set year [string range $version 3 6]
    if {[catch "expr 1.0*$year"] != 0} {
	puts stderr "\nVersion \"$version\" must be in format MMMYYYY"
	puts stderr "Last 4 characters of \"$version\" must be digits\n"
	exit 1
    }    

    set basename compat.${version}.config

    # INTROOT and VLTROOT are defined by the prologue added by vltMake

    if {[file exist $basename]} {
	set configFile ./$basename
    } elseif {[file exist ../config/$basename]} {
	set configFile ../config/$basename
    } elseif {[file exist config/$basename]} {
	set configFile config/$basename
    } elseif {[file exist ${INTROOT}/config/$basename]} {
	set configFile ${INTROOT}/config/$basename
    } elseif {[file exist ${VLTROOT}/config/$basename]} {
	set configFile ${VLTROOT}/config/$basename
    } else {
	puts stderr "\nCould not find configuration file \"$basename\"\n"
	exit 1
    }
}

# Check type of device of stdout
set tty [fstat stdout tty]

# it is always best to Announce Yourself...
set timestamp [clock format [clock seconds] -format %Y-%m-%dT%H:%M:%S -gmt 1]
if {[info exists version]} {
    puts "\nVLT SW release $version compatibility check - $timestamp"
    puts "================================================================"
} else {
    puts "\nVLT SW compatibility check - $timestamp"
    puts "================================================"
}
puts "configFile = $configFile"
puts ""

# Get the list of files to check; default: read from stdin
if {[lempty $argv]} {
    regsub -all "\n" [read stdin] " " fileList
} else {
    set fileList $argv
}

# Get the list of things to check for.  Each entry is a keyed list
set checkContentList ""
set checkNameList ""
set fileId [open $configFile]
while {[lgets $fileId next] != -1} {
    set index0 [lindex $next 0]
    catch "keylget index0 TYPE" type
    switch -- $type filename {
        set checkNameList [concat $checkNameList $next]
    } content {
        set checkContentList [concat $checkContentList $next]
    }
}     
close $fileId

# Clean up: remove redundant and disturbing newlines
regsub -all "\n\{" $checkNameList " \{" x
regsub -all "\}\n" $x "\}" checkNameList

regsub -all "\n\{" $checkContentList " \{" x
regsub -all "\}\n" $x "\}" checkContentList

set compatList ""

# Count the different lists
set counter1 [llength $checkNameList];		# nr of name-incompatibilities
set counter2 [llength $checkContentList];	# nr of content-incompatibilities
set counter3 [llength $fileList];		# nr of files to investigate

# First part: filename-incompatibilitities ordered according to filename
# pattern

puts "\nFirst part: file-formats or file-names which have changed:"
puts   "----------------------------------------------------------"

set any 0
foreach entry $checkNameList {
    set headerListed 0
    foreach file $fileList {
        set pattern [keylget entry PATTERN]
        if {[string match $pattern [file tail $file]]} {
            set class [keylget entry LEVEL]
            set shorthelp [keylget entry HELP]
            if {!$headerListed} {
		set helpText {}
		if {[info exists verbose]} {
		    if {[catch {split [keylget entry VERBOSE] \n} help]} {
			# this keyword is not there; use the standard help
			set helpText "[keylget entry HELP] ($class) - check:"
		    } else {
			# write the help-text as found in the VERBOSE key
			foreach line $help {
			    append helpText "${line}\n"
			}
			append helpText "Check and correct this for:"
		    }
		} else {
		    # -verbose option was not specified - get std HELP
		    set helpText "[keylget entry HELP] ($class) - check:"
		}
                puts "\n$helpText"
                set headerListed 1;	# flags header already printed
                set any 1;		# flags "a" header printed
            }
            puts "  - $file"
        }
    }
}
if {!$any} {puts "none."}

# Second pass: content-incompatibilities ordered per file
puts "\nSecond part: file-contents to be checked:"
puts   "-----------------------------------------"
set any 0
set counter 0
foreach file $fileList {
    if {$tty} {
	incr counter
	puts -nonewline "\r...checking file $counter of $counter3"
	flush stdout
    }
    if {![file exist $file]} {
        puts "\nFile \"$file\" does not exist"
        set any 1
        continue
    }
    if {[file isdirectory $file]} continue
    set fileId [open $file];		# open the file
    set fileContent [read $fileId];	# swallow it in completely
    close $fileId;			# and close it immediately
    set headerListed 0
    foreach entry $checkContentList {
        set pattern [keylget entry PATTERN]
	##puts "checking $file for pattern >$pattern<"
        if {[regexp -- ${pattern} $fileContent]} {
            if {!$headerListed} {
                puts "\n$file for all occurrences of:"
                set headerListed 1;	# flags header already printed
                set any 1;		# flags "a" header printed
            }
	    if {[catch {keylget entry LABEL} label]} {
		# pattern is a regexp, so remove eventual backslashes
		set label \"[subst $pattern]\"
	    }
            puts "  - $label ([keylget entry LEVEL]): [keylget entry HELP]"
            if {[lsearch -exact $compatList $entry] == -1} {
                lappend compatList $entry
            }
        }
    }       
}
if {!$any} {puts "none."}

if {[llength $compatList] > 0 && [info exists verbose]} {
    puts "\n\nExplanation for part 2:"
    puts   "-----------------------"
    foreach entry $compatList {
	set helpText {}
	if {[catch {keylget entry LABEL} label]} {
	    set label [keylget entry PATTERN]
	}
	if {[catch {split [keylget entry VERBOSE] \n} help]} {
	    # this keyword is not there; use the standard help
	    lappend list "- $label ([keylget entry LEVEL]):\
		    [keylget entry HELP]"
	} else {
	    foreach line $help {
		append helpText "    ${line}\n"
	    }
	    lappend list "- $label ([keylget entry LEVEL]):\n$helpText"
	}
    }
    set list [lsort $list];		# order help-list alphabetically
    foreach entry $list {
        puts $entry
    }
}

puts "\n========================================================================"
puts "\nChecked $counter3 files for:"
puts "     - $counter1 file-format or file-name incompatibilities"
puts "     - $counter2 file-content incompatibilities"
if {[info exists version]} {
    puts "\nVLT SW release $version compatibility evaluation done.\n"
} else {
    puts "\nVLT SW compatibility evaluation done.\n"
}
#
# ___oOo___

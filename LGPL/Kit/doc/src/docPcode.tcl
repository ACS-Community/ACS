#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: docPcode.tcl,v 1.31 2002/06/08 17:20:46 vltsccm Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# bghiozzi  09/03/95  created
# gfilippi  21/03/97  do not use X commands (so tcl instead of wishx) 
#

#************************************************************************
#   NAME
#   docpcode.tcl - Extract presudo code from sources
# 
#   SYNOPSIS
#   docpcode [-v] -i infile -o outfile
# 
#   DESCRIPTION
#
#   This tcl script extract pseudo code from the input file and write
#   it in the output file.
#   
#   It is considered "pseudo code":
#      - any comment line starting with //#=
#      - any comment line starting with /*#= and terminated with */
#      - any set of lines delimited by a comment starting with /*#+ and 
#        by a comment ending with #-*/
#      - any set of lines delimited by a comment starting with //#+ and 
#        by a comment ending with //#-
#
#   Pseudo code identifier can be nested
#
#   FILES
#
#   ENVIRONMENT
#
#   RETURN VALUES
#
#   CAUTIONS
#
#   Future versions should have a "better brain" and be able to recognize
#   and filter automatically out function delimiters and flow control
#   structures
#
#   Error handling is reduced to the minimum
#
#   EXAMPLES
#
#   SEE ALSO
#
#   BUGS     
#
#------------------------------------------------------------------------
#


########################################
# Default values and global variables

# General variables

# Used for verbose mode
set verbose      0

#
#####   END DEFAULT AND GLOBAL VARIABLES   ############

#*******************************************************************
# proc printLog {str}         prints on standard output the given string
#                             properly formatted
# proc printLogVerbose {str}  like printLog, but prints only if verbose == 1
#
#*******************************************************************
proc printLog {str} {
    puts "$str"
}

proc printLogVerbose {str} {
    global verbose
    if {$verbose == 1} {
	puts "$str"
    }
}

#*******************************************************************
# proc getpcode
#
# Procedure to extract the pseudo code
#*******************************************************************

proc getpcode { source dest } {

    # Counter used to turn on pcode writing
    set counter   0
    set linecount -1
    set error     0

    # Check if the requested input file exist
    if {![file exists $source]} {
	printLog "Input file ($source) does not exists"
	exit 1
    }

    # Check if the requested output file can be opened

    # Open the input file for read
    set fd [open $source r]

    # Open the output file for write
    set rfd [open $dest w]

    # Loop over the lines in the file.
    # Each line correspond to a tes
    while { [gets $fd line] >= 0} {

	incr linecount

	# Start pcode in C comment
	if { [string first "*#+" $line] != -1 } {
	    incr counter
	}
	# Start pcode in C++ comment
	if { [string first "//#+" $line] != -1 } {
	    incr counter
	}

	# If pcode is active, write the line
	if { ($counter > 0)                      || 
	     ([string first "//#=" $line] != -1)  ||
	     ([regexp {/\*#=.*\*/} $line] != 0)   } {

            set newLine $line
	    regsub {\*#\+} $newLine "\*  " newLine
	    regsub {//#\+} $newLine "//  " newLine
	    regsub {#\-\*} $newLine "  \*" newLine
	    regsub {//#\-} $newLine "//  " newLine

	    regsub {//#\=} $newLine "//  " newLine
	    regsub {\*#\=} $newLine "\*  " newLine

	    regsub {^ *// *$} $newLine "" newLine

	    puts $rfd $newLine
	}

	# End pcode in C comment
	if { [string first "#-*" $line] != -1 } {
	    incr counter -1
	}
	# End pcode in C++ comment
	if { [string first "//#-" $line] != -1 } {
	    incr counter -1
	}

	# Check if there are matching errors
	if { $counter < 0 } {
	    printLog "$source:$linecount - Pseudo code control character not matched"; 
	    set counter 0
	    set error 1
	}

    }

    # Check if there are matching errors
    if { $counter > 0 } {
	printLog "$source:$linecount - Pseudo code control character not matched"; 
	set error 1
    }


    return $error
}


#*******************************************************************
# proc usage {}
#
# Procedure to print a short help for this program
#*******************************************************************

proc usage {} {
    global argv0
    printLog "$argv0 \[-v\] \[-i\] source_file \[-o\] dest_file"
    exit 1
}

############################################
# main body 
############################################

global argc argv0 argv

set source   ""
set dest     ""

#
# Parse command line options
#
while {![lempty $argv]} {
	set arg [lvarpop argv]
	switch -glob -- $arg {
		-i	{set source [lvarpop argv]}
		-i*	{set source [string range $arg 3 end]}
		-o      {set dest   [lvarpop argv]}
		-o*     {set dest   [string range $arg 3 end]}
	        -v      {set verbose 1 }
		default	{printLog "Invalid argument: [lindex $argv 0]"; 
		         usage; break}
	}
}

#
# Options -i and -o are mandatory
# 
if { $source == "" } { 
    printLog "The source file (-i option) is mandatory"; 
    usage; 
    break
}
if { $dest == "" } { 
    printLog "The destination file (-o option) is mandatory"; 
    usage; 
    break
}


printLogVerbose "Extract pseudo code from c or c++ source file"

# Extracts the pseudo code
set retVal [getpcode $source $dest]

# End of work
exit $retVal


####### END of script ############################################

#
# ___oOo___

#*******************************************************************************
# ALMA project
#
# "@(#) $Id: acsReplace.tcl,v 1.1 2004/04/15 08:24:28 gchiozzi Exp $"
#
# Based on VLT tooReplace2 utility
#

#************************************************************************
#   NAME
#   acsReplace - do pattern replacements in a set of files
# 
#   SYNOPSIS
#   acsReplace ?-nobackup? ?-q? <pattern> <replacement> <file1> ?<file2>? ...
# 
#   DESCRIPTION
#   acsReplace will searchs in all <files> for the <pattern> (a regular
#   expression), and replace this string by <replacement>. The original file
#   will be kept as <fileN>~, unless the -nobackup option has been specified.
#   As the pattern is found in a file, this is reported to stdout. The -q 
#   option suppresses this output.
#
#   FILES
#
#   ENVIRONMENT
#   no environment variables are accessed.
#
#   CAUTIONS
#   Have a proper look on the manpage of Tcl's regexp, before experimenting
#   with special characters in regular expressions.
#
#   EXAMPLES
#
#   SEE ALSO
#   regexp(n)
#------------------------------------------------------------------------
#
# Parse command line options (could override default)
set backup 1;		# back-up the original file by default
set files {}
set quiet 0
while {![lempty $argv]} {
    set arg [lvarpop argv]
    switch -glob -- $arg {
	-nobackup {set backup 0}
	-quiet {set quiet 1}
	-q {set quiet 1}
	-* {puts stderr "wrong option \"$arg\"; run acsReplace as"; \
		puts stderr "acsReplace ?-nobackup? <pattern> <replacement> <file1> ?<file2>? ..."; \
		exit 1}
	default	{lappend files $arg}
    }
}
# Remaining arguments are  <pattern>, <replacement> and <fileN>
if {[llength $files] < 3} {
    puts stderr "wrong number of arguments; run acsReplace as"
    puts stderr "acsReplace ?-nobackup? <pattern> <replacement> <file1> ?<file2>? ..."
    exit 1
}
    
set regExp [lvarpop files]
set replacement [lvarpop files]
set var(mode) ""

foreach file $files {
    if {![file isfile $file]} continue
    set mode [file stat $file var]
    set contents [read_file $file]
    if {[regsub -all -- $regExp $contents $replacement contents]} {
	# can only get here if string replacements took place
	if {$backup} {
	    file rename -force -- $file ${file}~
	}
	if {!$quiet} {
	    puts "Replacing strings in $file...."
	}
	if {!$backup && [catch {chmod u+w $file} msg]} {
	    puts stderr $msg
	}
	write_file $file $contents
	chmod $mode $file
    }
}

if {!$quiet} {
    puts "\nDone\n"
}
# ___oOo___ 

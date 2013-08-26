#*******************************************************************************
# ALMA project
#
# "@(#) $Id: acsReplace.tcl,v 1.2 2009/05/11 15:18:03 eallaert Exp $"
#
# Based on VLT tooReplace2 utility by eallaert
#

#************************************************************************
#   NAME
#   acsReplace - do pattern replacements in a set of files
# 
#   SYNOPSIS
#   acsReplace ?-nobackup? ?-nobinary? ?-quiet? ?--? <pattern> <replacement> \
#                 <file1> ?<file2>? ...
# 
#   DESCRIPTION
#   acsReplace will search in all <files> for the <pattern> (a regular
#   expression), and replace this string by <replacement>. The original file
#   will be kept as <fileN>~, unless the '-nobackup' option has been specified.
#   As the pattern is found in a file, this is reported to stdout. The '-quiet' 
#   option suppresses this output. 
#
#   This utility will print the list of files to be searched and the regular
#   expression to use, unless the '-quiet' option is active. Whithout this
#   option, and if stdin corresponds to a tty, it will also prompt the user
#   to proceed before actually starting to replace strings. Note that when
#   e.g. a file-list is piped into this utility, stdin will not be a tty.
#
#   If the '-nobinary' option is specified, replacements of files containing
#   non-printable chars (i.e. bytes in the class [\x00-\x08\x0b\x0e-\x1f]) 
#   will be skipped, and this will be reported on stdout.
#
#   FILES
#
#   ENVIRONMENT
#   no environment variables are accessed.
#
#   CAUTIONS
#   Have a proper look on the manpage of Tcl's regexp, before experimenting
#   with special characters in regular expressions.
#   Exercising the string command 'regexp' on binary data may modify such data
#   in strange ways. Hence using this utility on binary files may produce 
#   unexpected results. Use the '-nobinary' option to let acsReplace skip
#   these files.
#
#   EXAMPLES
#   Replace all occurences of HIM by ME in all files in the current directory:
#       acsReplace -nobackup HIM ME *
#
#   Replace all occurences of HIM by ME in all files in the current directory
#   and it sibblings:
#       find . | xargs acsReplace -nobackup HIM ME 
#   Note that in this latter example, acsReplace will not prompt the user
#   before starting with the replacements, as stdin is not a tty.
#
#   SEE ALSO
#   regexp(n), re_syntax(n)
#------------------------------------------------------------------------
#
# Parse command line options (could override default)
# Parse command line options (could override default)
set backup 1;		# back-up the original file by default
set files {}
set quiet 0
set binary 1;		# permit non-printable chars by default

while {![lempty $argv]} {
    set arg [lvarpop argv]
    switch -glob -- $arg {
	-nobackup {set backup 0}
	-nobinary {set binary 0}
	-q - -quiet {set quiet 1}
        -- {set files $argv; break}
	-* {
	    puts stderr "wrong option \"$arg\"; run acsReplace as";
	    puts stderr "acsReplace ?-nobackup? ?-nobinary? ?-quiet? ?--? <pattern> <replacement> <file1> ?<file2>? ...";
	    exit 1
	}
	default	{set files [linsert $argv 0 $arg]; break}
    }
}
# Remaining arguments are  <pattern>, <replacement> and <fileN>
if {[llength $files] < 3} {
    puts stderr "wrong number of arguments; run acsReplace as"
    puts stderr "acsReplace ?-nobackup? ?-nobinary? ?-quiet? ?--? <pattern> <replacement> <file1> ?<file2>? ..."
    exit 1
}
    
set regExp [lvarpop files]
set replacement [lvarpop files]

if {!$quiet} {
    puts ""
    puts "file(s) to search:\n   $files"
    puts ""
    puts "regexp to match (see re_syntax(n) manpage for syntax rules):\n   \"$regExp\""
    puts ""
    puts "replacement string:\n   \"$replacement\""
    puts ""
    if {[fstat stdin tty]} {
	puts -nonewline {OK to proceed? [y] }
	flush stdout
	if {[gets stdin answer] != 0 && ![string match y* $answer]} {
	    puts ""
	    puts ""
	    puts "Quitting on user's request"
	    exit 1
	} else {
	    puts ""
	}
    }
}

set var(mode) ""
# class of "characters" that usually only appear in binary files
set bin_rx {[\x00-\x08\x0b\x0e-\x1f]}

foreach file $files {
    if {![file isfile $file]} continue
    set contents [read_file $file]
    if {!$binary && [regexp $bin_rx $contents]} {
	if {!$quiet} {
	    puts "Skipping binary file $file"
	}
	continue
    }
    if {[regsub -all -- $regExp $contents $replacement contents]} {
	# can only get here if string replacements took place
	file stat $file var
	if {$backup} {
	    file rename -force -- $file ${file}~
	}
	if {!$quiet} {
	    puts "Replacing strings in $file...."
	}
	if {!$backup && [catch {chmod u+w $file} msg]} {
	    puts stderr $msg
	}
	# Don't use "write_file" as that would append a trailing newline.
	set fp [open $file w]
	puts -nonewline $fp $contents
	close $fp
	chmod $var(mode) $file
    }
}

if {!$quiet} {
    puts "\nDone\n"
}
# ___oOo___ 

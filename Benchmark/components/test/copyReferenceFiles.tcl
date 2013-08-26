#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: copyReferenceFiles.tcl,v 1.1 2007/08/28 15:52:33 eallaert Exp $"
#
# who       when        what
# --------  ----------  ----------------------------------------------
# eallaert  2007-08-26  original version

# Deal with the reference files: copy the performance files for this
# specific host over to the generic names used by this test.
set host [info hostname]
if {$gv(generate)} {
    puts ""
    puts "To update the performance reference file(s) for $host, run"
    puts "   tat <language>LoggingBenchmark"
    puts "and then"
    puts "   cp tmp/<language>Report.csv ref/${host}_<language>Performance.ref"
    puts ""
} else {
    foreach language {cpp java python} {
	if {[file isfile ref/${host}_${language}Performance.ref]} {
	    file copy -force ref/${host}_${language}Performance.ref ref/${language}Performance.ref 
	} else {
	    puts ""
	    puts "The file ref ${host}_${language}Performance.ref does not exist."
	    puts "Please execute the following commands:"
	    puts "   tat -g ${language}LoggingBenchmark"
	    puts "   cp tmp/${language}Report.csv ref/${host}_${language}Performance.ref"
	    puts "and then try again."
	    puts ""
	}
    }
}

#
# ___oOo___

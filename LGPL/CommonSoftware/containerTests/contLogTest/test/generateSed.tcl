#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: generateSed.tcl,v 1.2 2008/05/19 11:15:07 eallaert Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# 

# Simple script (i.e. without error checking) to create an sed
# command-file which will replace pathnames by their corresponding 
# environment variable. To be used with tat, so that the reference 
# file is not dependent on a particular user or from which directory
# (s)he is running the tat-test.
set baseName [lindex $argv 0]

set fp [open ${baseName}.sed w]

set fqhostname [info hostname];			# fully qualified hostname
set hostname [lindex [split $fqhostname .] 0];	# basename only

puts $fp "s|$env(ACSROOT)|<ACSROOT>|g"
puts $fp "s|$env(INTROOT)|<INTROOT>|g"
puts $fp "s|$env(ACSDATA)|<ACSDATA>|g"
puts $fp "s|[pwd]|<pwd>|g"
puts $fp "s|$env(HOME)|<HOME>|g"
puts $fp "s|$fqhostname|<HOSTNAME>|g"
puts $fp "s|$hostname|<HOSTNAME>|g"

close $fp
#
# ___oOo___

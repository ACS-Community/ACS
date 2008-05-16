#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: generateSed.tcl,v 1.1 2008/05/16 12:41:02 eallaert Exp $"
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

puts $fp "s|$env(ACSROOT)|<ACSROOT>|g"
puts $fp "s|$env(INTROOT)|<INTROOT>|g"
puts $fp "s|$env(ACSDATA)|<ACSDATA>|g"
puts $fp "s|[pwd]|<pwd>|g"
puts $fp "s|$env(HOME)|<HOME>|g"
puts $fp "s|$env(HOSTNAME)|<HOSTNAME>|g"

close $fp
#
# ___oOo___

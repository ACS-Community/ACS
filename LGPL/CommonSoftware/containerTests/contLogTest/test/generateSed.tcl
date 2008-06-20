#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: generateSed.tcl,v 1.3 2008/06/20 08:47:15 eallaert Exp $"
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

# Under NRI there is INTROOT and ACSROOT point to the same dir, i.e. replace both env vars by generic name
puts $fp "s|$env(ACSROOT)|<ACS-/INT-ROOT>|g"
puts $fp "s|$env(INTROOT)|<ACS-/INT-ROOT>|g"
# When both INTROOT and ACSROOT exist and are the same (e.g. NRI), this directory appears twice for the
# "endorsed jar files"
puts $fp "s|-Djava.endorsed.dirs=<ACS-/INT-ROOT>/lib/endorsed:<ACS-/INT-ROOT>/lib/endorsed:|-Djava.endorsed.dirs=<ACS-/INT-ROOT>/lib/endorsed:|g"
puts $fp "s|$env(ACSDATA)|<ACSDATA>|g"
puts $fp "s|[pwd]|<pwd>|g"
puts $fp "s|$env(HOME)|<HOME>|g"
puts $fp "s|$fqhostname|<HOSTNAME>|g"
puts $fp "s|$hostname|<HOSTNAME>|g"

close $fp
#
# ___oOo___

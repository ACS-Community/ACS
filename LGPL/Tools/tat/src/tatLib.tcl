#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatLib.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  08/05/96  created
#

#************************************************************************
#   NAME
# 
#   SYNOPSIS
# 
#   DESCRIPTION
#
#   FILES
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
#   BUGS     
#
#------------------------------------------------------------------------
#

proc tatPuts { msg } {
    
    global env

    if { [catch { set verbose $env(TAT_VERBOSE) }] == 0 } {
	puts $msg
    } 

# puts -nonewline "."

}


#
# ___oOo___

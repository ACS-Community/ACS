#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatGetClock.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  13/10/95  created
#
#************************************************************************
#   NAME
#
#   tatGetClock -
# 
#   SYNOPSIS
#
#   tatGetClock
# 
#   DESCRIPTION
#
#   Implement tclX getclock for expect script tatWaitForCLU.
#   expect is built with tcl, not tclX.
#   CAUTION
#
#   BUGS     
#
#------------------------------------------------------------------------
#

puts [getclock]

#
# ___oOo___

#! /bin/bash
#*******************************************************************************
# ALMA - Atacama Large Millimiter Array
#
# "@(#) $Id: acsexmplFridgeOpenClose.sh,v 1.80 2004/01/25 03:11:00 dfugate Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# almadev 2002-01-11 created
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

# signal trap (if any)

acsexmplClientFridgeCmd FRIDGE1 OPEN
sleep 3
acsexmplClientFridgeCmd FRIDGE1 CLOSE
sleep 3

#
# ___oOo___

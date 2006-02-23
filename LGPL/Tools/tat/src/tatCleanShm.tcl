#************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatCleanShm.tcl,v 1.79 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# pforstma  18/07/95  created
#

#************************************************************************
#   NAME
#  
#   tatCleanShm -
# 
#   SYNOPSIS
#
#   tatCleanShm
#
#   DESCRIPTION
#
#   deletes all the shared memory not used any more and 
#   owned by the current user.
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

set ShmIdList [exec ipcs -m |  awk " { print \$2 } "  ]

foreach shmId $ShmIdList {
    catch {exec ipcrm -m $shmId}
}

exit 0

#
# ___oOo___

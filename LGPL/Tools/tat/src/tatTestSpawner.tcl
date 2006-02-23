#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: tatTestSpawner.tcl,v 1.3 2004/03/16 08:29:41 psivera Exp $"
#
# who       when      what
# --------  --------  ----------------------------------------------
# gchiozzi  12/07/95  created
#

#************************************************************************
#   NAME
#   tatTestSpawner - Prog used by TestDriver to spawn bg processes
#
#   SYNOPSIS
#   tatTestSpawner arg .....
# 
#   DESCRIPTION
#   This program creates a new process group id, so that all its descendent
#   processes will be in this process group id.
#   In this way it is easy to kill all the generated child processes.
#   
#   All the given arguments are considered as a command string to be esecuted
#   as a background task.
#
#   When the command has been spawned, the process esit with SUCCESS (0)
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
#   tatTestDriver(1)
#
#   BUGS     
#
#------------------------------------------------------------------------
#

id process group set

eval exec $argv &

exit 0

#
# ___oOo___

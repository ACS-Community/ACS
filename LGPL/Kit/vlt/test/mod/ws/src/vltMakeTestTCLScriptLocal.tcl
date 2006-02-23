#! /bin/sh
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: vltMakeTestTCLScriptLocal.tcl,v 1.1.1.1 2003/02/20 10:44:07 mzampare Exp $" 
#
# who       when      what
# --------  --------  ----------------------------------------------
# gfilippi  20/10/94  created
#

#************************************************************************
#   NAME
#   vltMakeTestTCLScriptLocal - dummy TCL script
# 
#   SYNOPSIS
# 
#   DESCRIPTION
#
#   This program is part of the vltMake modular test package
#
#------------------------------------------------------------------------

if { [vltMakeTestTCLProcedure1] == "Procedure1" && \
     [vltMakeTestTCLProcedure2] == "Procedure2" }\
    {\
    puts "vltMakeTestTCLScript - PASS"
    } \
else \
    {
    puts "vltMakeTestTCLScript - FAIL"
    }
    
exit 0
#___oOo___

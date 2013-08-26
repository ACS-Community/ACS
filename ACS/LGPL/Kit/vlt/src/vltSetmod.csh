#! /bin/csh
#  ^^^^^^^^ !!!!!! This is a C-shell script
#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: vltSetmod.csh,v 1.1.1.1 2003/02/20 10:44:07 mzampare Exp $" 
#
# who       when      what
# --------  --------  ----------------------------------------------
# gfilippi  11/05/93  created
# gfilippi  23/11/93  support Shared Libraries (SPR970213)
# gfilippi  03/13/93  reset old-style output to prevent interferences with tat
#

#************************************************************************
#   NAME
#   setmod - set path and some environmental variables to the current module
# 
#   SYNOPSIS
#        setmod 
# 
#
#   DESCRIPTION
#   The utility sets the MODROOT variable to the current directory
#   and adds the MODROOT/bin directory to PATH and MODROOT/lib
#   to SHLIB_PATH (HP) or LD_LIBRARY_PATH (Sun).
#
#   It is intended to be useful in the development of a module.
#   Some other utilities, like modXman and modMan, and the Makefile  
#   use the MODROOT variable to define the scope of their activity.
#   
#   To use it, first set your directory to the root of the module that you
#   want to have as the current module (i.e., the one on which commands are
#   executed, then issue the command. 
#    
#
#   ENVIRONMENT
#   MODROOT        <write>   the module root directory 
#   PATH           <update>  add MODROOT/bin to the current PATH 
#   OLDPATH        <write>   the value of PATH before setman 
#   SHLIB_PATH     <update>  add MODROOT/lib to the current SHLIB_PATH 
#   OLDSHLIB_PATH  <write>   the value of SHLIB_PATH before setman 
#   LD_LIBRARY_PATH     <update>  add MODROOT/lib to the current LD_LIBRARY_PATH 
#   OLDLD_LIBRARY_PATH  <write>   the value of LD_LIBRARY_PATH before setman 
#
#
#   RETURN VALUES
#   Always success.
#
#
#   EXAMPLES
#          example% cd $HOME/development/modx
#          example% setmod
#          example% echo $MODROOT
#          ......./development/modx
#          example% echo $PATH
#          ......./development/modx/bin:.......
#
#   BUGS    
#   setmod adds a new name each time it is used.
#   When used many times, the PATH becomes very long and can create 
#   problems in the precedence.
#   Improvement: if OLDPATH is defined, reset PATH to OLDPATH before 
#                adding MODROOT/bin. 
#
#
#----------------------------------------------------------------------

#
#----------------------------------------------------------------------
#  CAUTION!!!! This is a C-shell script.
#  It MUST be executed using:   source $VLTROOT/vltSetmod.csh
#----------------------------------------------------------------------
#         

#
# REMARK: because the setmod is used by tat, the output went in 
# the existing reference files. To avoid to have to change all
# reference file now, the old-style output has been re-established
# (the new output has been commented with #-->


setenv MODROOT $PWD
#--> echo "MODROOT defined as $MODROOT"

setenv OLDPATH $PATH
set    NEWPATH=$MODROOT/bin:$PATH
setenv PATH $NEWPATH
#--> echo "$MODROOT/bin added to PATH, previous PATH saved into OLDPATH"

if ( "`uname`" == "HP-UX" ) then
    setenv OLDSHLIB_PATH $SHLIB_PATH
    set    NEWPATH=$MODROOT/lib:$SHLIB_PATH
    setenv SHLIB_PATH $NEWPATH
#-->    echo "$MODROOT/lib added to SHLIB_PATH, previous SHLIB_PATH saved into OLDSHLIB_PATH"
else 
    setenv OLDLD_LIBRARY_PATH $LD_LIBRARY_PATH
    set    NEWPATH=$MODROOT/lib:$LD_LIBRARY_PATH
    setenv LD_LIBRARY_PATH $NEWPATH
#-->    echo "$MODROOT/lib added to LD_LIBRARY_PATH, previous LD_LIBRARY_PATH saved into OLDLD_LIBRARY__PATH"
endif

unset  NEW_NAME
    
echo ""
echo "MODROOT defined, MODROOT/bin added to PATH, previous PATH saved into OLDPATH"
#
# ___oOo___

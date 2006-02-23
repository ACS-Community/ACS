#
#*************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: vltMakeTestPanel2.pan,v 1.1.1.1 2003/02/20 10:44:07 mzampare Exp $" 
#
# vltMakeTestPanel2.pan
#
# This file has been produced by the panel editor
#   version: panel.tcl,v 2.35
#   date   : Tue Aug 22 08:48:38 MESZ 1995
#
#      !!!!!!!!!!  DO NOT MANUALLY EDIT THIS FILE  !!!!!!!!! 
#
#--------------------------------------------------------------
# 
#  Get directory root of VLT, integration, current module. 

global gvar  cv bcv actAreaHeight shArea panClassLib 
global panWidget panWidgetHlp panDbData panUsrProcs panUsrLib
global menu menuItem rootMenu ckMenuBar ckActBut ckShHelp ckCCS
global argv0 auto_path env VLTROOT INTROOT HOME 

if {[catch {set VLTROOT $env(VLTROOT)}]} {set VLTROOT undefined}
if {[catch {set INTROOT $env(INTROOT)}]} {set INTROOT undefined}
if {[catch {set HOME    $env(HOME)}   ]} {set HOME    undefined}
# 
# Set auto_path 
# 

if { [file exist ../lib/libpanPublic.tcl] } { 
   lappend auto_path ../lib/libpanPublic.tcl
} elseif { [file exist $INTROOT/lib/libpanPublic.tcl ] } { 
   lappend auto_path $INTROOT/lib/libpanPublic.tcl
} elseif { [file exist $VLTROOT/lib/libpanPublic.tcl ] } { 
   lappend auto_path $VLTROOT/lib/libpanPublic.tcl 
} else {
   puts stderr "panel EXITS : Tcl library libpanPublic.tcl not found " 
   exit 1
}

#
# Link panel to user libraries 
#

set panUsrLib   {}  
set panClassLib {}

panSetAutoPath  $panUsrLib $panClassLib 

if {[info exists EDITING]} { 
   set panRoot .vltMakeTestPanel2 
   set panUsrProcs {xxxInit xxxExit a}
   set argc 0 ; set argv {}
} else {
   set par(MY_NAME) [file tail $argv0] 
   set colordef winter
   set EDITING 0
   panReadOption
   set tk_strictMotif 0
   setPublicVar
   set panRoot . 
}

#
#-----------------------------------------------
#            Widget Definition  
#-----------------------------------------------
# 

set ckShHelp  1 ;  # Set Short Help
set ckActBut  0 ;  # Set Action Button Area
set ckMenuBar 1 ;  # Setup Menu Bar
set ckCCS     1 ;  # Initialize CCS
set actAreaHeight 5 ;  # Set height of action area 

panCreateFrame vltMakeTestPanel2 198 443;  # Create empty frame
panGetCvNames  vltMakeTestPanel2 cv bcv; # Get Canvas Names 

UifButton          $cv.vltMakeTestPanel2-wdg1 -popup 1 \
                   -stateVar gvar(dummy_panel-wdg1) -width 10 -state normal \
                   -command {panCallPanel   ./panel.pan "caio(1)=77" "pippo=20"} \
                   -canvasAnchor nw

#
#  Canvas Items
#


#
# Set Widget Configuration
#
set panWidget(vltMakeTestPanel2-wdg1)  {UifButton 20 20 1 0 0 0 0 dummy_panel-wdg1 0 {}}

#
# Set Database Configuration
#

#
# Set Short Help for each widget
#

#
#-----------------------------------------------
#           Menu Bar Definition  
#-----------------------------------------------
# 

set sepCounter 0
set rootMenu   { file help }

#
# Sub-Menu definition
#
set menu(file)  { File  1 0 {quit} }
set menu(help)  { Help  1 0 {extended_help }}

#
# Define menu buttons 
#
set menuItem(file.quit)  { {Quit}           1 cmd  {panClosePanel} }
set menuItem(help.extended_help)  { {Extended Help}  1 cmd  {puts "Not Implemented"} }

#
#-----------------------------------------------
#            Create the Panel  
#-----------------------------------------------
#

wm geometry  $panRoot 200x133
wm title     $panRoot "xxx"
set par(TITLE) "xxx" 

panMainStart  vltMakeTestPanel2 $argc $argv

#--------------------    End of Main   --------------------



#####################################################
#           Definition of Local Procedures
#####################################################


proc xxxInit { } {   

 # Declare global variables 

   global <var> 

 # User Initialization

   set <var> <value> ; # comment ..  
}


proc xxxExit { } {   

 # Declare global variables 

   global <var> 

 # Statements to be executed when exiting

    
}


proc a { } {  

   set list "caio(1)=77"
   append list "pippo=20"

   panCallPanel ./panel.pan $list
}

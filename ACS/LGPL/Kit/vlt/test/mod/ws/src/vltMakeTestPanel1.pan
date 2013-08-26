#!/usr/local/bin/sequencer -f
#*************************************************************
# E.S.O. - VLT project
#
# xxx.pan
#
# This file has been produced by the panel editor
#   version: panel - prototype version - 27 Sept 1994
#   date   : Fri Feb 17 17:00:33 MEZ 1995
#
#      !!!!!!!!!!  DO NOT MANUALLY EDIT THIS FILE  !!!!!!!!! 
#
#--------------------------------------------------------------
# 
#  Get directory root of VLT, integration, current module. 
if {[catch {set VLTROOT $env(VLTROOT)}]} {set VLTROOT undefined} 
if {[catch {set INTROOT $env(INTROOT)}]} {set INTROOT undefined} 
if {[catch {set HOME    $env(HOME)}   ]} {set HOME    undefined} 

global auto_path 
global help_message hotSpot widgetDescr textList maxid release
global menu menuItem rootMenu ckMenuBar ckActBut ckShHelp
global gvar varList dbInterface 

# 
# Set auto_path for  : libpanPrivate.tcl 
# 
if {[file exist ../lib/libpanPrivate.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 ../lib/libpanPrivate.tcl ] 
} elseif {[file exist $INTROOT/lib/libpanPrivate.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $INTROOT/lib/libpanPrivate.tcl ] 
} elseif {[file exist $VLTROOT/lib/libpanPrivate.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $VLTROOT/lib/libpanPrivate.tcl ] 
} else {
    puts stderr  "panel - ERROR:"; 
    puts stderr  "   Tcl library libpanPrivate.tcl not found. Unable to continue."; 
    puts stderr  "";
    exit 1;
}
# 
# Set auto_path for  : libpanPublic.tcl 
# 
if {[file exist ../lib/libpanPublic.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 ../lib/libpanPublic.tcl ] 
} elseif {[file exist $INTROOT/lib/libpanPublic.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $INTROOT/lib/libpanPublic.tcl ] 
} elseif {[file exist $VLTROOT/lib/libpanPublic.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $VLTROOT/lib/libpanPublic.tcl ] 
} else {
    puts stderr  "panel - ERROR:"; 
    puts stderr  "   Tcl library libpanPublic.tcl not found. Unable to continue."; 
    puts stderr  "";
    exit 1;
}
# 
# Set auto_path for  : libuif.tcl 
# 
if {[file exist ../lib/libuif.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 ../lib/libuif.tcl ] 
} elseif {[file exist $INTROOT/lib/libuif.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $INTROOT/lib/libuif.tcl ] 
} elseif {[file exist $VLTROOT/lib/libuif.tcl ] == 1} { 
    set auto_path [linsert $auto_path 0 $VLTROOT/lib/libuif.tcl ] 
} else {
    puts stderr  "panel - ERROR:"; 
    puts stderr  "   Tcl library libuif.tcl not found. Unable to continue."; 
    puts stderr  "";
    exit 1;
}
set release 1.2
if {![info exists EDITING]} {
   set par(MY_NAME) [file tail [info script]]
   set colordef bw
   set EDITING 0
   readoption
   set tk_strictMotif 0
   setPublicVar
   set cv .panel.top.wdg.can 
   set bcv .panel.bot.actBut.can
   trace variable gvar  w  varCallBack
}

#
#-----------------------------------------------
#            Widget Definition  
#-----------------------------------------------
# 

createEmptyPanel  $EDITING;     # Create empty frame 


set maxid 1

#
# Set Widget Configuration
#

#
# Set Database Configuration
#

#
# Set widget data variables
#

#
# Set Data variables used by the widgets
#

#
# Set List TCL commands for each widget
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
set rootMenu   { file  help }

#
# Sub-Menu definition
#
set menu(file)  { File  1 0 {quit} }
set menu(help)  { Help  1 0 {extended_help }    }

#
# Define menu buttons
#
set menuItem(help.extended_help)   { {Extended Help}  1 cmd  {puts "Not Implemented"} }
set menuItem(file.quit)   { {Quit}           1 cmd  {closePanel} }

#
#-----------------------------------------------
#            Create the Panel  
#-----------------------------------------------
#

loadWidget  

set ckActBut  0 ;                      # Set Action Area

set ckShHelp  1 ;                      # Set Short Help
set shArea    .panel.bot.sh

MakeShortHelpArea $shArea {top fillx} 

loadShortHelp

set ckMenuBar 1 ;                      # Setup Menu bar

loadMenuBar

setClassBinding ; # Set General Bindings 

#
# Define panel geometry
#

if {$EDITING} {
    wm geometry .panel 250x150+101+96
    wm title    .panel "Dummy"
    set par(TITLE) "Dummy"
} else {
    wm geometry . 250x150+101+96
    wm title . "Dummy"
}
update idletasks
set pollRate 3000 
if { [startPanel "3000" ] == 0 } { closePanel }
update idletasks

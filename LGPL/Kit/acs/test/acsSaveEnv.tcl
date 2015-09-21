# -*- tcl -*-
# Create our own namespace
namespace eval ::acsMakefile {
    foreach var {INTROOT INTLIST ACSROOT MODPATH} {
	if {[info exists ::env($var)]} {
	    set $var $::env($var)
	}
    }
}

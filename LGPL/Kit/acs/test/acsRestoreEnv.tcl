# -*- tcl -*-
namespace eval ::acsMakefile {
    foreach var {INTROOT INTLIST ACSROOT MODPATH} {
	if {[info exists $var]} {
	    set ::env($var) [set $var]
	} else {
	    catch {unset ::env($var)}
	}
    }
}

# -*- tcl -*-
namespace eval ::acsMakefile {
    variable savedVars
    if {[array exists savedVars]} {
        # The env vars have been saved before, so restore them now
        foreach var {INTROOT INTLIST ACSROOT} {
            catch {unset ::$var}
            catch {unset ::env($var)}
	}
	foreach {var val} [array get savedVars] {
	    set $var $val
	}
    } else {
        # The env vars have not been saved before, so save them now
	foreach var {INTROOT INTLIST ACSROOT MODPATH} {
	    if {[info exists ::env($var)]} {
		array set savedVars [list ::env($var) $::env($var)]
	    }
	    if {[info exists ::$var] && [set ::$var] == "undefined"} {
		# tat has the bad habit of setting global vars INTROOT, INTLIST and ACSROOT
                # to "undefined" if the corresponding env var does not exist; it will then
                # pass these global vars on to make - which will barf on the non-existing 
                # directory "undefined". So better undo this tat behaviour.
		unset ::$var
	    }
	    if {[info exists ::$var]} {
		array set savedVars [list ::$var [set ::$var]]
	    }
	}
    }
}

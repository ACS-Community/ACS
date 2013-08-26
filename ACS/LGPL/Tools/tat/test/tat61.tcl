#
# "@(#) $Id"
#
# tat61.tcl
#
# no output => no adress or directory name hard-coded
log_user 0
set LCU $env(LCU)
spawn rlogin $LCU
expect -gl ->
send "tatVxTest\r"
expect { 
         "*hello*" { puts "tat61.tcl OK" }
         "*undefined*" { puts "tatVxTest failed (not loaded) \n" }
         default   { puts "tatVxTest failed (timeout/eof) \n" }
       }
close






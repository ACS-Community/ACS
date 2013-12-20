s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/user='.*'/user=xxxx/g
s/\(1 - \)[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\(  INFO - publishing alarm change\)/\1xx:xx:xx\2/g
s/\(elapsed time in ms to send [0-9]* [A-Z]* logs from component TestcompJava[A-Z,a-z]*: \)[0-9.]*/\1xxxx/g

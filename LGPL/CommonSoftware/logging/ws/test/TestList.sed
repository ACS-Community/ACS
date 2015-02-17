s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/Cache saved to '[^']*'/Cache saved to '.\/log_cache.dat__XXXXXXXX_XXX'/g
s/ Function took [0-9]*\.[0-9]* sec//g
s/_[0-9]*:/_XXXXX:/g
s/during last [.,0-9]* seconds = [.,0-9]*/during last X seconds = X/g
s/Increment of logging errors from last period: -nan%/Increment of logging errors from last period: nan%/g
s|/.*/logging/ws/test/tmp|\<logging/ws/test\>/tmp|g
s/Routine="" Host=".*"/Routine="" Host="<host>"/g
/acs_tmp/!{s|Log file created = .*/tmp/[^\/]*/ACS|Log file created = tmp/<host>/ACS|}
/acs_tmp/!{s|Log file created = .*/tmp/[^\/]*/acs|Log file created = tmp/<host>/acs|}

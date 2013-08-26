s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/log_cache\.dat_[0-9,a-z,A-Z]*_[0-9]*/log_cache.dat__XXXXXXXX_XXX/g
s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/-classpath .* -Dabeans.config/-classpath ..... -Dabeans.config/g
s/\(0x[a-z,A-Z,0-9]*\)/xxxxxxx/g
s/\([0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]\)/12:00:00.000/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]/----------T--:--/g
s/user='.*'/user=xxxx/g
s/\.Jlog will use cache for [0-9]* log records\./Jlog will use cache for N log records/g
s/LogMultiFileCache uses file of max size: [0-9]+/LogMultiFileCache uses file of max size: N/g
s/\The WriteBuffer stores [0-9]* logs//g
s/Local file logger: Cache saved to .*/Local file logger: Cache saved to XXXXXX/g
s/Line="[0-9]*"/Line="NNN"/g
s/Line: [0-9]*/Line: NNN/g
s/Thread="[0-9]*"/Thread="NNN"/g
s/Thread: [0-9]*.*/Thread: NNN/g
s/LogId="[0-9]*"/ LogId="NNN"/g
s/Log ID: [0-9]*/Log ID: NNN/g
s/Host: .*/Host: hostname/g
s/Host=".*"/Host="hostname"/g
s/Time: .*/Time: xxx/g
s/LoggingChannelBin/LogChannel_Name/g
s/LoggingChannel/LogChannel_Name/g
s/Manager login done.*/Manager login donee, handle 'xxxxxxxxxx' obtained./g
s/.XML log record import finished with [0-9]* records in [0-9]* seconds/.XML log record import finished with nnnn records in xxx seconds/g

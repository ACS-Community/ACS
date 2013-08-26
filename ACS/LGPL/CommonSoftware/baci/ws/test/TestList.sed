s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/IOR:[a-z,0-9,.,]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Handle: [0-9]*/Handle: xxxxxxxxx/g
s/ProcessID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/ThreadID=\"[0-9]*\"/ThreadID=xxxxxxxxx/g
s/v[0-9]*\.[0-9]*\.[0-9]*.* built at [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/v-.-.- built at ---------- --:--:--/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/\/tmp\/acs_local_log_\([a-z,A-Z,0-9]*\)_[0-9]*/\/tmp\/acs_local_log_\1_xxxx/g
s/Starting thread [a-z,A-Z,0-9,.,\',\ ]* \[[0-9]*\]/Starting thread 'xxxx' [xxxx]/g
s/Stopping thread \[[0-9]*\]/Stopping thread [xxxx]/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/value = [0-9]* = 0x[0-9,a-f]*/value = xx/g
s/Cache saved to '.*'/Cache saved to '----'/g
s/Recovery filename: '.*'/Recovery filename: '----'/g
s/log_cache\.dat_[0-9,a-z,A-Z]*_[0-9]*/log_cache.dat__XXXXXXXX_XXX/g
s/HostName:   \"[a-z,A-Z,0-9,.]*\"/HostName:   xxxxxxxxx/g
s/Thread:     \"[a-z,A-Z,0-9]*: [0-9]*\"/Thread:     xxxxxxxxx/g
s/TimeStamp:  \"[0-9, ]*\"/TimeStamp:  xxxxxxxxx/g
s/PID: [0-9]*, ID: [0-9]* @ [a-z,A-Z,0-9,.]*/PID: ----, ID: ---- @ -----/g
s/corbaloc::[a-z,A-Z,0-9,_,.,-]*:[0-9]*/corbaloc:HOST:PORT/g
s/:[0-9]* in /:xxx in /g
s/BACI1::monitorThread/BACI1::xxxxxxThread/g
s/BACI2::monitorThread/BACI2::xxxxxxThread/g
s/BACI1::actionThread/BACI1::xxxxxxThread/g
s/BACI2::actionThread/BACI2::xxxxxxThread/g

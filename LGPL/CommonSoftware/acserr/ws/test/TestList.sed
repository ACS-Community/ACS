s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]*/----------T--:--:--.---/g
s/HostName:   \"[a-z,A-Z,0-9,.,_,-]*\"/HostName:   xxxxxxxxx/g
s/Thread:     \"[a-z,A-Z,0-9]*: [0-9]*\"/Thread:     xxxxxxxxx/g
s/TimeStamp:  \"[0-9, ]*\"/TimeStamp:  xxxxxxxxx/g
s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/Log outputed to the local file [/,.,a-z,A-Z,0-9,-,_]*/Log outputed to the local file xxxxxx/g
s/([0-9]*|[0-9]*) EXCEPTION/ (xxxxx|xxxx) EXCEPTION/g
s/dat__[0-9]*/dat__xxxxx/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/value = [0-9]* = 0x[0-9,a-f]*/value = xx/g
s/value = -1 = 0xffffffff = [a-z,A-Z,0-9,_,:,<,>,$,.]* + 0x[0-9,a-z]*/value = -1/g
s/HostName:[a-z,A-Z,0-9,.,_,-]*,/HostName:xxxxxxxxx/g
s/Thread [a-z,A-Z,0-9]*: [0-9]*,/Thread:xxxxxxxxx/g
s/Ran [0-9]* tests in [0-9].[0-9]*s/Ran xx tests in x.xxxs/g
s/Ran 1 test in [0-9].[0-9]*s/Ran xx test in x.xxxs/g
s/ Function took [0-9]*.[0-9]* sec//g

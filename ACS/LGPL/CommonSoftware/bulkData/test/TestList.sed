s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Cache saved to '*/Cache saved to '----'/g
s/IOR:[a-z,0-9,.,-,A-Z]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,-]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Handle: [0-9]*/Handle: xxxxxxxxx/g
s/handle [0-9]*/handle xxxxxxxxx/g
s/ProcessID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/ThreadID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/v[0-9]*\.[0-9]*\.[0-9]*.* built at [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/v-.-.- built at ---------- --:--:--/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/([0-9]*|[0-9]*)/(----|----)/g
s/Manager hostname generated using localhost address: '.*'/Manager hostname generated using localhost address: '---'/g
s/Manager hostname obtained via command line: '.*'/Manager hostname obtained via command line: '---'/g
s/iiop:\/\/[a-z,0-9,.,-]*:[0-9]*/iiop:\/\/---:----/g
s/corbaloc::[a-z,0-9,.,-]*:[0-9]*/corbaloc::---:----/g
s/Using DLL path: .*/Using DLL path: ...../g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/lib\/\(lib[a-z,A-Z,0-9,.,_,-]*\.so\)/\/----\/\1/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/bin\/\([a-z,A-Z,0-9,.,_,-]*\)/\/----\/bin\/\1/g
s/\.\.\/lib\(\/lib[a-z,A-Z,0-9,.,_,-]*\)/\/----\1/g
s/\.\.\/bin\(\/[a-z,A-Z,0-9,.,_,-]*\)/\/----\1/g
s/\/acsdata\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\/acsdata\/tmp\/acs_local_log_\1_xxxx/g
s/Cancelling: [0-9]*/Cancelling: xxxxxx/g
s/Heartbeat check requested for: [0-9]*/Heartbeat check requested for: xxxxx/g
s/Client with handle [0-9]* not respoding/Client with handle xxxxx not respoding/g
s/Registering client with handle: [0-9]*/Registering client with handle: xxxxx/g
s/CDB\: '[a-z,A-Z,0-9,-]*'/CDB\: 'WS'/g
s/spawn rlogin [a-z,A-Z,0-9,-]*/rlogin LCU/g
s/l[a-z,A-Z,0-9,-]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/value = [0-9]* = 0x[0-9,a-f]*/value = xx/g
s/value = -1 = 0xffffffff = [a-z,A-Z,0-9,_,:,<,>,$,.,-]* + 0x[0-9,a-z]*/value = -1/g
s/-ORBEndpoint iiop\:\/\/[a-z,A-Z,0-9,-]*/-ORBEndpoint iiop\:\/\/LCU/g
s/log_cache.dat_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\log_cache.dat_\1_xxxx/g
s/Invocation timeout set to [0-9, m, s]*/Invocation timeout set to xxxx /g
s/invocation timeout set to [0-9, m, s]*/invocation timeout set to xxxx /g
s/via environment: '[a-z,A-Z,0-9,-]*'/via environment: 'xxx'/g
s/Interrupted system call/Interrupted system call or resource temporarily unavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily unavailable/g
s/No such file or directory/Interrupted system call or resource temporarily unavailable/g
s/log_cache\.dat_[0-9,a-z,A-Z]*_[0-9]*/log_cache.dat__XXXXXXXX_XXX/g
s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/-classpath .* -Dabeans.config/-classpath ..... -Dabeans.config/g
s/\(0x[a-z,A-Z,0-9]*\)/xxxxxxx/g
s/\([0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]\)/12:00:00.000/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]/----------T--:--/g
s/user='.*'/user=xxxx/g
s/Login OK, welcome [a-z,A-Z,0-9,_,-]*./Login OK, welcome xxxx./g
s/User [a-z,A-Z,0-9,_,-]* logged out./User xxxx logged out./g
s/desc.id_tag: [0-9,-]*/desc.id_tag: xxx/g
s/Component 'IDL.*_[0-9]*'/Component '<dynamic name>'/g

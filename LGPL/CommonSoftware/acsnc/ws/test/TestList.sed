s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,10\}/----------T--:--:--.---/g
s/IOR:[a-z,0-9,.,-]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,-]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Handle: [0-9]*/Handle: xxxxxxxxx/g
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
s/-ORBEndpoint iiop\:\/\/[a-z,A-Z,0-9,-]*/-ORBEndpoint iiop\:\/\/xxx/g
s/log_cache.dat_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\log_cache.dat_\1_xxxx/g
s/Invocation timeout set to [0-9, m, s]*/Invocation timeout set to xxxx /g
s/invocation timeout set to [0-9, m, s]*/invocation timeout set to xxxx /g
s/via environment: '[a-z,A-Z,0-9,-]*'/via environment: 'xxx'/g
s/event: [0-9].[0-9]* seconds./event: x.yz seconds./g
s/creation_thread_[0-9]/creation_thread_XX/g
s/creation_with_domain_thread_[0-9]/creation_with_domain_thread_XX/g

s/Endpoint: [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/Endpoint: X.X.X.X:PPPP/g
s/[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/X.X.X.X:PPPP/g
s/Manager hostname obtained via command line: '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*'/Manager hostname obtained via command line: 'X.X.X.X'/g
s/endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*/endpoint X.X.X.X/g
s/Sent [0-9]* events via SimpleSupplier/Sent X events via SimpleSupplier/g
s/[0-9]* events cannot be sent via SimpleSupplier/X events cannot be sent via SimpleSupplier/g

s/===  Number of events dropped: [0-9]*/===  Number of events dropped: XXX/g
s/===  Number of events sent: [0-9]*/===  Number of events sent: XXX/g
s/===  Number of events queued: [0-9]*/===  Number of events queued: XXX/g
s/===  Number of transitions: [0-9]/===  Number of transitions: Y/g
s/===  Transitions in: [0-9,]*/===  Transitions in: YYY/g
s/===  Number of exceptions caught: [0-9]*/===  Number of exceptions caught: ZZZ/g

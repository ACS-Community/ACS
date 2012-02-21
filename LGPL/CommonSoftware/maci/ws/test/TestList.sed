s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Cache saved to '.*'/Cache saved to '----'/g
s/IOR:[a-z,0-9,.,]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,_,-]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Handle: [a-z,0-9]*/Handle: xxxxxxxxx/g
s/ProcessID=\"[0-9]*\"/ProcessID=xxxxxxxxx/g
s/Line=\"[0-9]*\"/Line="xxxxxxxxx"/g
s/Host=\"[a-z,A-Z,0-9,.,_,-]*\"/Host="xxxxxxxxx"/g
s/LogId=\"[0-9]*\"/logId="xxxxxxxxx"/g
s/Thread=\"[a-z,A-Z,0-9,.,_,-]*\"/Thread="xxxxxxxxx"/g
s/ThreadID=\"[0-9]*\"/ThreadID=xxxxxxxxx/g
s/v[0-9]*\.[0-9]*\.[0-9]*.* built at [0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]/v-.-.- built at ---------- --:--:--/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/([0-9]*|[0-9]*)/(----|----)/g
s/Manager hostname generated using localhost address: '.*'/Manager hostname generated using localhost address: '---'/g
s/iiop:\/\/[a-z,0-9,.,,_,-]*:[0-9]*/iiop:\/\/------:----/g
s/corbaloc::[a-z,0-9,.,]*:[0-9]*/corbaloc::---:----/g
s/Using DLL path: .*/Using DLL path: ...../g

s/\/[a-z,A-Z,0-9,.,/,_,-]*\/lib\/\(lib[a-z,A-Z,0-9,.,_]*\.so\)/\/----\/\1/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/bin\/\([a-z,A-Z,0-9,.,_]*\)/\/----\/bin\/\1/g

s/\.\.\/lib\/\(lib[a-z,A-Z,0-9,.,_,-]*\)/\/----\/\1/g
s/\.\.\/bin\/\([a-z,A-Z,0-9,.,_,-]*\)/\/----\/bin\/\1/g


s/\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_]*\)_[0-9]*/\/tmp\/acs_local_log_\1_xxxx/g
s/log_cache.dat_\([a-z,A-Z,0-9,_]*\)_[0-9]*/log_cache.dat_\1_xxxx/g
s/Cancelling: [0-9]*/Cancelling: xxxxxx/g
s/Clients: [a-z,0-9]*/Clients: xxxxxx/g
s/Heartbeat check requested for: [0-9]*/Heartbeat check requested for: xxxxx/g
s/Client with handle [0-9]* not respoding/Client with handle xxxxx not respoding/g
s/Registering client with handle: [0-9]*/Registering client with handle: xxxxx/g
s/CDB\: '[a-z,A-Z,0-9]*'/CDB\: 'WS'/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/value = [0-9]* = 0x[0-9,a-f]*/value = xx/g
s/value = -1 = 0xffffffff = [a-z,A-Z,0-9,_,:,<,>,$,.]* + 0x[0-9,a-z]*/value = -1/g
s/Invocation timeout set to [0-9,m,s]*/Invocation timeout set to xxxx /g
s/via environment: '[a-z,A-Z,0-9]*'/via environment: 'xxx'/g
s/Client: [0-9]*/Client: XXXXXX/g
s/ConstructComponent([0-9,a-z,A-Z]*/ConstructComponent(XXXXXXX/g
s/Components: [0-9,a-z,A-Z]*/Components: XXXXX/g
s/container_logged_out: [0-9,a-z,A-Z]*/container_logged_out: XXXX/g
s/client_logged_out: [0-9,a-z,A-Z]*/client_logged_out: XXXX/g
s/[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\:[0-9]*/XXX.XXX.XXX.XXX:XXX/g
s/user='[a-z,A-Z,0-9]*'/user='XXXX'/g
s/host=[a-z,A-Z,0-9,.,-]*/host=XXXX/g
s/id=[0-9]*/id=XXXX/g
s/timestamp=[0-9,T,:,-]*/timestamp=XXXX/g
s/Login OK, welcome .*/Login OK, welcome XXXX./g
s/RequestProcessor-[0-9]*,[0-9]*,main/RequestProcessor-xx,xx,main/g
s/Manager activated with IOR:[0-9,A-Z,a-z]*/Manager activated with IOR: XXXXXXXXXXXXXXXXXX/g
s/Unable to save manager's PID because '[A-Z,a-z,0-9,/,_,.,-]*/Unable to save manager's PID because 'XXXX/g
s/handle [0-9]*/handle XXXXXX/g
s/Manager hostname obtained via command line: '[A-Z,a-z,0-9,/,_,.,-]*'/Manager hostname obtained via command line: 'XXXXX'/g
#s/'IDL\:alma[0-9].*'/'IDL:almaXXXX'/g
#s/IDL\:[0-9]*.*XXX/IDL<dynamic Component>/g
#s/I am IDL\:alma[0-9][0-9,a-z,A-Z,\.,_,\:]*/I am a Dynamic Component/g
s/\[IDL\:[^ ]* /\[IDL<Dynamic Component> /g
s/IDL\:alma[0-9,a-z,A-Z,\.,_,\:,-]*_[0-9]*/IDL<Dynamic Component>/g
s/\.java:[0-9]*/.java:xxx/g
s/Filename='[0-9,a-z,A-Z,\/,_,-]*\/CDB-WRONG\/CDB/Filename='xxx\/CDB-WRONG\/CDB/g


s/logName="LOG_CompAct_Corba_OK" TimeMillis="[0-9]\+"/logName="LOG_CompAct_Corba_OK" TimeMillis="nnn"/g
s/logName="LOG_CompAct_Init_OK" TimeMillis="[0-9]\+"/logName="LOG_CompAct_Init_OK" TimeMillis="nnn"/g
s/logName="LOG_CompAct_Instance_OK" TimeMillis="[0-9]\+"/logName="LOG_CompAct_Instance_OK" TimeMillis="nnn"/g
s/logName="LOG_CompAct_Loading_OK" TimeMillis="[0-9]\+"/logName="LOG_CompAct_Loading_OK" TimeMillis="nnn"/g

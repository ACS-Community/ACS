s/Logging to log file: .*\/acsdata\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_]*\)_[0-9]*/Logging to log file: \/-----\/acsdata\/tmp\/acs_local_log_\1_xxxx/g
s/MainThread, acspyTestLogging, [a-z,A-Z,0-9,-]*,/MainThread, acspyTestLogging, xxxx,/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g 
s/:[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9]/----------T--:--:--/g 
s/[0-9][0-9][0-9][0-9][\/][0-9][0-9][\/][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [a-z,A-Z,0-9,-]*/---------- --:--:-- username/g
s/acspyTestLogging.py,v [0-9]*.[0-9]*/acspyTestLogging.py,v x.x/g
s/get local manager from [a-z,A-Z,0-9,-]*/get local manager from xxxx/g
s/corbaloc::[a-z,A-Z,0-9,_,.,-]*:[0-9]*/corbaloc::xxxx:xxxx/g
s/$Id: .* Exp/-Id:  acspyTestLogging.py,v x.x ---------- --:--:-- username Exp/g
s/The temperature difference is [0-9,.]*/The temperature difference is x.x/g
s/TimeStamp=[0-9,a-z,A-Z,:, ]*,/TimeStamp=xxxx,/g
s/File=[^,]*,/File=xxx/g
s/Line=[0-9]*,/Line=xxx/g
s/Host=[0-9,a-z,A-Z,_,.,-]*,/Host=xxxx,/g
s/Process=PID: [0-9]*/Process=PID: xxx/g
s/RTContext: ([-0-9,a-z,A-Z,_]*, [-0-9,a-z,A-Z,_]*, [-0-9,a-z,A-Z,_]*, [0-9,a-z,A-Z,-,_]*,/RTContext: (xxx, xxx, xxx, xxx/g
s/Thread=ID: [0-9]*/Thread=ID: xxx/g
s/instance at 0x[0-9,a-z,A-Z]*/instance at 0xXXXX/g
s/Process=[0-9]*/Process=xxx/g
s/The current time is:  [0-9]*$/The current time is:  xxx/g
s/Thread=omniORB-\+\([0-9]\)\+/Thread=omniORB--XXXXXXX/g 
s/-OAIAddr [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/-OAIAddr xxx.xxx.xxx.xxx/g
s/-ORBEndpoint iiop:\/\/[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/-ORBEndpoint iiop:\/\/xxx.xxx.xxx.xxx:xxxx/g
s/-OAport [0-9]*/-OAport xxx/g
s/activate_component: handle=[0-9]*/activate_component: handle=xxx/g
s/alma\.MicroArchive\.DbDir=\/[^ ]*/alma.MicroArchive.DbDir=\/somepath/g
s/-Djava\.endorsed\.dirs=\/[^ ]*/-Djava.endorsed.dirs=\/somepath/g
s/1 - Manager hostname obtained via command line: '[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*'/1 - Manager hostname obtained via command line: 'xxx.xxx.xxx.xxx'/g
s/Full path '\/.*\/\([^\/]*\.so\)'/Full path '\/somepath\/\1'/g
s/Loaded '\/.*\/\([^\/]*\.so\)'/Loaded '\/somepath\/\1'/g
s/Unloaded '\/.*\/\([^\/]*\.so\)'/Unloaded '\/somepath\/\1'/g
s/1 - \/.*\/\([^\/]*\.so\)/1 - \/somepath\/\1/g
s/2 -   File "\/.*\/\([^\/]*\)"/2 -   File "\/somepath\/\1/g
s/\(2 -[ ]*Thread=RequestProcessor-\)[0-9]*,/\1xxx,/g
s/Using DLL path: .*/Using DLL path: xxxx/
s/\.java:[0-9]*/.java:xxx/g
s/, line [0-9]*/, line XXX/g
s/TimeMillis="[0-9]*"/TimeMillis="XX"/g

s/corbaloc::[0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/corbaloc::a.b.c.d:xxxx/g
s/corbaloc::some[a-zA-Z]*:[0-9]*/corbaloc::someXXXX:NNNN/g
s/Logging to log file: .*\/acsdata\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_]*\)_[0-9]*/Logging to log file: \/-----\/acsdata\/tmp\/acs_local_log_\1_xxxx/g
s/MainThread, acspyTestLogging, [a-z,A-Z,0-9,-]*,/MainThread, acspyTestLogging, xxxx,/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g 
s/[0-9][0-9][0-9][0-9][\/][0-9][0-9][\/][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [a-z,A-Z,0-9,-]*/---------- --:--:-- username/g
s/acspyTestLogging.py,v [0-9]*.[0-9]*/acspyTestLogging.py,v x.x/g
s/acspy.acslog.Logger instance at 0x[0-9,a-z]*/acspy.acslog.Logger instance at 0x--------/g
s/get local manager from [a-z,A-Z,0-9,-]*/get local manager from xxxx/g
s/$Id: .* Exp/-Id:  acspyTestLogging.py,v x.x ---------- --:--:-- username Exp/g
s/The temperature difference is [0-9,.]*/The temperature difference is x.x/g
s/TimeStamp=[0-9]*,/TimeStamp=xxxx,/g
s/Line=[0-9]*,/Line=xxx/g
s/Host=[0-9,a-z,A-Z,-,_]*/Host=xxxx/g
s/Process=PID: [0-9]*/Process=PID: xxx/g
s/RTContext: ([-0-9,a-z,A-Z,_]*, [-0-9,a-z,A-Z,_]*, [-0-9,a-z,A-Z,_]*, [0-9,a-z,A-Z,-,_]*,/RTContext: (xxx, xxx, xxx, xxx,/g
s/Thread=ID: [0-9]*/Thread=ID: xxx/g
s/object at 0x[0-9,a-z,A-Z]*/object at 0xXXXX/g
s/Process=[0-9]*/Process=xxx/g
s/\/[a-z,A-Z,0-9,-,.,_,/]*[:] line [0-9]*[:] [0-9]* Killed/\/xxx: line xxx Killed/g
s/Unable to shutdown because the lock directory '.*\/ACS_INSTANCE/Unable to shutdown because the lock directory '$ACS_TMP\/ACS_INSTANCE/g
s/Freeing .*\/ACS_INSTANCE.[0-9]/Freeing $ACS_TMP\/ACS_INSTANCE.N/g
s/Starting ACS[.]*$/Starting ACS.../g
s/Please see .*$/Please see \/some\/file for debug output/g
s/'[^',=]*'/'something'/g
s|/[-,a-z,A-Z,0-9,.,_,/]*/acsdata/|/alma/ACS-x.y/acsdata/|g
s/ACS-[0-9].[0-9]/ACS-x.y/g
s/iiop:\/\/[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/iiop:\/\/a.b.c.d:xxxx/g
s/giop:tcp:[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/giop:tcp:a.b.c.d:xxxx/g
s/ssh -f [a-z,A-Z,0-9]*@/ssh -f user@/g
s/-OAIAddr [0-9]*.[0-9]*.[0-9]*.[0-9]*/-OAIAddr a.b.c.d/g
s/java_pid[0-9]*.hprof/java_pidXXX.hprof/g
s/-OAport [0-9][0-9][0-9][0-9]/-OAport nnnn/g
s/Ran [0-9]* tests in [0-9]*.[0-9]*s/Ran x tests in x.xxs/g
s/-Djava.endorsed.dirs=.*/-Djava.endorsed.dirs=xxxxx/g
s/Endpoint: [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/Endpoint: a.b.c.d:p/g
s/ACS_INSTANCE=[0-9]/ACS_INSTANCE=N/g
s/Locking instance [0-9] with lock file .*/Locking instance N with lock file ..acsInstanceN.lock/g
s/Freeing instance [0-9] removing lock file .*/Freeing instance N removing lock file ..acsInstanceN.lock/g
s/org.jacorb.naming.NameServer .*/org.jacorb.naming.NameServer.../g
s/Starting Java application: org.jacorb.naming.NameServer .*/Starting Java application: org.jacorb.naming.NameServer.../g

s/TEST[0-9]* \([0-9]*\) [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/TEST \1 a.b.c.d/g
s/Stopping the CORBA TEST[0-9]* Notification Service/Stopping the CORBA TEST Notification Service/g
s/Name Service without channel entries in the endpoint [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*:[0-9]*/Name Service without channel entries in the endpoint a.b.c.d:xxxx/g
s/([0-9]*|[0-9]*) EXCEPTION/EXCEPTION/g
s/\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\] deleted in the Naming Service/[a.b.c.d:xxxx] deleted in the Naming Service/g
s/-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]:[0-9][0-9]:[0-9][0-9]/-YYYY-MM-DD_HH:MM:SS/g

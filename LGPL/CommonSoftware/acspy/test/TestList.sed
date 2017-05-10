s/MainThread, acspyTestLogging, [a-z,A-Z,0-9,-]*,/MainThread, acspyTestLogging, xxxx,/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,10\}/----------T--:--:--.---/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9]/----------T--:--:--/g 
s/[0-9][0-9][0-9][0-9][\/][0-9][0-9][\/][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [a-z,A-Z,0-9,-]*/---------- --:--:-- username/g
s/acspyTestLogging.py,v [0-9]*.[0-9]*/acspyTestLogging.py,v x.x/g
s/acspy.acslog.Logger instance at 0x[0-9,a-z]*/acspy.acslog.Logger instance at 0x--------/g
s/0x[0-9,a-z,A-Z]*>/0xXXXX>/g
s/=[0-9]*L/=XXXXL/g
s/get local manager from [a-z,A-Z,0-9,-]*/get local manager from xxxx/g
s/corbaloc::[a-z,A-Z,0-9,-,.]*:[0-9]*\//corbaloc::xxxx:xxxx\//g
s/$Id: .* Exp/-Id:  acspyTestLogging.py,v x.x ---------- --:--:-- username Exp/g
s/The temperature difference is [0-9,.]*/The temperature difference is x.x/g
s/TimeStamp=[0-9,a-z,A-Z,:, ]*,/TimeStamp=xxxx,/g
s/Line=[0-9]*,/Line=xxx/g
s/Host=[0-9]*.[0-9]*.[0-9]*.[0-9]*,/Host=xxxx,/g
s/Host=.*,/Host=xxxx,/g
s/host=.*,/host=xxxx,/g
s/Process=PID: [0-9]*/Process=PID: xxx/g
s/RTContext: ([0-9,a-z,A-Z,-,_]*, [0-9,a-z,A-Z,-,_]*, [0-9,a-z,A-Z,-,_]*, [0-9,a-z,A-Z,-,_]*,/RTContext: (xxx, xxx, xxx, xxx/g
s/Thread=ID: [0-9]*/Thread=ID: xxx/g
s/Process=[0-9]*/Process=xxx/g
s/File "\/.*\/\(.*\)", line [0-9]*/File "\1", line xxx/g
s/Ran [0-9]* tests in [0-9]*.[0-9]*s/Ran xx tests in x.xxxs/g
s/process='[0-9]*'/process=XXXX/g
s/file=.*acspy/file='acspy/g
s/file=.*ACSSW/file='acspy/g
s/lineNum=[0-9]*/lineNum=XXXX/g
s/TimeMillis=[0-9]\+/TimeMillis=nnn/g
s/ initialized by \w*@\S*/ initialized by user@host/g
s/test_reconn@DEFAULTDOMAIN\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\]/test_reconn@DEFAULTDOMAIN[X.X.X.X:YYYY]/g
s/Name Service without channel entries in the endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/Name Service without channel entries in the endpoint X.X.X.X:YYYY/g
s/Deleting channels entries in the Name Service of the  Notify Service \[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\]/Deleting channels entries in the Name Service of the  Notify Service [X.X.X.X:YYYY]/g
s/Notification Service \[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\] which has been stopped/Notification Service [X.X.X.X:YYYY] which has been stopped/g
s/@DEFAULTDOMAIN\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\] deleted in the Naming Service/@DEFAULTDOMAIN[X.X.X.X:YYYY] deleted in the Naming Service/g
s/===  [0-9]*: [0-9]* exceptions caught/===  X: Y exceptions caught/g
s/===  [0-9]*: [0-9]* events dropped/===  X: X events dropped/g
s/===  [0-9]*: [0-9]* events sent/===  X: Y events sent/g
s/===  [0-9]*: Transitions: \[[0-9, ]*\]/===  X: Transitions: [Y, Z, ...]/g

s/MainThread, acspyTestLogging, [a-z,A-Z,0-9,-]*,/MainThread, acspyTestLogging, xxxx,/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g 
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
s/LastPeriodDuration=[.,0-9]*/LastPeriodDuration=X/g
s/MessageStatistics=[.,0-9]*/MessageStatistics=X/g
s/ErrorMessageStatistics=[.,0-9]*/ErrorMessageStatistics=X/g
s/-nan%/ nan%/g

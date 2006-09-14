s/MainThread, acspyTestLogging, [a-z,A-Z,0-9,-]*,/MainThread, acspyTestLogging, xxxx,/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9]/----------T--:--:--/g 
s/[0-9][0-9][0-9][0-9][\/][0-9][0-9][\/][0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] [a-z,A-Z,0-9,-]*/---------- --:--:-- username/g
s/acspyTestLogging.py,v [0-9]*.[0-9]*/acspyTestLogging.py,v x.x/g
s/acspy.acslog.Logger instance at 0x[0-9,a-z]*/acspy.acslog.Logger instance at 0x--------/g
s/get local manager from [a-z,A-Z,0-9,-]*/get local manager from xxxx/g
s/corbaloc::[a-z,A-Z,0-9,-,.]*:[0-9]*\//corbaloc::xxxx:xxxx\//g
s/$Id: .* Exp/-Id:  acspyTestLogging.py,v x.x ---------- --:--:-- username Exp/g
s/The temperature difference is [0-9,.]*/The temperature difference is x.x/g
s/TimeStamp=[0-9]*,/TimeStamp=xxxx,/g
s/Line=[0-9]*,/Line=xxx/g
s/Host=[0-9,a-z,A-Z,-,_]*/Host=xxxx/g
s/Process=PID: [0-9]*/Process=PID: xxx/g
s/RTContext: ([0-9,a-z,A-Z,-,_]*, [0-9,a-z,A-Z,-,_]*, [0-9,a-z,A-Z,-,_]*,/RTContext: (xxx, xxx, xxx,/g
s/Thread=ID: [0-9]*/Thread=ID: xxx/g
s/instance at 0x[0-9,a-z,A-Z]*/instance at 0xXXXX/g
s/Process=[0-9]*/Process=xxx/g

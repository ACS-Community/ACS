s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,10\}/----------T--:--:--.---/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9]/----------T--:--:--/g
s/corbaloc::[a-z,A-Z,0-9,-,.]*:/corbaloc::xxxx:/g
s/IOR:[a-z,0-9,.,]*/IOR:xxxxxxxxxxxxxx/g
s/ChannelId='[0-9]*'/ChannelId='xxx'/g
s/TimeMillis='[0-9]*'/TimeMillis='xxx'/g
s/Name Service without channel entries in the endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/Name Service without channel entries in the endpoint X.X.X.X:YYYY/g
s/@DEFAULTDOMAIN\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\] deleted in the Naming Service/@DEFAULTDOMAIN[X.X.X.X:YYYY] deleted in the Naming Service/g

s/===   Number of events dropped: [0-9]*/===   Number of events dropped: X/g
s/===   Number of events queued: [0-9]*/===   Number of events queued: X/g
s/===   Number of events sent: [0-9]*/===   Number of events sent: X/g
s/===   Number of exceptions: [0-9]*/===   Number of exceptions: X/g
s/===   Transitions: [0-9,]*/===   Transitions: X,Y,/g
s/couldn't reconnect to [0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/couldn't reconnect to X.X.X.X:YYYY/g
s/-[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]_[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\]/-YYYY-MM-DD_HH:MM:SS[X.X.X.X:YYYY]/g
s/SimpleConsumerReconnClient-[-,0-9]*/SimpleConsumerReconnClient-XXXXX/g
s/Exceptions found: .*/Exceptions found: XXXX/g
s/Time first event received: .*/Time first event received: XXX/g
s/Time last event received: .*/Time last event received: XXX/g


# Always at the end!
s/[2,3,4] - ----------T--:--:--.--- INFO \[SimpleConsumerReconnClient-XXXXX\] Successfully created/X - ----------T--:--:--.--- INFO [SimpleConsumerReconnClient-XXXXX] Successfully created/g

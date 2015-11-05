s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g 
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9]/----------T--:--:--/g
s/corbaloc::[a-z,A-Z,0-9,-,.]*:/corbaloc::xxxx:/g
s/IOR:[a-z,0-9,.,]*/IOR:xxxxxxxxxxxxxx/g
s/ChannelId='[0-9]*'/ChannelId='xxx'/g
s/TimeMillis='[0-9]*'/TimeMillis='xxx'/g
s/Name Service without channel entries in the endpoint [0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*/Name Service without channel entries in the endpoint X.X.X.X:YYYY/g
s/@DEFAULTDOMAIN\[[0-9]*.[0-9]*.[0-9]*.[0-9]*:[0-9]*\] deleted in the Naming Service/@DEFAULTDOMAIN[X.X.X.X:YYYY] deleted in the Naming Service/g

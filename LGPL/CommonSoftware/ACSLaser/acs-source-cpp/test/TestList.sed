s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/<source-hostname>[-a-z,\.,A-Z,0-9]*<\/source-hostname>/<source-hostname><\/source-hostname>/
s/seconds=\"[0-9]*/seconds=\"/
s/microseconds=\"[0-9]*/seconds=\"/
s/Host="[^ ]*"/Host="xxx"/g

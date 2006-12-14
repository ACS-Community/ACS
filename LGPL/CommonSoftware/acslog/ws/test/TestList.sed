s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]*/----------T--:--:--.---/g
s/StackId=\"[a-z,A-Z,0-9,-]*\"/StackId=\"XXXXXXXX\"/g
s/Host=\"[a-z,A-Z,0-9,.-]*\"/Host=\"xxxxxxxxx\"/g
s/Thread=\"ID: [a-z,A-Z,0-9]*\"/Thread=ID: xxxxxxxxx/g

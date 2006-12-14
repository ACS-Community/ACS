s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]*/----------T--:--:--.---/g
s/StackId=\"[a-z,A-Z,0-9,-]*\"/StackId=\"XXXXXXXX\"/g
s/Host=\"[a-z,A-Z,0-9,.-]*\"/Host=\"xxxxxxxxx\"/g
s/Thread=\"[a-z,A-Z,0-9,:, ]*\"/Thread=xxxxxxxxx/g
s/Line="[0-9]*"/Line="---"/g


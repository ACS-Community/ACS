s/TimeStamp="[^ ]*"/TimeStamp="xxx"/g
s/Host="[^ ]*"/Host="xxx"/g
s/Line="[^ ]*"/Line="xxx"/g
s/LogId="[^ ]*"/LogId="xxx"/g
#perhaps also blank "LogId" ?
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/--iso_time--/g 
s/during last [.,0-9]* seconds = [.,0-9]*/during last X seconds = X/g

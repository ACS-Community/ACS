s/TimeStamp="[^ ]*"/TimeStamp="xxx"/g
s/Host="[^ ]*"/Host="xxx"/g
s/Line="[^ ]*"/Line="xxx"/g
#perhaps also blank "LogId" ?
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/--iso_time--/g 

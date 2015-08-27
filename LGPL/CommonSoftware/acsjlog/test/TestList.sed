s/TimeStamp="[^ ]*"/TimeStamp="xxx"/g
s/Host="[^ ]*"/Host="xxx"/g
s/Line="[^ ]*"/Line="xxx"/g
s/LogId="[^ ]*"/LogId="xxx"/g
#perhaps also blank "LogId" ?
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/--iso_time--/g 
s/LastPeriodDuration"><!\[CDATA\[[.,0-9]*\]\]/LastPeriodDuration"><!\[CDATA\[X\]\]/g
s/MessageStatistics"><!\[CDATA\[[.,0-9]*\]\]/MessageStatistics"><!\[CDATA\[X\]\]/g
s/ErrorMessageStatistics"><!\[CDATA\[[.,0-9]*\]\]/ErrorMessageStatistics"><!\[CDATA\[X\]\]/g
s/-nan%/ nan%/g
s/CDATA\[Infinity\]/CDATA\[X\]/g
s/CDATA\[NaN\]/CDATA\[X\]/g

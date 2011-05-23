s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]*/----------T--:--:--.---/g
s/[a-z, A-Z, ]* [0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9] UTC [0-9]* [0-9]* [0-9]* [0-9]*/XXX XXX XX XX:XX:XX UTC XXXXXX XXXX XXXX XXXX/g
s/Using [a-z,A-Z,0-9,_,\/,\.,-]*\/rtai\/[a-z,A-Z,0-9,_,\.,-]*/Using .....\/rtai\/XXX/g
s/Host=[a-z,A-Z,0-9,.,_,-]*/Host=xxxxxxxxx/g
s/Line="[0-9]*"/Line="---"/g 
s/Process=[0-9][0-9]*/Process=xxxxxxxxx/g
s/TimeStamp=[0-9,a-z,A-Z, :,-, ]*/TimeStamp=XXXX,/g
s/Line=[0-9]*/Line=XXXX/g
s/Thread=RequestProcessor-[0-9]*/Thread=RequestProcessor-XX/g
s/ime=[0-9]*L/ime=XXXX/g
s/'time': [0-9]*L/'time': ------------------L/g

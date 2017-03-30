s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Interrupted system call/Interrupted system call or resource temporarily anavailable/g
s/Resource temporarily unavailable/Interrupted system call or resource temporarily anavailable/g
s/No such file or directory/Interrupted system call or resource temporarily anavailable/g
s/Unable to open recovery file (r) .*/Creating or loading recovery file/g
s/Reading recovery file .*/Creating or loading recovery file/g
s/spawn rlogin [a-z,A-Z,0-9]*/rlogin LCU/g
s/l[a-z,A-Z,0-9]*->/LCU->/g
s/task spawned: id = 0x[0-9,a-f]*, name = t[0-9]*/task spawned/g
s/log_cache\.dat_[0-9,a-z,A-Z]*_[0-9]*/log_cache.dat__XXXXXXXX_XXX/g
s/corbaloc::[a-z,A-Z,0-9,_,.,-]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/-classpath .* -Dabeans.config/-classpath ..... -Dabeans.config/g
s/.java:[0-9]*/.java:xx/g
s/^  ..$/ /g

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

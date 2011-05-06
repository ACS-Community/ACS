s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/\([0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]\)/xx:xx:xx.xxx/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]/----------T--:--/g
s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/Manager login done.*/Manager login done, handle 'xxxxxxxxxx' obtained./g
s/java -classpath .*/java -classpath .../g
s/exported MANAGER_COMPUTER_NAME=.*/exported MANAGER_COMPUTER_NAME=xxx.xxx.xxx.xxx/g

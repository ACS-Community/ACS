s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/\(remote debuggers on port \)[0-9][0-9]*$/\1----/ 
s/\(transport dt_socket at address: \)[0-9][0-9]*$/\1----/
s/corbaloc::[a-z,A-Z,0-9,_,.,-]*:[0-9]*/corbaloc::xxxx:xxxx/g
s/-OAIAddr [0-9]*\.[0-9]*\.[0-9]*\.[0-9]*/-OAIAddr xxx.xxx.xxx.xxx/g
s/-OAport [0-9]*/-OAport xxx/g
s/Max\/Min for JacORB's thread pool: [0-9]\+\/[0-9]\+/Max\/Min for JacORB's thread pool: xx\/xx/g
s/Got silentContainer's PID: [0-9]\+/Got silentContainer's PID: xxxx/g
s/ACS Container threads average (idle\/stress): [0-9]\+\.[0-9]\+\/[0-9]\+\.[0-9]\+/ACS Container threads average (idle\/stress): xx\.xx\/xx\.xx/g
s/JacORB threads average        (idle\/stress): [0-9]\+\.[0-9]\+\/[0-9]\+\.[0-9]\+/JacORB threads average        (idle\/stress): xx\.xx\/xx\.xx/g
s/Total number of JacORB threads: [0-9]\+/Total number of JacORB threads: xx/g
s/Total number of ACS threads: [0-9]\+/Total number of ACS threads: xx/g
s/org.jacorb.poa.RequestProcessor[ \t]\+WAITING[ \t]\+[0-9]\+/org.jacorb.poa.RequestProcessor                   WAITING        xx/g

s/Cache saved to '*/Cache saved to '----'/g
s/Manager hostname generated using localhost address: '.*'/Manager hostname generated using localhost address: '---'/g
s/\/acsdata\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\/acsdata\/tmp\/acs_local_log_\1_xxxx/g
s/Container-[0-9]*/Container-XXXX/g
s/corbaloc::[a-z,0-9,.,-]*:[0-9]*/corbaloc::---:----/g
s/log_cache.dat_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\log_cache.dat_\1_xxxx/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/([0-9]*|[0-9]*)/(XXXX|XXXXX)/g
s/Unloaded '\/[a-z,A-Z,0-9,.,_,\/,-]*\//'XXXXXX\//g
s/\/[a-z,A-Z,0-9,.,_,\/,-]*\/libbaci.so/XXXXXX\/libbaci.so/g
s/Component-[0-9]*/Component-XXXX/g
s/iiop:\/\/[a-z,A-Z,0-9,_,.,-]*:[0-9]*/iiop:\/\/xxxxxxx:yyyy/g
s/Using DLL path: \/[a-z,A-Z,0-9,.,_,\/,-,:,-]*/Using DLL path: (check the out.orig file for the real path)/g

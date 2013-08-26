s/The temperature difference is [-,0-9,e,E,+,.]*$/The Python temperature difference is ---/g
s/The temp difference is: [-,0-9,e,E,+,.]*$/The Java temperature difference is ---/g
s/::myHandlerFunction(...): [0-9,e,E,-,+,.]* is the tempdiff.$/The C++ temperature difference is ---/g
s/instance at 0x[0-9,a-z,A-Z]*/instance at 0x----/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/acs_local_log_[0-9,a-z,A-Z,.,_,-]*/acs_local_log_---/g
s/corbaloc::[0-9,a-z,A-Z,.,_,-]*:[0-9]*\//corbaloc::---:---\//g
s/Manager hostname generated using localhost address: '[0-9,a-z,A-Z,.,_,-]*'/Manager hostname generated using localhost address: 'xxx'/g
s/handle '[0-9]*'/handle 'xxx'/g
s/[-,0-9,a-z,A-Z,.,_,/]*\/acsexmplClientFridgeNC/acsexmplClientFridgeNC/g

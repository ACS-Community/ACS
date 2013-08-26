s/IOR:[a-z,0-9,.,-,A-Z]*/IOR:xxxxxxxxxxxxxx/g
s/iioploc:\/\/[a-z,0-9,.,-]*:[0-9]*/iioploc:\/\/xxxxx\.xxxxx:xxxx/g
s/Manager hostname generated using localhost address: '.*'/Manager hostname generated using localhost address: '---'/g
s/iiop:\/\/[a-z,0-9,.,-]*:[0-9]*/iiop:\/\/---:----/g
s/corbaloc::[a-z,0-9,.,-]*:[0-9]*/corbaloc::---:----/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/lib\/\(lib[a-z,A-Z,0-9,.,_,-]*\.so\)/\/----\/\1/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/bin\/\([a-z,A-Z,0-9,.,_,-]*\)/\/----\/bin\/\1/g
s/\.\.\/lib\(\/lib[a-z,A-Z,0-9,.,_,-]*\)/\/----\1/g
s/\.\.\/bin\(\/[a-z,A-Z,0-9,.,_,-]*\)/\/----\1/g
s/\/acsdata\/tmp\/acs_local_log_\([a-z,A-Z,0-9,_,-]*\)_[0-9]*/\/acsdata\/tmp\/acs_local_log_\1_xxxx/g
s/-ORBEndpoint iiop\:\/\/[a-z,A-Z,0-9,-]*/-ORBEndpoint iiop\:\/\/LCU/g
s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/java -classpath .*/java.../g
s/MANAGER_COMPUTER_NAME\=[a-z,A-Z,0-9,-,_,.]*/MANAGER_COMPUTER_NAME=xxxxxxx/g
s/@(#) $Revision: [0-9,.]*/CVS Revision: x.x/g
s/0x[0-9,a-f,A-F]*/xxxxxxxxxx/g

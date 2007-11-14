s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]/----------T--:--/g
s/user='.*'/user=xxxx/g
s/\(<hostName>\)[^<]*\(<\/hostName>\)/\1xxxx\2/
s/\(<sourceHostname>\)[^<]*\(<\/sourceHostname>\)/\1xxxx\2/
s/\(CategorySubscriber.hostname \).*$/\1xxxx/
s/Process [0-9]* killed/Process xxxxx killed/g
s/<date>[0-9]*<.date>/<date>......./g
s/<nanos>[0-9]*<.nanos>/<nanos>......./g

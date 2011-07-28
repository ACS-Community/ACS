s/corbaloc::[a-z,A-Z,0-9,-,_,.]*:[0-9]*/corbaloc::xxxxxxx:yyyy/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9][0-9][0-9]/----------T--:--:--.---/g
s/user='.*'/user=xxxx/g
s/\(<hostName>\)[^<]*\(<\/hostName>\)/\1xxxx\2/
s/\(<sourceHostname>\)[^<]*\(<\/sourceHostname>\)/\1xxxx\2/
s/\(CategorySubscriber.hostname \).*$/\1xxxx/
s/Process [0-9]* killed/Process xxxxx killed/g
s/<date>[0-9]*<.date>/<date>......./g
s/<nanos>[0-9]*<.nanos>/<nanos>......./g
s/AlarmSevice instance IOR.*/AlarmSevice instance IOR: .../g
s/Time:.*/Time: xx.xx/g
s/Manager login done, handle .* obtained./Manager login done, handle xxxxxx obtained./g
s/ChannelName='[^']*'/ChannelName='xxx'/g
s/ChannelId='[^']*'/ChannelId='xxx'/g
s/TimeMillis='[0-9]*'/TimeMillis='xxx'/g

s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/^1 - [0-9]* *INFO/1 - --non-ISO timestamp-- INFO/g
s/^1 - [0-9]* *WARN/1 - --non-ISO timestamp-- WARN/g
s/CMW.ALARM_SYSTEM.CLIENTS.[- 0-9]*/CMW.ALARM_SYSTEM.CLIENTS.------/g
s/ChannelId='.*'/ChannelId='xx'/g

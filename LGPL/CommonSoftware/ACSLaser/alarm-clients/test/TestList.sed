s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/[0-9]* *INFO/--non-ISO timestamp-- INFO/g
s/[0-9]* *WARN/--non-ISO timestamp-- WARN/g
s/CMW.ALARM_SYSTEM.CLIENTS.[- 0-9]*/CMW.ALARM_SYSTEM.CLIENTS.------/g
s/ChannelId='.*'/ChannelId='xx'/g

s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/CMW.ALARM_SYSTEM.CLIENTS.[- 0-9]*/CMW.ALARM_SYSTEM.CLIENTS.------/g
s/ChannelId='.*'/ChannelId='xx'/g
s/[0-9][0-9]:[0-9][0-9]:[0-9][0-9]/HH:MM:SS -non ISO timestamp-/g
s/Java.*/Java... (build...)/g
s/java version.*/java version..../g
s/Time: .*/Time: hh:mm:ss/g
s/-D user.dir.*/user.dir=xxxxx/g

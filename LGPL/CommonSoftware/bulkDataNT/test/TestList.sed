s/Cache saved to '[a-z,A-Z,0-9,.,_,\/,-]*/Cache saved to '----'/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Counter is: [0-9]*/Counter is: XXX/g
s/[0-9]* bytes/XXX bytes/g
s/Rate: [0-9,.]*MBytes\/sec/Rate: XXXMBytes\/sec/g
s/ Bytes in [0-9,.]*secs./ Bytes in XXXXsecs./g
s/ Function took [0-9,.]*/ Function took XYZ.ASD/g
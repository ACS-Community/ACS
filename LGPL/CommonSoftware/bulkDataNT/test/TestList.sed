s/Cache saved to '[a-z,A-Z,0-9,.,_,\/,-]*/Cache saved to '----'/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Counter is: [0-9]*/Counter is: XXX/g
s/[0-9]* bytes/XXX bytes/g
s/Rate: [0-9,.]*MBytes\/sec/Rate: XXXMBytes\/sec/g
s/ Bytes in [0-9,.]*secs./ Bytes in XXXXsecs./g
s/ Function took [0-9,.]*/ Function took XYZ.ASD/g
s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+/xxx.xxx.xxx.xxx/g
s/([0-9]\+|[0-9]\+) EXCEPTION/(XXX|XXX) EXCEPTION/g
s/TimeMillis="[0-9]\+"/TimeMillis="xxx"/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/lib\/\(lib[a-z,A-Z,0-9,.,_]*\.so\)/\/----\/\1/g
s/component with handle [0-9]\+/component with handle XXX/g

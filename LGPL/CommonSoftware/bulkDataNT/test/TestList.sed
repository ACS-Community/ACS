s/Cache saved to '[a-z,A-Z,0-9,.,_,\/,-]*/Cache saved to '----'/g
s/[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9][ T][0-9][0-9]:[0-9][0-9]:[0-9][0-9].[0-9]\{1,3\}/----------T--:--:--.---/g
s/Counter is: [0-9]*/Counter is: XXX/g
s/[0-9]* bytes/XXX bytes/g
s/Rate: [0-9,.]*MBytes\/sec/Rate: XXXMBytes\/sec/g
s/ Bytes in [0-9,.]*secs./ Bytes in XXXXsecs./g
s/ Function took [0-9,.]*/ Function took XYZ.ASD/g
s/[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+:[0-9]*/xxx.xxx.xxx.xxx:XYZZ/g
s/Manager hostname obtained via command line: '[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+'/Manager hostname obtained via command line: 'xxx.xxx.xxx.xxx'/g
s/([0-9]\+|[0-9]\+) EXCEPTION/(XXX|XXX) EXCEPTION/g
s/TimeMillis="[0-9]\+"/TimeMillis="xxx"/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/lib\/\(lib[a-z,A-Z,0-9,.,_]*\.so\)/\/----\/\1/g
s/component with handle [0-9]\+/component with handle XXX/g
s/unacknowledged_sample_count ([0-9]) before waiting: [0-9]/unacknowledged_sample_count (X) before waiting: Y/g
s/ActualProcessTime="[0-9]*\.[0-9]*"/ActualProcessTime="X.YZ"/g
s/Transfer rate: [0-9,.]*/Transfer rate: XY.Z/g
s/New transfer rate: [0-9,.]*/New transfer rate: XY.Z/g
s/Receiver Data Rate: [0-9,.]*/Receiver Data Rate: XY.Z/g
s/in [0-9,.]* sec/in xy.z sec/g
s/before waiting: [0-9]*/before waiting: XY/g
s/Sub([0-9]*)/Sub(XXX)/g
s/\/[a-z,A-Z,0-9,.,/,_,-]*\/config\/bulkDataNTDefaultQosProfiles.default.xml/----\/config\/bulkDataNTDefaultQosProfiles.default.xml/g
s/cbReceive(): [0-9,.]*s. What corresponds to throughput of: [0-9,.]*MB\/sec/cbReceive(): X.Ys. What corresponds to throughput of: Z.XXXMB\/sec/g
s/sample rcv: [0-9]* ([0-9]*) HB: [0-9]* ([0-9]*) ACKs: [0-9]* ([0-9]*) NACKs: [0-9]* ([0-9]*) Rejected: 0/sample rcv: XX (YYYYY) HB: XX (YYYYY) ACKs: XX (YYYYY) NACKs: X (YY) Rejected: 0 /g
s/HB: [0-9]* ([0-9]*) ACKs: [0-9]* ([0-9]*) NACKs: [0-9]* ([0-9]*)/HB: XX (YY) ACKs: XX (YY) NACKs: XX (YY)/g
s/AvgProcessTimeoutSec="[0-9,.]*"/AvgProcessTimeoutSec="X.YZ"/g
s/Throughput="[0-9,.]*"/Throughput="X.YZ"/g
s/waiting for ACKs: [0-9]*/waiting for ACKs: XY/g
s/Found ACS_INSTANCE [0-9]/Found ACS_INSTANCE X/g
s/using it as DDS domainID : [0-9]/using it as DDS domainID : X/g

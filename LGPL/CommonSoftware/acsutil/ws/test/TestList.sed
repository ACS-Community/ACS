s/.*acsutilCASAPathInsert/acsutilCASAPathInsert/
s/\/ACS_INSTANCE.[0-9]\//\/ACS_INSTANCE.X\//
s/line [0-9]\+:/line XX:/
s/[ 0-9]\+ Killed/PID Killed/
/^2 - ..\/bin\/acsutilBlock: line XX:PID Killed/d

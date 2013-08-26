#
# Remove ISO time-stamps by symbolic value
s/[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]T[0-5][0-9]:[0-5][0-9]:[0-5][0-9][.0-9]*/<timestamp>/g
#
# Replace IP-address/port by symbolic values
s/[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*:[0-9][0-9]*/<IP address>:<port>/g
#
# Remove handle IDs by symbolic value
s/handle ['0-9]['0-9]*/handle <ID>/g
#
# Remove timing data by symbolic value
s/Time: [0-9.]*/Time: <seconds>/g
s/id=[0-9.]*/id=XXX/g
s/Will wait [0-9]* seconds/Will wait <N> seconds/g
#
# Remove exception data. "(" is hex 28, ")" is hex 29
s/[(0-9][0-9|)]* EXCEPTION/(<ID>) EXCEPTION/g

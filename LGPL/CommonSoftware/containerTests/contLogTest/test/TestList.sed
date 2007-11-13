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

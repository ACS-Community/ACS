#
# Remore ISO time-stamps by symbolic value
s/[0-9][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]T[0-5][0-9]:[0-5][0-9]:[0-5][0-9][.0-9]*/<timestamp>/g
#
# Replace absolute directory path by symbolic name.
s|/.*components/|<components_dir>/|g
#
# Replace ACS_INSTANCE number in paths
s|/ACS_INSTANCE\.[0-9]/|/ACS_INSTANCE.N/|g
# Replace ACS_INSTANCE enumerated value by symbolic value.
# (Note that also some port numbers depend on the value assigned to ACS_INSTANCE)
s/started ACS_INSTANCE [0-9]/started ACS_INSTANCE <N>/g
s|\:[0-9][0-9]*/Manager|:<port>/Manager|g
#
# Replace source code line numbers and PIDs by symbolic values
s/line [0-9]*:[ 0-9]*Terminated/line <lineNr>: <pid> Terminated/g
s/bringing down logging client with ID [0-9][0-9]*/bringing down logging client with ID <pid>/g
s/maciContainerLogLevel_[0-9][0-9]*/maciContainerLogLevel_<pid>/g
#
# Replace IP-addresses by symbolic values
##s/[0-9]{1,}\.[0-9]{1,}\.[0-9]{1,}\.[0-9]{1,}/<IP address>/g
s/[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*/<IP address>/g

# Replace timing info by symbolic values
##s|logs in [0-9]{1,}s => [0-9]{1,} logs/s => within |logs in <number1>s => <number2> logs/s => within |g
s|logs in [.0-9]*s = [0-9]* logs/s => within |logs in <number1>s => <number2> logs/s => within |g
s/expected range (min [0-9][0-9]*, max [0-9][0-9]*)/expected range (min <min>, max <max>)/g
s/Needed [.0-9]*s to send all logs/Needed <time1>s to send all logs/g
s/ [.0-9]*s after first [0-9][0-9]* logs came to / <time2>s after first <number> logs came to /g
s/Last log digested within [.0-9]*s/Last log digested within <number>s/g

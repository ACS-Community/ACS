#
# Replace absolute directory path by symbolic name
s|/.*ACS/Benchmark/components/bin|<directory>/ACS/Benchmark/components/bin|g
#
# Replace source code line numbers and PIDs by symbolic values
s/line [0-9]*:[ 0-9]*Terminated/line <lineNr>: <pid> Terminated/g

#!/bin/bash

export TMCDB_CONFIGURATION_NAME=Test

# Start the blobber container
# We start it with acsStartContainer instead of the good acsutilAwaitContainerStart
# because later we need to inspect its output
HEAP_SIZE="-Xmx512m"
export JAVA_OPTIONS="$HEAP_SIZE"
export JAVA_OPTIONS="$JAVA_OPTIONS -Dalma.acs.monitoring.blobber.plugin=alma.acs.monitoring.blobber.TestingBlobberPlugin"
export JAVA_OPTIONS="$JAVA_OPTIONS -Dalma.acs.monitoring.blobber.checkmemory=true"
export JAVA_OPTIONS="$JAVA_OPTIONS -Djacorb.maxManagedBufSize=12" # This will make JacORB not use more than 64KB for buffering
acsStartContainer -java ARCHIVE/TMCDB/BLOBBER1/javaContainer &> $ACS_TMP/blobberContainer.out &
sleep 5

# The file-based collector reads the values for monitor points from these text files
rm -rf monitoringFiles
tar xf monitoringFiles.tar.gz
export JAVA_OPTIONS="$HEAP_SIZE -Dalma.acs.monitoring.filesDir=$PWD/monitoringFiles"
acsutilAwaitContainerStart -java ARCHIVE/TMCDB/MONITOR_COLLECTOR/javaContainer
unset JAVA_OPTIONS

# Start the test, who orchestrates the collector and controller lifecycles
acsStartJava alma.acs.testsupport.tat.TATJUnitRunner alma.acs.monitoring.blobber.FileReaderControllerTest

# Let it sleep a bit more, so the Blobber container releases its memory
sleep 10;

# Stop the containers
acsStopContainer ARCHIVE/TMCDB/MONITOR_COLLECTOR/javaContainer
acsStopContainer ARCHIVE/TMCDB/BLOBBER1/javaContainer
#acsStopContainer ARCHIVE/TMCDB/MONITOR_CONTROL/javaContainer
#acsutilTATEpilogue

output="$(grep GC $ACS_TMP/blobberContainer.out)"
first_measure=$(echo "$output" | head -n 1 | awk '{print $14}')
last_measure=$(echo "$output" | tail -n 1 | awk '{print $14}')

variation=$(( ($last_measure - $first_measure) * 100 / $first_measure ))
if [ $variation -gt 20 ]
then
	echo "ERROR: We measured a variation of more than 20% on the memory usage of the blobber container. This might indicate a possible memory leak in the application"
fi

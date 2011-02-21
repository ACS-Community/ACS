#!/bin/bash

export TMCDB_CONFIGURATION_NAME=Test
export ACS_CDB=$PWD
export ACS_TMP=$PWD/tmp
rm -rf $ACS_TMP
mkdir -p $ACS_TMP

acsutilTATPrologue

acsutilAwaitContainerStart -java ARCHIVE/TMCDB/MONITOR_CONTROL/javaContainer

# We use a file HSQLDB db, which we create and populate
SQLTOOL="acsStartJava org.hsqldb.util.SqlTool --rcFile sqltool.rc tmcdb"

for i in swconfig{core,ext} hwconfigmonitoring; do
	$SQLTOOL $ACSDATA/config/DDL/hsqldb/TMCDB_$i/CreateHsqldbTables.sql
done
$SQLTOOL basic-insertions.sql

export JAVA_OPTIONS="-Darchive.configFile=archiveConfig.properties.blobberTest"
acsutilAwaitContainerStart -java ARCHIVE/TMCDB/BLOBBER1/javaContainer

# The file-based collector reads the values for monitor points from these text files
rm -rf monitoringFiles
tar xf monitoringFiles.tar.gz
export JAVA_OPTIONS="-Dalma.tmcdb.monitoring.filesDir=$PWD/monitoringFiles"
acsutilAwaitContainerStart -java ARCHIVE/TMCDB/MONITOR_COLLECTOR/javaContainer
unset JAVA_OPTIONS

# Start the test, who orchestrates the collector and controller lifecycles
acsStartJava alma.acs.testsupport.tat.TATJUnitRunner alma.archive.tmcdb.monitor.FileReaderControllerTest

acsStopContainer ARCHIVE/TMCDB/BLOBBER1/javaContainer
acsStopContainer ARCHIVE/TMCDB/MONITOR_CONTROL/javaContainer
acsStopContainer ARCHIVE/TMCDB/MONITOR_COLLECTOR/javaContainer
acsutilTATEpilogue

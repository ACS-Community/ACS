#!/bin/bash
export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`
taskRunner -name TEST_TASK -l characteristicTaskTestImpl "Hello World"
#!/bin/sh

# run with LOCATION set
LOCATION=TST-STE ./testLoggerStatistics 2&>1;

#run without LOCATION ENV VAR
./testLoggerStatistics 2&>1;

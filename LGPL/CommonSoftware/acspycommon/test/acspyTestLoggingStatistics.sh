#!/bin/sh

echo "=> Test With LOCATION env var"
LOCATION=TST-STE python ../bin/acspyTestLoggingStatistics 2>&1;

echo "=> Test without LOCATION env var"
python ../bin/acspyTestLoggingStatistics 2>&1;


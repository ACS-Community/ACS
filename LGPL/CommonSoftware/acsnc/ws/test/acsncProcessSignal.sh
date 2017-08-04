#!/bin/sh

NS_PID=$(ps aux | grep -i "tao_cosnotification" | grep "$2" | sed -E 's/\s+/ /g' | cut -d ' ' -f 2)

if [ "X$NS_PID" != "X" ]; then
    echo "Sending signal code: $1  to process id: $2"
    kill -$1 $NS_PID
fi

#!/bin/sh

getChildProc() {
	name=$1
	ppid=$2
	ps -C $name -opid,ppid | awk '{if($2=='${ppid}') print($1)}'
}

LOGFILE=/dev/null

acsStart -noloadifr -b 9 > $LOGFILE 2>&1
acsStartContainer -java -b 9 --passthroughProcessStart='-maxHeapSize 99m' jCont > $LOGFILE 2>&1 &
start_container_pid=$!
sleep 10

java_container_pid=`getChildProc acsStartJavaContainer $start_container_pid`
start_java_pid=`getChildProc acsStartJava $java_container_pid`
java_pid=`getChildProc java $start_java_pid`

ps -p $java_pid -o args |
sed -e 's/java \(.*\)\( -Xmx[^ ]*\) \(.*\)/java many-options \2 other-options/'

acsStop -b 9 > $LOGFILE 2>&1

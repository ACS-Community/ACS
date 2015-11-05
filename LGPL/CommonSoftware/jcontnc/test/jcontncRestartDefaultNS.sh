#!/bin/bash
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`
if [ "$2" == "VERBOSE" ]; then
	acsNotifyService -k -b 0 -w -n NotifyEventChannelFactory -x corbaloc::$HOST:$NS_PORT/NameService
	echo "Waiting $1 seconds before starting the Notify Service"
	sleep $1
	acsNotifyService -s -b 0 -w -n NotifyEventChannelFactory -x corbaloc::$HOST:$NS_PORT/NameService
else
	acsNotifyService -k -b 0 -w -n NotifyEventChannelFactory -x corbaloc::$HOST:$NS_PORT/NameService &> /dev/null
	sleep $1
	acsNotifyService -s -b 0 -w -n NotifyEventChannelFactory -x corbaloc::$HOST:$NS_PORT/NameService &> /dev/null
fi

#!/bin/bash
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`
acsNotifyService -k -s -b 0 -w -n $1 -x corbaloc::$HOST:$NS_PORT/NameService

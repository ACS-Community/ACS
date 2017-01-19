#!/bin/bash
. acsstartupAcsPorts
NS_PORT=`getNamingServicePort`
acsncDelChannelsInNameS --name_service corbaloc::$HOST:$NS_PORT/NameService --notify_service "$1" --baseport $ACS_INSTANCE

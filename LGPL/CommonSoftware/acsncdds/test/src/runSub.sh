#!/bin/bash

#../bin/ddsSubscriber -ORBSvcConf tcp.conf -DCPSConfigFile sub.ini
$1 -DCPSConfigFile sub.ini -ORBSvcConf tcp.conf

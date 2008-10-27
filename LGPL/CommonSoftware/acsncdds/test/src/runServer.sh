#!/bin/bash

$DDS_ROOT/bin/DCPSInfoRepo -ORBEndpoint iiop://:4000 -ORBSvcConf tcp.conf -o /tmp/repo.ior -d domain_ids

#!/bin/bash

acsStart

# Starts a consumer and a supplier in the same process, sending N events every M seconds T times
acsStartJava alma.acs.nc.refactored.SimpleSupplierConsumerClient 10 15 2 &
sleep 10
acsNotifyService -k
acsNotifyService -s

wait

acsStop

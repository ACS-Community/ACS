#!/bin/bash
export ACS_TMP=../test/tmp
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
acsutilTATTestRunner logClient 2 5000  0 >/dev/null &
wait
acsutilTATTestRunner logClient 2 1000  1 2>&1 
sleep 30
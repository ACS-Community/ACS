#! /bin/bash
acsutilAwaitContainerStart -$5 $1
sleep 3
acsStartJava alma.lifecycleTest.TestLifeCycleCompImpl.TestMultipleComponents $2 $3 $4
sleep 3
acsStopContainer -t 10 $1


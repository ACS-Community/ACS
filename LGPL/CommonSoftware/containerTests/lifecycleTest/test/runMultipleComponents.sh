#! /bin/bash
acsutilAwaitContainerStart -cpp $1
sleep 3
acsStartJava alma.lifecycleTest.TestLifeCycleCompImpl.TestMultipleComponents $2 $3 $4
sleep 3
acsStopContainer $1


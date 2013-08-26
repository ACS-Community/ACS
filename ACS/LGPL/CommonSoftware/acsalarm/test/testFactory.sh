#!/bin/bash
export PATH=$PATH:../bin
acsStartORBSRVC
sleep 20
FactoryTestCase
acsStopORBSRVC

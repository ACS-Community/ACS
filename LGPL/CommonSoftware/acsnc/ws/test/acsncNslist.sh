#!/bin/bash
. acsstartupAcsInstance
ACS_TMP_DIR=`getInstanceDirName $ACS_INSTANCE`
NS_IOR=`cat $ACS_TMP_DIR/iors/NameServiceIOR`
$ACE_ROOT/TAO/utils/nslist/tao_nslist --ns $NS_IOR

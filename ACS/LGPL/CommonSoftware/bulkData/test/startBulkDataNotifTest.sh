#!/bin/bash
# Start the activator

. acsstartupAcsPorts

export ACS_INSTANCE=`cat $ACS_TMP/acs_instance`

acsStartContainer -cpp ContainerNotifTest



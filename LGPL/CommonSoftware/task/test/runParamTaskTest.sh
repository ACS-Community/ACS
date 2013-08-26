#!/bin/sh
if [ "`uname`" = "Linux" ]; then enable -n echo; fi

#DWF-added to discover the place this test fails during SE's nightly build/test. 
#    Note that this failure cannot be duplicated simply by running the test or tat
#    by hand.
export ACS_LOG_STDOUT=2


paramTaskTest -nosvcs msname=myvladata.ms inputfile=c477_040502.xpl project=ad477 start=2004/05/02/18:30:00 stop=2004/05/02/19:00:00 centerfreq=1.489 bandwidth=10.01 bandname=P source=3C49 overwrite=1 testString=changeme testInt=-12 testStringArray="1 with space ",'2 space','3 space' testIntArray=1,2,3 testDoubleArray=-1.1,2.2,3.3d5

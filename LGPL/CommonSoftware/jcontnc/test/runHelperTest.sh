#!/bin/bash

# The following lines are commented out because this issue now gets tracked in http://jira.alma.cl/browse/COMP-8956 
# and we rather have fewer NRI failures
# 
#echo "Dumped naming service bindings for 'ChannelFactory' objects: (to investigate recurrent problems with 'Failed to resolve factory's MC extension object in the naming service')" 
#acsStartJava org.jacorb.naming.ContextLister | grep ChannelFactory | sort | paste -s

acsStartJava -endorsed alma.acs.testsupport.tat.TATJUnitRunner   alma.acs.nc.HelperTest

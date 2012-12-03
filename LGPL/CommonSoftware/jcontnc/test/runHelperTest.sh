#!/bin/bash

echo "Dumped naming service bindings for 'ChannelFactory' objects: (to investigate recurrent problems with 'Failed to resolve factory's MC extension object in the naming service')" 
acsStartJava org.jacorb.naming.ContextLister | grep ChannelFactory | sort | paste -s

acsStartJava -endorsed alma.acs.testsupport.tat.TATJUnitRunner   alma.acs.nc.HelperTest

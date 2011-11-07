package acs.benchmark.nc.consumer;

import acs.benchmark.nc.CorbaNotifyBaseImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.benchmark.CorbaNotifyConsumerOperations;
import alma.benchmark.NcEventSpec;

public class CorbaNotifyConsumerImpl extends CorbaNotifyBaseImpl implements CorbaNotifyConsumerOperations
{

	@Override
	public void receiveEvents(NcEventSpec[] ncEventSpec, int processingDelayMillis, int numberOfEvents)
			throws CouldntPerformActionEx {
		// TODO Auto-generated method stub

	}

}

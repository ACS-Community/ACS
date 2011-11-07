package acs.benchmark.nc.supplier;

import acs.benchmark.nc.CorbaNotifyBaseImpl;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.benchmark.CorbaNotifySupplierOperations;
import alma.benchmark.NcEventSpec;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

public class CorbaNotifySupplierImpl extends CorbaNotifyBaseImpl implements CorbaNotifySupplierOperations
{

	@Override
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);
		// TODO Auto-generated method stub
		
	}

	@Override
	public void cleanUp() throws AcsJComponentCleanUpEx {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void sendEvents(NcEventSpec[] ncEventSpec, int delayBetweenEvents, int numberOfEvents)
			throws CouldntPerformActionEx {
		// TODO Auto-generated method stub
		
	}

}

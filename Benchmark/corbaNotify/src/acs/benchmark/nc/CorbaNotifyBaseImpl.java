package acs.benchmark.nc;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.benchmark.CorbaNotifyCompBaseOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

public class CorbaNotifyBaseImpl extends ComponentImplBase implements CorbaNotifyCompBaseOperations
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
	public void ncConnect(String[] ncNames) throws CouldntPerformActionEx {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void ncDisconnect() throws CouldntPerformActionEx {
		// TODO Auto-generated method stub
		
	}

}

package acs.benchmark.nc;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.benchmark.CorbaNotifyCompBaseOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

public abstract class CorbaNotifyBaseImpl extends ComponentImplBase implements CorbaNotifyCompBaseOperations
{

	/**
	 * Flag set by interrupt method
	 */
	protected volatile boolean cancel;

	@Override
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);
	}

	@Override
	public void cleanUp() throws AcsJComponentCleanUpEx {
	}

	@Override
	public void interrupt() {
		cancel = true;
	}

}

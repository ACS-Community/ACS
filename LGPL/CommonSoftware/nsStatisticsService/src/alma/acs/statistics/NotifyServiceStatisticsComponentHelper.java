package alma.acs.statistics;

import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import alma.ACS.ACSComponentOperations;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.maciErrType.wrappers.AcsJComponentCreationEx;

import alma.statistics.NotifyServiceStatisticsComponentOperations;
import alma.statistics.NotifyServiceStatisticsComponentPOATie;

public class NotifyServiceStatisticsComponentHelper extends ComponentHelper {

	public NotifyServiceStatisticsComponentHelper(Logger containerLogger) {
		super(containerLogger);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected ComponentLifecycle _createComponentImpl()
			throws AcsJComponentCreationEx {
		// TODO Auto-generated method stub
		return new NotifyServiceStatisticsComponentImpl();
	}

	@Override
	protected Class<? extends Servant> _getPOATieClass() {
		// TODO Auto-generated method stub
		return NotifyServiceStatisticsComponentPOATie.class;
	}

	@Override
	protected Class<? extends ACSComponentOperations> _getOperationsInterface() {
		// TODO Auto-generated method stub
		return NotifyServiceStatisticsComponentOperations.class;
	}

}

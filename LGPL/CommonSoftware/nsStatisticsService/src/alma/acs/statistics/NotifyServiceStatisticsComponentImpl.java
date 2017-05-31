package alma.acs.statistics;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

import alma.statistics.NotifyServiceStatisticsComponentOperations;

import alma.acs.nsstatistics.EventModel;
import alma.acs.service.ServiceParameters;
import alma.acs.service.nsStatisticsService;

public class NotifyServiceStatisticsComponentImpl implements ComponentLifecycle, NotifyServiceStatisticsComponentOperations {
	
	private Logger logger;
	private ContainerServices containerServices;
	private nsStatisticsService notifyServiceStatisticsServiceThread;
	
	private EventModel eventModel;
	
	@Override
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {

		this.containerServices = containerServices;
		this.logger = containerServices.getLogger();
		
		this.logger.info("Initializing NotifyService Statistics Component");
		
		try {
			this.eventModel = EventModel.getInstance();
		} catch (Throwable e) {
			e.printStackTrace();
		}

		this.notifyServiceStatisticsServiceThread = null;
	}

	@Override
	public void execute() throws ComponentLifecycleException {
		this.start();
	}

	@Override
	public void cleanUp() throws AcsJComponentCleanUpEx {
		// TODO Auto-generated method stub
		this.stop();
	}

	@Override
	public void aboutToAbort() {
		// TODO Auto-generated method stub

	}

	@Override
	public String name() {
		// TODO Auto-generated method stub
		return this.containerServices.getName();
	}

	@Override
	public ComponentStates componentState() {
		// TODO Auto-generated method stub
		return this.containerServices.getComponentStateManager().getCurrentState();
	}

	@Override
	public void start() {
		this.logger.info("Starting Notify Service Statistics...");
		
		if (this.notifyServiceStatisticsServiceThread != null && this.notifyServiceStatisticsServiceThread.isAlive()){
			this.logger.warning("An instance of nsStatistics is already active");
			return;
		}
		
		this.logger.fine("Starting nsStatistics thread");
		
		// TODO: Read from TMCDB configurations...
		String[] args = { "-f",  "1" };
		ServiceParameters params = new ServiceParameters();
		List<String> errors = new ArrayList<String>();
		params.read("nsStatistics", args, errors);
		
		Logger logger = eventModel.getLogger();
		
		this.notifyServiceStatisticsServiceThread = new nsStatisticsService(eventModel, params);
		this.notifyServiceStatisticsServiceThread.start();
		
		this.logger.info("nsStatistics started");
	}
	
	public void stop() {
		this.logger.info("Stopping Notify Service Statistics");
		
		if(this.notifyServiceStatisticsServiceThread != null) {
			this.notifyServiceStatisticsServiceThread.stopIt();
		}
	}	
}


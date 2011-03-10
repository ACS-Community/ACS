/**
 * 
 */
package alma.acs.monitoring.blobber;
import java.lang.reflect.Constructor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.ControllerOperations;
import alma.MonitorArchiver.ControllerHelper;
import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.concurrent.ThreadLoopRunner;
import alma.acs.concurrent.ThreadLoopRunner.ScheduleDelayMode;
import alma.acs.container.ContainerServices;

/**
 * Implementation of the blobber component.
 * @author apersson, hsommer
 */
public class BlobberImpl extends ComponentImplBase implements BlobberOperations {

	/**
	 * The code that gets run at fixed time intervals.
	 */
	protected BlobberWorker myWorker;

	/**
	 * Default interval between collecting data.
	 * @see BlobberPlugin#getCollectorIntervalSec()
	 */
	protected int collectorIntervalSec;

	/**
	 * We use this convenience class from ACS to execute the blobber code at fixed intervals.
	 */
	protected ThreadLoopRunner blobberLoopRunner;

	
    //////////////////////////////////////
    ///////// ComponentLifecycle /////////
    //////////////////////////////////////

	/**
	 * Initializes the component: creates the BlobberPlugin and the BlobberWorker objects.
	 * <p>
	 * Raises an alarm (FF="Monitoring", FM="MonitorArchiver", FC="2") if initialization fails.
	 * This makes sense because the blobber runs as an autostart component, and the manager being 
	 * the client that activates this blobber would not know the alarm details. 
	 * @TODO: Shouldn't we use a FM specific to this blobber instance? 
	 * 
	 * @see alma.acs.component.ComponentImplBase#initialize(alma.acs.container.ContainerServices)
	 */
	@Override
	public void initialize(ContainerServices inContainerServices) throws ComponentLifecycleException {
		super.initialize(inContainerServices);

		BlobberPlugin blobberPlugin = null;
		try {
			try {
				inContainerServices.clearAlarm("Monitoring", "MonitorArchiver", 2);
			} catch (AcsJContainerServicesEx ex) {
				throw new ComponentLifecycleException("Failed to clear alarm", ex);
			}
			blobberPlugin = createBlobberPlugin();
			collectorIntervalSec = blobberPlugin.getCollectorIntervalSec();
			// Create the blobber runnable (worker)
			this.myWorker = createWorker(blobberPlugin);
		} catch (AcsJCouldntCreateObjectEx ex) {
			try {
				inContainerServices.raiseAlarm("Monitoring", "MonitorArchiver", 2);
			} catch (AcsJContainerServicesEx ex1) {
				m_logger.severe("Blobber initialization failed and alarm could not be raised.");
				// fall through to ComponentLifecycleException
			}
			throw new ComponentLifecycleException(ex);
		}	

		ControllerOperations controller = null;
		try {
			controller = ControllerHelper.narrow(m_containerServices
				.getDefaultComponent("IDL:alma/MonitorArchiver/Controller:1.0"));
		} catch (AcsJContainerServicesEx ex1) {
			throw new ComponentLifecycleException(
				"Failed to get ARCHIVE_CONTROLLER instance.", ex1);
		}
		controller.registerKnownCollectors(name());
	}

	/**
	 * Creates (using reflection) an instance of <code>alma.acs.monitoring.blobber.BlobberPluginAlmaImpl</code>
	 * that <code>ARCHIVE/TMCDB/</code> modules must provide.
	 * <p>
	 * Overriding this method allows other projects or unit tests that should run without alma archive code 
	 * to create a different implementation of <code>BlobberPlugin</code>.
	 * <p>
	 * @TODO: Perhaps also allow specifying the plugin impl class through a property 
	 *        to avoid having to subclass BlobberImpl to get an alternative plugin impl.
	 */
	protected BlobberPlugin createBlobberPlugin() throws AcsJCouldntCreateObjectEx {
        try {
			Class<? extends BlobberPlugin> pluginClass = Class.forName("alma.acs.monitoring.blobber.BlobberPluginAlmaImpl").asSubclass(BlobberPlugin.class);
			Constructor<? extends BlobberPlugin> ctor = pluginClass.getConstructor(Logger.class);
			return ctor.newInstance(m_logger);
		} catch (Exception ex) {
			AcsJCouldntCreateObjectEx ex2 = new AcsJCouldntCreateObjectEx(ex);
			throw ex2;
		} 
	}

	/**
	 * Factored out from {@link #initialize(ContainerServices)} for testing with mock BlobberWorker
	 * @throws AcsJCouldntCreateObjectEx 
	 */
	protected BlobberWorker createWorker(BlobberPlugin blobberPlugin) throws AcsJCouldntCreateObjectEx {
		return new BlobberWorker(m_containerServices, blobberPlugin);
	}

	/**
	 * Repeatedly runs the blobber worker code in a separate thread (every 60 seconds). If any execution of the blobber worker
	 * takes longer than 60 s, then subsequent executions may start late, but will not concurrently execute.
	 * @see alma.acs.component.ComponentImplBase#execute()
	 */
	@Override
	public void execute() {
		blobberLoopRunner = new ThreadLoopRunner(this.myWorker, collectorIntervalSec, TimeUnit.SECONDS, m_containerServices.getThreadFactory(), m_logger);
		blobberLoopRunner.setDelayMode(ScheduleDelayMode.FIXED_RATE);
		blobberLoopRunner.runLoop();
		m_logger.info("BlobberWorker thread loop is running.");
	}

//	/**
//	 * Modification of the collector interval during blobber runs 
//	 * is only used in the legacy tests, not for blobber operations.
//	 * @TODO Remove this method and the tests, unless there is a clear purpose.
//	 * @param collectorIntervalSeconds	new interval in seconds, must be greater than zero.
//	 * @see ThreadLoopRunner#setDelayTime(long, TimeUnit)
//	 */
//	protected boolean setCollectorIntervalSeconds(long collectorIntervalSeconds) {
//		if (collectorIntervalSeconds <= 0) {
//			throw new IllegalArgumentException("collectorIntervalSeconds must be > 0.");
//		}
//		myWorker.notifyCollectorIntervalChange(collectorIntervalSeconds);
//		return blobberLoopRunner.setDelayTime(collectorIntervalSeconds, TimeUnit.SECONDS);
//	}
//	
	
	/**
	 * Stops the blobber loop, but does not forcibly stop the running blobber.
	 * However this method returns after 30 seconds, even if the blobber has not yet finished by then.
	 * @see alma.acs.component.ComponentImplBase#cleanUp()
	 */
	@Override
	public void cleanUp() {
		if (blobberLoopRunner != null && blobberLoopRunner.isLoopRunning()) {
			try {
				if (blobberLoopRunner.shutdown(30, TimeUnit.SECONDS)) {
					m_logger.info("Shut down the BlobberWorker thread loop.");
				}
				else {
					m_logger.warning("Failed to cleanly shut down the blobber loop within 30 s.");
				}
			} catch (InterruptedException ex) {
				m_logger.warning("Thread interrupted while shutting down the blobber loop");
			}
		}
	}


    /////////////////////////////////
    ///////// IDL interface /////////
    /////////////////////////////////

    @Override
    public CollectorListStatus addCollector(String inComponentName) {
        return this.myWorker.addCollector(inComponentName);
    }

    @Override
    public CollectorListStatus containsCollector(String inComponentName) {
        return this.myWorker.containsCollector(inComponentName);
    }

    @Override
    public CollectorListStatus removeCollector(String inComponentName) {
        return this.myWorker.removeCollector(inComponentName);
    }

}

/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

/**
 * 
 */
package alma.acs.monitoring.blobber;
import java.lang.reflect.Constructor;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.MonitorArchiver.ControllerHelper;
import alma.MonitorArchiver.ControllerOperations;
import alma.acs.alarmsystem.source.AlarmSource;
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

	private static final String MONITOR_BLOBBER_PLUGIN = System.getProperty("alma.acs.monitoring.blobber.plugin", "alma.acs.monitoring.blobber.BlobberPluginAlmaImpl");

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

	protected BlobberPlugin blobberPlugin;

	protected AlarmSource alarmSource;
	
	/**
	 * There have been problems with slow blobber activation (http://ictjira.alma.cl/browse/ICT-462).
	 * Note that also non-ACS code gets run (plugin, DAOs, JMX setup and possibly invocations).
	 * During startup we feed this map with "activity" - "time elapsed in ns" pairs and log a single profiling message
	 * in the end of {@link #execute()}.
	 * @see #logComponentActivationTimes()
	 */
	protected Map<String, Long> componentActivationTimesNanosMap = new LinkedHashMap<String, Long>();
	
	/**
	 * Similar to {@link #componentActivationTimesNanosMap}, but used to compute the total component activation.
	 */
	protected long componentActivationStartNanos;
	
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
		long now = System.nanoTime();
		componentActivationStartNanos = now;
		super.initialize(inContainerServices);
		componentActivationTimesNanosMap.put("super init", System.nanoTime() - now);

		now = System.nanoTime();
		alarmSource = inContainerServices.getAlarmSource();
		// clear alarm that might have been left active from a previous run
		alarmSource.clearAlarm("Monitoring", "MonitorArchiver", 2); // @TODO use component-specific alarm triplet
		componentActivationTimesNanosMap.put("alarm init", System.nanoTime() - now);

		try {
			now = System.nanoTime();
			blobberPlugin = createBlobberPlugin();
			componentActivationTimesNanosMap.put("plugin creation", System.nanoTime() - now);
			now = System.nanoTime();
			blobberPlugin.init();
			componentActivationTimesNanosMap.put("plugin init", System.nanoTime() - now);

			collectorIntervalSec = blobberPlugin.getCollectorIntervalSec();
			m_logger.finer("Instantiated blobber plugin object.");
			// Create the blobber runnable (worker)
			now = System.nanoTime();
			this.myWorker = createWorker();
			componentActivationTimesNanosMap.put("worker creation", System.nanoTime() - now);
			m_logger.finer("Instantiated blobber worker object.");
		} catch (AcsJCouldntCreateObjectEx ex) {
			alarmSource.raiseAlarm("Monitoring", "MonitorArchiver", 2);
			throw new ComponentLifecycleException(ex);
		}	

		// In case this blobber is restarting after a shutdown while collectors were still assigned, 
		// we ask the controller to add these otherwise lost collectors.
		ControllerOperations controller = null;
		now = System.nanoTime();
		try {
			controller = ControllerHelper.narrow(m_containerServices
				.getDefaultComponent("IDL:alma/MonitorArchiver/Controller:1.0"));
			controller.registerKnownCollectors(name());
			m_logger.finer("Requested monitor controller to re-register collectors.");
		} catch (AcsJContainerServicesEx ex1) {
			throw new ComponentLifecycleException(
				"Failed to get ARCHIVE_CONTROLLER instance.", ex1);
		} finally {
			if (controller != null) {
				m_containerServices.releaseComponent(controller.name(), null);
			}
		}
		componentActivationTimesNanosMap.put("controller handshake", System.nanoTime() - now);
	}

	/**
	 * Creates (using reflection) an instance of <code>alma.acs.monitoring.blobber.BlobberPluginAlmaImpl</code>
	 * that <code>ARCHIVE/TMCDB/</code> modules must provide.
	 * <p>
	 * Overriding this method allows other projects or unit tests that should run without alma archive code 
	 * to create a different implementation of <code>BlobberPlugin</code>.
	 */
	protected BlobberPlugin createBlobberPlugin() throws AcsJCouldntCreateObjectEx {
		try {
			Class<? extends BlobberPlugin> pluginClass = Class.forName(MONITOR_BLOBBER_PLUGIN).asSubclass(BlobberPlugin.class);
			Constructor<? extends BlobberPlugin> ctor = pluginClass.getConstructor(ContainerServices.class);
			return ctor.newInstance(m_containerServices);
		} catch (Exception ex) {
			AcsJCouldntCreateObjectEx ex2 = new AcsJCouldntCreateObjectEx(ex);
			throw ex2;
		} 
	}

	/**
	 * Factored out from {@link #initialize(ContainerServices)} for testing with mock BlobberWorker
	 * @throws AcsJCouldntCreateObjectEx 
	 */
	protected BlobberWorker createWorker() throws AcsJCouldntCreateObjectEx {
		return new BlobberWorker(m_containerServices, blobberPlugin);
	}

	/**
	 * Repeatedly runs the blobber worker code in a separate thread (every 60 seconds). If any execution of the blobber worker
	 * takes longer than 60 s, then subsequent executions may start late, but will not concurrently execute.
	 * @see alma.acs.component.ComponentImplBase#execute()
	 */
	@Override
	public void execute() {
		long now = System.nanoTime();
		blobberLoopRunner = new ThreadLoopRunner(this.myWorker, collectorIntervalSec, TimeUnit.SECONDS, 
				m_containerServices.getThreadFactory(), m_logger, name());
		blobberLoopRunner.setDelayMode(ScheduleDelayMode.FIXED_RATE);
		blobberLoopRunner.runLoop();
		componentActivationTimesNanosMap.put("start worker", System.nanoTime() - now);
		m_logger.info("BlobberWorker thread loop is running.");
		logComponentActivationTimes();
	}

	/**
	 * Creates an INFO log from the times collected in {@link #componentActivationTimesNanosMap}
	 * and then nulls that map.
	 */
	protected void logComponentActivationTimes() {
		if (componentActivationTimesNanosMap != null && m_logger.isLoggable(Level.INFO)) {
			String msg = "Elapsed times in ms to activate blobber '" + name() + "': ";
			for (String activityName : componentActivationTimesNanosMap.keySet()) {
				long timeElapsedMillis = TimeUnit.NANOSECONDS.toMillis(
						componentActivationTimesNanosMap.get(activityName) );
				msg += activityName + " = " + timeElapsedMillis + ", ";
			}
			long totalMillis = TimeUnit.NANOSECONDS.toMillis(
					System.nanoTime() - componentActivationStartNanos );
			msg += "Total = " + totalMillis;
			m_logger.info(msg);
		}
		componentActivationTimesNanosMap = null;
		componentActivationStartNanos = -1;
	}
	
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
		blobberPlugin.cleanUp();
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

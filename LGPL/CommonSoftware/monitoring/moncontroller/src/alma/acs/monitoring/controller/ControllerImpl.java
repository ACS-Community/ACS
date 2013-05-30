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
package alma.acs.monitoring.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.BlobberHelper;
import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.MonitorArchiver.ControllerOperations;
import alma.MonitorErr.CollectorRegistrationFailedEx;
import alma.MonitorErr.wrappers.AcsJCollectorRegistrationFailedEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.controller.BlobberList.BlobberInfo;


/**
 * The monitor collector components contact the controller, to get assigned to a blobber.
 */
public class ControllerImpl extends ComponentImplBase implements ControllerOperations {

	/**
	 * The IDL type of the blobber components ("IDL:alma/MonitorArchiver/Blobber:1.0"). 
	 * Used to retrieve all configured instances of this type.
	 */
	private static final String BLOBBER_IDL_TYPE = BlobberHelper.id();

	/**
	 * List of known blobbers, either just the component names or also the references.
	 */
	protected final BlobberList blobberList = new BlobberList();

	/**
	 * Maps names of MonitorCollector components to the corresponding Blobber components. 
	 * The blobber name is expected to be contained in {@link #myBlobberList}.
	 * <p>
	 * @TODO: Replace this map with a list of collectors inside BlobberInfo,
	 * to avoid possible inconsistencies. The performance loss cannot be serious with
	 * 10 - 66 blobber components in the full system.
	 */
	protected final Map<String, String> collector2BlobberName = new HashMap<String, String>();


	//////////////////////////////////////
	///////// ComponentLifecycle /////////
	//////////////////////////////////////

	@Override
	public void initialize(ContainerServices inContainerServices) throws ComponentLifecycleException {
		super.initialize(inContainerServices);

		try {
			String[] componentNames = getConfiguredBlobberCompNames();
			if (componentNames.length == 0) {
				throw new ComponentLifecycleException("No blobbers (" + BLOBBER_IDL_TYPE
						+ ") found in the CDB. At least one must be configured.");
			}

			for (String componentName : componentNames) {
				try {
					blobberList.createBlobberInfo(componentName);
				} catch (AcsJIllegalArgumentEx ex) {
					m_logger.warning("Failed to process blobber component name '" + componentName + "', which apparently is a duplicate.");
				}
			}
			m_logger.info("The following configured blobber components were found: "
					+ blobberList.getBlobberNames(false) + ". Blobbers will be requested later on demand.");
		} catch (AcsJContainerServicesEx e) {
			throw new ComponentLifecycleException(e);
		}
	}

	/**
	 * Broken out from {@link #initialize(ContainerServices)} to allow mock impl.
	 * @throws AcsJContainerServicesEx
	 */
	protected String[] getConfiguredBlobberCompNames() throws AcsJContainerServicesEx {
		return m_containerServices.findComponents(null, BLOBBER_IDL_TYPE);
	}
	
	@Override
	public void cleanUp() {
                m_logger.info("cleanUp");
		for (String blobberName : blobberList.getBlobberNames(true)) {
                        m_logger.info("blobberName=" + blobberName);
			// async release of blobber components
			m_containerServices.releaseComponent(blobberName, null);
		}
	}

	///////////////////////////////////////////////
	//////////// Controller IDL interface /////////
	///////////////////////////////////////////////

	/**
	 * Note that a collector may call this method before all blobber containers have started. Thus we may get blobber
	 * activation failures in the beginning that later go away.
	 * 
	 * @throws CollectorRegistrationFailedEx If the collector cannot be registered with any of the available blobbers.
	 *         Here we only log this problem; the calling collector is expected to raise an alarm. 
	 * @see alma.MonitorArchiver.ControllerOperations#registerCollector(java.lang.String)
	 */
	@Override
	public void registerCollector(String collectorCompName) throws CollectorRegistrationFailedEx {
		m_logger.info("Attempting to register collector " + collectorCompName);
		String errorMsg = null;
		String blobberName = null;
		int numBlobbers = blobberList.size();
		m_logger.info("First evaluation. numBlobbers=" + numBlobbers);

		if (numBlobbers == 0) {
			errorMsg = "No blobbers available.";
		}
		else {
			try {
				blobberName = findBlobberAssignedToCollector(collectorCompName, false);
                                m_logger.info("After of findBlobberAssignedToCollector. blobberName=" + blobberName);
				
				if (blobberName == null) {
                                        m_logger.info("blobberName is null");
					// Assign the collector to the first available blobber.
					// attemptCount is used to eventually exit the loop even if all blobbers are full;
					//   in this case we generously iterate more than once over the blobbers, to benefit from new blobbers
					//   being possibly added during this iteration, or blobbers becoming available by other collectors signing out.
					// In the good case we exit the loop as soon as the collector has been registered.
					for (int attemptCount = 0; attemptCount < 2 * numBlobbers; attemptCount++) {
						BlobberInfo nextBlobberInfo = blobberList.getNextBlobberInfo();
                                                m_logger.info("nextBlobberInfo.blobberName=" + nextBlobberInfo.blobberName);
						getBlobberRef(nextBlobberInfo, true);
						if (nextBlobberInfo.blobberRef != null) {
                                                        m_logger.info("nextBlobberInfo.blobberRef is not null");
							CollectorListStatus status = nextBlobberInfo.blobberRef.addCollector(collectorCompName);
                                                        m_logger.info("status=" + status);
							if (status == CollectorListStatus.ADDED) {
								collector2BlobberName.put(collectorCompName, nextBlobberInfo.blobberName);
								blobberName = nextBlobberInfo.blobberName;
								break;
							}
						}
                                                else {
                                                        m_logger.info("nextBlobberInfo.blobberRef is null");
                                                }
					 }
				}
                                else {
                                       m_logger.info("blobberName is not null");
                                }
				if (blobberName == null) {
					// Bad... let's update the number of blobbers for the exception, in case it has changed from the earlier number
					numBlobbers = blobberList.size();
                                        m_logger.info("Second evaluation. numBlobbers=" + numBlobbers);
					errorMsg = "Collector was not accepted by any of the " + numBlobbers + " blobber(s).";
				} 
				else {
					m_logger.info("Collector '" + collectorCompName + "' registered to blobber '" + blobberName + "'.");
				}
			} catch (Exception e) {
				errorMsg = "Unexpected problem: " + e.getMessage();
			}
		}
		
		if (errorMsg != null) {
			m_logger.severe("Failed to register collector '" + collectorCompName + "': " + errorMsg);
			
			alma.MonitorErr.wrappers.AcsJCollectorRegistrationFailedEx ex = new AcsJCollectorRegistrationFailedEx();
			ex.setNumberOfBlobbers(numBlobbers);
			throw ex.toCollectorRegistrationFailedEx();
		}
	}

	@Override
	public void deregisterCollector(String collectorCompName) {
		m_logger.info("Attempting to de-register collector " + collectorCompName);
		m_logger.info("collectorCompName=" + collectorCompName);

		boolean removed = false;
		String blobberName = null;
		
		try {
			// cheap cache-based attempt at first
			blobberName = findBlobberAssignedToCollector(collectorCompName, true);
                        m_logger.info("After of findBlobberAssignedToCollector. blobberName=" + blobberName);

			if (blobberName != null) {
                                m_logger.info("blobberName is not null");
				BlobberInfo blobberInfo = blobberList.getBlobberInfo(blobberName);
                                m_logger.info("blobberInfo.blobberName=" + blobberInfo.blobberName);
				getBlobberRef(blobberInfo, true); // just in case, should not be needed though.
				CollectorListStatus status = blobberInfo.blobberRef.removeCollector(collectorCompName);
                                m_logger.info("status=" + status);
				if (status == CollectorListStatus.REMOVED) {
					removed = true;
                                        m_logger.info("status is removed");
                                        m_logger.info("removed is true");
				}
				else {
					// TODO log unexpected status CollectorListStatus.UNKNOWN (e.g. cache error, blobber error)
                                        m_logger.info("status is not removed");
				}
				collector2BlobberName.remove(collectorCompName);
			}
			else {
                                m_logger.info("blobberName is null");
				// log unexpected missing blobber mapping.
			}
			
			if (!removed) {
                                m_logger.info("removed is false");
				// Now we call all blobbers to be sure our collector isn't assigned there by mistake
				blobberName = findBlobberAssignedToCollector(collectorCompName, false);
                                m_logger.info("blobberName = " + blobberName);
				if (blobberName != null) {
                                        m_logger.info("blobberName is not null");
					// cache was wrong in saying collector isn't assignied. We must deregister it from the surprise blobber.
					BlobberInfo blobberInfo = blobberList.getBlobberInfo(blobberName);
                                        m_logger.info("blobberInfo.blobberName=" + blobberInfo.blobberName);
					CollectorListStatus status = blobberInfo.blobberRef.removeCollector(collectorCompName);
                                        m_logger.info("status=" + status);
					if (status == CollectorListStatus.REMOVED) {
					    removed = true;
                                            m_logger.info("status is removed");
                                            m_logger.info("removed is true");
					}
					else {
						// TODO log unexpected status CollectorListStatus.UNKNOWN (blobber error)
                                            m_logger.info("status is not removed");
					}
				}
				else {
                                        m_logger.info("blobberName is null");
					// none of the blobbers knows our collector. 
					// TODO log the error, e.g. that this collector was never assigned.
				}
			}
		} catch (Exception e) {
			m_logger.warning("Unexpected problem while de-registering collector '" + collectorCompName 
					+ "' from blobber '" + blobberName + "': " + e.getMessage());
		}
		
		if (removed) {
			m_logger.info("Collector '" + collectorCompName + "' de-registered from blobber '" + blobberName + "'.");
		}
		else {
			// the error case we've already treated through log messages in the code above.
                        m_logger.info("Second evaluation. removed is false");
		}
	}

	/**
	 * @TODO: Shouldn't we give preference to the same old blobber, 
	 * especially since we do not yet do load balancing?
	 * 
	 * @see alma.MonitorArchiver.ControllerOperations#registerKnownCollectors(java.lang.String)
	 */
	@Override
	public void registerKnownCollectors(final String blobberCompName) {
                m_logger.info("Attempting to register known collectors " + blobberCompName);
		if (!collector2BlobberName.containsValue(blobberCompName)) {
			m_logger.fine("Blobber '" + blobberCompName
					+ "' seems to start cleanly, thus cannot register previously served collectors.");
			return;
		}

		m_containerServices.getThreadFactory().newThread(new Runnable() {
			public void run() {
				for (Map.Entry<String, String> m : collector2BlobberName.entrySet()) {
					if (blobberCompName.equals(m.getValue())) {
                                                m_logger.info("blobberCompName is equals to " + m.getValue());
						// Re-register known Collector
						try {
                                                        m_logger.info("Registering collector " + m.getKey());
							registerCollector(m.getKey());
						} catch (CollectorRegistrationFailedEx ex) {
							// registerCollector already has a SEVERE log, so here just the additional info about re-registration.
							m_logger.log(Level.WARNING, "Failed to asynchronously re-register collector '" + m.getKey()
									+ "' previously served by blobber '" + blobberCompName + "'.", ex); 
						}
					}
                                        else {
                                                m_logger.info("blobberCompName is not equals to " + m.getValue());
                                        }
				}
			}
		}).start();
		m_logger.info("Started asynchronous re-registration of collectors previously assigned to blobber '" + blobberCompName + "'.");
	}

    //////////////////////////////////////
    ///////// Other impl methods /////////
    //////////////////////////////////////

	/**
	 * Retrieves the blobber component reference for the given blobber name, unless we have it already.
	 * In case of success, the blobber reference is stored in the given <code>blobberInfo</code> object,
	 * otherwise the reference field stays <code>null</code>.
	 * 
	 * @param blobberInfo
	 * @param skipKnownProblematicBlobber If <code>true</code> then we skip the blobber reference retrieval 
	 *                                    if the last failure occurred less than 3 s ago.
	 */
	protected void getBlobberRef(BlobberInfo blobberInfo, boolean skipKnownProblematicBlobber) {
		synchronized (blobberInfo) {
			if (blobberInfo.blobberRef == null) {
                                m_logger.info("blobberInfo.blobberName=" + blobberInfo.blobberName + " , blobberInfo.blobberRef=null , skipKnownProblematicBlobber=" + skipKnownProblematicBlobber);
				if (!skipKnownProblematicBlobber || !blobberInfo.hadRefRequestFailureWithinLastSec(3)) {
                                        m_logger.info("Traying to get the ref from container services");
					try {
						blobberInfo.blobberRef = getBlobberRefFromContainerServices(blobberInfo.blobberName);
						blobberInfo.lastRefRequestFailedTimeMillis = -1;
					} catch (Exception ex) {
						blobberInfo.lastRefRequestFailedTimeMillis = System.currentTimeMillis();
                                                m_logger.info("blobberInfo.blobberName=" + blobberInfo.blobberName + " , failed to acquire refrence, save the error timestamp " + blobberInfo.lastRefRequestFailedTimeMillis);
                                                ex.printStackTrace();
						// todo log
					}
				}
                                else {
                                        m_logger.info("Was not possible tray to get the ref from container services");
                                }
			}
                        else {
                                m_logger.info("blobberInfo.blobberName=" + blobberInfo.blobberName + " , blobberInfo.blobberRef has been already acquired, just return");
                        }
		}
	}

	/**
	 * Broken out from {@link #getBlobberRef(BlobberInfo, boolean)}, 
	 * to allow mock impl in tests.
	 * @throws AcsJContainerServicesEx 
	 */
	protected BlobberOperations getBlobberRefFromContainerServices(String blobberName) throws AcsJContainerServicesEx {
		return BlobberHelper.narrow(m_containerServices.getComponent(blobberName));
	}
	
	/**
	 * @param collectorCompName
	 * @param trustLocalCache
	 * @return Name of blobber that is assigned to the give collector, or null if no blobber was found.
	 */
	protected String findBlobberAssignedToCollector(String collectorCompName, boolean trustLocalCache) {
                m_logger.info("Start findBlobberAssignedToCollector");
 
		String ret = null;
		
		// check the cache
                m_logger.info("collectorCompName=" + collectorCompName);
                m_logger.info("trustLocalCache=" + trustLocalCache);
		String assignedBlobberName = collector2BlobberName.get(collectorCompName);
		
		if (trustLocalCache) {
                        m_logger.info("trustLocalCache is true");
			ret = assignedBlobberName;
		}
		else {
                        m_logger.info("trustLocalCache is false");
			if (assignedBlobberName != null) {
                          m_logger.info("assignedBlobberName is not null");
				// validate that that the cache hit blobber is really assigned
				BlobberInfo assignedBlobberInfo = blobberList.getBlobberInfo(assignedBlobberName);
				if (assignedBlobberInfo != null && assignedBlobberInfo.blobberRef != null // should always be true
						&& assignedBlobberInfo.blobberRef.containsCollector(collectorCompName) == CollectorListStatus.KNOWN) {
					// confirmed 
					m_logger.fine("Collector '" + collectorCompName + "' assigned to blobber '" + assignedBlobberName + "' as expected.");
					ret = assignedBlobberName;
                                        m_logger.info("Into the if. ret=" + ret);
				}
				else {
					collector2BlobberName.remove(assignedBlobberName);
					m_logger.info("Cache error: collector '" + collectorCompName
							+ "' was expected to be assigned to blobber '" + assignedBlobberName
							+ "' but this blobber denies it. Will re-assign this collector.");
					// fallthrough: check other blobbers
				}
			}
                        else {
                          m_logger.info("assignedBlobberName is null");
                        }
			if (ret == null) {
				// Getting here means the collector was not found in the cache, 
				// or the cache did not reflect the actual assignment (only possible by very strange error).
				//
				// TODO: Iterate over all blobbers and check if the collector is assigned to one of them.
				// This is unlikely, but a mistake would be really bad for the collector and the monitor data flow in general.
                                m_logger.info("ret is null. It is dangerous");
			}
		}
                m_logger.info("Before of the output. ret=" + ret);
		return ret;
	}
	
}

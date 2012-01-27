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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.BlobberHelper;
import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.MonitorArchiver.ControllerOperations;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;


/**
 * The monitor collector components contact the controller, to get assigned to a blobber.
 * <p>
 * @TODO (HSO): Any access to the blobber components should check for corba runtime exceptions, 
 * and those blobbers should eventually be deregistered (and the monitor collectors they served be re-assigned)
 */
public class ControllerImpl extends ComponentImplBase implements ControllerOperations {

    /**
     * The IDL type of the blobber components. Used to retrieve all instances of this type.
     */
    private static final String BLOBBER_IDL_TYPE = "IDL:alma/MonitorArchiver/Blobber:1.0";

    /**
     * List of blobber component names.
     */
    protected final List<String> myBlobberList = new ArrayList<String>();

	/**
	 * Mapping Blobber name to known CORBA reference.
	 * <p>
	 * @TODO (HSO): If myBlobberRefMap and myBlobberList contain information about the same blobbers, (which it seems
	 *       to me it does), then we should try to keep that data only once. How about using a LinkedHashMap for myBlobberRefMap
	 *       whose reliable iteration order then makes myBlobberList unnecessary?
	 *       We would avoid possible misalignment between the two, at the expense that we'd have to iterate
	 *       over myBlobberRefMap.keySet where now we call myBlobberList(index).
	 */
	protected final Map<String, BlobberOperations> myBlobberRefMap = new HashMap<String, BlobberOperations>();

	/**
	 * Maps names of MonitorCollector components to the corresponding Blobber components. 
	 * The blobber name is expected to be contained in {@link #myBlobberList}.
	 */
	protected final Map<String, String> collector2BlobberName = new HashMap<String, String>();

    /**
     * Current position in {@link #myBlobberList}, pointing to the registered blobber component
     * which will serve the next incoming collector.
     * @see #addCollector(String) 
     */
    protected int myBlobberListIndex = 0;

    
    //////////////////////////////////////
    ///////// ComponentLifecycle /////////
    //////////////////////////////////////

    @Override
    public void cleanUp() {
    	synchronized (myBlobberList) {
            for (String blobberName : myBlobberList) {
                if (myBlobberRefMap.containsKey(blobberName)) {
                    m_containerServices.releaseComponent(blobberName, null);
                }
            }
    	}
    }

    @Override
    public void initialize(ContainerServices inContainerServices)
            throws ComponentLifecycleException {
        super.initialize(inContainerServices);

        try {
			String[] componentNames = m_containerServices.findComponents(null, BLOBBER_IDL_TYPE);
            if (componentNames.length == 0) {
                throw new ComponentLifecycleException(
                        "No blobbers (" + BLOBBER_IDL_TYPE + ") found in the CDB. At least one must be configured.");
            }
            myBlobberList.addAll(Arrays.asList(componentNames));
            m_logger.info("The following blobbers were found: " + myBlobberList);
        } catch (AcsJContainerServicesEx e) {
            throw new ComponentLifecycleException(e);
        }
    }

    /////////////////////////////////
    ///////// IDL interface /////////
    /////////////////////////////////

    @Override
    public synchronized void registerCollector(String collectorCompName) {
        m_logger.info("Attempting to register collector " + collectorCompName);
        try {
            String blobberName = isRegistered(collectorCompName);
            if (blobberName == null) {
                blobberName = addCollector(collectorCompName);
                if (blobberName == null) {
                    // @TODO add dynamic blobber component (probably in method addCollector) and/or raise alarm.
                    m_logger.log(Level.SEVERE, collectorCompName + " could not be registered to any of the blobbers.");
                } else {
                    m_logger.info(collectorCompName + " was registered to blobber " + blobberName);
                }
            } else {
                m_logger.warning(collectorCompName + " is already registered to blobber " + blobberName);
            }
        } catch (Exception e) {
        	// @TODO (HSO): Shouldn't the IDL declare an exception for this?
            m_logger.log(Level.SEVERE, "Unexpected problem.", e);
        }
    }

	@Override
	public synchronized void deregisterCollector(String inComponentName) {
		try {
			boolean removed = false;
			String blobberName = null;
			synchronized (myBlobberList) {
				for (String name : myBlobberList) {
					blobberName = name;
					if (getBlobber(name).removeCollector(inComponentName) == CollectorListStatus.REMOVED) {
						collector2BlobberName.remove(inComponentName);
						removed = true;
						break;
					}
				}
			}
			if (!removed) {
				m_logger.severe(inComponentName + " could not be deregistered.");
				if (blobberName == null) {
					// @TODO (HSO): this can never happen, since blobberName gets assigned while iterating over the blobber name list,
					// regardless of whether that blobber served the given collector.
					m_logger.severe("Collector was not registered.");
				} else {
					// @TODO (HSO): If this means that we found the right blobber but failed to deregister the monitor collector,
					// then the log message should be changed.
					m_logger.severe("Collector was registered to blobber " + blobberName + ".");
				}
			} else {
				m_logger.info(inComponentName + " was deregistered from blobber " + blobberName + ".");
			}
		} catch (Exception e) {
			// @TODO (HSO): Shouldn't the IDL declare an exception for this?
			m_logger.log(Level.SEVERE, "Unexpected problem.", e);
		}
	}

    @Override
    public void registerKnownCollectors(String blobberCompName) {
        if (!collector2BlobberName.containsValue(blobberCompName)) {
            m_logger.fine("Blobber '" + blobberCompName + "' seems to start cleanly, thus cannot register previously served collectors.");
            return;
        }

        AsynchronousRegistration process = new AsynchronousRegistration(blobberCompName);
        Thread t = m_containerServices.getThreadFactory().newThread(process);
        t.start();
        m_logger.info("Started asynchronous registration of collectors previously assigned to " + blobberCompName);
    }


    //////////////////////////////////////
    ///////// Other impl methods /////////
    //////////////////////////////////////

    /**
     * Checks whether the given monitor collector component is registered with any of the blobber components.
     * @return The matching blobber component name, or <code>null</code> if none was found. 
     */
    protected String isRegistered(String inComponentName) throws AcsJContainerServicesEx {
        String outValue = null;
        synchronized (myBlobberList) {
            for (String blobberName : myBlobberList) {
                try {
                    if (getBlobber(blobberName).containsCollector(inComponentName) == CollectorListStatus.KNOWN) {
                        outValue = blobberName;
                        break;
                    }
                } catch (AcsJContainerServicesEx ex) {
                	// @TODO (HSO): what is meant by "inactive"? Is it normal that we configure blobber instances that cannot be instantiated?
                    m_logger.log(Level.WARNING, "Failed to get the configured Blobber component '" + blobberName + 
                    		"', probably inactive. Will skip verification of its assignment to '" + inComponentName + "'.", ex);
                }
            }
        }
        return outValue;
    }

    /**
     * Iterates over the list of blobbers, starting from {@link #myBlobberListIndex},
     * and attempts to add the given collector to the first available blobber.
     * @param collectorCompName Name of collector component.
     * @return Name of blobber component to which the collector was added, or <code>null</code> if none of the blobbers could add the collector.
     * @throws IllegalStateException if no blobber components are available.
     *         @TODO: This case should perhaps be treated together with having only blobbers that no longer accept collectors.
     * @throws AcsJContainerServicesEx if a blobber component reference cannot be retrieved.
     */
    protected String addCollector(String collectorCompName) throws Exception {
        String outValue = null;
    	synchronized (myBlobberList) {
	        int startIndex = myBlobberListIndex;
	        if (myBlobberList.size() == 0) {
	            throw new IllegalStateException("Attempt to add a collector but the list of blobbers is empty.");
	        }
	        while (true) {
                    String blobberName = null;
                    if (collector2BlobberName.containsKey(collectorCompName)) {
                        blobberName = collector2BlobberName.get(collectorCompName);
                        m_logger.info(collectorCompName + " will be re-registered to blobber " + blobberName);
                        // @TODO: Change log message. We just try the blobber first which served or still serves this collector.
                        //        In the end the collector could end up with a different blobber if the first choice is full.
                    } else {
                        blobberName = myBlobberList.get(myBlobberListIndex);
                    }

                    // Register to Blobber
                    if (getBlobber(blobberName).addCollector(collectorCompName) == CollectorListStatus.ADDED) {
                        outValue = blobberName;
                        if (!collector2BlobberName.containsKey(collectorCompName)) {
                            // Add to myCollectorMap
                        	collector2BlobberName.put(collectorCompName, blobberName);
                            myBlobberListIndex++;
                        }
                    }

                    if (myBlobberListIndex == myBlobberList.size()) {
                        myBlobberListIndex = 0;
                    }
                    if (outValue != null || myBlobberListIndex == startIndex) {
                        break;
                    }
	        }
    	}
        return outValue;
    }

	/**
	 * Gets the blobber component reference for the given component name.
	 * 
	 * @throws AcsJContainerServicesEx
	 */
	protected BlobberOperations getBlobber(String inBlobberName) throws AcsJContainerServicesEx {
		synchronized (myBlobberList) {
			// If not already referenced, get reference
			if (!myBlobberRefMap.containsKey(inBlobberName)) {
				BlobberOperations newBlobber = BlobberHelper.narrow(m_containerServices.getComponent(inBlobberName));
				myBlobberRefMap.put(inBlobberName, newBlobber);
			}
			return myBlobberRefMap.get(inBlobberName);
		}
	}

	/**
	 * Thread to re-register known collectors to a Blobber if restarted.
	 */
	private class AsynchronousRegistration implements Runnable
	{
		String blobberName = null;

		public AsynchronousRegistration(String blobberName) {
			this.blobberName = blobberName;
		}

		public void run() {
			for (Map.Entry<String, String> m : collector2BlobberName.entrySet()) {
				if (this.blobberName.equals(m.getValue())) {
					// Re-register known Collector
					registerCollector(m.getKey());
				}
			}
		}
	}

}

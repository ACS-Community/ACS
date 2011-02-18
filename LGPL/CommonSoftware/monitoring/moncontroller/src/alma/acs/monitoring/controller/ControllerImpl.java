package alma.acs.monitoring.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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
 * and those blobbers should eventually be deregistered (and the monitor collectors they served be re-assigned,
 * which would require to keep a list of them in this class...)
 */
public class ControllerImpl extends ComponentImplBase implements ControllerOperations {

    /**
     * The IDL type of the blobber components. Used to retrieve all instances of this type.
     */
    private static final String BLOBBER_IDL_TYPE = "IDL:alma/MonitorArchiver/Blobber:1.0";

    /**
     * List of blobber component names.
     * @TOOD (HSO): Probably should be a map [String compName, Blobber compRef], see other comments below.
     */
    protected final List<String> myBlobberList = new ArrayList<String>();

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
    	synchronized (this.myBlobberList) {
	        for (String blobberName : this.myBlobberList) {
	            m_containerServices.releaseComponent(blobberName);
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
                        "No blobbers ("
                                + BLOBBER_IDL_TYPE
                                + ") found in the CDB. At least one must be configured.");
            }
            this.myBlobberList.addAll(Arrays.asList(componentNames));
            m_logger.info("The following blobbers were found: "
                    + this.myBlobberList);
        } catch (AcsJContainerServicesEx e) {
            throw new ComponentLifecycleException(e);
        }
    }

    /////////////////////////////////
    ///////// IDL interface /////////
    /////////////////////////////////

    @Override
    public synchronized void registerCollector(String inComponentName) {
        m_logger.info("Attempting to register collector " + inComponentName);
        try {
            String blobberName = isRegistered(inComponentName);
            if (blobberName == null) {
                blobberName = addCollector(inComponentName);
                if (blobberName == null) {
                    // TODO add dynamic blobber component (probably in method
                    // addCollector) and/or raise alarm.
                    m_logger.warning(inComponentName + " could not be registered to any of the blobbers.");
                } else {
                    m_logger.info(inComponentName
                            + " was registered to blobber " + blobberName);
                }
            } else {
                m_logger.warning(inComponentName
                        + " is already registered to blobber " + blobberName);
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
            BlobberOperations blobber = null;
            String blobberName = null;
        	synchronized (this.myBlobberList) {
	            for (String name : this.myBlobberList) {
	                blobberName = name;
	                blobber = getBlobber(name);
	                if (blobber.removeCollector(inComponentName) == CollectorListStatus.REMOVED) {
	                    removed = true;
	                    break;
	                }
	            }
        	}
            if (!removed) {
                m_logger.severe(inComponentName + " could not be deregistered.");
                if (blobberName == null) {
                	// @TODO (HSO): this can never happen, since blobberName gets assigned while iterating over the blobber name list,
                	//              regardless of whether that blobber served the given collector. 
                    m_logger.severe("Collector was not registered.");
                } else {
                	// @TODO (HSO): If this means that we found the right blobber but failed to deregister the monitor collector,
                	//              then the log message should be changed.
                    m_logger.severe("Collector was registered to blobber " + blobberName + ".");
                }
            } else {
                m_logger.info(inComponentName
                                + " was deregistered from blobber "
                                + blobberName + ".");
            }
        } catch (Exception e) {
        	// @TODO (HSO): Shouldn't the IDL declare an exception for this?
            m_logger.log(Level.SEVERE, "Unexpected problem.", e);
        }
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
    	synchronized (this.myBlobberList) {
	        for (String blobberName : this.myBlobberList) {
	            if (getBlobber(blobberName).containsCollector(inComponentName) == CollectorListStatus.KNOWN) {
	                outValue = blobberName;
	                break;
	            }
	        }
    	}
        return outValue;
    }

    /**
     * Iterates over the list of blobbers, starting from {@link #myBlobberListIndex},
     * and attempts to add the given collector to the first available blobber.
     * @param inComponentName Name of collector component.
     * @return Name of blobber component to which the collector was added, or <code>null</code> if none of the blobbers could add the collector.
     * @throws IllegalStateException if no blobber components are available.
     * @throws AcsJContainerServicesEx if a blobber component reference cannot be retrieved.
     *                                 (see inlined TODO comment about not retrieving all blobber component references again and again.)
     */
    protected String addCollector(String inComponentName) throws Exception {
        String outValue = null;
    	synchronized (this.myBlobberList) {
	        int startIndex = this.myBlobberListIndex;
	        if (this.myBlobberList.size() == 0) {
	            throw new IllegalStateException(
	                    "Attempt to add a collector but the list of blobbers is empty.");
	        }
	        while (true) {
	            String blobberName = this.myBlobberList.get(this.myBlobberListIndex);
	            // @TODO (HSO): it's not totally clean to retrieve the blobber component reference again and again 
	            // from the container services. The CS will return the old reference so that no remote call is made,
	            // but an ugly log about retrieving the reference twice gets produced. 
	            // Better remember the blobber component reference in addition to the component name,
	            // e.g. by turning myBlobberList into a map.
	            if (getBlobber(blobberName).addCollector(inComponentName) == CollectorListStatus.ADDED) {
	                outValue = blobberName;
	                m_logger.fine("Added collector '" + inComponentName + "' to blobber '" + blobberName + '.');
	            }
	            this.myBlobberListIndex++;
	            if (this.myBlobberListIndex == this.myBlobberList.size()) {
	                this.myBlobberListIndex = 0;
	            }
	            if (outValue != null || this.myBlobberListIndex == startIndex) {
	                break;
	            }
	        }
    	}
        return outValue;
    }

    /**
     * Gets the blobber component reference for the given component name.
     * @TODO (HSO): Known blobber component references should be cached instead of getting retrieved from container services each time.
     * @throws AcsJContainerServicesEx
     */
    protected BlobberOperations getBlobber(String inBlobberName)
            throws AcsJContainerServicesEx {
        return BlobberHelper.narrow(m_containerServices.getComponent(inBlobberName));
    }

}

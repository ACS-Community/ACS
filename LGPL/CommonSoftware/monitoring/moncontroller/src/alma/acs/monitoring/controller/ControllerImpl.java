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

public class ControllerImpl extends ComponentImplBase implements
        ControllerOperations {

    private static final String COMPONENT_URL = "IDL:alma/MonitorArchiver/Blobber:1.0";

    protected List<String> myBlobberList = new ArrayList<String>();

    protected int myBlobberListIndex = 0;

    @Override
    public void cleanUp() {
        for (String blobberName : this.myBlobberList) {
            m_containerServices.releaseComponent(blobberName);
        }
    }

    @Override
    public void initialize(ContainerServices inContainerServices)
            throws ComponentLifecycleException {
        super.initialize(inContainerServices);

        try {
            String[] componentNames = m_containerServices.findComponents(null,
                    COMPONENT_URL);
            if (componentNames.length == 0) {
                throw new ComponentLifecycleException(
                        "No blobbers ("
                                + COMPONENT_URL
                                + ") found in the CDB. At least one must be configured.");
            }
            this.myBlobberList = Arrays.asList(componentNames);
            m_logger.info("The following blobbers were found: "
                    + this.myBlobberList);
        } catch (AcsJContainerServicesEx e) {
            throw new ComponentLifecycleException(e);
        }
    }

    protected String isRegistered(String inComponentName) throws Exception {
        String outValue = null;
        for (String blobberName : this.myBlobberList) {
            if (getBlobber(blobberName).containsCollector(inComponentName) == CollectorListStatus.KNOWN) {
                outValue = blobberName;
                break;
            }
        }
        return outValue;
    }

    protected String addCollector(String inComponentName) throws Exception {
        String outValue = null;
        int startIndex = this.myBlobberListIndex;
        if (this.myBlobberList.size() == 0) {
            throw new IllegalStateException(
                    "Attempt to add a collector but the list of blobbers is empty.");
        }
        while (true) {
            String blobberName = this.myBlobberList
                    .get(this.myBlobberListIndex);
            if (getBlobber(blobberName).addCollector(inComponentName) == CollectorListStatus.ADDED) {
                outValue = blobberName;
            }
            this.myBlobberListIndex++;
            if (this.myBlobberListIndex == this.myBlobberList.size()) {
                this.myBlobberListIndex = 0;
            }
            if (outValue != null || this.myBlobberListIndex == startIndex) {
                break;
            }
        }
        return outValue;
    }

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
                } else {
                    m_logger.info(inComponentName
                            + " was registered to blobber " + blobberName);
                }
            } else {
                m_logger.warning(inComponentName
                        + " is already registered to blobber " + blobberName);
            }
        } catch (Exception e) {
            m_logger.log(Level.SEVERE, "Unexpected problem.", e);
        }
    }

    @Override
    public synchronized void deregisterCollector(String inComponentName) {
        try {
            boolean removed = false;
            BlobberOperations blobber = null;
            String blobberName = null;
            for (String name : this.myBlobberList) {
                blobberName = name;
                blobber = getBlobber(name);
                if (blobber.removeCollector(inComponentName) == CollectorListStatus.REMOVED) {
                    removed = true;
                    break;
                }
            }
            if (!removed) {
                m_logger
                        .severe(inComponentName + " could not be deregistered.");
                if (blobberName == null) {
                    m_logger.severe("Collector was not registered.");
                } else {
                    m_logger.severe("Collector was registered to blobber "
                            + blobberName + ".");
                }
            } else {
                m_logger
                        .info(inComponentName
                                + " was deregistered from blobber "
                                + blobberName + ".");
            }
        } catch (Exception e) {
            m_logger.log(Level.SEVERE, "Unexpected problem.", e);
        }
    }

    protected BlobberOperations getBlobber(String inBlobberName)
            throws Exception {
        return BlobberHelper.narrow(m_containerServices
                .getComponent(inBlobberName));
    }

}

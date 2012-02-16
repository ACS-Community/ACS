package alma.acs.monitoring.blobber;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.MonitorErr.DeviceAlreadyRegisteredEx;
import alma.MonitorErr.DeviceNotRegisteredEx;
import alma.MonitorErr.RegisteringDeviceProblemEx;
import alma.MonitorErr.StartMonitoringProblemEx;
import alma.MonitorErr.StopMonitoringProblemEx;
import alma.TMCDB.MonitorCollectorOperations;
import alma.TMCDB.MonitorDataBlock;
import alma.TMCDB.propertySerailNumber;

/**
 * Mock implementation of the MonitorCollector interface.
 */
public class MonitorTestCollector implements MonitorCollectorOperations {

//    private static final long serialVersionUID = -7497756569813748359L;

    private final DataLock<MonitorDataBlock[]> myDataLock;

	private final Logger logger;

    public MonitorTestCollector(Logger logger) {
    	this.logger = logger;
    	myDataLock = new DataLock<MonitorDataBlock[]>(logger, "collector");
    }

    /**
     * Gets the data that was stored through {@link #setMonitorData(MonitorDataBlock[])}.
     * Blocks if called before setMonitorData or if called twice in a row, until data gets set in setMonitorData.
     */
    @Override
    public MonitorDataBlock[] getMonitorData() {
        MonitorDataBlock[] outArray = null;
		try {
			outArray = this.myDataLock.take();
		} catch (InterruptedException ex) {
			logger.log(Level.WARNING, "Failed to get monitor data.", ex);
		}
        if (outArray == null) {
            outArray = new MonitorDataBlock[0];
        }
        return outArray;
    }

    /**
     * Stores the data that later can be read through {@link #getMonitorData()}.
     * Blocks if called twice in a row, until data gets taken out in getMonitorData.
     * @throws InterruptedException 
     */
    public void setMonitorData(MonitorDataBlock[] inData) throws InterruptedException {
        this.myDataLock.put(inData);
    }

    @Override
    public void deregisterMonitoredDevice(String componentName)
            throws DeviceNotRegisteredEx {
    }

    @Override
    public void registerMonitoredDevice(String componentName,
            String serialNumber) throws RegisteringDeviceProblemEx,
            DeviceAlreadyRegisteredEx {
    }

    @Override
    public void registerMonitoredDeviceWithMultipleSerial(String componentName,
            propertySerailNumber[] serialNumbers)
            throws RegisteringDeviceProblemEx, DeviceAlreadyRegisteredEx {
    }

    @Override
    public void startMonitoring(String componentName)
            throws StartMonitoringProblemEx {
    }

    @Override
    public void stopMonitoring(String componentName)
            throws StopMonitoringProblemEx {
    }

    @Override
    public void set_archiving_interval(String compName, String propName, long time) {
    }

    @Override
    public void enable_archiving(String compName, String propName) {
    }

    @Override
    public void suppress_archiving(String compName, String propName) {
    }

    @Override
    public ComponentStates componentState() {
        return null;
    }

    @Override
    public String name() {
        return "MonitorTestCollector";
    }

}

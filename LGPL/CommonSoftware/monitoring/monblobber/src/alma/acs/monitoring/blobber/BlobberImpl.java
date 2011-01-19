/**
 * 
 */
package alma.acs.monitoring.blobber;

import java.sql.Timestamp;
import java.util.logging.Logger;

import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * @author apersson
 * 
 */
public class BlobberImpl extends ComponentImplBase implements BlobberOperations {

    protected BlobberWorker myWorker;
    protected Thread myWorkerThread;

    private Logger log;

    @Override
    public void cleanUp() {
        this.myWorker.terminate();
        if (this.myWorkerThread != null) {
            this.myWorkerThread.interrupt();
        }
    }

    @Override
    public void initialize(ContainerServices inContainerServices)
            throws ComponentLifecycleException {
        super.initialize(inContainerServices);

        log = inContainerServices.getLogger();

        send_alarm("Monitoring", "MonitorArchiver", 2, false);
        try {
            initialize();
        } catch(java.lang.Exception ex) {
            send_alarm("Monitoring", "MonitorArchiver", 2, true);
            throw new ComponentLifecycleException(ex);
        }
    }

    public void initialize() {
        this.myWorker = createWorker();
        startWorker(this.myWorker);
    }

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

    protected BlobberWorker createWorker() {
        return new BlobberWorker(m_containerServices);
    }

    protected void startWorker(BlobberWorker inWorker) {
        this.myWorkerThread = m_containerServices.getThreadFactory().newThread(
                inWorker);
        this.myWorkerThread.start();
    }

    private void send_alarm(String faultFamily, String faultMember,
    		int faultCode, boolean active) {
    	try {
    		ACSAlarmSystemInterface alarmSource = ACSAlarmSystemInterfaceFactory
    				.createSource("ALARM_SYSTEM_SOURCES");
    		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
    				faultFamily, faultMember, faultCode);
    		if (active) {
    			fs.setDescriptor(ACSFaultState.ACTIVE);
    		} else {
    			fs.setDescriptor(ACSFaultState.TERMINATE);
    		}
    
    		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
    
    		alarmSource.push(fs);
    
    	} catch (ACSASFactoryNotInitedEx ex) {
    		log.severe("Alarm with FF=" + faultFamily + " FM=" + faultMember
    				+ " FC=" + faultCode + " could not be thrown. Message="
    				+ ex.getMessage());
    	} catch (SourceCreationErrorEx ex) {
    		log.severe("Alarm with FF=" + faultFamily + " FM=" + faultMember
    				+ " FC=" + faultCode + " could not be thrown. Message="
    				+ ex.getMessage());
    	} catch (FaultStateCreationErrorEx ex) {
    		log.severe("Alarm with FF=" + faultFamily + " FM=" + faultMember
    				+ " FC=" + faultCode + " could not be thrown. Message="
    				+ ex.getMessage());
    	}
    }

}

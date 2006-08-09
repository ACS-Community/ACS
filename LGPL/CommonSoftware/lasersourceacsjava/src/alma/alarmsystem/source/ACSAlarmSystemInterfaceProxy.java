package alma.alarmsystem.source;

import java.util.Collection;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;

import java.util.logging.Level;
import java.util.logging.Logger;
import alma.acs.logging.ClientLogManager;

import java.util.Iterator;

/**
 * ACS implementation of the AlarmSystemInterface.
 * The alarms are published in the logging.
 * 
 * @author acaproni
 *
 */
public class ACSAlarmSystemInterfaceProxy implements AlarmSystemInterface {
	
	// The logger to publish the alarms
	private  Logger m_logger;
	
	// The name of the source
	String name;
	
	/**
	 * The basic constructor
	 * 
	 * @param name The name of the source
	 */
	public ACSAlarmSystemInterfaceProxy(String name) {
		this.name=name;
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getClass().getName(), true);
		m_logger.fine("Alarm source of "+name+" connected to the logging");
	}
	
	/**
	 * Set the source name.
	 * @param newSourceName the source name.
	 */
	public void setSourceName(String newSourceName) {
		name = newSourceName;
	}
	
	/**
	 * Get the source name.
	 * @return the source name.
	 */
	public String getSourceName() {
		return name;
	}
	
	/**
	 * Close and deallocate resources.
	 */
	public void close() {}
	
	/**
	 * Push a fault state.
	 * @param state the fault state change to push.
	 * @throws ASIException if the fault state can not be pushed.
	 */
	public void push(FaultState state) throws ASIException {
		logFaultState(state);
	}
	
	/**
	 * Push a collection of fault states.
	 * @param states
	 * @throws ASIException if the fault state collection can not be pushed.
	 */
	public void push(Collection states) throws ASIException {
		if (states==null || states.size()==0) {
			return;
		}
		Iterator iterator = states.iterator();

	    while (iterator.hasNext()) {
	      Object next = iterator.next();

	      if (next instanceof FaultState) {
	        push((FaultState) next);
	      } else {
	        throw new IllegalArgumentException("states collection does not contain FaultState instances");
	      }
	    }
	}
	
	/**
	 * Push the set of active fault states.
	 * @param active the active fault states.
	 * @throws ASIException if the fault state active list can not be pushed.
	 */
	public void pushActiveList(Collection active) throws ASIException {
		if (active==null || active.size()==0) {
			return;
		}
	    push(active);
	}
	
	/**
	 * Write the FaultState in the log
	 * 
	 * @param fs The FaultState to log
	 */
	private void logFaultState(FaultState fs) {
		if (fs==null) {
			return;
		}
		StringBuilder sb = new StringBuilder("Alarm sent: <");
		sb.append(fs.getFamily()+','+fs.getMember()+','+fs.getCode()+'>');
		sb.append(" "+fs.getDescriptor());
		m_logger.severe(sb.toString());
	}
}

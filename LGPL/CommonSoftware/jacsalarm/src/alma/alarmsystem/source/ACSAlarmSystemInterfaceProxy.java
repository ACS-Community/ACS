package alma.alarmsystem.source;

import java.util.Collection;

import java.util.logging.Level;
import java.util.logging.Logger;

import java.util.Iterator;

import alma.acs.util.XmlNormalizer;

/**
 * ACS implementation of the AlarmSystemInterface.
 * The alarms are published in the logging.
 * 
 * @author acaproni
 *
 */
public class ACSAlarmSystemInterfaceProxy implements ACSAlarmSystemInterface {
	
	// The logger to publish the alarms
	private  Logger m_logger;
	
	// The name of the source
	String name;
	
	/**
	 * The basic constructor
	 * 
	 * @param name The name of the source
	 * @param logger The logger to log alarms in
	 */
	public ACSAlarmSystemInterfaceProxy(String name, Logger logger) {
		if (logger==null) {
			throw new IllegalArgumentException("Invalid null logger in constructor");
		}
		this.name=name;
		m_logger = logger;
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
	public void push(ACSFaultState state) {
		logFaultState(state);
	}
	
	/**
	 * Push a collection of fault states.
	 * @param states
	 * @throws ASIException if the fault state collection can not be pushed.
	 */
	public void push(Collection states) {
		if (states==null || states.size()==0) {
			return;
		}
		Iterator iterator = states.iterator();

	    while (iterator.hasNext()) {
	      Object next = iterator.next();

	      if (next instanceof ACSFaultState) {
	        push((ACSFaultState) next);
	      } else {
	        throw new IllegalArgumentException("states collection does not contain ACSFaultState instances");
	      }
	    }
	}
	
	/**
	 * Push the set of active fault states.
	 * @param active the active fault states.
	 * @throws ASIException if the fault state active list can not be pushed.
	 */
	public void pushActiveList(Collection active) {
		if (active==null || active.size()==0) {
			return;
		}
	    push(active);
	}
	
	/**
	 * Write the ACSFaultState in the log
	 * 
	 * @param fs The ACSFaultState to log
	 */
	private void logFaultState(ACSFaultState fs) {
		if (fs==null) {
			return;
		}
		StringBuilder sb = new StringBuilder("Alarm sent: <");
		sb.append(fs.getFamily()+','+fs.getMember()+','+fs.getCode()+'>');
		sb.append(" "+fs.getDescriptor());
		m_logger.severe(XmlNormalizer.normalize(sb.toString()));
	}
}


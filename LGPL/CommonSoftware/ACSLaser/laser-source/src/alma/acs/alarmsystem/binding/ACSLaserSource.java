package alma.acs.alarmsystem.binding;

import java.util.Collection;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.ASIException;

import alma.acs.logging.AcsLogLevel;
import alma.acs.util.XmlNormalizer;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSFaultState;

public class ACSLaserSource extends AlarmSystemInterfaceProxy implements ACSAlarmSystemInterface {
	
	// The logger
	private Logger logger=null;
	
	public ACSLaserSource(String sourceName, Logger logger) throws ASIException {
		super(sourceName);
		if (logger==null) {
			throw new IllegalArgumentException("Invalid null logger in constructor");
		}
		this.logger=logger;
	}
	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	@Override
	public synchronized void push(ACSFaultState state){
		try {
			this.push((FaultState)state);
			logFaultState(state);
		} catch (Throwable t) {
			StringBuilder logStr = new StringBuilder("Exception "+t.getMessage()+" throwing alarm <");
			logStr.append(state.getFamily()+','+state.getMember()+','+state.getCode()+">");
			logStr.append(" "+state.getDescriptor());
			logger.log(AcsLogLevel.ERROR,logStr.toString(),t);
		}
	}
	
	@Override
	public synchronized void push(Collection states) {
		try {
			super.push(states);
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR,"Error pushing the collection of alarms",t);
		}
	}
	
	@Override
	public synchronized void pushActiveList(Collection active) {
		try {
			super.pushActiveList(active);
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR,"Error pushing the activeList of alarms",t);
		}
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
		sb.append(fs.getFamily()+','+fs.getMember()+','+fs.getCode()+">");
		sb.append(" "+fs.getDescriptor());
		logger.log(AcsLogLevel.DEBUG,XmlNormalizer.normalize(sb.toString()));
	}
}

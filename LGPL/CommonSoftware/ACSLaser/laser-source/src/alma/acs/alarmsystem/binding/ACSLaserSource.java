package alma.acs.alarmsystem.binding;

import java.util.Collection;

import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.ASIException;

import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSFaultState;

public class ACSLaserSource extends AlarmSystemInterfaceProxy implements ACSAlarmSystemInterface {
	
	public ACSLaserSource(String sourceName) throws ASIException {
		super(sourceName);
	}
	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void push(ACSFaultState state){
		try {
			this.push((FaultState)state);
		} catch (Exception e) {}
	}
	
	public void push(Collection states) {
		this.push(states);
	}
	
	public void pushActiveList(Collection active) {
		this.pushActiveList(active);
	}
}

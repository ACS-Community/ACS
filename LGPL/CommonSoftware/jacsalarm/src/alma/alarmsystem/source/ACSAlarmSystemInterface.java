package alma.alarmsystem.source;

import java.util.Collection;

/**
 * This interface is derived from cern.laser.source.alarmsysteminterface.AlarmSystemInterface.
 * This is repeated here to avoid dependency with LASER.
 * 
 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
 * 
 * @author acaproni
 */
public interface ACSAlarmSystemInterface {
	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void setSourceName(String newSourceName);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public String getSourceName();

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void close();

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void push(ACSFaultState state);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void push(Collection states);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void pushActiveList(Collection active);
}

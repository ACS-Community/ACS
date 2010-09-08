package alma.alarmsystem.source;

import java.util.Collection;

/**
 * This interface is derived from cern.laser.source.alarmsysteminterface.AlarmSystemInterface.
 * This is repeated here to avoid dependency with LASER.
 * <p>
 * @TODO Shouldn't we have a factory method for alarm sources here in this interface, 
 *       so that alarms can be produced with only the ACSAlarmSystemInterface,
 *       and without the need to use {@link ACSAlarmSystemInterfaceFactory#createFaultState(String, String, int)}?
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
	public void push(Collection<ACSFaultState> states);

	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	public void pushActiveList(Collection<ACSFaultState> active);
}

package alma.alarmsystem.clients.test.utils;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;

/**
 * Alarm listener for this test
 * 
 * @author acaproni
 * @since ACS-12.2
 *
 */
public class AlarmListenerForTesting implements AlarmSelectionListener {
	
	public int numAlarms=0;
	public int numExceptions=0;
	
	/**
	 * A name associated to the listener so that we can distinguish
	 * wich listener prints messages
	 */
	public final String name;
	
	/**
	 * Constructor
	 * 
	 * @param name The name of the listener
	 */
	public AlarmListenerForTesting(String name) {
		this.name=name;
	}

	@Override
	public void onAlarm(Alarm alarm) {
		numAlarms++;
		System.out.println(name+": alarm received ["+alarm.getAlarmId()+"]");
	}

	@Override
	public void onException(LaserSelectionException e) {
		numExceptions++;
		System.out.println(name+": exception received. Code is "+e.getCode());
	}
	
}

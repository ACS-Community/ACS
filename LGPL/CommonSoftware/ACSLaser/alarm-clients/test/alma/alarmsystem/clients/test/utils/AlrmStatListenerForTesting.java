package alma.alarmsystem.clients.test.utils;

import alma.alarmsystem.clients.alarm.AlarmCategoryStatListener;

/**
 * Alarm statistics listener for this test
 * 
 * @author acaproni
 * @since ACS-12.2
 *
 */
public class AlrmStatListenerForTesting implements AlarmCategoryStatListener {
	
	public int numActivationNoifies=0;
	public int numReductionNofies=0;

	@Override
	public void activationAlarmsStats(Integer active, Integer priority1,
			Integer priority2, Integer priority3, Integer priority4) {
		numActivationNoifies++;
	}

	@Override
	public void reductionAlarmsStat(Integer reduced, Integer masked,
			Integer multiplicityParent, Integer nodeParent,
			Integer multiplicityChild, Integer nodeChield) {
		numReductionNofies++;
	}
	
}

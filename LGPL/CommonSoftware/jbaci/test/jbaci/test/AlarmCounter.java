package jbaci.test;

/**
 * @author <a href="mailto:takashi.nakamotoATnao.ac.jp">Takashi Nakamoto</a>
 * @version $id$
 * 
 * This class counts the number of occurrences of alarm_raised() and alarm_cleared().
 * All the instance methods of this class is thread-safe, which can be called by
 * different threads at the same time.
 */
public class AlarmCounter {
	private int count_alarm_raised;
	private int count_alarm_cleared;
	
	public AlarmCounter() {
		reset();
	}

	public synchronized void reset() {
		count_alarm_raised  = 0;
		count_alarm_cleared = 0;
	}
	
	public synchronized void incrementRaised() {
		count_alarm_raised++;
	}
	
	public synchronized void incrementCleared() {
		count_alarm_cleared++;
	}
	
	public synchronized int getRaisedCount() {
		return count_alarm_raised;
	}
	
	public synchronized int getClearedCount() {
		return count_alarm_cleared;
	}

}

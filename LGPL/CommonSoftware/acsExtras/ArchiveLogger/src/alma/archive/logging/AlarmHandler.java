package alma.archive.logging;

/**
 * Allows sub-classes to call back to the ACS component.
 */
public interface AlarmHandler {
	/**
	 * 
	 * @param code don't know what the code is. It was hard-coded before I started refactoring.
	 */
	void sendAlarm(int code);
}


package alma.acs.concurrent;

import java.util.concurrent.ThreadFactory;

/**
 * Convenience class to wrap a thread factory with, so that new threads
 * get a given suffix appended to their names.
 * The intended purpose of this class is to make it easier to read thread dumps
 * (or logs related to thread names) 
 * when one component or application uses a thread factory for different purposes.
 * 
 * @author hsommer
 * @since ACS 10.1
 */
public class NamedThreadFactory implements ThreadFactory
{
	private final ThreadFactory tf;
	private final String threadNameSuffix;

	public NamedThreadFactory(ThreadFactory tf, String threadNameSuffix) {
		this.tf = tf;
		this.threadNameSuffix = ( (threadNameSuffix != null && !threadNameSuffix.trim().isEmpty()) ? threadNameSuffix.trim() : null );

	}
	
	@Override
	public Thread newThread(Runnable r) {
		Thread t = tf.newThread(r);
		if (threadNameSuffix != null) {
			t.setName(t.getName() + "-" + threadNameSuffix);
		}
		return t;
	}

}

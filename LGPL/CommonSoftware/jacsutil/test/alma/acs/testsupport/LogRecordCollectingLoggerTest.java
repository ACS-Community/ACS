package alma.acs.testsupport;

import junit.framework.TestCase;

public class LogRecordCollectingLoggerTest extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * Simply construct two collecting loggers. 
	 * This test exercises the somewhat experimental construction of a logger subclass from the 
	 * static creator method of the base class, which uses generics and thus does not need a cast of the return value. 
	 */
	public void testGetLogger() {
		LogRecordCollectingLogger logger1 = LogRecordCollectingLogger.getCollectingLogger("MyCollectingLogger");
		MyCollectingLoggerSubclass logger2 = LogRecordCollectingLogger.getCollectingLogger("MyCollectingLoggerWithSubclass", MyCollectingLoggerSubclass.class);
	}
	
	
	public static class MyCollectingLoggerSubclass extends LogRecordCollectingLogger {
		public MyCollectingLoggerSubclass(String name, String resourceBundleName) {
			super(name, resourceBundleName);
		}
	}
}

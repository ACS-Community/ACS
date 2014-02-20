package alma.acs.classloading;

import java.util.logging.Logger;

import org.junit.Before;
import org.junit.Test;

import alma.acs.testsupport.TestLogger;
import alma.acs.util.MemoryUtil;


/**
 * This test runs 30 minutes and in case of failure simply crashes the JVM. 
 * Use it only for manual runs. 
 */
public class AcsComponentClassLoaderEnduranceTest
{
	private Logger logger;

	@Before
	public void setUp() throws Exception {
		logger = TestLogger.getLogger(getClass().getSimpleName());
	}

	@Test 
	public void testWatchMyNativeMemoryConsumption() throws Exception {
		
		// all jars, to get lots of JarFile native memory waste
		System.setProperty(AcsComponentClassLoader.PROPERTY_JARDIRS, System.getProperty(AcsSystemClassLoader.PROPERTY_JARDIRS));
		
		System.setProperty(AcsComponentClassLoader.PROPERTY_CLASSLOADERVERBOSE, "false"); // toggle to debug
		
		// to allow JVM to settle down after the start, for jvisualvm to attach etc.
		System.gc();
		Thread.sleep(20000);
		long testRunTimeMinutes = 30;
		logger.info("Will create and use AcsComponentClassLoader instances for " + testRunTimeMinutes + " minutes, to check for non-heap OutOfMemoryError; " 
					+ MemoryUtil.getHeapSizeMessage());
		
		long startTime = System.currentTimeMillis();
		long lastSleep = startTime;
		
		long count = 0;
		while (System.currentTimeMillis() - startTime < testRunTimeMinutes * 60 * 1000) {
			count++;
			
			// sleep for 1 s every 10 s
			if (System.currentTimeMillis() - lastSleep >= 10000) {
				Thread.sleep(1000);
				lastSleep = System.currentTimeMillis();
			}
			
			// create a new classloader instance, making the old instance available for GC 
			AcsComponentClassLoader compCL = new AcsComponentClassLoader(Thread.currentThread().getContextClassLoader(), logger, "myComponent");
			Class.forName("java.lang.String", true, compCL);
//			compCL.close(); // comment this out to verify that the test can produce OutOfMemoryError
			
			logger.info("Used AcsComponentClassLoader #" + count);
		}
		
		// The known issues are not with the heap, but anyway we clear it now 
		System.gc();
		// to allow JVM to settle down after GC
		Thread.sleep(20000);
		logger.info("Done; " + MemoryUtil.getHeapSizeMessage());
	}
}

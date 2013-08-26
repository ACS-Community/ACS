/*
 * Created on Oct 27, 2005 by mschilli
 */
package alma.acs.vmtools;

import junit.framework.TestCase;




public class LauncherTest extends TestCase {

	public void testLaunchApplication() throws Exception {
		String[] args = new String[]{"alma.acs.vmtools.LauncherTestHelper", "A", "B"};
		Launcher.launchApplication(args);
		
		for (int i=0; i < 10; i++) {
			Thread.sleep(500);
			if (LauncherTestHelper.output != null) {
				break;
			}
		};
		assertTrue("application did not start", LauncherTestHelper.output != null);
		
		assertEquals(2, LauncherTestHelper.output.length);
		assertEquals("A", LauncherTestHelper.output[0]);
		assertEquals("B", LauncherTestHelper.output[1]);
	}
	
}




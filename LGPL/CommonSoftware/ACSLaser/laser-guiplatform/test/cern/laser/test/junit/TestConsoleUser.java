/*
 * Created on Feb 7, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package cern.laser.test.junit;

import cern.laser.console.LaserConsoleException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;
import cern.laser.guiplatform.user.UserHandlerFactory;
import junit.framework.TestCase;

/**
 * @author woloszyn
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TestConsoleUser extends TestCase {
	
	public TestConsoleUser() {
	        super();
	}
	
	public void testGetUser() {
		User user = getUser();
		assertNotNull( user );
	}
	
	public void testGetDefaultPrinter() {
		User user = getUser();
		try {
			String oldPrinter = user.getDefaultPrinter();
			String desiredPrinterName = "test-printer-name"; 
			user.setDefaultPrinter(desiredPrinterName);
			String userPrinterName = user.getDefaultPrinter();
			assertEquals(desiredPrinterName,userPrinterName);
			// Rollback of previous value
			user.setDefaultPrinter(oldPrinter);
		} catch (LaserConsoleException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	private User getUser() {	
		User user = null;
		UserHandler userHandler;
		try {
			userHandler = UserHandlerFactory.getHandler();
			try {
				user = userHandler.getUser("laser");
			} catch (LaserConsoleException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
		} catch (LaserConsoleException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return user;
	}
	public static void main( String[] args) {
		junit.textui.TestRunner.run(TestConsoleUser.class);
	}
}

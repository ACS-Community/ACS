/*
*ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2007 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
/** 
 * @author  nbarriga
 * @version $Id: testLTSClient.java,v 1.2 2007/07/06 08:57:51 hsommer Exp $
 * @since    
 */

package alma.acs.loggingtstest;

import java.util.logging.Logger;

import alma.ACSLogTypeExample.complexLog;
import alma.ACSLogTypeExample.simpleLog;
import alma.acs.component.client.ComponentClient;

/**
 * The test is based on TAT comparing with the reference output the log records sent by this class
 * and then received and printed by {@link alma.acs.logclient.LogListener}.
 * This is why we need to have the ACS manager available, to give access to the Log service. 
 */
public class testLTSClient extends ComponentClient
{ 
	public testLTSClient(Logger logger, String managerLoc, String clientName) throws Exception
	{
		super(logger, managerLoc, clientName);
	}

	
	public static void main(String[] args) {
		try {
			String managerLoc = System.getProperty("ACS.manager");
			if (managerLoc == null) {
				System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
				System.exit(-1);
			}

			testLTSClient client = new testLTSClient(null, managerLoc, "testLTSClient");			
			Logger logger = client.m_logger;
			
			simpleLog slog=new simpleLog(logger);
			slog.log();		
			
			complexLog clog=new complexLog(logger);
			clog.setsomeDoubleMember(3.14159);
			clog.setsomeStringMember("test string");
			clog.setsomeLongMember((long)42);
			clog.log();
			
			Thread.sleep(2000);
		}
		catch (Throwable thr) {
			// bad luck
			thr.printStackTrace();
			System.exit(-1);
		}
	}

}

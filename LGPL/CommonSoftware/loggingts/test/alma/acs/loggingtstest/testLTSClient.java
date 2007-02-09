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
 * @version $Id: testLTSClient.java,v 1.4 2007/02/09 12:21:14 nbarriga Exp $
 * @since    
 */

package alma.acs.loggingtstest;

import java.util.logging.Logger;
import alma.acs.component.client.ComponentClient;
import alma.ACSLogTypeExample.*;

/**
 * Insert a Class/Interface comment.
 * 
 */

public class testLTSClient  extends ComponentClient
{ 
	/**
	 * @param logger
	 * @param managerLoc
	 * @param clientName
	 * @throws Exception
	 */
	public testLTSClient(Logger logger, String managerLoc, String clientName) throws Exception
	{
		super(logger, managerLoc, clientName);
	}

	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null)
		{
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "testLTSClient";
		testLTSClient client = null;

		try
		{
			client = new testLTSClient(null, managerLoc, clientName);
		}
		catch (Exception e)
		{
			e.printStackTrace(System.err);
			System.exit(-1);
		}
		simpleLog slog=new simpleLog(client.m_logger);
		slog.log();		
		complexLog clog=new complexLog(client.m_logger);
		clog.setsomeDoubleMember(3.14159);
		clog.setsomeStringMember("test string");
		clog.setsomeLongMember((long)42);
		clog.log();		
		try
		{
			Thread.sleep(2000);
		}
		catch (Exception e)
		{
			// bad luck
			e.printStackTrace(System.err);
                        System.exit(-1);
		}

	}




}

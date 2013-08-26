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
 * @version $Id: testLTSClient.java,v 1.4 2012/03/06 19:16:56 tstaig Exp $
 * @since    
 */

package alma.acs.loggingtstest;

import java.util.logging.Logger;

import alma.ACSLogTypeExample.ComplexLog;
import alma.ACSLogTypeExample.SimpleLog;
import alma.acs.component.client.ComponentClient;
import alma.acs.util.AcsLocations;

/**
 * The test is based on TAT comparing with the reference output the log records sent by this class
 * and then received and printed by {@link alma.acs.logclient.LogListener}.
 * This ensures that we test also the log details found only in the XML logs,
 * and explains why we need to have ACS running and use a ComponentClient.
 */
public class LtsTestClient
{ 
	private static class RemoteLoggerProvider extends ComponentClient {

		public RemoteLoggerProvider() throws Exception {
			super(null, AcsLocations.figureOutManagerLocation(), LtsTestClient.class.getSimpleName());
		}
		
		public Logger getLogger() {
			return m_logger;
		}
	}
	
	public static void main(String[] args) {
		RemoteLoggerProvider client = null;
		try {
			client = new RemoteLoggerProvider();
			Logger logger = client.getLogger();
			
			// log #1
			SimpleLog slog = new SimpleLog(logger);
			slog.log();
			
			// log #2
			SimpleLog slogAA = new SimpleLog(logger, "Array01", "Antenna01");
			slogAA.log();
			
			// log #3
			ComplexLog clog = new ComplexLog(logger);
			clog.setsomeDoubleMember(3.14159);
			clog.setsomeStringMember("test string");
			clog.setsomeLongMember((long)42);
			clog.setsomeBooleanMember(true);
			clog.log();
			
			// log #4
			ComplexLog clogAA = new ComplexLog(logger);
			clogAA.setArray("Array01");
			clogAA.setAntenna("Antenna01");
			clogAA.setsomeDoubleMember(3.14159);
			clogAA.setsomeStringMember("test string");
			clogAA.setsomeLongMember((long)42);
			clogAA.setsomeBooleanMember(true);
			clogAA.log();

			// log #5
			ComplexLog.log(logger, 2.22, "test string", 42l, true);
			
			// log #6
			new ComplexLog(logger).setsomeBooleanMember(false).setsomeDoubleMember(2.22).setArray("myArray").setAntenna("whatAntenna").log();
			
			// To avoid "failed to flush logging queue because remote logging service has not been made available."
			// when the client is too short-lived...
			Thread.sleep(2000);
		}
		catch (Throwable thr) {
			// bad luck
			thr.printStackTrace();
			System.exit(-1);
		}
		finally {
			if (client != null) {
				try {
					client.tearDown();
				} catch (Exception ex) {
					ex.printStackTrace();
				}
			}
		}
	}

}

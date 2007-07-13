/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.lang.Thread;

import alma.acs.component.client.ComponentClient;
import alma.acscommon.OPERATOR;

class TestAudArr extends ComponentClient{
        public TestAudArr(String managerLoc, String clientName) throws Exception {
                super(null, managerLoc, clientName);
        }

        public static void main(String args[]){
                String managerLoc = System.getProperty("ACS.manager");
                if (managerLoc == null) {
                        System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                        System.exit(-1);
                }
                String clientName = "TestAudArr";
                TestAudArr client = null;
                try{
                        client = new TestAudArr(managerLoc, clientName);
                        Logger logger = client.getContainerServices().getLogger();
                        logger.log(Level.WARNING, "Normal Log");
                        new NotReallyTypeSafeLog(logger, Level.WARNING, "Log with audience, array and antenna", OPERATOR.value, "Array01", "Antenna01");
                        new NotReallyTypeSafeLog(logger, Level.WARNING, "Log with audience", OPERATOR.value);

                        Thread.sleep(1000);
                }catch(Exception e){
                        System.out.println("Error creating test client");
                }
                try{
                        client.tearDown();
                }catch(Exception e){
                        System.out.println("Error destroying test client");
                }

                
        }

}

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

import alma.acs.component.client.ComponentClient;
import alma.acs.logging.domainspecific.AntennaContextLogger;
import alma.log_audience.OPERATOR;

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
                        AcsLogger m_logger = (AcsLogger)client.getContainerServices().getLogger();
                        AntennaContextLogger logger = new AntennaContextLogger(m_logger);
                        m_logger.log(Level.WARNING, "Normal Log");
                        m_logger.logToAudience(Level.WARNING, "Log with audience", OPERATOR.value);
                        m_logger.logToAudience(Level.WARNING, "Log exception with audience", new Exception("My dummy exception"), OPERATOR.value);
                        
                        
                        logger.log(Level.WARNING, "Log with audience, array and antenna", OPERATOR.value, "Array01", "Antenna01");
                        logger.log(Level.WARNING, "Log with array and antenna", "Array01", "Antenna01");
                        logger.log(Level.WARNING, "Log exception with audience, array and antenna", new Exception("My dummy exception"), OPERATOR.value, "Array01", "Antenna01");
                        logger.log(Level.WARNING, "Log exception with array and antenna", new Exception("My dummy exception"), "Array01", "Antenna01");
                        
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

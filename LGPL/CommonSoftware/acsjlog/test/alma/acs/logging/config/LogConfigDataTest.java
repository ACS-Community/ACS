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
package alma.acs.logging.config;

import junit.framework.TestCase;

/**
 * @author hsommer
 * created 05.05.2006 10:07:08
 */
public class LogConfigDataTest extends TestCase {
        
    private LogConfigData logConfigData;
    
    public LogConfigDataTest() {
        super("LogConfigDataTest");
    }

    protected void setUp() throws Exception {
        super.setUp();
        logConfigData = new LogConfigData();
    }
    protected void tearDown() throws Exception {
        super.tearDown();
    }
    

    public void testParseContainerXml() throws LogConfigException {
        assertEquals("Log", logConfigData.getCentralizedLoggerName());
        assertEquals(0, logConfigData.getMinLogLevel());
        assertEquals(9, logConfigData.getExpeditedDispatchLevel());

        String bilboContainerXml = 
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?> " +  
                    "<Container xmlns:cdb=\"urn:schemas-cosylab-com:CDB:1.0\" " + 
                               "xmlns=\"urn:schemas-cosylab-com:Container:1.0\" " + 
                               "xmlns:baci=\"urn:schemas-cosylab-com:BACI:1.0\" " + 
                               "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" " + 
                               "CommandLine=\"\" " + 
                               "Timeout=\"20.0\" " + 
                               "UseIFR=\"1\" " + 
                               "ManagerRetry=\"10\" " + 
                               "ManagerReference=\"\" " + 
                               "CacheSize=\"1\" " + 
                               "MinCachePriority=\"3\" " + 
                               "MaxCachePriority=\"5\" " + 
                               "CentralizedLogger=\"LogBilbo\"> " + 
                      "<Autoload>" + 
                        "<cdb:_ string=\"baci\"/>" + 
                      "</Autoload>" + 
                    "</Container>";
        logConfigData.takeCdbContainerXml(bilboContainerXml);
        
        assertEquals("LogBilbo", logConfigData.getCentralizedLoggerName());
        assertEquals(3, logConfigData.getMinLogLevel());
        assertEquals(5, logConfigData.getExpeditedDispatchLevel());
    }
    
}

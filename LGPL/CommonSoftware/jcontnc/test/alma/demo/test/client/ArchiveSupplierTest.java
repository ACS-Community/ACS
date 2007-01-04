/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.demo.test.client;

import java.util.logging.Logger;
import alma.acs.component.client.ComponentClient;
import alma.acs.nc.ArchiveSupplier;
//import alma.ACS.impl.ROdoubleImpl;

/**
 * Client application that accesses the HelloDemo component. It demonstrates
 * how the class {@link ComponentClient}can be used as a base class.
 */
public class ArchiveSupplierTest extends ComponentClient
{
    /**
     * @param logger
     * @param managerLoc
     * @param clientName
     * @throws Exception
     */
    public ArchiveSupplierTest(Logger logger, String managerLoc, String clientName)
	throws Exception 
	{
	    super(logger, managerLoc, clientName);
	    
	    m_supplier = new ArchiveSupplier(getContainerServices());
	    //m_supplier.consumerReady();
	}
    
    public void tearDown() throws Exception
	{
	    m_supplier.disconnect();
	    super.tearDown();
	}

    public void publish() throws Exception
    {
	    m_supplier.publishEvent(new Double(3.14));	
	    m_supplier.publishEvent(new Float(3.1));	
	    m_supplier.publishEvent(new Long(1));	
	    m_supplier.publishEvent(new String("2"));	
	    m_supplier.publishEvent(new Long(3));	
	    m_supplier.publishEvent(new Long(4));	
	    m_supplier.publishEvent(new String("a string"));
	    double [] a = new double[1];
	    a[0] = (double)3.14;
	    m_supplier.publishEvent(a);	
	    float [] b = new float[1];
	    b[0] = (float)3.1;
	    m_supplier.publishEvent(b);	
	    //long sequence
	    int [] c = new int[1];
	    c[0] = (int)1;
	    m_supplier.publishEvent(c);	
	    String [] d = new String[2];
	    d[0] = "a";
            d[1] = "string";
	    m_supplier.publishEvent(d);	
    }   
 
    private ArchiveSupplier m_supplier = null;
    
    /**
     * Checks whether the Java property 'ACS.manager' is set and calls the
     * other methods from this class.
     */
    public static void main(String[] args) {
	String managerLoc = System.getProperty("ACS.manager");
	if (managerLoc == null) 
	    {
	    System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
	    System.exit(-1);
	    }
	String clientName = "ArchiveSupplierTest";
	ArchiveSupplierTest hlc = null;
	
	try 
	    {
	    hlc = new ArchiveSupplierTest(null, managerLoc, clientName);
	    hlc.publish();
	    }
	catch (Exception e) 
	    {
	    e.printStackTrace(System.err);
	    }
	
	}
 }


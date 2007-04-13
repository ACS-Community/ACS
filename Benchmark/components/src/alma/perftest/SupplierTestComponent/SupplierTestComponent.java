/*ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2004 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/** 
 * @author  dfugate
 * @version $Id: SupplierTestComponent.java,v 1.5 2007/04/13 02:51:27 sharring Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.SupplierTestComponent;

import alma.perftest.BasePerfCompImpl.BasePerfCompImpl;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.SimpleSupplier;

/**
 * 
 * @author  dfugate
 */
public class SupplierTestComponent extends BasePerfCompImpl
{
    private SimpleSupplier m_supplier = null;

    public void initialize(ContainerServices containerServices)
	throws ComponentLifecycleException
	{
	    super.initialize(containerServices);
	    try
		{
		m_supplier = new SimpleSupplier("perf channel", m_containerServices);
		}
	    catch(alma.acs.exceptions.AcsJException ex)
		{
		throw new ComponentLifecycleException();
		}
	}

    public void method()
    {
	m_profiler.reset();
	
	//populate the string to be used for logging
	char[] tArray = new char[m_size];
	for(int i=0; i<m_size; i++)
	    {
	    tArray[i] = '*';
	    }
	alma.perftest.charSeqStruct joe = new alma.perftest.charSeqStruct(tArray);
	
	try
	    {
	    for(int i=0; i<m_count; i++)
		{
		m_profiler.start();
		m_supplier.publishEvent(joe);
		m_profiler.stop();
		waitAwhile();
		}
	    
	    m_profiler.fullDescription("Event Channel Event of Size '" + m_size + "' Bytes from within a CharacteristicComponent");
	    return;
	    }
	catch(Exception ex)
	    {
	    System.err.println(ex);
	    }
    }
    
    /** Disconnects the supplier. */
    public void cleanUp()
	{
	    m_logger.info("cleanUp() called...");
	    
	    try
		{
		m_supplier.disconnect();
		}
	    catch(Exception e)
		{
		e.printStackTrace();  
		}
	}
}

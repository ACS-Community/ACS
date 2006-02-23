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
 * @version $Id: SimpleBACIComponentImpl.java,v 1.1 2004/09/28 17:27:40 dfugate Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package almaCompileTime.IdlCompilationTime.SimpleBACIComponentImpl;

import java.util.logging.Logger;

import alma.ACS.impl.CharacteristicComponentImpl;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

import alma.IdlCompilationTime.SimpleBACIComponentOperations;


/**
 * 
 * @author  dfugate
 */
public class SimpleBACIComponentImpl extends CharacteristicComponentImpl implements SimpleBACIComponentOperations
{
    private ContainerServices m_containerServices;
    private Logger m_logger;

    

    public void method()
    {
        return;
    }

    public void action(alma.ACS.CBvoid cb, alma.ACS.CBDescIn desc)
    {
        return;
    }

    public alma.ACS.RWlong property()
	{
	    return null;
	}
    
    /**
     * Delegates to <code>cleanUp</code>. 
	 * <p>
	 * {@inheritDoc}
     */
    public void aboutToAbort()
    {
        cleanUp();
        m_logger.info("managed to abort...");
    }

    /**
     * Disconnects the supplier. 
	 * <p>
	 * {@inheritDoc}
     */
    public void cleanUp()
    {
        m_logger.info("cleanUp() called...");
    }
    

    /**
     * Sets up the SimpleSupplier.
     * <p>
     * {@inheritDoc}
     */
    public void initialize(ContainerServices containerServices)
	throws ComponentLifecycleException
	{
	    m_containerServices = containerServices;
	    m_logger = m_containerServices.getLogger();
	    m_logger.info("initialize() called...");
	}
}

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
 * @version $Id: BasePerfCompImpl.java,v 1.7 2007/04/13 02:51:27 sharring Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.BasePerfCompImpl;

import alma.ACS.impl.CharacteristicComponentImpl;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.time.Profiler;
import alma.perftest.BasePerfCompOperations;

/**
 * 
 * @author  dfugate
 */
public class BasePerfCompImpl extends CharacteristicComponentImpl implements BasePerfCompOperations
{
    protected int m_count;
    protected int m_size;
    protected long m_waitTime;
    
    protected Profiler m_profiler;

    public void method()
    {
        return;
    }

    public void setup(int count, int size, long waitTime)
    {
	m_count = count;
	m_size = size;
	if (waitTime < 10000)
	    {
	    //bad...Java cannot handle this
	    m_waitTime = 0;
	    }
	else
	    {
	    m_waitTime = waitTime/10000;  //convert to MS
	    }
        return;
    }

    protected void waitAwhile()
	throws Exception
	{
	    if (m_waitTime != 0)
		{
		Thread.sleep(m_waitTime);
		}
	}
    
    public void initialize(ContainerServices containerServices)
	throws ComponentLifecycleException
	{
	    super.initialize(containerServices);
	    m_logger.info("initialize() called...");
	    m_count = 0;
	    m_size = 0;
	    m_profiler = new Profiler();
	}
}

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
 * @version $Id: LogTestComponent.java,v 1.4 2004/10/21 22:48:12 dfugate Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.LogTestComponent;

import alma.perftest.BasePerfCompImpl.BasePerfCompImpl;



/**
 * 
 * @author  dfugate
 */
public class LogTestComponent extends BasePerfCompImpl
{
    public void method()
	{
	    m_profiler.reset();

	    //populate the string to be used for logging
	    char[] tArray = new char[m_size];
	    for(int i=0; i<m_size; i++)
		{
		tArray[i] = '*';
		}
	    String tString = new String(tArray);

	    for(int i=0; i<m_count; i++)
		{
		m_profiler.start();
		m_logger.info(tString);
		m_profiler.stop();
		try
		    {
		    waitAwhile();
		    }
		catch(Exception e)
		    {
		    System.err.println(e);
		    }
		}

	    m_profiler.addData("ACS_LOG_STDOUT", System.getProperty("ACS_LOG_STDOUT", "Unknown"));
	    
	    m_profiler.fullDescription("ACS Log of Size '" + m_size + "' Bytes from within a CharacteristicComponent");
	    return;
	}
}

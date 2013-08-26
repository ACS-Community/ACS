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
 * @version $Id: MethodTestComponentImpl.java,v 1.5 2007/04/13 02:51:27 sharring Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.MethodTestComponentImpl;

import alma.perftest.BasePerfCompImpl.BasePerfCompImpl;
import alma.perftest.MethodTestComponentOperations;

/**
 * 
 * @author  dfugate
 */
public class MethodTestComponentImpl extends BasePerfCompImpl implements MethodTestComponentOperations
{
    private char[] m_retVal;
    
    public void setup(int count, int size, long baseTime)
	{
	    super.setup(count, size, baseTime);
	    
	    //populate the string to be used for logging
	    m_retVal = new char[m_size];
	    for(int i=0; i<m_size; i++)
		{
		m_retVal[i] = '*';
		}
	}

    public char[] testReturnSize()
    {
	return m_retVal;
    }

    //void testInParam(in charSeq chars);
    public void testInParam(char [] chars)
	{
	    return;
	}
}

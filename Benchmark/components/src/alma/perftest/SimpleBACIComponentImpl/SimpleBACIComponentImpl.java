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
 * @version $Id: SimpleBACIComponentImpl.java,v 1.2 2007/04/13 02:51:27 sharring Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.SimpleBACIComponentImpl;

import alma.perftest.BasePerfCompImpl.BasePerfCompImpl;
import alma.perftest.SimpleBACIComponentOperations;

/**
 * 
 * @author  dfugate
 */
public class SimpleBACIComponentImpl extends BasePerfCompImpl implements SimpleBACIComponentOperations
{
    public void action(alma.ACS.CBvoid cb, alma.ACS.CBDescIn desc)
    {
        return;
    }

    public alma.ACS.RWlong property()
	{
	    return null;
	}
}

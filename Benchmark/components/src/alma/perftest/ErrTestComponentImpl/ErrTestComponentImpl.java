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
 * @version $Id: ErrTestComponentImpl.java,v 1.3 2007/04/13 02:51:27 sharring Exp $
 * @since    
 */

/**
 * Insert a Class/Interface comment.
 * 
 */

package alma.perftest.ErrTestComponentImpl;

import alma.perftest.BasePerfCompImpl.BasePerfCompImpl;
import alma.perftest.ErrTestComponentOperations;
import alma.BenchmarkErrType.BenchmarkErr0Ex;
import alma.BenchmarkErrType.wrappers.AcsJBenchmarkErr0Ex;

/**
 * 
 * @author  dfugate
 */
public class ErrTestComponentImpl extends BasePerfCompImpl implements ErrTestComponentOperations
{
    public AcsJBenchmarkErr0Ex getException(int depth, AcsJBenchmarkErr0Ex prevExcept)
	{
	    if (depth==0)
		{
		return prevExcept;
		}

	    else
		{
		return getException(depth-1, new AcsJBenchmarkErr0Ex(prevExcept));
		}
	}
    
    public void testExceptions(int depth, boolean isError) throws BenchmarkErr0Ex
	{
	    //sanity check
	    if (depth < 0)
		{
			AcsJBenchmarkErr0Ex myExcept = new AcsJBenchmarkErr0Ex();
			throw myExcept.toBenchmarkErr0Ex();
		}

	    BenchmarkErr0Ex except = getException(depth, new AcsJBenchmarkErr0Ex()).toBenchmarkErr0Ex();
	    
	    if(isError)
		{
		throw except;
		}
	}
}

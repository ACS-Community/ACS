/*
ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

/** 
 * @author  cparedes
 * @version $Id$
 * @since    
 */
package alma.acs.JVMSettingsTest;

import java.util.ArrayList;
import java.util.List;

/**
 * This class very quickly consumes heap memory until an OutOfMemoryError is thrown.
 * Depending on the parameter given to main ("throw" or "return"), 
 * the call either fails with the OutOfMemoryError, or simply returns.
 */
public class ConsumeHeap
{
	public void produceOutOfMemoryError(boolean throwOutOfMemoryError) { 
		List<Long[]> list = new ArrayList<Long[]>();
		while(true) {
			try {
				list.add(new Long[1000000]);
			}
			catch (OutOfMemoryError e) {
				System.out.println("OutOfMemoryError was thrown as expected.");
				if (throwOutOfMemoryError) {
					throw e;
				}
				else {
					return;
				}
			}
		}
	}

	public static void main(String args[]) {
		if (args.length != 1 && !args[0].equals("throw") && !args[0].equals("return")) {
			throw new IllegalArgumentException("one arg 'throw' or 'no_throw' expected");
		}
		ConsumeHeap t = new ConsumeHeap();
		t.produceOutOfMemoryError(args[0].equals("throw"));
	}
}

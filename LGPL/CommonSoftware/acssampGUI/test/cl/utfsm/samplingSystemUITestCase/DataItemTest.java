/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUITestCase;

import junit.framework.TestCase;
import cl.utfsm.samplingSystemUI.core.DataItem;

public class DataItemTest extends TestCase {

	public void testEquals() throws Exception {
		DataItem sd1 = new DataItem(100,3.14);
		DataItem sd2 = new DataItem(100,3.14);
		DataItem sd3 = new DataItem(117,2.70);
		assertEquals(sd1,sd2);
		assertTrue(sd1.hashCode()==sd2.hashCode());
		assertNotSame(sd2,sd3);
		assertTrue(sd1.getTime()==100);
		assertTrue(sd1.getValue()==3.14);
	}



}

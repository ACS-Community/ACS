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
import cl.utfsm.samplingSystemUI.core.SampDetail;

public class SampDetailTest extends TestCase {

	public void testEquals() throws Exception {
		SampDetail sd1 = new SampDetail("SAMP1","brightness",100,1);
		SampDetail sd2 = new SampDetail("SAMP1","brightness",100,1);
		SampDetail sd3 = new SampDetail("SAMP1","brightness",200,1);
		assertEquals(sd1,sd2);
		assertTrue(sd1.hashCode()==sd2.hashCode());
		assertNotSame(sd2,sd3);
		assertTrue(sd1.getComponent().equals("SAMP1"));
		assertTrue(sd1.getProperty().equals("brightness"));
		assertTrue(sd1.getFrequency()==100);
		assertTrue(sd1.getReportRate()==1);


	}



}

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2014
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
package alma.acs.nc;

import static org.hamcrest.Matchers.arrayWithSize;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertThat;

import org.junit.Test;

/**
 * This class contains tests for {@link Helper} that do not require a running infrastructure.
 * Rather than adding these tests to the old JUnit3 class, we created a separate test class.
 */
public class HelperTestStandalone
{
	/**
	 * Tests {@link Helper#createRandomizedClientName(String)}.
	 */
	@Test
	public void testCreateRandomizedClientName() {
		
		String name = Helper.createRandomizedClientName("SimpleClientName");
		assertClientName("SimpleClientName", name);
		
		name = Helper.createRandomizedClientName("CONTROL/PM01/Mount");
		assertClientName("CONTROL_PM01_Mount", name);
		
		name = Helper.createRandomizedClientName("PRAEFIX_CONTROL/PM01/Mount");
		assertClientName("PRAEFIX_CONTROL_PM01_Mount", name);
	}
	
	
	private void assertClientName(String expectedStem, String actual) {
		String[] nameStemAndNumber = actual.split("-");
		assertThat(nameStemAndNumber, arrayWithSize(2));
		String nameStem = nameStemAndNumber[0];
		assertThat(nameStem, equalTo(expectedStem));
		Integer.parseInt(nameStemAndNumber[1]); // will throw NumberFormatException if it's not an int
		System.out.println("OK: validated client name translation to '" + actual + "'.");
	}
}

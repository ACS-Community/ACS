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
package alma.acs.tmcdb;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

public class TestPojosInitialization extends TestCase {

	private static final String CONTNAME = "SUPERCONTAINER";
	private static final String ACNAME = "SUPERCATEGORY";

	public void testEquals() {

		// Test default equals (java equals/hashcode)
		ComponentType ct1 = new ComponentType();  ct1.setIDL("1");
		ComponentType ct2 = new ComponentType();  ct2.setIDL("1");
		ComponentType ct3 = new ComponentType();  ct3.setIDL("1");

		assertTrue(ct1.equals(ct1));
		assertTrue(ct2.equals(ct2));
		assertTrue(ct3.equals(ct3));

		assertFalse(ct1.equals(ct2));
		assertFalse(ct1.equals(ct3));
		assertFalse(ct2.equals(ct1));
		assertFalse(ct2.equals(ct3));
		assertFalse(ct3.equals(ct1));
		assertFalse(ct3.equals(ct2));


		// Now let's use the custom content-based equals()
		ct1.setUseContentEqualsAndHashCode(true);
		ct2.setUseContentEqualsAndHashCode(true);
		ct3.setUseContentEqualsAndHashCode(true);
		assertTrue(ct1.equals(ct1));
		assertTrue(ct2.equals(ct2));
		assertTrue(ct3.equals(ct3));

		assertTrue(ct1.equals(ct2));
		assertTrue(ct1.equals(ct3));
		assertTrue(ct2.equals(ct1));
		assertTrue(ct2.equals(ct3));
		assertTrue(ct3.equals(ct1));
		assertTrue(ct3.equals(ct2));
	}

	public void testAdd() {

		Configuration conf = new Configuration();

		// Test simple add* method
		for(int i=0; i!= 10; i++) {
			Container cont = new Container();
			cont.setContainerName(CONTNAME + "-" + i);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			conf.addContainerToContainers(cont);
		}

		assertEquals(conf.getContainers().size(), 10);
		for(Container cont : conf.getContainers()) {
			assertTrue(cont.getContainerName().startsWith(CONTNAME));
			assertEquals(cont.getCallTimeout().intValue(), 0);
			assertEquals(cont.getConfiguration(), conf);
		}

		// Test addSeveral* method
		Set<Container> set = new HashSet<Container>();
		for (int i = 10; i != 20; i++) {
			Container cont = new Container();
			cont.setContainerName(CONTNAME + "-" + i);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			set.add(cont);
		}
		conf.addContainers(set);

		assertEquals(conf.getContainers().size(), 20);
		for(Container cont : conf.getContainers()) {
			assertTrue(cont.getContainerName().startsWith(CONTNAME));
			assertEquals(cont.getCallTimeout().intValue(), 0);
			assertEquals(cont.getConfiguration(), conf);
		}

		// Now testing with ManyToMany
		FaultFamily ff1 = new FaultFamily();
		ff1.setFamilyName("FF1");
		FaultFamily ff2 = new FaultFamily();
		ff2.setFamilyName("FF2");

		List<AlarmCategory> categories = new ArrayList<AlarmCategory>();
		for(int i = 0; i!= 10; i++)  {
			AlarmCategory ac = new AlarmCategory();
			ac.setAlarmCategoryName(ACNAME + "-" + i);
			ac.setConfiguration(conf);
			ac.setPath("MyPath");
			ac.addFaultFamilyToFaultFamilies(ff1);
			ac.addFaultFamilyToFaultFamilies(ff2);
			ff1.addAlarmCategoryToAlarmCategories(ac);
			ff2.addAlarmCategoryToAlarmCategories(ac);
			categories.add(ac);
		}

		assertEquals(ff1.getAlarmCategories().size(), 10);
		assertEquals(ff2.getAlarmCategories().size(), 10);
		for(AlarmCategory ac: categories) {
			assertEquals(ac.getFaultFamilies().size(), 2);
			for(FaultFamily ff: ac.getFaultFamilies()) {
				if( !ff.equals(ff1) )
					assertEquals(ff, ff2);
				if( !ff.equals(ff2) )
					assertEquals(ff, ff1);
			}
		}
	}

	public void testAddWithEqualsContent() {

		Configuration conf = new Configuration();

		// Test simple add* method
		for(int i=0; i!= 10; i++) {
			Container cont = new Container();
			cont.setUseContentEqualsAndHashCode(true);
			cont.setContainerName(CONTNAME);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			conf.addContainerToContainers(cont); // We set the same name and configuration, so we should only add once
		}
		assertEquals(conf.getContainers().size(), 1);

		// Test addSeveral* method
		Set<Container> set = new HashSet<Container>();
		for (int i = 10; i != 20; i++) {
			Container cont = new Container();
			cont.setUseContentEqualsAndHashCode(true);
			cont.setContainerName(CONTNAME + "-" + i);
			cont.setCallTimeout(0);
			cont.setConfiguration(conf);
			set.add(cont);
		}
		conf.addContainers(set);
		assertEquals(conf.getContainers().size(), 11);

		// Now testing with ManyToMany
		FaultFamily ff1 = new FaultFamily();
		ff1.setFamilyName("FF1");
		FaultFamily ff2 = new FaultFamily();
		ff2.setFamilyName("FF2");

		List<AlarmCategory> categories = new ArrayList<AlarmCategory>();
		for(int i = 0; i!= 10; i++)  {
			AlarmCategory ac = new AlarmCategory();
			ac.setUseContentEqualsAndHashCode(true);
			ac.setAlarmCategoryName(ACNAME);
			ac.setConfiguration(conf);
			ac.setPath("MyPath");
			ac.addFaultFamilyToFaultFamilies(ff1);
			ac.addFaultFamilyToFaultFamilies(ff2);
			ff1.addAlarmCategoryToAlarmCategories(ac);
			ff2.addAlarmCategoryToAlarmCategories(ac);
			categories.add(ac);
		}

		assertEquals(ff1.getAlarmCategories().size(), 1);
		assertEquals(ff2.getAlarmCategories().size(), 1);

	}

}

/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2012
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.alarmsystem.alarmsource.test;

import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.alarmsystem.source.AlarmSourceFactory;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * Test the {@link AlarmSourceFactory}
 * 
 * @author acaproni
 *
 */
public class SourceFactoryTest extends ComponentClientTestCase  {
	
	/**
	 * The object to test
	 */
	private AlarmSourceFactory factory;

	/**
	 * Constructor
	 */
	public SourceFactoryTest(String name) throws Exception  {
		super(SourceFactoryTest.class.getName());
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		System.out.println("setUp");
		factory= new AlarmSourceFactory(this.getContainerServices());
		assertNotNull(factory);
	}

	@Override
	protected void tearDown() throws Exception {
		System.out.println("tearDown");
		factory.tearDown();
		super.tearDown();
	}
	
	/**
	 * Check that the factory return the same {@link AlarmSource} for the same name
	 */
	public void testSameReference() throws Exception {
		System.out.println("testSameReference");
		String name = "componentName";
		AlarmSource src1=factory.getAlarmSource(name);
		assertNotNull(src1);
		AlarmSource src2=factory.getAlarmSource(name);
		assertNotNull(src2);
		
		assertTrue("The returned sources differ!", src1==src2);
	}
	
	/**
	 * Check that the factory return the same {@link AlarmSource} for the same name
	 */
	public void testReferences() throws Exception {
		System.out.println("testReferences");
		AlarmSource src1=factory.getAlarmSource("componentName1");
		assertNotNull(src1);
		AlarmSource src2=factory.getAlarmSource("componentName2");
		assertNotNull(src2);
		
		assertTrue("The returned sources do NOT differ!", src1!=src2);
	}
	
	/**
	 * Test that factory removes a source after calling release
	 */
	public void testGetRelease() {
		System.out.println("testGetRelease");
		String name = "componentName";
		AlarmSource src1=factory.getAlarmSource(name);
		assertNotNull(src1);
		factory.releaseAlarmSource(name);
		AlarmSource src2=factory.getAlarmSource(name);
		assertNotNull(src2);
		
		assertTrue("The returned sources do NOT differ!", src1!=src2);
	}
	
}

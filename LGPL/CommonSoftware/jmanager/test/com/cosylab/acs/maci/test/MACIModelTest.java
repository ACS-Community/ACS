/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.MessageType;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * JUnit Test for MACI model.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class MACIModelTest extends TestCase
{

	/**
	 */
	public MACIModelTest(String name)
	{
		super(name);
	}
	
	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(MACIModelTest.class);
	}
	

	/**
	 * "Test" COBStatus
	 */
	public void testComponentStatus()
	{
		System.out.println(ComponentStatus.COMPONENT_ACTIVATED);
		System.out.println(ComponentStatus.COMPONENT_NOT_ACTIVATED);
		System.out.println(ComponentStatus.COMPONENT_DOES_NO_EXIST);
	}
	
	/**
	 * "Test" MessageType
	 */
	public void testMessageType()
	{
		System.out.println(MessageType.MSG_INFORMATION);
		System.out.println(MessageType.MSG_ERROR);
	}

}


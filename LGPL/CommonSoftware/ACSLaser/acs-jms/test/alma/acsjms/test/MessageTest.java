/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

/** 
 * @author  acaproni   
 * @version $Id: MessageTest.java,v 1.2 2006/09/25 09:15:58 acaproni Exp $
 * @since    
 */

package alma.acsjms.test;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;

import com.cosylab.acs.jms.ACSJMSMessage;
import com.cosylab.acs.jms.ACSJMSMessageEntity;
import com.cosylab.acs.jms.ACSJMSObjectMessage;
import com.cosylab.acs.jms.ACSJMSTextMessage;

/**
 * Test the messages (Message, ObjectMessage and TextMessage)
 *
 */
public class MessageTest extends ComponentClientTestCase {
	
	public MessageTest() throws Exception {
		super("MessageTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/**
	 * Test some of the implemented methods of ACSJMSMessage
	 * 
	 * @throws Exception
	 */
	public void testACSJMSMessage() throws Exception {
		ContainerServices cs = getContainerServices();
		assertNotNull("ContainerServices is null!",cs);
		ACSJMSMessage msg = new ACSJMSMessage(cs);
		assertNotNull("Error building ACSJMSMessage",msg);
		ACSJMSMessageEntity entity = msg.getEntity();
		assertNotNull("Enitity is null!",entity);
		// Insert some props
		Boolean boolPropVal = new Boolean(false);
		Integer intPropVal = new Integer(1234);
		String strPropVal = new String ("String prperty value");
		String boolPropName = "boolPropName";
		String intPropName = "intPropName";
		String strPropName = "strPropName";
		msg.setObjectProperty(intPropName,intPropVal);
		msg.setObjectProperty(strPropName,strPropVal);
		msg.setObjectProperty(boolPropName,boolPropVal);
		// Check if the properties are in the message
		assertTrue("Boolean property not found!",msg.propertyExists(boolPropName));
		assertTrue("Integer property not found!",msg.propertyExists(intPropName));
		assertTrue("String property not found!",msg.propertyExists(strPropName));
		// Check if a wrong property is in the message
		assertFalse("This property should not be found!",msg.propertyExists("NotExistingProp"));
		// Check the values of the properties
		assertEquals("Wrong value of Integer property",intPropVal,(Integer)msg.getIntProperty(intPropName));
		assertEquals("Wrong value of String property",strPropVal,msg.getStringProperty(strPropName));
		assertEquals("Wrong value of Boolean property",boolPropVal,(Boolean)msg.getBooleanProperty(boolPropName));
		// Clean the property and try to find a property
		msg.clearProperties();
		assertFalse("This Integer property should not be here",msg.propertyExists(intPropName));
		assertFalse("This String property should not be here",msg.propertyExists(strPropName));
		assertFalse("This Boolean property should not be here",msg.propertyExists(boolPropName));
	}
	
	/**
	 * Test the ACSJMSTextMessage
	 * 
	 * @throws Exception
	 */
	public void testTextMessage() throws Exception {
		ContainerServices cs = getContainerServices();
		assertNotNull("ContainerServices is null!",cs);
		ACSJMSTextMessage txtMsg = new ACSJMSTextMessage(cs);
		assertNotNull("Error building the ACSJMSTextMessage",txtMsg);
		String text = "The test of the message";
		txtMsg.setText(text);
		assertEquals("Wrong test returned by the message",text,txtMsg.getText());
	}
	
	/**
	 * Test the ACSJMSObjectMessage
	 * 
	 * @throws Exception
	 */
	public void testObjectMessage() throws Exception {
		ContainerServices cs = getContainerServices();
		assertNotNull("ContainerServices is null!",cs);
		ACSJMSObjectMessage objMsg = new ACSJMSObjectMessage(cs);
		assertNotNull("Error building the ACSJMSObjectMessage",objMsg);
		Integer intVal = new Integer(1099);
		objMsg.setObject(intVal);
		Object obj = objMsg.getObject();
		assertTrue("Wrong class type",obj instanceof Integer);
		assertEquals("The object differs",intVal,(Integer)obj);
	}
	
	public void testEntity() throws Exception {
		ContainerServices cs = getContainerServices();
		assertNotNull("ContainerServices is null!",cs);
		ACSJMSMessageEntity entity = new  ACSJMSMessageEntity();
		String entityTxt = "Text for the entity";
		Integer priority = 100;
		entity.priority = priority;
		entity.text=entityTxt;
		ACSJMSMessage msg = new ACSJMSMessage(entity,cs);
		assertNotNull("Error building ACSJMSMessage",msg);
		ACSJMSMessageEntity entityReturned = msg.getEntity();
		assertNotNull("Enitity is null!",entityReturned);
		assertEquals("Wrong text in the returned entity",entityTxt,entityReturned.text);
		assertEquals("Wrong priority in the returned entity",priority,(Integer)entityReturned.priority);
	}
}

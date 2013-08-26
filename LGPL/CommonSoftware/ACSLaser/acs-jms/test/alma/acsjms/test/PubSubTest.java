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
 * @author  almadev   
 * @version $Id: PubSubTest.java,v 1.2 2006/09/25 09:15:58 acaproni Exp $
 * @since    
 */

package alma.acsjms.test;

import alma.acs.component.client.ComponentClientTestCase;

import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.jms.TopicPublisher;
import javax.jms.TopicSubscriber;
import javax.jms.MessageListener;

import com.cosylab.acs.jms.ACSJMSTopicConnectionFactory;
import com.cosylab.acs.jms.ACSJMSTextMessage;

public class PubSubTest extends ComponentClientTestCase implements MessageListener {
	
	String msgToSend = new String ("Message for test");
	
	ACSJMSTopicConnectionFactory factory = null;
	
	public PubSubTest() throws Exception {
		super("PubSubTest");
		
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void setUp() throws Exception {
		super.setUp();
		factory = new ACSJMSTopicConnectionFactory(getContainerServices());
		assertNotNull("Error building the ACSJMSTopicConnectionFactory",factory);
	}
	
	public void testTopicConnection() throws Exception {
		TopicConnection topicConnection = factory.createTopicConnection();
		assertNotNull("Error creating the TopicConnection",topicConnection);
		TopicSession topicSession = topicConnection.createTopicSession(true,0);
		assertNotNull("Error creating the TopicSession",topicSession);
		String chName ="TOPIC.TEST";
		Topic topic = topicSession.createTopic(chName);
		assertNotNull("Error creating Topic",topic);
		assertEquals("Wrong name for the topic",chName,topic.getTopicName());
		TopicPublisher publisher = topicSession.createPublisher(topic);
		assertNotNull("Error creating the TopicPublisher ",publisher);
		TopicSubscriber subscriber = topicSession.createSubscriber(topic);
		assertNotNull("Error creating the TopicSubscriber",subscriber);
		subscriber.setMessageListener(this);
		ACSJMSTextMessage msg = new ACSJMSTextMessage(getContainerServices());
		assertNotNull("Error creating ACSJMSTextMessage",msg);
		msg.setText(msgToSend);
		publisher.publish(msg);
		// Wait for awhile to have enough time to receive the message
		try {
			Thread.sleep(10000);
		} catch (Exception e) {}
		publisher.close();
		subscriber.close();
	}
	
	public void onMessage(Message msg) {
		if (msg instanceof ACSJMSTextMessage) {
			ACSJMSTextMessage txtMsg = (ACSJMSTextMessage)msg;
			try {
				assertEquals("Wrong message received",txtMsg.getText(),msgToSend);
			} catch (Exception e) {
				System.out.println("Exception caught while getting the text of the msg:"+e.getMessage());
				e.printStackTrace();
			}
		} else {
			System.out.println("PubSubTest.onMessage(javax.jms.Message): Wrong msg type received");
		}
	}
}

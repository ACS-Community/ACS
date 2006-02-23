/*
 * Copyright (c) 2004 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import alma.acs.component.client.ComponentClient;

import junit.framework.TestCase;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;
import javax.jms.MessageProducer;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;


/**
 * DOCUMENT ME!
 *
 * @author kzagar To change the template for this generated type comment go to
 *         Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and
 *         Comments
 */
public class JMSTest extends TestCase
{
	static ComponentClient client;

	/**
	 * Constructor for JMSTest.
	 *
	 * @param name
	 *
	 * @throws Exception DOCUMENT ME!
	 */
	public JMSTest(String name) throws Exception
	{
		super(name);

		if (client == null) {
			String managerLoc = System.getProperty("ACS.manager");

			if (managerLoc == null) {
				System.out.println(
				    "Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
				System.exit(-1);
			}

			client = new ComponentClient(null, managerLoc, "JMSTest.testJMS");
		}
	}

	/**
	 * DOCUMENT ME!
	 *
	 * @throws Exception DOCUMENT ME!
	 */
	public void testSyncJMS() throws Exception
	{
		final String MESSAGE_BODY = "hello, world!";

		ACSJMSConnectionFactory connectionFactory = new ACSJMSConnectionFactory(client
			    .getContainerServices());
		Topic queue = new ACSJMSTopic("TestTopic");
		ACSJMSConnection connection = (ACSJMSConnection)connectionFactory.createConnection();
		ACSJMSSession session = (ACSJMSSession) connection.createSession(false,
			    Session.AUTO_ACKNOWLEDGE);
		MessageProducer sender = session.createProducer(queue);
		MessageConsumer consumer = session.createConsumer(queue);
		connection.start();

		TextMessage message = session.createTextMessage();
		message.setText(MESSAGE_BODY);

		message.setStringProperty("name", "value");
		assertEquals("value", message.getStringProperty("name"));
		sender.send(message);

		TextMessage message2 = (TextMessage)consumer.receive(1000);
		assertNotNull(message2);
		assertEquals(MESSAGE_BODY, message2.getText());
		// TODO Message properties aren't properly transported through CORBA Notification Service yet.
		//assertEquals("value", message2.getStringProperty("name"));
	}

	/**
	 * DOCUMENT ME!
	 *
	 * @throws Exception DOCUMENT ME!
	 */
	public void testAsyncJMS() throws Exception
	{
		final String MESSAGE_BODY = "hello, world!";

		ACSJMSConnectionFactory connectionFactory = new ACSJMSConnectionFactory(client
			    .getContainerServices());
		Topic queue = new ACSJMSTopic("TestTopic");
		ACSJMSConnection connection = (ACSJMSConnection) connectionFactory.createConnection();
		Session session = connection.createSession(false,
			    Session.AUTO_ACKNOWLEDGE);
		MessageProducer sender = session.createProducer(queue);
		MessageConsumer consumer = session.createConsumer(queue);
		TestListener listener = new TestListener();
		consumer.setMessageListener(listener);
		connection.start();

		TextMessage message = session.createTextMessage();
		message.setText(MESSAGE_BODY);
		sender.send(message);

		Thread.sleep(100);

		assertEquals(MESSAGE_BODY, listener.getMessage());
	}

	/**
	 * DOCUMENT ME!
	 *
	 * @throws Exception DOCUMENT ME!
	 */
	public void testStressJMS() throws Exception
	{
		final String MESSAGE_BODY = "hello, world!";
		final int REPEAT = 200;

		ConnectionFactory connectionFactory = new ACSJMSConnectionFactory(client
			    .getContainerServices());
		Topic queue = new ACSJMSTopic("TestTopic");
		Connection connection = connectionFactory.createConnection();
		Session session = connection.createSession(false,
			    Session.AUTO_ACKNOWLEDGE);
		MessageProducer sender = session.createProducer(queue);
		MessageConsumer consumer = session.createConsumer(queue);
		connection.start();

		for (int i = 0; i < REPEAT; ++i) {
			TextMessage message = session.createTextMessage();
			message.setText(MESSAGE_BODY + i);
			sender.send(message);
		}

		for (int i = 0; i < REPEAT; ++i) {
			TextMessage message2 = (TextMessage)consumer.receive(1000);
			assertNotNull(message2);
			assertEquals(MESSAGE_BODY + i, message2.getText());
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	protected void finalize() throws Throwable
	{
		if (client != null) {
			client.tearDown();
			client = null;
		}

		super.finalize();
	}

	private static class TestListener implements MessageListener
	{
		private String message;

		/* (non-Javadoc)
		 * @see javax.jms.MessageListener#onMessage(javax.jms.Message)
		 */
		public void onMessage(Message message)
		{
			try {
				this.message = ((TextMessage)message).getText();
			} catch (JMSException e) {
				this.message = null;
			}
		}

		/**
		 * DOCUMENT ME!
		 *
		 * @return
		 */
		public String getMessage()
		{
			return this.message;
		}
	}
}

/* __oOo__ */

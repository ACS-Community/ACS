/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import java.util.ArrayList;
import java.util.List;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageListener;

import alma.acs.container.ContainerServicesBase;

import com.codestreet.selector.parser.InvalidSelectorException;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public abstract class ACSJMSConsumer implements MessageConsumer {
	private MessageListener listener;

	protected Destination destination;

	private List messages = new ArrayList();
	
	private ContainerServicesBase containerServices;
	
	
	private ACSJMSMessageSelector selector=null;

	/**
	 * The constructor of the consumer
	 * 
	 * @param destination
	 * @param contServices The Container services
	 * @param selector The message selector (can be empty or null)
	 */
	public ACSJMSConsumer(Destination destination, ContainerServicesBase contServices, String selector) throws JMSException {
		this.destination = destination;
		this.containerServices=contServices;
		try {
			this.selector = new ACSJMSMessageSelector(selector);
		} catch (InvalidSelectorException e) {
			System.err.println("Exception caught while building an ACSJMSConsumer:");
			System.err.println(e.getMessage());
			JMSException newException = new JMSException(e.getMessage());
			throw newException;
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#getMessageSelector()
	 */
	public String getMessageSelector() throws JMSException {
		return selector.getSelectorString();
	}
	
	/**
	 * Set a new SQL92 selector string for the consumer
	 * 
	 * @param selectorString
	 */
	public void setMessageSelector(String selectorString) {
		try {
			this.selector.setSelectorString(selectorString);
			System.out.println("## Selector set to ["+selectorString+"]");
		} catch (InvalidSelectorException e) {
			System.err.println("Exception setting the selector "+selectorString);
			System.err.println("Exception message: "+e.getMessage());
			e.printStackTrace(System.err);
			System.err.println("The selector will be set to null!");
			try {
				this.selector.setSelectorString(null);
			} catch (InvalidSelectorException ex) {}
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#getMessageListener()
	 */
	public MessageListener getMessageListener() throws JMSException {
		return this.listener;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#setMessageListener(javax.jms.MessageListener)
	 */
	public void setMessageListener(MessageListener listener) throws JMSException {
		this.listener = listener;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#receive()
	 */
	public Message receive() throws JMSException {
		// TODO Synchronized receiving of messages should be implemented with events.
		while(true) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				return null;
			}
			synchronized(messages) {
				if(!messages.isEmpty()) {
					Message retVal = (Message) messages.get(0);
					messages.remove(0);
					return retVal;
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#receive(long)
	 */
	public Message receive(long timeout) throws JMSException {
		// TODO Synchronized receiving of messages should be implemented with events.
		long sleep = 0;
		while(sleep < timeout) {
			synchronized(messages) {
				if(!messages.isEmpty()) {
					Message retVal = (Message) messages.get(0);
					messages.remove(0);
					return retVal;
				}
			}
			try {
				sleep += 100;
				Thread.sleep(100);
			} catch (InterruptedException e) {
				return null;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#receiveNoWait()
	 */
	public Message receiveNoWait() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#close()
	 */
	public void close() throws JMSException {
		throw new UnsupportedOperationException();
	}
	
	public void receive(ACSJMSMessageEntity message)
	{
		if (message==null) {
			throw new NullPointerException("The message received is null");
		}
		
		ACSJMSMessage jmsMessage = null;
		if (message.type.compareTo("com.cosylab.acs.jms.ACSJMSObjectMessage")==0){
			jmsMessage = new ACSJMSObjectMessage(message,containerServices);
		} else {
			jmsMessage = new ACSJMSTextMessage(message,containerServices);
		}
		
		synchronized(messages) {
			this.messages.add(jmsMessage);
		}
		
		if(this.listener == null) {
			return;
		} else if (selector.match(jmsMessage)) {
			this.listener.onMessage(jmsMessage);
		}
	}
}

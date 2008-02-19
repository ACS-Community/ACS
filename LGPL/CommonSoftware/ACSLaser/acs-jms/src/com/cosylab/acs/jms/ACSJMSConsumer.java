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
import alma.acs.logging.AcsLogLevel;

import com.codestreet.selector.parser.InvalidSelectorException;

/**
 * This class supports only the listener mechanism.
 * 
 * The overloaded <code>receive</code> methods and <code>receiveNoWait</code> 
 * are not implemented and throw an <code>UnsupportedOperationException</code>.
 * Having both methods (receive and callback) in place causes an out of memory
 * if the queue of message is not flushed on disk.
 * To avoid memory neverending memory consumption I have preferred to remove 
 * the implementations of the calls leaving only the callback 
 * (given that at the present this is the only one used) 
 * 
 * @author kzagar
 */
public abstract class ACSJMSConsumer implements MessageConsumer {
	private MessageListener listener;

	protected Destination destination;

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
		if (contServices==null) {
			throw new IllegalArgumentException("The ContainerServicesBase can't be null");
		}
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
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#receive(long)
	 */
	public Message receive(long timeout) throws JMSException {
		throw new UnsupportedOperationException();
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
		containerServices.getLogger().log(AcsLogLevel.DEBUG,"acs-jms message received");
		ACSJMSMessage jmsMessage = null;
		if (message.type.compareTo("com.cosylab.acs.jms.ACSJMSObjectMessage")==0){
			jmsMessage = new ACSJMSObjectMessage(message,containerServices);
		} else {
			jmsMessage = new ACSJMSTextMessage(message,containerServices);
		}
		if(this.listener == null) {
			return;
		} else if (selector.match(jmsMessage)) {
			this.listener.onMessage(jmsMessage);
		}
	}
}

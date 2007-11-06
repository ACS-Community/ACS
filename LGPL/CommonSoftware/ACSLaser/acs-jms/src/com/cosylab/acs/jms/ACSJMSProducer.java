/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageProducer;

import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public abstract class ACSJMSProducer implements MessageProducer {
	protected ContainerServicesBase containerServices;

	private long timeToLive;

	private int deliveryMode;

	protected Destination destination;

	/**
	 * @param arg0
	 */
	public ACSJMSProducer(Destination destination, ContainerServicesBase containerServices) throws JMSException {
		this.destination = destination;
		this.containerServices = containerServices;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#setDisableMessageID(boolean)
	 */
	public void setDisableMessageID(boolean arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#getDisableMessageID()
	 */
	public boolean getDisableMessageID() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#setDisableMessageTimestamp(boolean)
	 */
	public void setDisableMessageTimestamp(boolean arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#getDisableMessageTimestamp()
	 */
	public boolean getDisableMessageTimestamp() throws JMSException {
		throw new UnsupportedOperationException();
	}

    /**
     * 
     * @uml.property name="deliveryMode"
     */
    /* (non-Javadoc)
     * @see javax.jms.MessageProducer#setDeliveryMode(int)
     */
    public void setDeliveryMode(int deliveryMode) throws JMSException {
        this.deliveryMode = deliveryMode;
    }

    /**
     * 
     * @uml.property name="deliveryMode"
     */
    /* (non-Javadoc)
     * @see javax.jms.MessageProducer#getDeliveryMode()
     */
    public int getDeliveryMode() throws JMSException {
        return this.deliveryMode;
    }


	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#setPriority(int)
	 */
	public void setPriority(int arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#getPriority()
	 */
	public int getPriority() throws JMSException {
		throw new UnsupportedOperationException();
	}

    /**
     * 
     * @uml.property name="timeToLive"
     */
    /* (non-Javadoc)
     * @see javax.jms.MessageProducer#setTimeToLive(long)
     */
    public void setTimeToLive(long timeToLive) throws JMSException {
        this.timeToLive = timeToLive;
    }

    /**
     * 
     * @uml.property name="timeToLive"
     */
    /* (non-Javadoc)
     * @see javax.jms.MessageProducer#getTimeToLive()
     */
    public long getTimeToLive() throws JMSException {
        return this.timeToLive;
    }

    /**
     * 
     * @uml.property name="destination"
     */
    /* (non-Javadoc)
     * @see javax.jms.MessageProducer#getDestination()
     */
    public Destination getDestination() throws JMSException {
        return this.destination;
    }

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#send(javax.jms.Message, int, int, long)
	 */
	public void send(Message arg0, int arg1, int arg2, long arg3)
		throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#send(javax.jms.Destination, javax.jms.Message)
	 */
	public void send(Destination arg0, Message arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#send(javax.jms.Destination, javax.jms.Message, int, int, long)
	 */
	public void send(
		Destination arg0,
		Message arg1,
		int arg2,
		int arg3,
		long arg4)
		throws JMSException {
		throw new UnsupportedOperationException();
	}
}

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
/*
 * Created on Jun 26, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import java.util.Enumeration;

import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;

import org.omg.CosPropertyService.Property;
import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSMessage implements Message {

	private Destination destination;

	protected ACSJMSMessageEntity entity;
	
	private ContainerServicesBase containerServices;

	/**
	 * @param message
	 */
	public ACSJMSMessage(ACSJMSMessageEntity message, ContainerServicesBase cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		this.entity = message;
		containerServices=cs;
	}

	public ACSJMSMessage(ContainerServicesBase cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		this.entity = new ACSJMSMessageEntity();
		this.entity.properties = new Property[0];
		containerServices=cs;
	}
		
	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSMessageID()
	 */
	public String getJMSMessageID() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSMessageID(java.lang.String)
	 */
	public void setJMSMessageID(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSTimestamp()
	 */
	public long getJMSTimestamp() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSTimestamp(long)
	 */
	public void setJMSTimestamp(long arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSCorrelationIDAsBytes()
	 */
	public byte[] getJMSCorrelationIDAsBytes() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSCorrelationIDAsBytes(byte[])
	 */
	public void setJMSCorrelationIDAsBytes(byte[] arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSCorrelationID(java.lang.String)
	 */
	public void setJMSCorrelationID(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSCorrelationID()
	 */
	public String getJMSCorrelationID() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSReplyTo()
	 */
	public Destination getJMSReplyTo() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSReplyTo(javax.jms.Destination)
	 */
	public void setJMSReplyTo(Destination destination) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSDestination()
	 */
	public Destination getJMSDestination() throws JMSException {
		return this.destination;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSDestination(javax.jms.Destination)
	 */
	public void setJMSDestination(Destination destination) throws JMSException {
		this.destination = destination;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSDeliveryMode()
	 */
	public int getJMSDeliveryMode() throws JMSException {
		return this.entity.delivery_mode;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSDeliveryMode(int)
	 */
	public void setJMSDeliveryMode(int deliveryMode) throws JMSException {
		this.entity.delivery_mode = deliveryMode;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSRedelivered()
	 */
	public boolean getJMSRedelivered() throws JMSException {
		return this.entity.redelivered;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSRedelivered(boolean)
	 */
	public void setJMSRedelivered(boolean redelivered) throws JMSException {
		this.entity.redelivered = redelivered;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSType()
	 */
	public String getJMSType() throws JMSException {
		return this.entity.type;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSType(java.lang.String)
	 */
	public void setJMSType(String type) throws JMSException {
		this.entity.type = type;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSExpiration()
	 */
	public long getJMSExpiration() throws JMSException {
		return this.entity.expiration;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSExpiration(long)
	 */
	public void setJMSExpiration(long expiration) throws JMSException {
		this.entity.expiration = expiration;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getJMSPriority()
	 */
	public int getJMSPriority() throws JMSException {
		return this.entity.priority;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setJMSPriority(int)
	 */
	public void setJMSPriority(int priority) throws JMSException {
		this.entity.priority = priority;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#clearProperties()
	 */
	public void clearProperties() throws JMSException {
		this.entity.properties = new Property[0];
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#propertyExists(java.lang.String)
	 */
	public boolean propertyExists(String name) throws JMSException {
		return (findProperty(name, false) != -1);
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getBooleanProperty(java.lang.String)
	 */
	public boolean getBooleanProperty(String name) throws JMSException {
		int idx = findProperty(name, false);
		if(idx == -1) {
			throw new JMSException("Propery "+name+" not found");
		} else {
			boolean val = this.entity.properties[idx].property_value.extract_boolean();
			return val;
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getByteProperty(java.lang.String)
	 */
	public byte getByteProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getShortProperty(java.lang.String)
	 */
	public short getShortProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getIntProperty(java.lang.String)
	 */
	public int getIntProperty(String name) throws JMSException {
		int idx = findProperty(name, false);
		if(idx == -1) {
			throw new JMSException("Propery "+name+" mot found");
		} else {
			return this.entity.properties[idx].property_value.extract_long();
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getLongProperty(java.lang.String)
	 */
	public long getLongProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getFloatProperty(java.lang.String)
	 */
	public float getFloatProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getDoubleProperty(java.lang.String)
	 */
	public double getDoubleProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getStringProperty(java.lang.String)
	 */
	public String getStringProperty(String name) throws JMSException {
		int idx = findProperty(name, false);
		if(idx == -1) {
			return null;
		} else {
			return this.entity.properties[idx].property_value.extract_string();
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getObjectProperty(java.lang.String)
	 */
	public Object getObjectProperty(String arg0) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#getPropertyNames()
	 */
	public Enumeration getPropertyNames() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setBooleanProperty(java.lang.String, boolean)
	 */
	public void setBooleanProperty(String arg0, boolean arg1)
		throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setByteProperty(java.lang.String, byte)
	 */
	public void setByteProperty(String arg0, byte arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setShortProperty(java.lang.String, short)
	 */
	public void setShortProperty(String arg0, short arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setIntProperty(java.lang.String, int)
	 */
	public void setIntProperty(String arg0, int arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setLongProperty(java.lang.String, long)
	 */
	public void setLongProperty(String arg0, long arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setFloatProperty(java.lang.String, float)
	 */
	public void setFloatProperty(String arg0, float arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setDoubleProperty(java.lang.String, double)
	 */
	public void setDoubleProperty(String name, double value)
		throws JMSException {
		
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setStringProperty(java.lang.String, java.lang.String)
	 */
	public void setStringProperty(String name, String value)
		throws JMSException {
		int idx = findProperty(name, true);
		this.entity.properties[idx].property_value.insert_string(value);
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#setObjectProperty(java.lang.String, java.lang.Object)
	 */
	public void setObjectProperty(String name, Object value)
		throws JMSException {
		if (name==null) {
			throw new NullPointerException("Name is null!!!");
		}
		
		
		if (value==null) {
			return;
		}
		int idx = findProperty(name, true);
		if (value==null) {
			throw new NullPointerException("Value is null!!!");
		}
		
		// Probably not the best way of doing it.
		if (value instanceof Boolean)
		{
			this.entity.properties[idx].property_value.insert_boolean(((Boolean)value).booleanValue());
		}
		else if (value instanceof Integer)
		{
			this.entity.properties[idx].property_value.insert_long(((Integer)value).intValue());
		}
		else if (value instanceof String)
		{
			this.entity.properties[idx].property_value.insert_string((String)value);
		}
		else if (value instanceof Long)
		{
			this.entity.properties[idx].property_value.insert_longlong(((Long)value).longValue());
		}
		else if (value instanceof Short)
		{
			this.entity.properties[idx].property_value.insert_short(((Short)value).shortValue());
		}
		else if (value instanceof Byte)
		{
			this.entity.properties[idx].property_value.insert_octet(((Byte)value).byteValue());
		}
		else if (value instanceof Double)
		{
			this.entity.properties[idx].property_value.insert_double(((Double)value).doubleValue());
		}
		else if (value instanceof org.omg.CORBA.Object)
		{
			this.entity.properties[idx].property_value.insert_Object((org.omg.CORBA.Object)value);
		}
		else
		{
			throw new UnsupportedOperationException(); 
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#acknowledge()
	 */
	public void acknowledge() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see javax.jms.Message#clearBody()
	 */
	public void clearBody() throws JMSException {
		throw new UnsupportedOperationException();
	}

    /**
     * @return
     * 
     * @uml.property name="entity"
     */
    public ACSJMSMessageEntity getEntity() {
        return this.entity;
    }

	/**
	 * Find a message's property in the IDL structure by name, creating it if
	 * doesn't exist and if so specified.
	 * 
	 * @param name The name of the property to look for.
	 * @param create <code>true</code> if the property is to be created.
	 * @return The index of the property with the given name in the array.
	 *         -1 if the property was not found.
	 */
	private int findProperty(String name, boolean create) {
		// Find the property with the given name in the array.
		for(int i = 0; i < this.entity.properties.length; ++i) {
			if(name.equals(this.entity.properties[i].property_name)) {
				return i;
			}
		}
		
		// Not found, but we can create it.
		if(create) {
			Property[] newProperties = new Property[this.entity.properties.length+1];
			for(int i = 0; i < this.entity.properties.length; ++i) {
				newProperties[i] = this.entity.properties[i];
			}
			//newProperties[newProperties.length-1] = new Property(name, alma.acs.container.corba.AcsCorba.getAcsCorba().getORB().create_any());
			newProperties[newProperties.length-1] = new Property(name,containerServices.getAdvancedContainerServices().getAny());
			this.entity.properties = newProperties;
			return newProperties.length-1;
		}
		
		// Not found, but we can't create it.
		return -1;
	}
}

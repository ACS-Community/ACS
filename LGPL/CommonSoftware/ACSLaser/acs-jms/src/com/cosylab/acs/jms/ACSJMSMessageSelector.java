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
package com.cosylab.acs.jms;

import com.codestreet.selector.Selector;
import com.codestreet.selector.ISelector;
import com.codestreet.selector.parser.Identifier;
import com.codestreet.selector.parser.InvalidSelectorException;
import com.codestreet.selector.parser.IValueProvider;
import com.codestreet.selector.parser.Result;
import com.codestreet.selector.parser.NumericValue;

/**
 * This object is a message selector.
 * It owns an SQL92 string describing an SQL statement and checkes
 * if a message matches with that statement.
 * To enhance the response time of the process, it uses ValueProvider
 * method of selector.
 * 
 * @see JMS specifications for further details
 * 
 * @author acaproni
 *
 */
public class ACSJMSMessageSelector {
	
	/**
	 * The parser makes callbacks to the getValue method of
	 * this object whenever needs a value.
	 * I need to customize the value provider becase we need to check
	 * the content of the message. The package provides a ValueProvider
	 * for JMS messages that allows to use the dot notation to access
	 * the field of the message but I prefer this iplementation.
	 * 
	 * @author acaproni
	 *
	 */
	private class ACSJMSValueProvider implements IValueProvider {
		/**
		 * The message whose values are neeed by the parser
		 */
		ACSJMSMessage message;
		
		/**
		 * Constructor 
		 * @param jmsMessage The message whose values are neeed by the parser
		 */
		public ACSJMSValueProvider(ACSJMSMessage jmsMessage) {
			this.message=jmsMessage;
		}
		
		/**
		 * Return the value of a property.
		 * It looks for the property in the body of the message, not in the header.
		 * JMS specification states that the properties to check should only be 
		 * those of the header so we are not following the specs here (even if it
		 * is common use). 
		 * It should be better at least to check ALSO in the header properties
		 * 
		 *  NOTE: Properties are object of type org.jacorb.orb.Any containing a String.
		 *        This method tries to parse the String in order to return
		 *        an object of the right type (true and false for booleans)
		 * 
		 * @param identifier The identifier of the property (the key, it is a String)
		 * @param correlation The correlation object (not used)
		 * @return The value of the property or null if the property was not found
		 */
		public Object getValue(Object identifier, Object correlation) {
			if (!(identifier instanceof Identifier)) {
				throw new IllegalArgumentException("Wrong class "+identifier.getClass().getName()+" for identifier!");
			}
			String key = ((Identifier)identifier).getIdentifier().trim();
			if (key.length()==0) {
				throw new IllegalArgumentException("The identifier can't be empty!");
			}
			// Look for the key in the properties in the body of the message
			Object retVal = null;
			for(int i = 0; i < message.entity.properties.length; ++i) {
				if (message.entity.properties[i].property_name.trim().equals(key)) {
					retVal = message.entity.properties[i].property_value;
					break;
				}
			}
			if (retVal==null) {
				return null;
			} else {
				/*
				 * We have the value now we try to build an object of the
				 * right type before returning the value
				 * This is done trying to build a Numeric object with the Any
				 * object and returning it in case of success.
				 * If the conversion to a NumericValue failed then the object is 
				 * converted to a string an compared to true and false 
				 * (for booleans) or returned as String.
				 */
				
				
				// Integer
				try {
					NumericValue num = new NumericValue((Integer)retVal);
					return num;
				} catch (Exception e) {}
				// Long
				try {
					NumericValue num = new NumericValue((Long)retVal);
					return num;
				} catch (Exception e) {}
				// Float
				try {
					NumericValue num = new NumericValue((Float)retVal);
					return num;
				} catch (Exception e) {}
				// Double
				try {
					NumericValue num = new NumericValue((Double)retVal);
					return num;
				} catch (Exception e) {}
				// Boolean
				String val = retVal.toString().trim();
				if (val.compareToIgnoreCase("true")==0) {
					return true;
				}
				if (val.compareToIgnoreCase("false")==0) {
					return false;
				}
				// String
				return val;
			}
		}
	}
	
	/**
	 * The message selector string
	 * It is an SQL92 string as described in JMS specification
	 * All the messages that do not match with this statement
	 * are discarded, the others are sent to the listeners
	 * If the selector is null or empty it matches to true with all messages.
	 */
	private String selectorString;
	
	/**
	 * The selector object
	 * @see com.codestreet.selector.ISelector
	 */
	private ISelector selector = null;

	/**
	 * The constructor
	 * 
	 * @param sqlSelectorString The SQL selector string (it can be
	 *                          null or empty)
	 * 
	 * @throws InvalidSelectorException 
	 * @see com.codestreet.selector.parser.InvalidSelectorException
	 */
	public ACSJMSMessageSelector(String sqlSelectorString) throws InvalidSelectorException {
		setSelectorString(sqlSelectorString);
	}
	
	/**
	 * 
	 * @return The (eventually null or empty) SQL selector string 
	 * 
	 */
	public String getSelectorString() {
		return selectorString;
	}
	
	/**
	 * Set the new SQL selector string and create the instance of 
	 * selector. selector is set to null if the string is null or empty.
	 *
	 * 
	 * @param newSQLSelString The SQL92 selector string
	 * @throws InvalidSelectorException 
	 * 
	 * @see com.codestreet.selector.parser.InvalidSelectorException
	 */
	public void setSelectorString(String newSQLSelString) throws InvalidSelectorException {
		this.selectorString=newSQLSelString;
		if (selectorString==null) {
			selector = null;
		} else if (selectorString.trim().length()==0) {
			selector = null;
		} else {
			selector = Selector.getInstance(selectorString);
		}
	}
	
	/** 
	 * Check if the message matches with the SQL selector string.
	 * The test passes also if the string is empty or null.
	 * 
	 * @param message The message to check
	 * @return true if the message matches with the string
	 *         false otherwise
	 */
	public boolean match(ACSJMSMessage message) {
		if (selectorString==null) {
			return true;
		} else if (selectorString.length()==0) {
			return true;
		} else {
			ACSJMSValueProvider valueProvider = new ACSJMSValueProvider(message);
			Result result = selector.eval(valueProvider,null);
			return result==Result.RESULT_TRUE;
		}
	}

}

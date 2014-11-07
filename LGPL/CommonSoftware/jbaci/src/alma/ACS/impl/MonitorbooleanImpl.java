/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.ACS.impl;

import org.omg.CORBA.BooleanHolder;

import alma.ACS.CBDescIn;
import alma.ACS.Callback;
import alma.ACS.MonitorbooleanOperations;

/** 
 * Implementation of <code>alma.ACS.Monitorboolean</code>.
 * @author  acaproni
 * @since  2014.6
 */
public class MonitorbooleanImpl
	extends CommonComparableMonitorImpl
	implements MonitorbooleanOperations {

	/**
	 * @param property
	 * @param callback
	 * @param descIn
	 * @param startTime
	 */
	public MonitorbooleanImpl(
		CommonPropertyImpl property,
		Callback callback,
		CBDescIn descIn,
		long startTime) {
		super(property, callback, descIn, startTime);
	}

	/**
	 * @see alma.ACS.MonitorbooleanOperations#get_value_trigger(org.omg.CORBA.DoubleHolder, org.omg.CORBA.BooleanHolder)
	 */
	public void get_value_trigger(BooleanHolder deltaHolder, BooleanHolder enableHolder) {
		deltaHolder.value = ((Boolean)getValueTrigger(enableHolder)).booleanValue();
	}

	/**
	 * @see alma.ACS.MonitorbooleanOperations#set_value_trigger(double, boolean)
	 */
	public void set_value_trigger(boolean delta, boolean enable) {
		setValueTrigger(Boolean.valueOf(delta), enable);
	}

	/**
	 * @see alma.ACS.MonitorbooleanOperations#get_value_percent_trigger(org.omg.CORBA.DoubleHolder, org.omg.CORBA.BooleanHolder)
	 */
	public void get_value_percent_trigger(org.omg.CORBA.DoubleHolder deltaHolder, org.omg.CORBA.BooleanHolder enableHolder) {
	}

	/**
	 * @see alma.ACS.MonitorbooleanOperations#set_value_percent_trigger(org.omg.CORBA.DoubleHolder, boolean)
	 */
	public void set_value_percent_trigger(double delta, boolean enable) {
	}
}



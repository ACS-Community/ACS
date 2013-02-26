/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
package com.cosylab.logging.engine;

import java.util.Date;

import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The filter for the {@link Constraint} <code>EXACT</code> type.
 * 
 * @author acaproni
 * @since ACS 11.2
 */
public class ExactFilter extends Filter {
	
	/**
	 * The object to compare
	 */
	private final Object exact;
	
	/**
	 * Constructor for {@link Constraint} types <code>EXACT</code>.
	 * 
	 * @param field The field
	 * @param isLethal The activation state of the filter
	 * @param exact The value to comapre logs against 
	 * @param notFilter Usage of the filter (normal or not)
	 */
	public ExactFilter(LogField field, boolean isLethal, Comparable exact, boolean notFilter)
			throws InvalidFilterConstraintException {
		super(field, Constraint.EXACT, isLethal, notFilter);
		if (exact==null) {
			throw new InvalidFilterConstraintException("The value for comparison can't be null");
		}
		this.exact = checkAndConvertObjectType(field, exact);
	}

	protected boolean applyTo(Object obj) {
		if (obj == null) {
			return false;
		}
		
		// Temporary: Used to remember if the test passes
		// and apply the not policy (if requested)
		boolean res = false;

		res = exact.equals(obj);

		if (notFilter)
			return !res;
		else
			return res;
	}
	
	/**
	 * Build a description of the filter
	 * 
	 * @return The description of the filter
	 */
	public String toString() {
		StringBuffer type = new StringBuffer(field.getName());
		type.append(' ');
		if (notFilter) {
			type.append("NOT ");
		}

		type.append("Exact value = ");
		if (field == LogField.ENTRYTYPE) {
			type.append(((LogTypeHelper)exact).logEntryType);
		} else if (field == LogField.TIMESTAMP) {
			type.append(new Date((Long)exact).toString());
		} else 	{
			type.append(exact.toString());
		}
		return type.toString();
	}
	
	@Override
	protected void appendSpecializedXML(StringBuffer buffer) {
		buffer.append("\t\t<EXACT class=\"");
		buffer.append(field.fieldClass.getName()); 
		buffer.append("\">");
		if (exact.getClass().toString().substring(6).compareTo(
				"java.util.Date") == 0) {
			// If it is a Date the we save the date as a long
			java.util.Date date = (java.util.Date) (exact);
			buffer.append(date.getTime());
		} else {
			buffer.append(exact.toString());
		}
		buffer.append("</EXACT>\n");
	}
}

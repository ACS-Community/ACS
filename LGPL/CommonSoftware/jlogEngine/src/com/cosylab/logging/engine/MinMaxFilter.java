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
 * The filter for filter {@link Constraint} types <code>MINMAX</code>, <code>MINIMUM</code> and <code>MAXIMUM</code>MINMAX.
 * 
 * @author acaproni
 * @since ACS 11.2
 */
public class MinMaxFilter extends Filter {
	
	/**
	 * The minimum value.
	 * At least one betwen {@link #minimum} and {@link #maximum} must be not <code>null</code>.
	 */
	private final Comparable minimum;
	
	/**
	 * The maximum value
	 * At least one betwen {@link #minimum} and {@link #maximum} must be not <code>null</code>.
	 */
	private final Comparable maximum;
	
	/**
	 * Constructor for {@link Constraint} types <code>MINMAX</code>, <code>MINIMUM</code> and <code>MAXIMUM</code>.
	 * <P>
	 * This filter has type <code>MINMAX</code> unless one between either <code>minimum</code> 
	 * or <code>maximum</code> is <code>null</code>.
	 *
	 * @param field The field
	 * @param isLethal The activation state of the filter
	 * @param minimum The min value that can be assumed by logs that matches with this filter
	 * @param maximum The max value that can be assumed by logs that matches with this filter 
	 * @param notFilter Usage of the filter (normal or not)
	 */
	public MinMaxFilter(
			LogField field, 
			boolean isLethal, 
			Comparable minimum,
			Comparable maximum, 
			boolean notFilter) throws InvalidFilterConstraintException {
		super(field, Constraint.MINMAX, isLethal, notFilter);
		
		if ((minimum == null) && (maximum == null)) {
			throw new InvalidFilterConstraintException("No constraints specified");
		}
		this.minimum=checkAndConvertObjectType(field, minimum);
		this.maximum=checkAndConvertObjectType(field, maximum);
		
		if (this.minimum == null) {
			constraint = Constraint.MAXIMUM;
		}

		if (this.maximum == null) {
			constraint = Constraint.MINIMUM;
		}
		// Ensure that max>min if Constraint type is MINMAX
		if (constraint==Constraint.MINMAX) {
			if (this.maximum.compareTo(this.minimum)<0) {
				throw new InvalidFilterConstraintException("Invalid constraint max<min: "+maximum+"<"+minimum);
			}
		}
	}

	
	protected boolean applyTo(Object obj) {
		if (obj == null) {
			return false;
		}
		
		// Temporary: Used to remember if the test passes
		// and apply the not policy (if requested)
		boolean res = false;

		boolean minimumCondition = true;
		boolean maximumCondition = true;
		Comparable logField = (Comparable) (obj);
		if ((constraint == Constraint.MINIMUM) || (constraint == Constraint.MINMAX)) {
			minimumCondition = minimum.compareTo(logField) <= 0;
		}

		if ((constraint == Constraint.MAXIMUM) || (constraint == Constraint.MINMAX)) {
			maximumCondition = maximum.compareTo(logField) >= 0;
		}

		res = minimumCondition && maximumCondition;

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

		// The string is built by reading the regularExpression, max, min and
		// exact fields of the filter
		// If the filter is defined on the entry type 1 (i.e. debug, trace...)
		// then the number is replaced by the string describing the type itself
		switch (constraint) {
		case MINMAX:
			type.append("Mininum = ");
			if (field == LogField.ENTRYTYPE) {
				type.append(((LogTypeHelper)minimum).logEntryType);
			} else if (field == LogField.TIMESTAMP) {
				type.append(new Date((Long)minimum).toString());
			} else {
				type.append(minimum.toString());
			}
			type.append(", Maximum = ");
			if (field == LogField.ENTRYTYPE) {
				type.append(((LogTypeHelper)maximum).logEntryType);
			} else if (field == LogField.TIMESTAMP) {
				type.append(new Date((Long)maximum).toString());
			}else {
				type.append(maximum.toString());
			}
			break;
		case MINIMUM:
			type.append("Minimum = ");
			if (field == LogField.ENTRYTYPE) {
				type.append(LogTypeHelper.values()[(Integer.parseInt(minimum
						.toString()))].logEntryType);
			} else if (field == LogField.TIMESTAMP) {
				type.append(new Date((Long)minimum).toString());
			} 
				type.append(minimum.toString());
			break;
		case MAXIMUM:
			type.append("Maximum = ");
			if (field == LogField.ENTRYTYPE) {
				type.append(LogTypeHelper.values()[(Integer.parseInt(maximum
						.toString()))].logEntryType);
			} else if (field == LogField.TIMESTAMP) {
				type.append(new Date((Long)maximum).toString());
			} else {
				type.append(maximum.toString());
			}
			break;
		}

		return type.toString();
	}
	
	@Override
	protected void appendSpecializedXML(StringBuffer buffer) {
	
		if (minimum != null) {
			buffer.append("\t\t<MIN class=\"");
			buffer.append(field.fieldClass.getName()); 
			buffer.append("\">");
			if (minimum.getClass().toString().substring(6).compareTo(
					"java.util.Date") == 0) {
				// If it is a Date the we save the date as a long
				java.util.Date date = (java.util.Date) (minimum);
				buffer.append(date.getTime());
			} else {
				buffer.append(minimum.toString());
			}
			buffer.append("</MIN>\n");
		}
		if (maximum != null) {
			buffer.append("\t\t<MAX class=\"");
			buffer.append(field.fieldClass.getName()); 
			buffer.append("\">");
			if (maximum.getClass().toString().substring(6).compareTo(
					"java.util.Date") == 0) {
				// If it is a Date the we save the date as a long
				java.util.Date date = (java.util.Date) (maximum);
				buffer.append(date.getTime());
			} else {
				buffer.append(maximum.toString());
			}
			buffer.append("</MAX>\n");
		}
	}


	/**
	 * @return the minimum
	 */
	public Comparable getMinimum() {
		return minimum;
	}


	/**
	 * @return the maximum
	 */
	public Comparable getMaximum() {
		return maximum;
	}
}

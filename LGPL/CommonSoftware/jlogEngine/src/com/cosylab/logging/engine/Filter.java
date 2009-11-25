/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.engine;

import java.util.Date;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;


/**
 * A Filter is a class used for filtering LogEntries. It has the following characteristics:
 * <UL><LI>Filterable field: A LogEntryXML's attribute to be used as a filterable criteria.</LI>
 * <LI>Constraint type: A type of filter to apply, e.g. regular expression for Strings, lower bound for integers, etc</LI>
 * <LI>Constraints: Specific constraints to be used for this filter. Number and class of constraints depends on
 * Constraint type</LI>
 * <LI>isLethal: A boolean attribute that specifies when the filter is applied. If the filter is lethal, it is applied 
 * at the engine level - the logs that do not get through are discarded immediately and can never reach the data holders. 
 * If the filter is not lethal, the GUI determines whether the logs can be seen or not. The GUI should make a clear
 * distinction between the two types of filters.</LI>
 * <LI>notFilter: A boolean that the filter has to be used as a not specification 
 * (i.e. log entries pass if DO NOT match the filter)</LI>
 * </UL>
 */
public class Filter {
	
	/**
	 * The possible comparison types
	 * 
	 * @author acaproni
	 *
	 */
	public enum Constraint {
		MINIMUM,	
		MAXIMUM,
		MINMAX,
		EXACT,   
		STRING_WILDCHAR
	}
	
	// Filterable field
	public LogField field = null;
	// Constraint type
	public Constraint constraint = null;
	
	// Lethalicity
	private boolean isLethal = false;
	
	// The boolean that specifies how to use the filter (NOT policy)
	// The variable defaults to false (the filter is normal)
	private boolean notFilter = false;
	
	// Constraints
	public String regularExpression = null;

	public Comparable minimum = null;
	public Comparable maximum = null;
	public Object exact = null;

	/**
	 * Constructor
	 * 
	 * @param field The field
	 * @param constraint The constraint
	 * @param isLethal The activation state of the filter 
	 * @param notFilter Usage of the filter (normal or not)
	 */
	private Filter(LogField field, Constraint constraint, boolean isLethal,
			boolean notFilter) {
		this.field = field;
		this.constraint = constraint;
		this.isLethal = isLethal;
		this.notFilter = notFilter;
	}

	/**
	 * Constructor
	 */
	public Filter(LogField field, boolean isLethal, Comparable minimum,
			Comparable maximum, boolean notFilter)
			throws InvalidFilterConstraintException {
		this(field, Constraint.MINMAX, isLethal, notFilter);

		if ((minimum == null) && (maximum == null))
			throw new InvalidFilterConstraintException(
					"No constraint specified");

		if (minimum != null) {
			if (!(field.getType().isInstance(minimum))) {
				throw new InvalidFilterConstraintException("Invalid minimum");
			}
			this.minimum = minimum;
		} else {
			constraint = Constraint.MAXIMUM;
		}

		if (maximum != null) {
			if (!(field.getType().isInstance(maximum)))
				throw new InvalidFilterConstraintException("Invalid maximum");
			this.maximum = maximum;
		} else {
			constraint = Constraint.MINIMUM;
		}
	}

	/**
	 * 
	 */
	public Filter(LogField field, boolean isLethal, Integer exact,
			boolean notFilter) throws InvalidFilterConstraintException {
		this(field, Constraint.EXACT, isLethal, notFilter);

		this.exact = exact;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 2:46:21 PM)
	 * 
	 * @param field
	 *            int
	 * @param isLethal
	 *            boolean
	 * @param minimum
	 *            java.lang.Integer
	 * @param maximum
	 *            java.lang.Integer
	 */
	public Filter(LogField field, boolean isLethal, Integer minimum,
			Integer maximum, boolean notFilter)
			throws InvalidFilterConstraintException {
		this(field, Constraint.MINMAX, isLethal, notFilter);

		if ((minimum == null) && (maximum == null)) {
			throw new InvalidFilterConstraintException(
					"No constraint specified");
		}

		if (minimum != null) {
			this.minimum = minimum;
		} else {
			constraint = Constraint.MAXIMUM;
		}

		if (maximum != null) {
			this.maximum = maximum;
		} else {
			constraint = Constraint.MINIMUM;
		}
	}

	/**
	 * 
	 */
	public Filter(LogField field, boolean isLethal, Object exact, boolean notFilter)
			throws InvalidFilterConstraintException {
		this(field, Constraint.EXACT, isLethal, notFilter);

		if (field.getType() != exact.getClass())
			throw new InvalidFilterConstraintException("Invalid exact value: "
					+ exact);

		this.exact = exact;
	}

	/**
	 * Build a filter with a regular expression Check if the string is a valid
	 * regular expression
	 */
	public Filter(LogField field, boolean isLethal, String regularExpression,
			boolean notFilter) throws InvalidFilterConstraintException,
			PatternSyntaxException {
		this(field, Constraint.STRING_WILDCHAR, isLethal, notFilter);
		// System.out.println("short, boolean, String");
		// System.out.println(field+" "+isLethal+" "+regularExpression);

		if (!(field.getType().equals(String.class)))
			throw new InvalidFilterConstraintException(
					"Invalid regular expression: " + regularExpression);

		// Build a pattern to ensure if the regular expression is valid
		this.regularExpression = null;
		Pattern p = Pattern.compile(regularExpression);

		this.regularExpression = regularExpression;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 2:48:18 PM)
	 * 
	 * @param field
	 *            int
	 * @param isLethal
	 *            boolean
	 * @param minimum
	 *            java.lang.String
	 * @param maximum
	 *            java.lang.String
	 */
	public Filter(LogField field, boolean isLethal, String minimum,
			String maximum, boolean notFilter)
			throws InvalidFilterConstraintException {
		this(field, Constraint.MINMAX, isLethal, notFilter);
		// System.out.println("short, boolean, Comparable, Comparable");

		if ((minimum == null) && (maximum == null))
			throw new InvalidFilterConstraintException(
					"No constraint specified");

		if (minimum != null) {
			this.minimum = minimum;
		} else {
			constraint = Constraint.MAXIMUM;
		}

		if (maximum != null) {
			this.maximum = maximum;
		} else {
			constraint = Constraint.MINIMUM;
		}
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 2:47:35 PM)
	 * 
	 * @param field
	 *            int
	 * @param isLethal
	 *            boolean
	 * @param minimum
	 *            java.util.Date
	 * @param maximum
	 *            java.util.Date
	 */
	public Filter(LogField field, boolean isLethal, Date minimum, Date maximum,
			boolean notFilter) throws InvalidFilterConstraintException {
		this(field, Constraint.MINMAX, isLethal, notFilter);
		// System.out.println("short, boolean, Comparable, Comparable");

		if ((minimum == null) && (maximum == null))
			throw new InvalidFilterConstraintException(
					"No constraint specified");

		if (minimum != null) {
			this.minimum = minimum;
		} else {
			constraint = Constraint.MAXIMUM;
		}

		if (maximum != null) {
			this.maximum = maximum;
		} else {
			constraint = Constraint.MINIMUM;
		}
	}

	/**
	 * The most imporant method of this class. Returns true if LogEntryXML
	 * passes through the filter and false otherwise.
	 * 
	 * If this instance is a non-lethal filter and is called in lethal
	 * circumstances (at an engine level), this filter always returns true.
	 * 
	 * If this instance is a lethal filter and is called in non-lethal
	 * circumstances (at a GUI level), this filter always returns true.
	 */
	public boolean applyTo(ILogEntry logEntry, boolean lethalCircumstances) {

		if (lethalCircumstances != isLethal) {
			return true;
		}
		boolean ret=applyTo(logEntry.getField(field)); 
		return ret;
	}
	
	/**
	 * Apply the filter to the passed object.
	 * 
	 * @param obj The object to apply the filter to
	 * @return <code>true</code> if the object matches the filter
	 */
	public boolean applyTo(Object obj) {
		if (obj == null) {
			return false;
		}
		
		boolean minimumCondition = true;
		boolean maximumCondition = true;
		
		// The log type is converted to Integer
		if (field == LogField.ENTRYTYPE) {
			obj = Integer.valueOf(((LogTypeHelper) obj).ordinal());
		}

		// Temporary: Used to remember if the test passes
		// and apply the not policy (if requested)
		boolean res = false;

		if (constraint == Constraint.STRING_WILDCHAR) {
			// Here the regular expression should be well formed
			try {
				res = Pattern.matches(regularExpression,
						(String) obj);
			} catch (PatternSyntaxException exception) {
				// This is a problem! Ignore the filter returning true
				return true;
			}
		} else if (constraint == Constraint.EXACT) {
				res = exact.equals(obj);
		} else {
			Comparable logField = (Comparable) (obj);
			if ((constraint == Constraint.MINIMUM) || (constraint == Constraint.MINMAX)) {
				minimumCondition = minimum.compareTo(logField) <= 0;
			}

			if ((constraint == Constraint.MAXIMUM) || (constraint == Constraint.MINMAX)) {
				maximumCondition = maximum.compareTo(logField) >= 0;
			}

			res = minimumCondition && maximumCondition;
		}

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
		StringBuffer type = new StringBuffer();

		// The string is built by reading the regularExpression, max, min and
		// exact fields of the filter
		// If the filter is defined on the entry type 1 (i.e. debug, trace...)
		// then the number is replaced by the string describing the type itself
		switch (constraint) {
		case MINMAX:
			type.append("Mininum = ");
			if (field == LogField.ENTRYTYPE)
				type.append(LogTypeHelper.values()[(Integer.parseInt(minimum
						.toString()))].logEntryType);
			else
				type.append(minimum.toString());
			type.append(", Maximum = ");
			if (field == LogField.ENTRYTYPE)
				type.append(LogTypeHelper.values()[(Integer.parseInt(maximum
						.toString()))].logEntryType);
			else
				type.append(maximum.toString());
			break;
		case MINIMUM:
			type.append("Minimum = ");
			if (field == LogField.ENTRYTYPE)
				type.append(LogTypeHelper.values()[(Integer.parseInt(minimum
						.toString()))].logEntryType);
			else
				type.append(minimum.toString());
			break;
		case MAXIMUM:
			type.append("Maximum = ");
			if (field == LogField.ENTRYTYPE)
				type.append(LogTypeHelper.values()[(Integer.parseInt(maximum
						.toString()))].logEntryType);
			else
				type.append(maximum.toString());
			break;
		case STRING_WILDCHAR:
			type.append("Regular exp. mask = " + regularExpression);
			break;
		case EXACT:
			type.append("Exact value = ");
			if (field == LogField.ENTRYTYPE)
				type.append(LogTypeHelper.values()[(Integer.parseInt(exact
						.toString()))].logEntryType);
			else
				type.append(exact.toString());
			break;
		default:
			type.append("Undeclared");
			break;
		}

		type.insert(0, field.getName() + ", ");
		if (notFilter)
			type.insert(0, "NOT ");
		return type.toString();
	}

	/**
	 * Build an XML representation of the filter
	 * 
	 * @return The XML representing the filter
	 */
	public String toXMLString() {
		StringBuffer buffer = new StringBuffer("\t<FILTER type=\"");
		if (constraint!=null) {
			buffer.append(constraint.name());
		} else {
			buffer.append("UNDEFINED");
		}
		
		buffer.append("\">\n");
		// LogField
		buffer.append("\t\t<FIELD>" + field.getName() + "</FIELD>\n");
		// IsLethal
		buffer.append("\t\t<LETHAL>");
		if (isLethal) {
			buffer.append(1);
		} else {
			buffer.append(0);
		}
		buffer.append("\t\t</LETHAL>\n");
		// ApplyAsNot
		buffer.append("\t\t<APPLYNOT>");
		if (notFilter) {
			buffer.append(1);
		} else {
			buffer.append(0);
		}
		buffer.append("\t\t</APPLYNOT>\n");
		// The wildchar
		if (regularExpression != null) {
			buffer.append("\t\t<WILDCHAR>" + regularExpression + "</WILDCHAR>");
		}
		if (minimum != null) {
			buffer.append("\t\t<MIN class=\""
					+ minimum.getClass().toString().substring(6) + "\">");
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
			buffer.append("\t\t<MAX class=\""
					+ maximum.getClass().toString().substring(6) + "\">");
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
		if (exact != null) {
			buffer.append("\t\t<EXACT class=\""
					+ exact.getClass().toString().substring(6) + "\">");
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
		buffer.append("\t</FILTER>\n");
		return buffer.toString();
	}

	/**
	 * Return the NOT policy
	 * 
	 * @return True if the filter is used with not policy (i.e. The log entries
	 *         that pass are those who do NOT satify the constraints)
	 */
	public boolean notPolicyApplyed() {
		return this.notFilter;
	}

	/**
	 * 
	 * @return The value of IsLethal
	 */
	public boolean getIsLethal() {
		return this.isLethal;
	}

	/**
	 * Build a Filter object All the parameters are String objects. Before
	 * building the object, the value of each parameter is checked This method
	 * is too long (and boring) for my taste but it is very easy
	 * 
	 * @param field
	 *            The filed parameter fo Filter
	 * @param lethal
	 *            The isLethal parameter of Filter
	 * @param not
	 *            The applyAsNOT parameter of Filter
	 * @param min
	 *            The minimum parameter of Filter
	 * @param minType
	 *            The type of minimum
	 * @param max
	 *            The max parameter of Filter
	 * @param maxType
	 *            The type of max
	 * @param exact
	 *            The exact parameter of Filter
	 * @param exactType
	 *            The type of exact
	 * @param wildChar
	 *            The regularExpression parameter of Filter
	 * 
	 * @return The Filter object built or null if an error occurred decoding the
	 *         parameters
	 * @throws Exception
	 *             in case of error building the filter
	 */
	public static Filter buildFilter(LogField field, String lethal, String not,
			String min, String minType, String max, String maxType,
			String exact, String exactType, String wildChar) throws Exception {
		Filter f = null;
		// Trim all the strings
		if (lethal != null) {
			lethal = lethal.trim();
		}
		if (not != null) {
			not = not.trim();
		}
		if (min != null) {
			min = min.trim();
		}
		if (minType != null) {
			minType = minType.trim();
		}
		if (max != null) {
			max = max.trim();
		}
		if (maxType != null) {
			maxType = maxType.trim();
		}
		if (exact != null) {
			exact = exact.trim();
		}
		if (exactType != null) {
			exactType = exactType.trim();
		}
		if (wildChar != null) {
			wildChar = wildChar.trim();
		}
		// Read the int from field
		int fieldInt;
		if (field == null) {
			throw new IllegalArgumentException("Parameter field can't be null");
		}
		// Translate lethal into boolean
		int temp;
		if (lethal == null) {
			throw new IllegalArgumentException("Parameter lethal can't be null");
		}
		temp = Integer.parseInt(lethal);
		boolean isLethal = (temp == 1);
		// Translate not into boolean
		if (not == null) {
			throw new IllegalArgumentException("Parameter not can't be null");
		}
		temp = Integer.parseInt(not);
		boolean notPolicy = (temp == 1);
		// If wildChar is defined then min, max and exact should not
		if (wildChar != null && (min != null || max != null || exact != null)) {
			throw new IllegalArgumentException(
					"Ambiguous parameters: wildChar, min, max, exact");
		}
		// If wild char is not defined then at least one between min, max and
		// exat must be not null
		if (wildChar == null && min == null && max == null && exact == null) {
			throw new IllegalArgumentException(
					"Ambiguous null params: wildChar, min, max, exact");
		}
		// If exact is defined then min and max should not
		if (exact != null && (min != null || max != null)) {
			throw new IllegalArgumentException(
					"Ambiguous parameters: exact, min, max");
		}
		// For min, max and exact the type must be specified
		if (exact != null && exactType == null) {
			throw new IllegalArgumentException("Exact parameter can't be null");
		}
		if (min != null && minType == null) {
			throw new IllegalArgumentException("Min - minType parameters wrong");
		}
		if (max != null && maxType == null) {
			throw new IllegalArgumentException("Max - maxType parameters wrong");
		}
		// If both min and max are specified they must have the same type
		if (minType != null && maxType != null) {
			if (minType.compareTo(maxType) != 0) {
				throw new IllegalArgumentException("minType- maxType mismatch");
			}
		}
		//
		// Build the filter
		//

		// WILDCHAR
		if (wildChar != null) {
			f = new Filter(field, isLethal, wildChar, notPolicy);
			return f;
		}
		// EXACT
		if (exact != null) {
			if (exactType.compareTo("java.lang.Integer") == 0) {
				Integer integer;
				try {
					integer = new Integer(exact);
				} catch (NumberFormatException e) {
					throw new IllegalArgumentException("Wrong int parameter "
							+ exact);
				}
				f = new Filter(field, isLethal, integer, notPolicy);
			} else if (exactType.compareTo("java.util.Date") == 0) {
				Date date = null;
				try {
					date = new Date(Long.parseLong(exact));
				} catch (NumberFormatException e) {
					throw new IllegalArgumentException("Wrong date parameter "
							+ exact);
				}
				f = new Filter(field, isLethal, date, notPolicy);
			} else if (exactType.compareTo("java.lang.String") == 0) {
				f = new Filter(field, isLethal, (Object) exact, notPolicy);
			} else {
				// Unrecognized type
				throw new IllegalArgumentException("Unrecognized type "
						+ exactType);
			}
			return f;
		}
		// MINMAX (it implements MINIMUM and MAXIMUM)
		if (minType == null) {
			minType = maxType;
		} else {
			maxType = minType;
		}
		if (minType.compareTo("java.lang.String") == 0) {
			f = new Filter(field, isLethal, min, max, notPolicy);
		} else if (minType.compareTo("java.util.Date") == 0) {
			Date minDate = null;
			Date maxDate = null;
			try {
				if (min != null) {
					minDate = new Date(Long.parseLong(min));
				}
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Wrong min date parameter "
						+ min);
			}
			try {
				if (max != null) {
					maxDate = new Date(Long.parseLong(max));
				}
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Wrong max date parameter "
						+ max);
			}
			f = new Filter(field, isLethal, minDate, maxDate, notPolicy);
		} else if (minType.compareTo("java.lang.Integer") == 0) {
			Integer minInt = null;
			Integer maxInt = null;
			try {
				if (min != null) {
					minInt = new Integer(min);
				}
				if (max != null) {
					maxInt = new Integer(max);
				}
			} catch (NumberFormatException e) {
				throw new IllegalArgumentException("Invalid min/max " + min
						+ "/" + max);
			}
			f = new Filter(field, isLethal, minInt, maxInt, notPolicy);
		} else {
			// Unrecognized type
			throw new IllegalArgumentException("Unrecognized type");
		}
		return f;
	}

	/**
	 * Return the type of the filter
	 * 
	 * @return The type of the filter
	 */
	public LogField getField() {
		return field;
	}

}
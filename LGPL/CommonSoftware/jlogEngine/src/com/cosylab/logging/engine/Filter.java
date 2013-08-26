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

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;


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
 * <P>
 * The constraint must match they type of the field to match as defined in {@link LogField#fieldClass}.
 * <code>Filter</code> allows to accepts different value types for the following cases ({@link #checkAndConvertObjectType(LogField, Object)}):
 * <UL>
 * 	<LI><code>ENTRYTYPE</code>: can be a {@link LogTypeHelper} as well as a {@link Integer} (the latter is used internally for comparison)
 *  <LI><code>TIMESTAMP</code>: can be a {@link Date} as well as a {@link Long} (the latter is used internally for comparison)
 * </UL>
 */
public abstract class Filter {
	
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
		STRING_WILDCHAR;
		
		/**
		 * Build a Constraint from its name as returned by {@link #name()}.
		 * 
		 * @param name The name of the constraint
		 * @return The Constraint with the passed name
		 * @throws InvalidFilterConstraintException If the passed name does not represent any Constraint
		 */
		public static Constraint fromName(String name) throws InvalidFilterConstraintException {
			if (name==null || name.isEmpty()) {
				throw new IllegalArgumentException("Invalid name to build a Constraint");
			}
			for (Constraint c: Constraint.values()) {
				if (c.name().equalsIgnoreCase(name.trim())) {
					return c;
				}
			}
			throw new InvalidFilterConstraintException("No Constraint with name "+name);
		}
	}
	
	/**
	 *  Filterable field
	 */
	protected final LogField field;
	
	/**
	 *  Constraint type
	 */
	protected Constraint constraint;
	
	/**
	 *  Lethalicity
	 */
	protected final boolean isLethal;
	
	/**
	 * The boolean that specifies how to use the filter (NOT policy)
	 * The variable defaults to false (the filter is normal)
	 */
	protected final boolean notFilter;
	

	/**
	 * Constructor.
	 * 
	 * @param field The field
	 * @param constraint The constraint
	 * @param isLethal The activation state of the filter 
	 * @param notFilter Usage of the filter (normal or not)
	 */
	protected Filter(
			LogField field, 
			Constraint constraint, 
			boolean isLethal,
			boolean notFilter) throws InvalidFilterConstraintException	{
		if (field==null) {
			throw new InvalidFilterConstraintException("No log field specified in filter");
		}
		if (constraint==null) {
			throw new InvalidFilterConstraintException("No constraint specified in filter");
		}
		this.field = field;
		this.constraint = constraint;
		this.isLethal = isLethal;
		this.notFilter = notFilter;
	}

	
	
	
	/**
	 * Sometimes for optimization, it is possible to have a file of one class
	 * and an object of another type.
	 * For example for the timestamp it is possible to pass a {@link Date} or a {@link Long} and both
	 * should work upon conversion.
	 * 
	 * @param field The filed used by this Filter for comparison
	 * @param obj The object to compare; can be <code>null</code>
	 * @return The object of the converted type
	 * @throws InvalidFilterConstraintException In case of mismatch
	 */
	protected Comparable checkAndConvertObjectType(LogField field, Comparable obj) throws InvalidFilterConstraintException {
		if (field==null) {
			throw new IllegalArgumentException("The passed LogFiled can't be null");
		}
		if (obj==null) {
			return null;
		}

		switch (field) {
		case ENTRYTYPE: {
			if (obj instanceof LogTypeHelper) {
				return obj;
			} else if (obj instanceof Integer) {
				LogTypeHelper ret;
				try { 
				ret=LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger((Integer)obj));
				} catch (AcsJIllegalArgumentEx ex) {
					throw new InvalidFilterConstraintException("Invalid definition for LogEntryType: "+obj);
				}
				return ret;
			} else {
				throw new InvalidFilterConstraintException("Invalid class for ENTRYTYPE object "+obj.toString()+": "+obj.getClass().getName());
			}
		}
		case TIMESTAMP: {
			if (obj instanceof Date) {
				return Long.valueOf(((Date)obj).getTime());
			} else if (obj instanceof Long) {
				return obj;
			} else {
				throw new InvalidFilterConstraintException("Invalid class for TIMESTAMP object "+obj.toString()+": "+obj.getClass().getName());
			}
		}
		default: {
			return obj;
		}
		}
	}
	
	/**
	 * The most important method of this class. Returns true if LogEntryXML
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
	abstract protected boolean applyTo(Object obj); 
	
	/**
	 * Append to the buffer, the XML for the specific type of filter.
	 * 
	 * @param buffer The buffer to append data into.
	 */
	protected abstract void appendSpecializedXML(StringBuffer buffer);

	/**
	 * Build an XML representation of the filter.
	 * <P>
	 * The initial part of the XML of each filter is common to all the types of filters.
	 * The part depending on the specialized filter is added by {@link #appendSpecializedXML(StringBuffer)}.
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
		appendSpecializedXML(buffer);
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
	 * @param type The type of filter to build
	 * @param field The filed parameter fo Filter
	 * @param lethal The isLethal parameter of Filter
	 * @param not The applyAsNOT parameter of Filter
	 * @param min The minimum parameter of Filter
	 * @param minType The type of minimum
	 * @param max The max parameter of Filter
	 * @param maxType The type of max
	 * @param exact The exact parameter of Filter
	 * @param exactType The type of exact
	 * @param wildChar The regularExpression parameter of Filter
	 * @return The Filter object built or null if an error occurred decoding the
	 *         parameters
	 * @throws Exception in case of error building the filter
	 */
	public static Filter buildFilter(Constraint type, LogField field, String lethal, String not,
			String min, String minType, String max, String maxType,
			String exact, String exactType, String wildChar) throws Exception {
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
		// Finally build the filter
		//
		if (type==Constraint.STRING_WILDCHAR) {
			return new RegExpFilter(field, isLethal, wildChar, notPolicy);
		} else if (type==Constraint.EXACT) {
			if (exact!=null && !exact.isEmpty() && exactType!=null && !exactType.isEmpty()) {
				if (exactType.equals(Integer.class.getName())) {
					Integer integer;
					try {
						integer = new Integer(exact);
					} catch (NumberFormatException e) {
						throw new IllegalArgumentException("Wrong int parameter "
								+ exact);
					}
					return new ExactFilter(field, isLethal, integer, notPolicy);
				} else if (exactType.equals(Long.class.getName())) {
					Long date = null;
					try {
						date = Long.decode(exact);
					} catch (NumberFormatException e) {
						throw new IllegalArgumentException("Wrong date parameter "+ exact);
					}
					return new ExactFilter(field, isLethal, date, notPolicy);
				} else if (exactType.equals(String.class.getName())) {
					return new ExactFilter(field, isLethal, exact, notPolicy);
				} else if (exactType.equals(LogTypeHelper.class.getName())) {
					LogTypeHelper logType=LogTypeHelper.fromLogTypeDescription(exact);
					return new ExactFilter(field, isLethal, logType, notPolicy);
				} else {
					// Unrecognized type
					throw new IllegalArgumentException("Unrecognized type "+exactType);
				}
			} else {
				throw new IllegalArgumentException("Value and type can't be null/empty to build a ExactFilter");
			}
		} else if (type==Constraint.MINMAX || type==Constraint.MINIMUM || type==Constraint.MAXIMUM) {
			if (minType == null) {
				minType = maxType;
			} else {
				maxType = minType;
			}
			if (minType.equals(String.class.getName())) {
				return new MinMaxFilter(field, isLethal, min, max, notPolicy);
			} else if (minType.equals(Long.class.getName())) {
				Long minDate = null;
				Long maxDate = null;
				try {
					if (min != null) {
						minDate = Long.decode(min);
					}
				} catch (NumberFormatException e) {
					throw new IllegalArgumentException("Wrong min date parameter "
							+ min);
				}
				try {
					if (max != null) {
						maxDate = Long.decode(max);
					}
				} catch (NumberFormatException e) {
					throw new IllegalArgumentException("Wrong max date parameter "
							+ max);
				}
				return new MinMaxFilter(field, isLethal, minDate, maxDate, notPolicy);
			} else if (minType.equals(Integer.class.getName())) {
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
				return new MinMaxFilter(field, isLethal, minInt, maxInt, notPolicy);	
			} else if (minType.equals(LogTypeHelper.class.getName())) {
				LogTypeHelper minLogType = null;
				LogTypeHelper maxLogType = null;
				try {
					if (min != null) {
						minLogType= LogTypeHelper.fromLogTypeDescription(min);
					}
					if (max != null) {
						maxLogType= LogTypeHelper.fromLogTypeDescription(max);
					}
				} catch (NumberFormatException e) {
					throw new IllegalArgumentException("Invalid min/max " + min
							+ "/" + max);
				}
				return new MinMaxFilter(field, isLethal, minLogType, maxLogType, notPolicy);	
			} else {
				throw new IllegalArgumentException("Min/Max values/type can't be null/empty to build a MinMaxFilter");
			}
		} else {
			throw new Exception("Error building a filter");
		}
	}

	/**
	 * Return the type of the filter
	 * 
	 * @return The type of the filter
	 */
	public LogField getField() {
		return field;
	}

	/**
	 * @return the constraint
	 */
	public Constraint getConstraint() {
		return constraint;
	}

}
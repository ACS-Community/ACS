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

import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.regex.*;
import javax.swing.JOptionPane;

import com.cosylab.logging.LogTypeHelper;

/**
 * A Filter is a class used for filtering LogEntries. It has the following characteristics:
 * <UL><LI>Filterable field: A LogEntry's attribute to be used as a filterable criteria.</LI>
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
	
	// Comparison option

	public static final int MINIMUM = 1;	
	public static final int MAXIMUM = 2;
	public static final int MINMAX = 3;
	public static final int EXACT = 0;   
	public static final int STRING_WILDCHAR = 4; 
	
	public static final int UNDECLARED = -1;

	// Filterable field
	public int field = UNDECLARED;
	// Constraint type
	public int constraint = UNDECLARED;
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
private Filter(int field, int constraint, boolean isLethal,boolean notFilter) {
	this.field = field;
	this.constraint = constraint;
	this.isLethal = isLethal;
	this.notFilter=notFilter;
}
/**
 * Constructor
 */
public Filter(
		int field, 
		boolean isLethal,
		Comparable minimum,
		Comparable maximum,
		boolean notFilter
		) throws InvalidFilterConstraintException {
	this(field, MINMAX, isLethal,notFilter);
	
	if ((minimum == null) && (maximum == null)) throw new InvalidFilterConstraintException("No constraint specified");
	 
	if (minimum != null) {
	    if (!(LogEntry.getFieldClass(field).isInstance(minimum))) {
			throw new InvalidFilterConstraintException("Invalid minimum");
	    }
	    this.minimum = minimum;
	} else {
		constraint = MAXIMUM;
	}
	
	if (maximum != null) {
	    if (!(LogEntry.getFieldClass(field).isInstance(maximum)))
 	       throw new InvalidFilterConstraintException("Invalid maximum");
	    this.maximum = maximum;
	} else {
		constraint = MINIMUM;
	}
}
/**
 * 
 */
public Filter(int field, boolean isLethal, Integer exact,boolean notFilter) throws InvalidFilterConstraintException {
	this(field, EXACT, isLethal,notFilter);

	this.exact = exact;
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 2:46:21 PM)
 * @param field int
 * @param isLethal boolean
 * @param minimum java.lang.Integer
 * @param maximum java.lang.Integer
 */
public Filter(int field, boolean isLethal, Integer minimum, Integer maximum, boolean notFilter)
	throws InvalidFilterConstraintException {
	this(field, MINMAX, isLethal,notFilter);
    
	if ((minimum == null) && (maximum == null)) {
		throw new InvalidFilterConstraintException("No constraint specified");
    }

	if (minimum != null) {
		this.minimum = minimum;
	} else {
		constraint = MAXIMUM;
	}

	if (maximum != null) {
		this.maximum = maximum;
	} else {
		constraint = MINIMUM;
	}
}
/**
 * 
 */
public Filter(int field, boolean isLethal, Object exact, boolean notFilter) throws InvalidFilterConstraintException {
	this(field, EXACT, isLethal,notFilter);

	if (LogEntry.getFieldClass(field) != exact.getClass())
 		throw new InvalidFilterConstraintException("Invalid exact value: "+exact);
	 
	this.exact = exact;
}
/**
 * Build a filter with a regular expression
 * Check if the string is a valid regular expression
 */
public Filter(int field, boolean isLethal, String regularExpression, boolean notFilter) 
	throws InvalidFilterConstraintException,  PatternSyntaxException
{
	this(field, STRING_WILDCHAR, isLethal,notFilter);
//	System.out.println("short, boolean, String");
//	System.out.println(field+" "+isLethal+" "+regularExpression);
	
	if (!(LogEntry.getFieldClass(field).equals(String.class)))
 		throw new InvalidFilterConstraintException("Invalid regular expression: "+regularExpression);
 	
	// Build a pattern to ensure if the regular expression is valid
	this.regularExpression=null;
	Pattern p = Pattern.compile(regularExpression);
	
	this.regularExpression = regularExpression;
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 2:48:18 PM)
 * @param field int
 * @param isLethal boolean
 * @param minimum java.lang.String
 * @param maximum java.lang.String
 */
public Filter(int field, boolean isLethal, String minimum, String maximum, boolean notFilter)
	throws InvalidFilterConstraintException {
	this(field, MINMAX, isLethal,notFilter);
	//	System.out.println("short, boolean, Comparable, Comparable");

	if ((minimum == null) && (maximum == null))
		throw new InvalidFilterConstraintException("No constraint specified");

	if (minimum != null) {
		this.minimum = minimum;
	} else {
		constraint = MAXIMUM;
	}

	if (maximum != null) {
		this.maximum = maximum;
	} else {
		constraint = MINIMUM;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 2:47:35 PM)
 * @param field int
 * @param isLethal boolean
 * @param minimum java.util.Date
 * @param maximum java.util.Date
 */
public Filter(int field, boolean isLethal, Date minimum, Date maximum, boolean notFilter)
	throws InvalidFilterConstraintException {
	this(field, MINMAX, isLethal,notFilter);
	//	System.out.println("short, boolean, Comparable, Comparable");

	if ((minimum == null) && (maximum == null))
		throw new InvalidFilterConstraintException("No constraint specified");

	if (minimum != null) {
		this.minimum = minimum;
	} else {
		constraint = MAXIMUM;
	}

	if (maximum != null) {
		this.maximum = maximum;
	} else {
		constraint = MINIMUM;
	}
}

/**
 * The most imporant method of this class. Returns true
 * if LogEntry passes through the filter and false
 * otherwise.
 *
 * If this instance is a non-lethal filter and is called in lethal circumstances
 * (at an engine level), this filter always returns true.
 *
 * If this instance is a lethal filter and is called in non-lethal circumstances
 * (at a GUI level), this filter always returns true.
 */
public boolean applyTo(LogEntry logEntry, boolean lethalCircumstances) {
	
	if (lethalCircumstances != isLethal) return true;

	boolean minimumCondition = true;
	boolean maximumCondition = true;
	boolean exactCondition = true;
	
	Object filterableField = logEntry.getField(field);
	if (filterableField == null) return false;
	
	// Temporary: Used to remember if the test passes
	// and apply the not policy (if requested)
	boolean res=false;

	if (constraint == STRING_WILDCHAR) {
		// Here the regular expression should be well formatted
		try {
			res = Pattern.matches(regularExpression,(String)filterableField);
		} catch (PatternSyntaxException exception) {
			// This is a problem! I ignore the filter returning true 
			return true;
		}
	} else if (constraint == EXACT) {
		res = exact.equals(logEntry.getField(field));
	} else {
		Comparable logField = (Comparable)(filterableField);
		if ((constraint == MINIMUM) || (constraint == MINMAX)) {
			minimumCondition = minimum.compareTo(logField) <= 0;
		}

		if ((constraint == MAXIMUM) || (constraint == MINMAX)) {
			maximumCondition = maximum.compareTo(logField) >= 0;
		}

		res = minimumCondition && maximumCondition;
	}
	
	if (notFilter) return !res;
	else return res;
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
	switch(constraint) {
		case UNDECLARED : type.append("Undeclared"); break;
		case MINMAX : 
			type.append("Mininum = ");
			if (field==1) type.append(LogTypeHelper.getLogTypeDescription(Integer.parseInt(minimum.toString())));
			else type.append(minimum.toString());
			type.append(", Maximum = ");
			if (field==1) type.append(LogTypeHelper.getLogTypeDescription(Integer.parseInt(maximum.toString())));
			else type.append(maximum.toString());
			break;
		case MINIMUM : 
			type.append("Minimum = ");
			if (field==1) type.append(LogTypeHelper.getLogTypeDescription(Integer.parseInt(minimum.toString())));
			else type.append(minimum.toString()); 
			break;
		case MAXIMUM : 
			type.append("Maximum = ");
			if (field==1) type.append(LogTypeHelper.getLogTypeDescription(Integer.parseInt(maximum.toString())));
			else type.append(maximum.toString());
			break;
		case STRING_WILDCHAR : 
			type.append("Regular exp. mask = "+regularExpression); 
			break;
		case EXACT : 
			type.append("Exact value = ");
			if (field==1) type.append(LogTypeHelper.getLogTypeDescription(Integer.parseInt(exact.toString())));
			else type.append(exact.toString()); 
			break;
	}
	
	type.insert(0,LogEntry.getFieldDescription(field)+", ");
	if (notFilter) type.insert(0,"NOT ");
	return type.toString();
}

/**
 * Write a filter in the stream in XML format
 * 
 * @param outF The output stream
 * @param f The filter to write in the stream
 */
public String toXMLString() 
{
	StringBuffer buffer = new StringBuffer("\t<FILTER type=\"");
	switch (constraint) {
		case 1: {
			buffer.append("MINIMUM");
			break;
		}
		case 2: {
			buffer.append("MAXIMUM");
			break;
		}
		case 3: {
			buffer.append("MINMAX");
			break;
		}
		case 4: {
			buffer.append("STRINGWCHAR");
			break;
		}
		case 0: {
			buffer.append("EXACT");
			break;
		}
		default: {
			buffer.append("UNDEFINED");
			break;
		}
	}
	buffer.append("\">\n");
	// Field
	buffer.append("\t\t<FIELD>"+field+"</FIELD>\n");
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
	if (regularExpression!=null) {
		buffer.append("\t\t<WILDCHAR>"+regularExpression+"</WILDCHAR>");
	}
	if (minimum!=null) {
		buffer.append("\t\t<MIN class=\""+minimum.getClass().toString().substring(6)+"\">");
		if (minimum.getClass().toString().substring(6).compareTo("java.util.Date")==0) {
			// If it is a Date the we save the date as a long
			java.util.Date date=(java.util.Date)(minimum);
			buffer.append(date.getTime());
		} else {
			buffer.append(minimum.toString());
		}
		buffer.append("</MIN>\n");
	}
	if (maximum!=null) {
		buffer.append("\t\t<MAX class=\""+maximum.getClass().toString().substring(6)+"\">");
		if (maximum.getClass().toString().substring(6).compareTo("java.util.Date")==0) {
			// If it is a Date the we save the date as a long
			java.util.Date date=(java.util.Date)(maximum);
			buffer.append(date.getTime());
		} else {
			buffer.append(maximum.toString());
		}
		buffer.append("</MAX>\n");
	}
	if (exact!=null) {
		buffer.append("\t\t<EXACT class=\""+exact.getClass().toString().substring(6)+"\">");
		if (exact.getClass().toString().substring(6).compareTo("java.util.Date")==0) {
			// If it is a Date the we save the date as a long
			java.util.Date date=(java.util.Date)(exact);
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
 * that pass are those who do NOT satify the constraints)
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
 * Build a Filter object
 * All the parameters are String objects. 
 * Before building the object, the value of each parameter is checked
 * This method is too long (and boring) for my taste but it is very easy
 * 
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
 * 
 * @return The Filter object built or null if an error occurred decoding the parameters
 */
public static Filter buildFilter(
		String field,
		String lethal, 
		String not, 
		String min,
		String minType,
		String max,
		String maxType,
		String exact,
		String exactType,
		String wildChar) {
	Filter f = null;
	// Trim all the strings
	if (field!=null) {
		field=field.trim();
	}
	if (lethal!=null) {
		lethal = lethal.trim();
	}
	if (not!=null) {
		not=not.trim();
	}
	if (min!=null) {
		min=min.trim();
	}
	if (minType!=null) {
		minType=minType.trim();
	}
	if (max!=null) {
		max=max.trim();
	}
	if (maxType!=null) {
		maxType=maxType.trim();
	}
	if (exact!=null) {
		exact=exact.trim();
	}
	if (exactType!=null) {
		exactType=exactType.trim();
	}
	if (wildChar!=null) {
		wildChar=wildChar.trim();
	}
	// Read the int from field
	int fieldInt;
	if (field==null) {
		JOptionPane.showMessageDialog(null,"Missing parameter field","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	} 
	try {
		fieldInt=Integer.parseInt(field);
	} catch (Exception e) {
		JOptionPane.showMessageDialog(null,"Wrong parameter "+field,"Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	// Translate lethal into boolean
	int temp;
	if (lethal==null) {
		JOptionPane.showMessageDialog(null,"Missing parameter lethal","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	try {
		temp = Integer.parseInt(lethal);
	} catch (Exception e) {
		JOptionPane.showMessageDialog(null,"Wrong parameter "+lethal,"Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	boolean isLethal = (temp==1);
	// Translate not into boolean
	if (not==null) {
		JOptionPane.showMessageDialog(null,"Missing parameter not","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	try {
		temp = Integer.parseInt(not);
	} catch (Exception e) {
		JOptionPane.showMessageDialog(null,"Wrong parameter ","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	boolean notPolicy = (temp==1);
	// If wildChar is defined then min, max and exact should not
	if (wildChar!=null && (min!=null || max!=null || exact!=null)) {
		JOptionPane.showMessageDialog(null,"Ambiguous parameters","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	// If exact is defined then min and max should not
	if (exact!=null && (min!=null || max!=null)) {
		JOptionPane.showMessageDialog(null,"Ambiguous parameters","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	// For min, max and exact the type must be specified
	if (exact!=null && exactType==null) {
		JOptionPane.showMessageDialog(null,"Exact type not defined","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	if (min!=null && minType==null) {
		JOptionPane.showMessageDialog(null,"Min not defined","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	if (max!=null && maxType==null) {
		JOptionPane.showMessageDialog(null,"Max type not defined","Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	// If both min and max are specified they must have the same type
	if (minType!=null && maxType!=null) {
		if (minType.compareTo(maxType)!=0) {
			JOptionPane.showMessageDialog(null,"Type mismatch","Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
	}
	//
	// Build the filter
	//
	
	// WILDCHAR
	if (wildChar!=null) {
		try {
			f = new Filter(fieldInt,isLethal,wildChar,notPolicy);
		} catch(InvalidFilterConstraintException e) {
			JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		} catch(PatternSyntaxException e) {
			JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
		return f;
	}
	//EXACT
	if (exact!=null) {
		if (exactType.compareTo("java.lang.Integer")==0) {
			Integer integer;
			try {
				integer = new Integer(exact);
			} catch (NumberFormatException e) {
				JOptionPane.showMessageDialog(null,"Wrong int parameter "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
				return null;
			}
			try {
				f = new Filter(fieldInt,isLethal,integer,notPolicy);
			} catch(InvalidFilterConstraintException e) {
				JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
				return null;
			} 
		} else if (exactType.compareTo("java.util.Date")==0) {
			Date date=null;
			try {
				date = new Date(Long.parseLong(exact));
			} catch (NumberFormatException e) {
				JOptionPane.showMessageDialog(null,"Wrong date parameter "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
				return null;
			}
			try {
				f = new Filter(fieldInt,isLethal,date,notPolicy);
			} catch (InvalidFilterConstraintException e) {
				JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
				return null;
			}
		} else if (exactType.compareTo("java.lang.String")==0) {
			try {
				f = new Filter(fieldInt,isLethal,(Object)exact,notPolicy);
			} catch(InvalidFilterConstraintException e) {
				JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
				return null;
			}
		} else {
			// Unrecognized type
			JOptionPane.showMessageDialog(null,"Unrecognized type "+exactType,"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
		return f;
	}
	// MINMAX (it implements MINIMUM and MAXIMUM)
	// We use minType
	if (minType==null) {
		minType=maxType;
	} 
	if (minType.compareTo("java.lang.String")==0) {
		try {
			f = new Filter(fieldInt,isLethal,min,max,notPolicy);
		} catch(InvalidFilterConstraintException e) {
			JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
	} else if (minType.compareTo("java.util.Date")==0) {
		Date minDate=null;
		Date maxDate=null;
		try {
			if (min!=null) {
				minDate = new Date(Long.parseLong(min));
			}
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(null,"Wrong date parameter "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
		try {
			if (max!=null) {
				maxDate = new Date(Long.parseLong(max));
			}
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(null,"Wrong date parameter "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
		try {
			f = new Filter(fieldInt,isLethal,minDate,maxDate,notPolicy);
		} catch(InvalidFilterConstraintException e) {
			JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
	} else if (minType.compareTo("java.lang.Integer")==0) {
		Integer minInt=null;
		Integer maxInt=null;
		try {
			if (min!=null) {
				minInt=new Integer(min);
			}
			if (max!=null) {
				maxInt=new Integer(max);
			}
		} catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(null,"Wrong int parameter "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
		try {
			f=new Filter(fieldInt,isLethal,minInt,maxInt,notPolicy);
		} catch(InvalidFilterConstraintException e) {
			JOptionPane.showMessageDialog(null,"Exception "+e.getMessage(),"Error",JOptionPane.ERROR_MESSAGE);
			return null;
		}
	} else {
		// Unrecognized type
		JOptionPane.showMessageDialog(null,"Unrecognized type "+exactType,"Error",JOptionPane.ERROR_MESSAGE);
		return null;
	}
	return f;
}

/**
 * Return the type of the filter
 * 
 * @return The type of the filter
 */
public int getField() {
    return field;
}

}
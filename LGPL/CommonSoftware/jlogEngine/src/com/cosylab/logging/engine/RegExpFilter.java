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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import com.cosylab.logging.engine.log.LogField;

/**
 * The filter for the {@link Constraint} <code>REGEXP</code> type.
 * @author acaproni
 * @since ACS 11.2
 */
public class RegExpFilter extends Filter {
	
	/**
	 * The string representing the regular expression to filter against.
	 */
	private final String regularExpression;
	
	/**
	 * The pattern generated from the {@link #regularExpression} 
	 *  and used to check the entry. 
	 */
	private final Pattern pattern;
	
	
	/**
	 * Build a filter with a regular expression i.e. {@link Constraint} types <code>STRING_WILDCHAR</code>.
	 * It checks if the string is a valid regular expression.
	 * 
	 * @param field The field
	 * @param isLethal The activation state of the filter
	 * @param regularExpression The regular expression to match against the log
	 * @param notFilter Usage of the filter (normal or not)
	 */
	public RegExpFilter(LogField field, boolean isLethal, String regularExpression,
			boolean notFilter) throws InvalidFilterConstraintException,
			PatternSyntaxException {
		super(field, Constraint.STRING_WILDCHAR, isLethal, notFilter);

		if (!(field.getType().equals(String.class))) {
			throw new InvalidFilterConstraintException("Regular expressions can filter only Strings:field "+field+" type is "+field.fieldClass.getName());
		}
		if (regularExpression==null||regularExpression.isEmpty()) {
			throw new IllegalArgumentException("The regular expression can't be null nor empty");
		}
		this.regularExpression = regularExpression;
		this.pattern = Pattern.compile(regularExpression, getPatternFlags());
	}
	
	/**
	 * Special-hack subclasses may overwrite this method to return values
	 * such as <code>Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE</code>.
	 * If this functionality should be needed more generally, then we should 
	 * allow setting the flags in the constructor. 
	 */
	protected int getPatternFlags() {
		return 0;
	}

	protected boolean applyTo(Object obj) {
		if (obj == null) {
			return false;
		}
		
		// Temporary: Used to remember if the test passes
		// and apply the not policy (if requested)
		boolean res = false;

		try {
			Matcher m = pattern.matcher((String)obj);
			res = m.matches();
		} 
		catch (PatternSyntaxException exception) {
			// This is a problem! Ignore the filter returning true
			return true;
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
		StringBuffer type = new StringBuffer(field.getName());
		type.append(' ');
		if (notFilter) {
			type.append("NOT ");
		}

		type.append("Regular exp. mask = " + regularExpression);

		return type.toString();
	}
	
	@Override
	protected void appendSpecializedXML(StringBuffer buffer) {
		buffer.append("\t\t<WILDCHAR>");
		buffer.append(regularExpression);
		buffer.append("</WILDCHAR>\n");
	}

	/**
	 * @return the regularExpression
	 */
	public String getRegularExpression() {
		return regularExpression;
	}
	
}

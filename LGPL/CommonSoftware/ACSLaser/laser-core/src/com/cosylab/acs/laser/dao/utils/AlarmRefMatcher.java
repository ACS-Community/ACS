/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
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
package com.cosylab.acs.laser.dao.utils;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.Triplet;

import com.cosylab.util.WildcharMatcher;

/**
 * <p>Utility class to check if an alarm matches an alarm reference specification.
 * Given a fault family, a fault member, and a fault code specification (that is,
 * an alarm reference), it is able to decide if a given {@link Alarm} matches
 * with that definition. Alarm references are present, for example, under the
 * <code>parent</code> and <code>child</code> elements of each <code>reduction-link</code>,
 * which represents a Reduction Rule on the Alarm System.
 *
 * <p>Alarm references might use either wildcards or regular expressions to specify
 * several alarms at a time. The <code>alma.acs.alarms.refpattern</code> property
 * controls whether the strings stored in the CDB should be interpreted as wildcards
 * (like <code>'ab*cd??d'</code>) or as Java regular expressions (like<code>'^ab.*cd..d$'</code>).
 * It can take the following values: <code>wildcard</code> or <code>regexp</code> values
 *
 * <p>This class was originally embedded in the ACSAlarmDAOImpl class. Most of the
 * following commented code was thought for handling also code ranges. This was
 * designed to be specified as a String, but since the current
 * schema for the RRs defines the "fault-code" field in the "alarm-definition"
 * node as "xs:int", the received value is always an integer, which then was
 * being String.valueOf()-ed to get it as String and pass it to this class constructor.
 *
 * @author acaproni, rtobar
 * @TODO: only compile patterns when adequate,
 *        check http://almasw.hq.eso.org/almasw/bin/view/Main/RodrigoTobarDailyWorklog#08_02_11
 *        for details
 */
public class AlarmRefMatcher
{

	/**
	 * Reduction rules might use either wildcards or regular expressions to specify several reduction rules at a time.
	 * The <code>alma.acs.alarms.reductionrulepattern</code> property controls whether the strings stored in the CDB
	 * should be interpreted as wildcards (like <code>'ab*cd??d'</code>), or as Java regular expressions (like
	 * <code>'^ab.*cd..d$'</code>). It can take the following values: <code>wildcard</code> or <code>regexp</code> values.
	 */
	private final String  _reduction_rule_patter = System.getProperty("alma.acs.alarms.reductionrulepattern", "wildcard");

	private final Pattern _familyPattern;
	private final Pattern _memberPattern;
	private final int _code;
	
	private final String matcherAlarmID;
	/*public final int minCode, maxCode;*/

	public AlarmRefMatcher(String familySpec, String memberSpec, int code, boolean interpretStringsAsPatterns) throws IllegalArgumentException
	{
		matcherAlarmID=familySpec+":"+memberSpec+":"+code;
		// TODO: only compile patterns when adequate, check:
		// http://almasw.hq.eso.org/almasw/bin/view/Main/RodrigoTobarDailyWorklog#08_02_11
		// for details

		if (familySpec==null || memberSpec==null /*|| code==null*/)
			throw new IllegalArgumentException();

		if( _reduction_rule_patter.equals("wildcard") ) {
			_familyPattern = Pattern.compile(WildcharMatcher.simpleWildcardToRegex(familySpec));
			_memberPattern = Pattern.compile(WildcharMatcher.simpleWildcardToRegex(memberSpec));
		}
		else {
			_familyPattern = Pattern.compile(familySpec);
			_memberPattern = Pattern.compile(memberSpec);
		}
		_code   = code;

//		try {
//			int minus=codeSpec.indexOf('-');
//			if (codeSpec.lastIndexOf('-')!=minus)
//				throw new IllegalArgumentException("Only a single - allowed in code spec");
//			if (minus<0) {
//				minCode=maxCode=Integer.parseInt(codeSpec);
//			} else {
//				if (minus==0) {
//					minCode=Integer.MIN_VALUE;
//					maxCode=Integer.parseInt(codeSpec.substring(1));
//				} else
//					if (minus==codeSpec.length()-1) {
//						minCode=Integer.parseInt(codeSpec.substring(0, codeSpec.length()-1));
//						maxCode=Integer.MAX_VALUE;
//					} else {
//						minCode=Integer.parseInt(codeSpec.substring(0, minus));
//						maxCode=Integer.parseInt(codeSpec.substring(minus+1));
//					}
//			}
//		} catch (NumberFormatException e) {
//			throw new IllegalArgumentException("Invalid code spec");
//		}
	}

	public boolean isMatch(Alarm a)
	{
		if (a==null)
			throw new IllegalArgumentException();

		// Checks in order of speed, so failure happens ASAP
		Triplet t=a.getTriplet();
		
		int code=t.getFaultCode().intValue();
		boolean ret;
		if ( code != _code ||
		    !_familyPattern.matcher(t.getFaultFamily()).matches() ||
		    !_memberPattern.matcher(t.getFaultMember()).matches() ) {
			ret= false;
		} else {
			ret= true;
		}
		
		//System.out.println("\t\t\tMatch "+a.getAlarmId()+" with ["+_familyPattern.toString()+","+_memberPattern.toString()+", "+_code+"] retuned "+ret);
		return ret;
	}
	
	/**
	 * Return the ID of the alarm. Note that this
	 * ID could contains wildcards and regular expression.
	 * <P>
	 * Parent alarms in reduction rules do not contain regular
	 * expression neither wildcards. 
	 *  
	 * @return The ID of the alarm
	 */
	public String getMatcherAlarmID() {
		return matcherAlarmID;
	}
	
	@Override
	public String toString() {
		StringBuilder ret = new StringBuilder("AlarmRefMatcher [FF=");
		ret.append(_familyPattern.toString());
		ret.append(", ");
		ret.append(_memberPattern.toString());
		ret.append(", ");
		ret.append(_code);
		ret.append(']');
		return ret.toString();
	}

}
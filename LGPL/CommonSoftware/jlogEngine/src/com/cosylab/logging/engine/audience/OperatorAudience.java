/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2010
 *    Copyright by ESO
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
package com.cosylab.logging.engine.audience;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The audience for the operators: this accepts all the logs having
 * the audience set to operator or a level greater then WARNING.
 * <P>
 * <I>Note</I>: an instance of this class should be acquired from
 * 				{@link AudienceInfo}
 *  
 * @author acaproni
 * @since ACS 8.1.0
 */
public class OperatorAudience implements Audience {
	
	/**
	 * The audience as defined in the IDL
	 */
	private static final String audience= alma.log_audience.OPERATOR.value;
	
	/**
	 * The Audienceinfo for this audience
	 */
	private static final AudienceInfo info=AudienceInfo.OPERATOR;
	
	/**
	 * @see Audience
	 */
	@Override
	public AudienceInfo getInfo() {
		return OperatorAudience.info;
	}

	/**
	 * @see Audience
	 */
	@Override
	public boolean matches(ILogEntry log){
		String logAudience=(String)log.getField(LogField.AUDIENCE);
                if (audience.equalsIgnoreCase(logAudience) || alma.log_audience.SCILOG.value.equalsIgnoreCase(logAudience)) {
                        return true;
                }
                if (log.getType().ordinal()>=LogTypeHelper.WARNING.ordinal() && !alma.log_audience.DEVELOPER.value.equalsIgnoreCase(logAudience)) {
                        return true;
                }
                return false;
        }
}

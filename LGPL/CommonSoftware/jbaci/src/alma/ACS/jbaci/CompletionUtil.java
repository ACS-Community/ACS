/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
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

package alma.ACS.jbaci;

import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErr.ErrorTrace;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.UTCUtility;

/**
 * <code>Completion</code> utilities.
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public final class CompletionUtil {

	/**
	 * Empty <code>ErrorTrace[]</code>, used not to recreate empty array over and over...
	 */
	public static final ErrorTrace[] EMPTY_ERRORTRACE = new ErrorTrace[0];
	
	/**
	 * Generate no-error completion with current time.
	 * @return	no-error completion.
	 */
	public static Completion generateNoErrorCompletion()
	{
		return generateNoErrorCompletion(UTCUtility.utcJavaToOmg(System.currentTimeMillis()));
	}

	/**
	 * Generate no-error completion with given time.
	 * @return	no-error completion.
	 */
	public static Completion generateNoErrorCompletion(long timestamp)
	{
		// TODO consider using object pool
		return new Completion(timestamp, 0, 0, EMPTY_ERRORTRACE);
	}
	
	/**
	 * Generate completion from ACS exception.
	 * @return	completion generated from ACS exception.
	 */
	public static Completion generateCompletion(AcsJException acsex)
	{
		ErrorTrace errorTrace = acsex.getErrorTrace();
		// TODO consider using object pool
		return new Completion(UTCUtility.utcJavaToOmg(errorTrace.timeStamp), 
							  errorTrace.errorType,
							  errorTrace.errorCode,
							  new ErrorTrace[] { errorTrace });
	}


	/**
	 * Expeditiously "creates" a <code>CompletionHolder</code> instance.
	 * @return <code>CompletionHolder</code> instance.
	 */
	// TODO implement object pool
	public static CompletionHolder createCompletionHolder()
	{
		// .value filed MUST to be null
		return new CompletionHolder();
	}

	/**
	 * Expeditiously "clones" a <code>Completion</code> instance.
	 * @param original	<code>Completion</code> instance to be cloned.
	 * @return clone of the given <code>Completion</code> instance.
	 */
	public static Completion cloneCompletion(Completion original)
	{
		// TODO use object pool
		Completion dolly = new Completion(original.timeStamp, original.type, original.code, original.previousError);
		return dolly; 
	}

}

/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
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
package alma.acs.exceptions;

import alma.ACSErr.Completion;
import alma.ACSErrTypeTest.wrappers.AcsJACSErrTest0Ex;


/**
 * Note for generator implementation: here it's assumed that 
 * AcsJACSErrTest0Ex is the exception that corresponds to this completion (type, code).
 * 
 */
public class ExampleWithErrAcsJCompletion extends AcsJCompletion
{
	/**
	 * Creates a new <code>ExampleWithErrAcsJCompletion</code>
	 * with a corresponding exception (AcsJACSErrTest0Ex) attached.
	 */
	public ExampleWithErrAcsJCompletion() {
		super(new AcsJACSErrTest0Ex()); 
	}

	/**
	 * Creates a new <code>ExampleWithErrAcsJCompletion</code>
	 * with a corresponding exception (AcsJACSErrTest0Ex) attached 
	 * that wraps an existing exception (<code>acsJEx</code>.)
	 */
	public ExampleWithErrAcsJCompletion(AcsJException acsJEx) {
		super(new AcsJACSErrTest0Ex(acsJEx));  
	}
	
	/**
	 * Creates a new <code>ExampleWithErrAcsJCompletion</code>
	 * from another <code>AcsJCompletion</code> (<code>acsJComp</code>).
	 * <p>
	 * If present, the existing error trace is converted to Java exceptions
	 * and wrapped with an <code>AcsJACSErrTest0Ex</code>.
	 */
	public ExampleWithErrAcsJCompletion(AcsJCompletion acsJComp) {		
//		if (acsJComp.isError())
//		{
			init(new AcsJACSErrTest0Ex(acsJComp.getAcsJException()));
//		}
//		else
//		{
//			init(5, 11); 
//		}
	}
	
	/**
	 * Converts a Corba completion to an <code>ExampleWithErrAcsJCompletion</code>
	 * such that a new <code>AcsJACSErrTest0Ex</code> is created as the attached error.
	 * If <code>corbaComp</code> carries error information, these <code>ErrorTrace</code>
	 * objects are converted to Java exceptions, which are attached as the course of
	 * the new <code>AcsJACSErrTest0Ex</code>.
	 * @param corbaComp
	 */
	public ExampleWithErrAcsJCompletion(Completion corbaComp) {
		
		 this(AcsJCompletion.fromCorbaCompletion(corbaComp));
	}
	
}

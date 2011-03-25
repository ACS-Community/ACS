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

import java.util.Properties;

import alma.ACSErr.Completion;
import alma.ACSErr.ErrorTrace;
import alma.acs.util.UTCUtility;

/**
 * Java class that facilitates working with the interoperable {@link Completion} that's 
 * generated from CORBA IDL.
 * <p>
 * A <code>Completion</code> transmits the result of a remote method invocation
 * to the client. It contains an error type and code as defined in the ACS framework,
 * as well as an optional <code>ErrorTrace</code>. 
 * Note that for asynchronous calls, it is not possible to transmit thrown exceptions
 * to the client directly; here the <code>Completion</code> fills the gap.
 * <p>
 * Similar to <code>AcsJException</code>, <code>AcsJCompletion</code> converts CORBA <code>struct</code>
 * data into its nicer Java representation upon construction. When an <code>AcsJCompletion</code>
 * must be transported further on over CORBA, the method {@link #toCorbaCompletion} will create
 * a corresponding <code>Completion</code> with correct <code>ErrorTrace</code> structs attached.
 * <p>   
 * For any combination of error type/code, the ACS framework generates subclasses
 * of <code>AcsJCompletion</code>, which ensure usage of the correct type and code.
 * <ul>
 *   <li> To create a completion object from scratch, use the appropriate subclass; 
 *   <li> To create a completion from the base type <code>AcsJException</code>,
 *        which may be useful inside a catch block, use {@link AcsJException#toAcsJCompletion()}.
 *   <li> To convert an existing CORBA <code>Completion</code> to an <code>AcsJCompletion</code>,
 *        use the static method {@link #fromCorbaCompletion}.
 * </ul>
 * 
 * @author hsommer
 * created Jan 20, 2004 3:11:34 PM
 */
public class AcsJCompletion
{
	private int m_type;
	private int m_code;
	private long m_timestamp;
	
	/** optional exception; if present, type and code of completion and exception must be the same */
	private AcsJException m_jex;

	// additional name-value pairs
	protected Properties m_properties;

	
	protected AcsJCompletion()
	{
	}
	
	
	/**
	 * To be called from ctors of generated subclasses that can't have exceptions attached.
	 * @param type
	 * @param code
	 */
	protected AcsJCompletion(int type, int code)
	{
		init(type, code);	
	}
	
	/**
	 * To be called from ctors of generated subclasses that have exceptions attached.
	 * 
	 * @param acsJEx  
	 */
	protected AcsJCompletion(AcsJException acsJEx)
	{
		init(acsJEx);
	}

	
	protected void init(int type, int code)
	{
		m_type = type;
		m_code = code;
		m_timestamp = System.currentTimeMillis();
		m_jex = null;			
	}

	/**
	 * Initializes this <code>AcsJCompletion</code> object with the data of 
	 * an <code>AcsJException</code> (chain).
	 * <p>
	 * Type, code, and timestamp are taken from <code>acsJEx</code>.
	 * 
	 * @param acsJEx
	 * @throws NullPointerException if <code>acsJEx</code> is <code>null</code>.
	 */
	protected void init(AcsJException acsJEx)
	{
		if (acsJEx != null)
		{
			m_type = acsJEx.getErrorType();
			m_code = acsJEx.getErrorCode();
			m_timestamp = acsJEx.getTimestampMillis();
			m_jex = acsJEx;
		}
		else
		{
			throw new NullPointerException("argument 'acsJEx' must not be null!");
		}
	}
	
	
	
	/**
	 * Returns the completion type.
	 */
	public int getType() {
		return m_type;
	}

	/**
	 * Returns the completion code.
	 */
	public int getCode() {
		return m_code;
	}

	/**
	 * Returns the timeStamp.
	 */
	public long getTimeStamp() {
		return m_timestamp;
	}

	/**
	 * True if this completion represents an error condition.
	 * The error information can be obtained from {@link #getAcsJException}.
	 */
	public boolean isError()
	{
		return (m_jex != null );
	}
	
	
	/**
	 * 
	 * @return  an exception (chain) corresponding to the <code>ErrorTrace</code>,
	 * 			or <code>null</code> if the completion does not correspond to an exception.
	 * @see #isError
	 */
	public AcsJException getAcsJException()
	{
		return m_jex;
	}
	
	
	/**
	 * Creates a CORBA style completion object from this Java style completion.
	 * If present, the attached exceptions are converted to <code>ErrorTrace</code>s.
	 * <p>
	 * This method should be used when a Java implementation (of a component etc)
	 * has to send an <code>AcsJCompletion</code> over Corba.
	 * @see #fromCorbaCompletion
	 */
	public Completion toCorbaCompletion()
	{
		Completion corbaCompl = new Completion();
		
		corbaCompl.type = getType();
		corbaCompl.code = getCode();
		corbaCompl.timeStamp = UTCUtility.utcJavaToOmg(getTimeStamp());
		
		if (isError())
		{
			corbaCompl.previousError = new ErrorTrace[1];
			corbaCompl.previousError[0] = getAcsJException().getErrorTrace();
		}
		else
		{
			// create empty array so that CORBA can transport the struct w/o NPE
			corbaCompl.previousError = new ErrorTrace[0];
		}
		
		return corbaCompl;
	}

	/**
	 * Factory method to create an <code>AcsJCompletion</code> from an existing CORBA completion.
	 * Note that the new <code>AcsJCompletion</code> is a direct translation, not a wrapper.
	 * <p>
	 * If <code>corbaCompletion</code> has error information attached,
	 * this will be converted, and type/code of the top-exception will 
	 * have precedence over type/code stored redundantly in <code>corbaCompletion</code>.
	 * <p> 
	 * To be used on the client side of a remote call.
	 * @param completion
	 */
	public static AcsJCompletion fromCorbaCompletion(Completion corbaCompletion) 
	{
		if (corbaCompletion == null)
			throw new NullPointerException("argument 'completion' must not be null!");
		
		AcsJCompletion jcompletion = new AcsJCompletion();
		
		// does completion have error info attached? If so, convert from CORBA structs to Java exc.
		if (corbaCompletion.previousError != null && corbaCompletion.previousError.length > 0)
		{
			AcsJException jex = null;
			ErrorTrace et = corbaCompletion.previousError[0];
			Throwable thr = CorbaExceptionConverter.recursiveGetThrowable(et);
			
			if (! (thr instanceof AcsJException))
			{
				// just in case... should never happen, as CorbaExceptionConverter.recursiveGetThrowable
				// already substitutes non-AcsJ exceptions with DefaultAcsJException...			
				jex = new DefaultAcsJException(et);
			}
			else
			{
				jex = (AcsJException) thr;
			}					
			jcompletion.init(jex);
		}
		else
		{
			jcompletion.init(corbaCompletion.type, corbaCompletion.code);
		}			 
		
		return jcompletion;
	}

	
	/**
	 * Allows extra information to be attached to the completion, but only if this completion represents an error.
	 * @return     the previous value of the specified key in this property
	 *             list, or <code>null</code> if it did not have one.
	 * @throws IllegalStateException if this completion has no exceptions attached (i.e. it is an "ok-completion"). 
	 *         TODO: modify ACS error system to allow properties also for ok-completions.
	 */
	public Object setProperty(String key, String value) {
		if (isError()) {
			return getAcsJException().setProperty(key, value);
		}
		else {
			throw new IllegalStateException("Failed to set property '" + key + "' because properties are not supported for ok-completion classes.");
		}
	}

	/**
	 * @see #setProperty
	 */
	public String getProperty(String key) {
		if (isError()) {
			return getAcsJException().getProperty(key);
		}
		else {
			throw new IllegalStateException("Failed to get property '" + key + "' because properties are not supported for ok-completion classes.");
		}
	}

}

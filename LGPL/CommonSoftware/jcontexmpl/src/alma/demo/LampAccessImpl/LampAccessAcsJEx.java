/*
 * Created on Jun 25, 2003
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package alma.demo.LampAccessImpl;
import org.omg.CORBA.UserException;

import alma.ACSErr.JContExmplErrTypeTest;
import alma.ACSErr.ErrorTrace;
import alma.acs.exceptions.AcsJException;

/**
 * TODO: remove this class as soon as the acserr souce code generator 
 * can generate it. For the time being we don't worry that it's not fully 
 * compliant with the new acserr requirements (deprecated ACSException, ...)
 * 
 * @author admin
 *
 */
public class LampAccessAcsJEx extends AcsJException
{
	/**
	 * 
	 */
	public LampAccessAcsJEx ()
	{
		super();
	}

	/**
	 * @param message
	 */
	public LampAccessAcsJEx(String message)
	{
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public LampAccessAcsJEx(String message, Throwable cause)
	{
		super(message, cause);
	}

	/**
	 * @param et
	 */
	public LampAccessAcsJEx(ErrorTrace et)
	{
		super(et);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param message
	 * @param etCause
	 */
	public LampAccessAcsJEx(String message, ErrorTrace etCause)
	{
		super(message, etCause);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param cause
	 */
	public LampAccessAcsJEx (Throwable cause)
	{
		super(cause);
	}

	/**
	 * @see alma.acs.exceptions.AcsJException#toCorbaException()
	 */
	public UserException toCorbaException()
	{
		return getACSException();
	}

	/* (non-Javadoc)
	 * @see alma.exceptions.AcsJException#getErrorType()
	 */
	protected int getErrorType()
	{
		return JContExmplErrTypeTest.value;
	}

	/* (non-Javadoc)
	 * @see alma.exceptions.AcsJException#getErrorCode()
	 */
	protected int getErrorCode()
	{
		return 0;
	}

}

/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBdouble;
import alma.ACS.CBdoubleSeq;
import alma.ACS.CBlong;
import alma.ACS.CBlongSeq;
import alma.ACS.CBstring;
import alma.ACS.CBstringSeq;
import alma.ACSErr.Completion;
import alma.acs.callbacks.Objects.MyAcsJCompletion;
import alma.acs.callbacks.Objects.MyAcsJException;
import alma.acs.exceptions.AcsJException;



/**
 * 
 * @author mschilli
 */
public class ResponderUtil {

		
	
	// ========================================================
	// =====================  Response  =======================
	// ========================================================
	

	public static Completion giveCompletion (Exception exc) {

		Completion ret;

		// wrap Exception in a AcsJException
		AcsJException acsJExc = new MyAcsJException(exc);
		
		// wrap AcsJException in a AcsJCompletion
		MyAcsJCompletion c = new MyAcsJCompletion(acsJExc);
		
		// transform AcsJCompletion to a Completion
		ret = c.toCorbaCompletion();
		
		return ret;
	}
	
	
	public static Completion giveCompletion () {

		/* DONE: Use AcsJCompletion instead of this straightforward code
		 * int errorType = 0;
		 * int errorCode = 0;
		 * long timeStamp = 0L;
		 * ErrorTrace[] previousError = new ErrorTrace[]{};
		 * return new Completion(timeStamp, errorType, errorCode, previousError);
		*/
		
		Completion ret;

		MyAcsJCompletion c = new MyAcsJCompletion(0, 0);
		ret = c.toCorbaCompletion();
		
		return ret;
	}

	
	
	public static CBDescOut giveDescOut (CBDescIn descIn) {
		CBDescOut ret;

		long estimatedTimeout = descIn.normal_timeout;
		int id_tag = descIn.id_tag;
		ret = new CBDescOut(estimatedTimeout, id_tag);

		return ret;
	}


	
	

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (String returnValue, CBstring cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBstring cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done("", completion, cbDescOut);
	}

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (String[] returnValue, CBstringSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBstringSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(new String[]{}, completion, cbDescOut);
	}
	

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (int returnValue, CBlong cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBlong cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(-1, completion, cbDescOut);
	}

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (int[] returnValue, CBlongSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBlongSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(new int[]{}, completion, cbDescOut);
	}

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (double returnValue, CBdouble cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBdouble cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(-1D, completion, cbDescOut);
	}

	/**
	 * @param returnValue
	 * @param cb
	 * @param descIn
	 */
	public static void respond (double[] returnValue, CBdoubleSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion();
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(returnValue, completion, cbDescOut);
	}

	/**
	 * @param exc
	 * @param cb
	 * @param descIn
	 */
	public static void respond (Exception exc, CBdoubleSeq cb, CBDescIn descIn) {
		Completion completion = ResponderUtil.giveCompletion(exc);
		CBDescOut cbDescOut = ResponderUtil.giveDescOut(descIn);

		cb.done(new double[]{}, completion, cbDescOut);
	}

	
}

//
//
//
//
//
//
//
//
//
//
//
//
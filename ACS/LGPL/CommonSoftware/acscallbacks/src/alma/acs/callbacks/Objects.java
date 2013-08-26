/*
 * Created on Oct 22, 2004 by mschilli
 */
package alma.acs.callbacks;

import org.apache.commons.lang.ArrayUtils;
import org.omg.CORBA.UserException;

import alma.ACS.CBDescOut;
import alma.ACSErr.Completion;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;



/**
 * Defines several helper classes.
 * 
 * 
 * @author mschilli
 */

public class Objects {


	/**
	 * Wraps an arbitrary Throwable in an AcsJException.
	 * TODO HSO 2006-08-17: why do we not use a generated subclass of AcsJException, e.g. one from module acserrTypes ?
	 */
	static public class MyAcsJException extends AcsJException {

		public MyAcsJException(Throwable cause) {
			super(null, cause);
		}

		protected int getErrorType () {
			return 0;
		}

		protected int getErrorCode () {
			return 0;
		}

		public UserException toCorbaException () {
			return null;
		}

	}

	/**
	 * Only necessary because the constructors in that AcsJCompletion class are protected
	 */
	static public class MyAcsJCompletion extends AcsJCompletion {

		public MyAcsJCompletion() {
			super();
		}

		public MyAcsJCompletion(AcsJException acsJEx) {
			super(acsJEx);
		}

		public MyAcsJCompletion(int type, int code) {
			super(type, code);
		}

	}



	// =================== String ====================


	/**
	 * 
	 * @author mschilli
	 */
	static public class CBstringImpl extends alma.ACS.CBstringPOA {

		protected ResponseReceiver<String> toBeCalledBack;

		public void setXXX (ResponseReceiver<String> x) {
			this.toBeCalledBack = x;
		}

		protected CBstringImpl(ResponseReceiver<String> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}

		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (String returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (String returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(returnValue);

			}
		}

	}


	/**
	 * 
	 * @author mschilli
	 */
	static public class CBstringSeqImpl extends alma.ACS.CBstringSeqPOA {

		protected ResponseReceiver<String[]> toBeCalledBack;

		public void setXXX (ResponseReceiver<String[]> x) {
			this.toBeCalledBack = x;
		}

		protected CBstringSeqImpl(ResponseReceiver<String[]> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}

		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (String[] returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (String[] returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(returnValue);

			}
		}

	}


	// =================== Long ====================

	/**
	 * 
	 * @author mschilli
	 */
	static public class CBlongImpl extends alma.ACS.CBlongPOA {

		protected ResponseReceiver<Integer> toBeCalledBack;

		public void setXXX (ResponseReceiver<Integer> x) {
			this.toBeCalledBack = x;
		}

		protected CBlongImpl(ResponseReceiver<Integer> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}


		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (int returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (int returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(returnValue);

			}
		}


	}


	/**
	 * 
	 * @author mschilli
	 */
	static public class CBlongSeqImpl extends alma.ACS.CBlongSeqPOA {

		protected ResponseReceiver<Integer[]> toBeCalledBack;

		public void setXXX (ResponseReceiver<Integer[]> x) {
			this.toBeCalledBack = x;
		}

		protected CBlongSeqImpl(ResponseReceiver<Integer[]> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}


		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (int[] returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (int[] returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(ArrayUtils.toObject(returnValue));

			}
		}


	}



	// =================== Double ====================

	/**
	 * 
	 * @author mschilli
	 */
	static public class CBdoubleImpl extends alma.ACS.CBdoublePOA {

		protected ResponseReceiver<Double> toBeCalledBack;

		public void setXXX (ResponseReceiver<Double> x) {
			this.toBeCalledBack = x;
		}

		protected CBdoubleImpl(ResponseReceiver<Double> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}


		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (double returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (double returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(returnValue);

			}
		}


	}


	/**
	 * 
	 * @author mschilli
	 */
	static public class CBdoubleSeqImpl extends alma.ACS.CBdoubleSeqPOA {

		protected ResponseReceiver<Double[]> toBeCalledBack;

		public void setXXX (ResponseReceiver<Double[]> x) {
			this.toBeCalledBack = x;
		}

		protected CBdoubleSeqImpl(ResponseReceiver<Double[]> x) {
			toBeCalledBack = x;
		}

		public boolean negotiate (long time_to_transmit, CBDescOut desc) {
			return true;
		}


		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSERR.Completion,
		 *      alma.ACS.CBDescOut)
		 */
		public void working (double[] returnValue, Completion completion, CBDescOut desc) {}

		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACS.Completion, alma.ACS.CBDescOut)
		 */
		public void done (double[] returnValue, Completion completion, CBDescOut desc) {
			AcsJCompletion c = AcsJCompletion.fromCorbaCompletion(completion);
			if (c.isError()) {
				toBeCalledBack.incomingException(c.getAcsJException());

			} else {
				toBeCalledBack.incomingResponse(ArrayUtils.toObject(returnValue));

			}
		}


	}

}


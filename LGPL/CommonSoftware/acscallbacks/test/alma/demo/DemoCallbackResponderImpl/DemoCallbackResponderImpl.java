/*
 * Created on Oct 20, 2004 by mschilli
 */

package alma.demo.DemoCallbackResponderImpl;


import alma.ACS.CBDescIn;
import alma.ACS.CBdouble;
import alma.ACS.CBdoubleSeq;
import alma.ACS.CBlong;
import alma.ACS.CBlongSeq;
import alma.ACS.CBstring;
import alma.ACS.CBstringSeq;
import alma.acs.callbacks.ResponderUtil;
import alma.acs.component.ComponentImplBase;
import alma.demo.DemoCallbackResponderOperations;


/**
 * 
 * @author mschilli
 */
public class DemoCallbackResponderImpl extends ComponentImplBase implements DemoCallbackResponderOperations {


	public void revertString (String text, CBstring cb, CBDescIn descIn) {

		System.out.println("revertString('"+text+"') called");
		
		// --- calculate returnvalue

		int length = text.length();
		char[] res = new char[length];
		for (int i=0; i<length; res[i] = text.charAt(length-1-i++));
		String returnValue = new String(res);

		
		// --- invoke callback

		System.out.println("invoking callback");
		ResponderUtil.respond(returnValue, cb, descIn);
		
	}
	

	public void countString (String text, CBlong cb, CBDescIn descIn) {

		System.out.println("countString('"+text+"') called");
		
		// --- calculate returnvalue

		int returnValue = text.length();

		
		// --- invoke callback

		System.out.println("invoking callback");
		ResponderUtil.respond(returnValue, cb, descIn);
		
	}

	public void convertString (String text, CBlongSeq cb, CBDescIn descIn) {

		System.out.println("convertString('"+text+"') called");
		
		// --- calculate returnvalue

		int length = text.length();
		int[] returnValue = new int[length];
		for (int i=0; i<length; returnValue[i] = text.charAt(i++));

		
		// --- invoke callback

		System.out.println("invoking callback");
		ResponderUtil.respond(returnValue, cb, descIn);
		
	}

	
	public void dummy1 (String text, CBstringSeq cb, CBDescIn descIn) {

		System.out.println("dummy1('"+text+"') called");
		System.out.println("invoking callback");
		
		String[] returnValue = new String[]{"Super", "Dude"};
		ResponderUtil.respond(returnValue, cb, descIn);

	}
		
	public void dummy2 (String text, CBdoubleSeq cb, CBDescIn descIn) {

		System.out.println("dummy2('"+text+"') called");
		System.out.println("invoking callback");
		
		double[] returnValue = new double[]{1.3D, 2.7D};
		ResponderUtil.respond(returnValue, cb, descIn);
	}
	
	public void dummy3 (String text, CBdouble cb, CBDescIn descIn) {

		System.out.println("dummy3('"+text+"') called");
		System.out.println("invoking callback");
		
		try {
		
			double returnValue = 2.7D;
			ResponderUtil.respond(returnValue, cb, descIn);
			throw new IllegalStateException("bla");
		
		} catch (Exception exc) {
			ResponderUtil.respond(exc, cb, descIn);
		}
		
		
	}

	////////////////////////////////////////////////////////
	/// ------------------- API ------------------------ ///
	////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////
	/// ----------------- Internal --------------------- ///
	////////////////////////////////////////////////////////

	////////////////////////////////////////////////////////
	/// ---------------- Inner Types ------------------- ///
	////////////////////////////////////////////////////////

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
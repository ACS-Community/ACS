/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;




/**
 * 
 * @author mschilli
 */
public abstract class ResponseReceiver {


	// ===== String =====

	public void incomingResponse (String s) {
		incomingResponse((Object) s);
	}

	public void incomingResponse (String[] s) {
		incomingResponse((Object) s);
	}

	// ===== Long =====

	public void incomingResponse (int s) {
		incomingResponse((Object) new Integer(s));
	}

	public void incomingResponse (int[] s) {
		incomingResponse((Object) s);
	}

	// ===== Double =====

	public void incomingResponse (double s) {
		incomingResponse((Object) new Double(s));
	}

	public void incomingResponse (double[] s) {
		incomingResponse((Object) s);
	}

	// ===== Generic =====

	public void incomingException (Exception e) {}
	
	public void incomingResponse (Object s) {}


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
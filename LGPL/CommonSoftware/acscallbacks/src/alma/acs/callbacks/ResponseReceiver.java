/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;

import alma.acs.exceptions.AcsJException;


/**
 * User should override the handler methods as needed.
 * @author mschilli, hsommer
 */
public abstract class ResponseReceiver<T> {
	
	public void incomingException (AcsJException e) {
	}
	
	public void incomingResponse (T value) {
	}
}

/*
 * Created on Oct 21, 2004 by mschilli
 */
package alma.acs.callbacks;


/**
 * 
 * @author mschilli, hsommer
 */
public abstract class ResponseReceiver<T> {
	
	public void incomingException (Exception e) {}
	
	public void incomingResponse (T value) {}
}

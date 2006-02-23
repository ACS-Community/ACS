/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import abeans.core.Identifiable;
import abeans.engine.CompletionException;
import abeans.engine.Request;

/**
 * Completion exception allowing programmer to set internal data (host, process, thread, etc.)
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class RemoteCompletionException extends CompletionException {

	/**
	 * "Not available" constant.
	 */
	private static final String NA = "n/a";
	 
	/**
	 * Remote host name.
	 */
	protected String host = NA;

	/**
	 * Constructs a new <code>RemoteCompletionException</code> with a specified message, 
	 * source instance (originating in the pluggable layer)  and a request
	 * that encountered the exception during execution. Delegates to the parent
	 * constructor.
	 * 
	 * @param instance	the Abeans component generating the exception or its
	 * 		   			most proximate instance in Abeans containment, non-<code>null</code>
	 * @param message 	message of this exception, non-<code>null</code>
	 * @param req 		the request during the execution of which an exception was raised,
	 * 					non-<code>null</code>
	 */
	public RemoteCompletionException(Identifiable instance,	String message,	Request req) {
		super(instance, message, req);
		username = NA;
	}

	/**
	 * Constructs a new <code>RemoteCompletionException</code> with the added cause
	 * parameter. Use this constructor when another exception caused this 
	 * exception to be raised. Other parameters are the same as in the other
	 * constructor form.
	 * 
	 * @param instance	the Abeans component generating the exception or
	 * 		   			its most proximate instance in Abeans containment, non-<code>null</code>
	 * @param message	message of this exception, non-<code>null</code>
	 * @param req		the request during the execution of which an exception was raised, 
	 * 					non-<code>null</code>
	 * @param t 		throwable instance that is causing this exception to be thrown, non-<code>null</code>
	 */
	public RemoteCompletionException(Identifiable instance,	String message,	Request req, Throwable t) {
		super(instance, message, req, t);
		username = NA;
	}

	/**
	 * @see com.cosylab.util.CommonThrowable#caughtIn()
	 */
	public void caughtIn(String routine, int lineNo) {
		caughtIn = routine + ":" + lineNo;
	}

	/**
	 * @see com.cosylab.util.CommonThrowable#getHost()
	 */
	public String getHost() {
		return host;
	}

	/**
	 * @see com.cosylab.util.CommonThrowable#getHost()
	 */
	public void setHost(String host) {
		this.host = host;
	}

	/**
	 * @see com.cosylab.util.CommonThrowable#getTimestamp()
	 */
	public void setTimestamp(long timestamp) {
		this.timestamp = timestamp;
	}

	/**
	 * @see com.cosylab.util.CommonThrowable#getUsername()
	 */
	public void setUsername(String username) {
		this.username = username; 
	}

}

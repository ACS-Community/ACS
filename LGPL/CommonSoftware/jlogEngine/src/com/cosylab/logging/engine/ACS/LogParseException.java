package com.cosylab.logging.engine.ACS;
/**
 * Exception used during parsing of the log messages in XML format.
 * 
 * @author sharring
 */
public class LogParseException extends Exception {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * LogParseException no-args constructor.
	 */
	public LogParseException() {
		super();
	}
	
	/**
     * Constructs a new LogParseException with the specified detail message and
     * cause.  <p>Note that the detail message associated with
     * <code>cause</code> is <i>not</i> automatically incorporated in
     * this exception's detail message.
     *
     * @param  message the detail message (which is saved for later retrieval
     *         by the {@link #getMessage()} method).
     * @param  cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A <tt>null</tt> value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     * @see java.lang.Exception
     */
    public LogParseException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Constructs a new LogParseException with the specified cause and a detail
     * message of <tt>(cause==null ? null : cause.toString())</tt> (which
     * typically contains the class and detail message of <tt>cause</tt>).
     * This constructor is useful for exceptions that are little more than
     * wrappers for other throwables (for example, {@link
     * java.security.PrivilegedActionException}).
     *
     * @param  cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A <tt>null</tt> value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     * @see java.lang.Exception
     */
    public LogParseException(Throwable cause) {
        super(cause);
    }	
    
	/**
	 * LogParseException constructor comment.
	 * @param s java.lang.String message for the exception
	 * @see java.lang.Exception
	 */
	public LogParseException(String s) {
		super(s);
	}
}

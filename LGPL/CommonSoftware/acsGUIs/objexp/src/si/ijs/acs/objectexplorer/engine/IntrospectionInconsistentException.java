package si.ijs.acs.objectexplorer.engine;

/**
 * Exception denoting an error in the remote introspection
 * process. More specifically, this exception should
 * be thrown whenever the engine detects an inconsistency
 * between the actual and expected interfaces of the objects
 * that it introspects. 
 */
public class IntrospectionInconsistentException extends RuntimeException {
/**
 * Constructs an instance of this exception.
 *
 * @param s debug message
 */
public IntrospectionInconsistentException(String s) {
	super(s);
}
}

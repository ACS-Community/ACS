package si.ijs.acs.objectexplorer.engine;

/**
 * Engine should throw this exception whenever there is
 * an error in the communication with the remote server
 * or whenever the result that arrived was unexpected
 * (for example non-initialized data, null references etc.).
 */
public class RemoteException extends RuntimeException {
/**
 * Constructs a new instance of this exception.
 *
 * @param s debug message
 */
public RemoteException(String s) {
	super(s);
}
}

package si.ijs.acs.objectexplorer.engine;

/**
 * A data structure representing an asynchronous notification
 * (monitor, callback, alarm). The structure contains the
 * name of the method invoked by the server and all the parameters
 * passed by the server. 
 */
public interface RemoteResponse {
/**
 * Returns the data passed by the server to the client
 * in the asynchronous calls. The names of the parameters
 * are passed in the corresponding <code>dataNames</code>
 * array.
 *
 * @return array of callback parameters
 */
Object[] getData();
/**
 * Returns the names of the parameter values stored in
 * the corresponding <code>data</code> array.
 *
 * @return parameter names, used to display the values
 */
String[] getDataNames();
/**
 * Returns the <code>Invocation</code> object that caused
 * this response to be generated.
 * 
 * @return <code>Invocation</code> that represents the remote
 *		   process generating these responses
 */
Invocation getInvocation();
/**
 * Returns the name of the callback method invoked by the
 * server. This name will be interpreted as the asynchronous
 * "message" or "event", accompanied by the <code>data</code>
 * array.
 * 
 * @return name of the asynchronous message
 */
String getName();
/**
 * Returns a unique response sequence number of this response.
 *
 * @return SeqN
 */
int getSequenceNumber();

/**
 * Returns <code>true</code> if, during the invocation of this method,
 * the engine determined that the remote object responded with error completion.
 * 
 * @return <code>true</code> if the method invocation was
 *		   completed with error
 */
boolean isErrorResponse();

/**
 * Return timestamp of the response.
 * Engine should use server timestamp that correspond to the data timestamp. If not available <code>System.currentTimeMillis()</code> should be used.
 * @return timestamp of the response.
 */
long getTimestamp();

}

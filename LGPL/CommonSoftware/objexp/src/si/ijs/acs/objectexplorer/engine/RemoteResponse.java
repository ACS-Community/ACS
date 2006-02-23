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
}

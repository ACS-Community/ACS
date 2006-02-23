package si.ijs.acs.objectexplorer.engine;

/**
 * This callback interface is implemented by the GUI.
 * Through its methods, the engine notifies GUI
 * when the new responses to asynchronous commands have
 * arrived. A message containing a remote response data
 * structure is sent every time a call is received from the
 * server side. The GUI should process these notifications
 * by displaying them as results of the <code>Invocation</code>
 * instance requests. <b>Since the callback cannot determine
 * which remote call originated it, the GUI must instantiate a
 * new instance of the callback for each asynchronous remote call.</b>
 */
public interface RemoteResponseCallback {
/**
 * A message identifying the destruction of the remote process
 * that delivers the responses for the given invocation.
 * This message is delivered, if: 1. server single-sidedly
 * closes the data delivery and reports this to the client, 2.
 * if the client destroys the data delivery and server does ACK,
 * 3. if server does not support ACKs, the engine must send this
 * when it determines that the data delivery is broken or timeouts.
 *
 */
void invocationDestroyed();
/**
 * Delivers the data structure that parametrizes
 * received notification from the server side.
 * 
 * @param response the data structure containing
 *		  details about the call from the server side
 */
void responseReceived(RemoteResponse response);
}

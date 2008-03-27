package alma.acsplugins.alarmsystem.gui;

/**
 * The listener of the state of the connection 
 * 
 * @author acaproni
 *
 */
public interface ConnectionListener {

	/**
	 * The client is connected
	 * i.e. the category client is connected to all the categories
	 */
	public void connected();
	
	/**
	 * The client is diconnected
	 */
	public void disconnected();
	
	/**
	 * The client is connecting.
	 */
	public void connecting();
	
	/**
	 * The heartbeat with the ASC has been lost.
	 * 
	 * The reconnection is signaled by <code>connected()</code>
	 */
	public void heartbeatLost();
}

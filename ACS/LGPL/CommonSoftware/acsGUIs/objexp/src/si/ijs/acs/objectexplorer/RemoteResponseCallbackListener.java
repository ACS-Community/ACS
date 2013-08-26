package si.ijs.acs.objectexplorer;

import si.ijs.acs.objectexplorer.engine.*;
/**
 * Insert the type's description here.
 * Creation date: (3.2.2002 14:25:42)
 * @author: 
 */
public interface RemoteResponseCallbackListener {
	public void invocationDestroyed(Invocation invocation);
	public void responseReceived(RemoteResponse response);
}

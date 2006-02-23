package si.ijs.acs.objectexplorer;

import javax.swing.*;
import java.util.ArrayList;
/**
 * Insert the type's description here.
 * Creation date: (25.2.2002 18:31:24)
 * @author: 
 */
public class RemoteResponseChartPanel extends JPanel implements RemoteResponseCallbackListener{
  ArrayList queue=null;	
/**
 * RemoteResponseChart constructor comment.
 */
public RemoteResponseChartPanel() {
	super();
	queue=new ArrayList();
}
/**
 * invocationDestroyed method comment.
 */
public void invocationDestroyed(si.ijs.acs.objectexplorer.engine.Invocation invocation) {
	setEnabled(false);
}
/**
 * responseReceived method comment.
 */
public synchronized void responseReceived(si.ijs.acs.objectexplorer.engine.RemoteResponse response) {}
}

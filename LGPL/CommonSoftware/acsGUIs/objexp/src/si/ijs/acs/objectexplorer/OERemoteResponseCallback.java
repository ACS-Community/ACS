package si.ijs.acs.objectexplorer;

/**
 * Insert the type's description here.
 * Creation date: (11/11/00 2:42:46 PM)
 * @author: Miha Kadunc
 */
import si.ijs.acs.objectexplorer.engine.*;
 
public class OERemoteResponseCallback implements RemoteResponseCallback {
	private RemoteResponseCallbackListener handler;
	private Invocation invoc = null;
	private int id=0;
/**
 * OERemoteResponseCallBack constructor comment.
 */
public OERemoteResponseCallback(RemoteResponseCallbackListener handler, int id) {
	super();
	this.id=id;
	this.handler=handler;
}
/**
 * invocationDestroyed method comment.
 */
public void invocationDestroyed() {
  handler.invocationDestroyed(invoc);  
}
/**
 * responseReceived method comment.
 */
public void responseReceived(si.ijs.acs.objectexplorer.engine.RemoteResponse response) {
  	if (id==ReporterBean.raID)
  	{
  		// do the conversion
  		Converter converter = ObjectExplorer.getConverter(response.getInvocation().getInvocationRequest().getIntrospectable());
  		Object[] data = response.getData();
  		if (converter != null && converter.acceptConvert(response.getName()))
  			converter.convert(response.getName(), data, null);

  		handler.responseReceived(response);
  	}
  	else System.out.println("AN OLD RESPONSE CAME IN: "+response);
}
/**
 * Insert the method's description here.
 * Creation date: (1.12.2000 0:23:34)
 * @param i si.ijs.acs.objectexplorer.engine.Invocation
 */
public void setInvocation(Invocation invoc) {
	if (invoc == null) throw new NullPointerException("invoc");
	this.invoc = invoc;
}
}

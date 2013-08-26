package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.*;
import org.omg.CORBA.OperationDescription;
/**
 * Insert the type's description here.
 * Creation date: (2.11.2000 21:23:22)
 * @author: 
 */
public class BACIOperation extends Operation {
	private BACIRemoteAccess ra = null;
	private OperationDescription desc = null;
/**
 * Insert the method's description here.
 * Creation date: (28.6.2001 20:35:24)
 * @param template si.ijs.acs.objectexplorer.engine.BACI.BACIOperation
 * @param parent si.ijs.acs.objectexplorer.engine.SimpleIntrospectable
 */
public BACIOperation(BACIOperation template, SimpleIntrospectable parent) {
	this(template.ra, template.desc, parent, template.getParameterNames(), template.getParameterTypes(), template.getMask(), template.isInvocation(), template.isSpecial());
}
/**
 * BACIOperation constructor comment.
 */
public BACIOperation(BACIRemoteAccess ra, OperationDescription desc, SimpleIntrospectable introspectable, String[] parameterNames, DataType[] parameterTypes, boolean[] mask, boolean invocation, boolean special) {
	super(desc.name, introspectable,new BACIDataType(Object.class),parameterNames, parameterTypes, mask, invocation, special);
	
	if (desc == null) throw new NullPointerException("desc");
	if (ra == null) throw new NullPointerException("ra");
	
	this.desc = desc;
	this.ra = ra;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 21:24:28)
 * @return org.omg.CORBA.OperationDescription
 */
public OperationDescription getOperationDesc() {
	return desc;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:56:24)
 * @return si.ijs.acs.objectexplorer.engine.RemoteCall
 */
public RemoteCall invoke(Object[] data) {
	if (data == null) throw new NullPointerException("data");
	
	if (isInvocation()) throw new IllegalStateException("The operation '" + this + "' represents an asynchronous invocation. You must call 'invokeAsync()' instead.");
	return (RemoteCall)ra.invoke((BACIRemote)getIntrospectable(), this, data, null);
}
/**
 * Insert the method's description here.
 * Creation date: (8.11.2000 0:40:25)
 * @return si.ijs.acs.objectexplorer.engine.Invocation
 * @param cb si.ijs.acs.objectexplorer.engine.RemoteResponseCallback
 */
public Invocation invokeAsync(Object[] data, RemoteResponseCallback cb) {
	if (data == null) throw new NullPointerException("data");
	if (cb == null) throw new NullPointerException("cb");
	
	if (!isInvocation()) throw new IllegalStateException("The operation '" + this + "' is not an asynchronous invocation. Call invoke() instead.");
	return (Invocation)ra.invoke((BACIRemote)getIntrospectable(), this, data, cb);
}
}

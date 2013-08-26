package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.*;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;
import org.omg.CORBA.InterfaceDefHelper;
import org.omg.CORBA.InterfaceDef;

/**
 * Insert the type's description here.
 * Creation date: (7.11.2000 21:50:14)
 * @author: 
 */
public class BACIInvocation extends Invocation implements BACIRemote, SimpleIntrospectable {
	BACIRemoteAccess ra = null;
	private org.omg.CORBA.Object ref = null;
	private FullInterfaceDescription desc = null;
	private String name = null;
	int responseCount = 0;
/**
 * BACIInvocation constructor comment.
 * @param type short
 * @param name java.lang.String
 * @param data si.ijs.acs.objectexplorer.engine.RemoteCall
 * @param tree si.ijs.acs.objectexplorer.OETree
 */
public BACIInvocation(short type, String name, RemoteCall data, RemoteResponseCallback cb, si.ijs.acs.objectexplorer.OETree tree, BACIRemoteAccess ra) {
	super(type, name, data, cb, tree);
	
	if (ra == null) throw new NullPointerException("ra");
	if (name == null) throw new NullPointerException("name");
	
	this.ra = ra;
	this.name = name;
	
	if (data != null) process(data);	
}
/**
 * Insert the method's description here.
 * Creation date: (1.12.2000 1:42:37)
 */
public void destroyDueToTimeout() {
	try {
		ra.getNotifier().reportDebug("BACIInvocation::destroyDueToTimeout", "Destroying invocation for '" + getName() + "'.");
	} catch (NullPointerException e) {
		ra.getNotifier().reportDebug("BACIInvocation::destroyDueToTimeout", "Destroying invocation for '" + name + "'.");
	}
	
	if (!isDestroyed()) getCallback().invocationDestroyed();
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:31:12)
 * @return si.ijs.acs.objectexplorer.engine.Attribute[]
 */
public si.ijs.acs.objectexplorer.engine.Attribute[] getAttributes() {
	return ra.getAttributes(this);
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:55:04)
 * @return java.lang.Object
 */
public org.omg.CORBA.Object getCORBARef() {
	return ref;
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:55:04)
 * @return org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
public org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription getIFDesc() {
	return desc;
}
/**
 * Returns the name of this introspectable. Note that
 * the implementing object should overload the
 * <code>toString()</code> method to return the same
 * value to provide GUI with a standardized way to
 * display the introspectable in components such as
 * lists or trees.
 * 
 * @return name of <code>this</code>
 */
public java.lang.String getName() {
	return getInvocationRequest().getIntrospectable().getName()+" : "+name;
}

/**
 * Get name to be displayed in the tree - used by DelegateInvocation
 * @return
 * @author rbertoncelj
 */
public java.lang.String getDisplayName() {
	return name;
}

/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:50:14)
 * @return si.ijs.acs.objectexplorer.engine.Operation[]
 * @param i si.ijs.acs.objectexplorer.engine.Introspectable
 */
public si.ijs.acs.objectexplorer.engine.Operation[] getOperations() {
	return ra.getOperations(this);
}
/**
 * Returns <code>true</code> iff this invocation instance
 * represents a remote process that is controllable through
 * a remote object (such as a subscription or monitor object).
 * <b>Only if this method returns true, can invocation be used
 * as introspectable, i.e. can have its methods queried, invoked
 * etc. Otherwise the GUI should not interpret this invocation
 * as a tree node and need not display it at all.</b>
 * 
 * @return boolean true if the remote process can be controlled
 *		   through a remote object, represented by the
 *		   <code>SimpleIntrospectable</code> interface which this
 *		   invocation implements
 */
public boolean isControllable() {
	return (ref!=null);
}
/**
 * Insert the method's description here.
 * Creation date: (2.12.2000 2:08:26)
 * @param data si.ijs.acs.objectexplorer.engine.RemoteCall
 */
private void process(RemoteCall data) {
	BACIIntrospector.InvocationObjectHolder ioh = ra.getIntrospector().extractInvocationObject(data);
	ref = ioh.ref;
	if (ref != null)
	{
		InterfaceDef def = InterfaceDefHelper.narrow(ra.lookupId(ioh.id));
		if (def != null) desc = def.describe_interface();
	}
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:58:53)
 * @param ref org.omg.CORBA.Object
 */
public void setCORBARef(org.omg.CORBA.Object ref) {
	this.ref = ref;
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:58:53)
 * @param desc org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
public void setIFDesc(org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription desc) {
	this.desc = desc;
}
/**
 * Insert the method's description here.
 * Creation date: (2.12.2000 1:46:42)
 * @param call si.ijs.acs.objectexplorer.engine.RemoteCall
 */
public void setRemoteCall(RemoteCall call) {
	if (call == null) throw new NullPointerException("call");
	if (getUserObject() != null) throw new IllegalStateException("Cannot set 'RemoteCall' twice.");
	setUserObject(call);
	process(call);
}
}

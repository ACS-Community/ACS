package si.ijs.acs.objectexplorer.engine.BACI;

import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;
import si.ijs.acs.objectexplorer.engine.*;

/**
 * Insert the type's description here.
 * Creation date: (1.11.2000 19:59:25)
 * @author: 
 */
public class BACIRemoteNode extends BACITreeDataNode implements BACIRemote, Introspectable {
	/* cached data */
	private org.omg.CORBA.Object remote = null;
	private FullInterfaceDescription desc = null;
	BACIRemoteAccess ra = null;
	private String name = null;
	/** state when objexp does not activate a component and it is not its owner */
	private boolean isNonSticky = false;
/**
 * BACIRemoteNode constructor comment.
 * @param newType int
 * @param name java.lang.String
 * @param data java.lang.Object
 * @param newParentTree si.ijs.acs.objectexplorer.OETree
 */
public BACIRemoteNode(int newType, String name, Object data, si.ijs.acs.objectexplorer.OETree newParentTree, BACIRemoteAccess ra) {
	super(newType, name, data, newParentTree, ra.getIcon((short)newType));
	if (ra == null) throw new NullPointerException("ra");
	this.ra = ra;
	this.name = name;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:44:07)
 */
public void connect() {
	ra.connect(this);
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:44:07)
 */
public synchronized void disconnect() {
	try
	{
		ra.disconnect(this);
	} finally
	{
	remote = null;
	desc = null;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:31:44)
 * @return si.ijs.acs.objectexplorer.engine.Attribute[]
 */
public Attribute[] getAttributes() {
	return ra.getAttributes(this);
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:47:30)
 * @return java.lang.Object
 */
public synchronized org.omg.CORBA.Object getCORBARef() {
	return remote;
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:47:30)
 * @return org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
public synchronized org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription getIFDesc() {
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
	
	if (getNodeType()!=BACIRemoteAccess.DEVICE) return ((si.ijs.acs.objectexplorer.OETreeNode)getParent()).getName()+" : "+name;
	else return name;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:49:32)
 * @return si.ijs.acs.objectexplorer.engine.Operation[]
 * @param i si.ijs.acs.objectexplorer.engine.Introspectable
 */
public Operation[] getOperations() {
	return ra.getOperations(this);
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 8:07:57 PM)
 *
 * 26.3.2001 Created by Miha 
 *
 * @return boolean
 */
public boolean isConnected() {
	return !(getCORBARef()==null);
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:52:02)
 * @param ref org.omg.CORBA.Object
 */
public synchronized void setCORBARef(org.omg.CORBA.Object ref) {
	this.remote = ref;
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:55:21)
 * @param desc org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
public synchronized void setIFDesc(org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription desc) {
	this.desc = desc;
}

/**
 * @return true if this is a device (component) node
 */
public boolean isDevice()
{
	return getNodeType() == BACIRemoteAccess.DEVICE;
}
/**
 * @return the isNonSticky
 */
public boolean isNonSticky() {
	return isNonSticky;
}
/**
 * @param isNonSticky the isNonSticky to set
 */
public void setNonSticky(boolean isNonSticky) {
	this.isNonSticky = isNonSticky;
}

}

package si.ijs.acs.objectexplorer.engine.BACI;

import org.omg.CORBA.Object;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;

import si.ijs.acs.objectexplorer.OETreeNode;
import si.ijs.acs.objectexplorer.TreeHandlerBean;
import si.ijs.acs.objectexplorer.engine.Attribute;
import si.ijs.acs.objectexplorer.engine.Operation;

public class DelegateRemoteNode extends BACIRemoteNode {
	private String name;
	private BACIRemoteNode remoteNode;
	
	public DelegateRemoteNode(String name, TreeHandlerBean parentTreeHandler, BACIRemoteNode remoteNode) {
		super(remoteNode.getNodeType(), name, remoteNode.getUserObject(), parentTreeHandler.getTreeByName(), (BACIRemoteAccess)parentTreeHandler.getRemoteAccess());
		this.remoteNode = remoteNode;
		this.name = name;	
		// should be already initialized
		this.childrenHolder = remoteNode.childrenHolder;
	}
	
	public BACIRemoteNode getRemoteNode() {
		return remoteNode;
	}
	
	public org.omg.CORBA.Object getCORBARef() {
		return remoteNode.getCORBARef();
	}

	public FullInterfaceDescription getIFDesc() {
		return remoteNode.getIFDesc();
	}

	public void setCORBARef(Object ref) {
		remoteNode.setCORBARef(ref);
	}

	public void setIFDesc(FullInterfaceDescription desc) {
		remoteNode.setIFDesc(desc);
	}

	public Attribute[] getAttributes() {
		return remoteNode.getAttributes();
	}

	public String getName() {
		return name;
	}

	public Operation[] getOperations() {
		return remoteNode.getOperations();
	}

	public void connect() {
		remoteNode.connect();
	}

	public void disconnect() {
		remoteNode.disconnect();
	}

	public boolean isConnected() {
		return remoteNode.isConnected();
	}

	public boolean isNonSticky() {
		return remoteNode.isNonSticky();
	}

	public void setNonSticky(boolean isNonSticky) {
		remoteNode.setNonSticky(isNonSticky);
	}

}

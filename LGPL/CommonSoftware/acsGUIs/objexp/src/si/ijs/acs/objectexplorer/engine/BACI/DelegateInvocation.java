package si.ijs.acs.objectexplorer.engine.BACI;

import org.omg.CORBA.Object;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;

import si.ijs.acs.objectexplorer.OETree;
import si.ijs.acs.objectexplorer.engine.Attribute;
import si.ijs.acs.objectexplorer.engine.Operation;

/**
 * 
 * @author rbertoncelj
 *
 */
public class DelegateInvocation extends BACIInvocation {
	private BACIInvocation invocation;
	
	public DelegateInvocation(BACIInvocation invocation, OETree parentTree, BACIRemoteAccess ra) {
		super((short)invocation.getNodeType(), invocation.getDisplayName(), invocation.getInvocationRequest(), invocation.getCallback(), parentTree, ra);
		this.invocation = invocation;
	}
	
	public Object getCORBARef() {
		return invocation.getCORBARef();
	}

	public FullInterfaceDescription getIFDesc() {
		return invocation.getIFDesc();
	}

	public void setCORBARef(Object ref) {
		invocation.setCORBARef(ref);
	}

	public void setIFDesc(FullInterfaceDescription desc) {
		invocation.setIFDesc(desc);
	}

	public Attribute[] getAttributes() {
		return invocation.getAttributes();
	}

	public String getName() {
		return invocation.getName();
	}

	public Operation[] getOperations() {
		return invocation.getOperations();
	}

}

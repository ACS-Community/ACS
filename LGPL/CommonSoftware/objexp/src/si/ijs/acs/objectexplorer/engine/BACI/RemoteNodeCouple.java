package si.ijs.acs.objectexplorer.engine.BACI;


public class RemoteNodeCouple {
	public BACIRemoteNode deviceByType = null;
	public DelegateRemoteNode deviceByName = null;
	
	public RemoteNodeCouple(BACIRemoteNode deviceByType, DelegateRemoteNode deviceByName) {
		this.deviceByType = deviceByType;
		this.deviceByName = deviceByName;
		
		
	}
}

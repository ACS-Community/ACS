package si.ijs.acs.objectexplorer.engine.BACI;

public class InvocationCouple {
	public BACIInvocation invocationByType;
	public DelegateInvocation invocationByName;
	
	public InvocationCouple(BACIInvocation invocationByType, DelegateInvocation invocationByName) {
		this.invocationByType = invocationByType;
		this.invocationByName = invocationByName;
	}
}

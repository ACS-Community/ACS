package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.*;
/**
 * Insert the type's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @author: 
 */
public class BACIRemoteCall implements RemoteCall {
	private BACIRemote node = null;
	private BACIOperation op = null;
	private Object[] params = null;
	private Object retVal = null;
	private java.lang.Object[] auxRetVal = null;
	private Throwable exception = null;
	private int serial = 0;
	private boolean timeout = false;
	private boolean error = false;
	private static volatile int SN = 0;
	private Attribute att = null;
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 23:06:33)
 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
 * @param att si.ijs.acs.objectexplorer.engine.BACI.BACIAttribute
 * @param param java.lang.Object
 * @param retVal java.lang.Object
 * @param timeout boolean
 * @param exception java.lang.Throwable
 */
public BACIRemoteCall(BACIRemote node, BACIAttribute att, Object[] param, Object retVal, boolean timeout, Throwable exception) {
	this(node, null, param, retVal, null, exception, timeout);
	if (att == null) throw new NullPointerException("att");
	this.att = att;
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:04:15)
 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
 * @param op si.ijs.acs.objectexplorer.engine.Operation
 * @param params java.lang.Object[]
 * @param retVal java.lang.Object
 * @param auxRetVal java.lang.Object[]
 */
public BACIRemoteCall(BACIRemote node, BACIOperation op, Object[] params, Object retVal, Object[] auxRetVal) {
	this(node, op, params, retVal, auxRetVal, null, false);
}
/**
 * BACIRemoteCall constructor comment.
 */
private BACIRemoteCall(BACIRemote node, BACIOperation op, Object[] params, Object retVal, Object[] auxRetVal, Throwable exception, boolean timeout) {
	super();
	if (node == null) throw new NullPointerException("node");
	if (params == null) throw new NullPointerException("params");
	this.node = node;
	this.op = op;
	this.params = params;
	this.retVal = retVal;
	this.auxRetVal = auxRetVal;
	this.exception = exception;
	if (exception != null)
		this.error = true;
	this.timeout = timeout;
	serial = SN;
	SN++;
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:05:49)
 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
 * @param op si.ijs.acs.objectexplorer.engine.BACI.BACIOperation
 * @param params java.lang.Object[]
 * @param exception java.lang.Throwable
 */
public BACIRemoteCall(BACIRemote node, BACIOperation op, Object[] params, Throwable exception) {
	this(node, op, params, null, null, exception, false);
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:06:12)
 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
 * @param op si.ijs.acs.objectexplorer.engine.BACI.BACIOperation
 * @param params java.lang.Object[]
 * @param timeout boolean
 */
public BACIRemoteCall(BACIRemote node, BACIOperation op, Object[] params, boolean timeout) {
	this(node, op, params, null, null, null, true);
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 23:04:20)
 * @return si.ijs.acs.objectexplorer.engine.Attribute
 */
public si.ijs.acs.objectexplorer.engine.Attribute getAttribute() {
	return att;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return java.lang.Object[]
 */
public java.lang.Object[] getAuxReturnValues() {
	return auxRetVal;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:54:31)
 * @return si.ijs.acs.objectexplorer.engine.Introspectable
 */
public SimpleIntrospectable getIntrospectable() {
	return node;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return si.ijs.acs.objectexplorer.engine.Operation
 */
public Operation getOperation() {
	return op;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return java.lang.Object[]
 */
public java.lang.Object[] getParameters() {
	return params;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return int
 */
public int getSN() {
	return serial;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return java.lang.Object
 */
public Object getSyncReturnValue() {
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 23:00:55)
 * @return java.lang.Throwable
 */
public Throwable getThrowable() {
	return exception;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 23:04:20)
 * @return boolean
 */
public boolean isAttributeAccess() {
	return (att != null);
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:01:21)
 * @return boolean
 */
public boolean isTimeout() {
	return timeout;
}

/* (non-Javadoc)
 * @see si.ijs.acs.objectexplorer.engine.RemoteCall#isErrorResponse()
 */
public boolean isErrorResponse() {
	return error;
}

/**
 * @param error The error to set.
 */
public void setErrorResponse(boolean error) {
	this.error = error;
}
}

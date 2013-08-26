package si.ijs.acs.objectexplorer.engine;

import si.ijs.acs.objectexplorer.OETree;
import si.ijs.acs.objectexplorer.OETreeNode;

/**
 * This type denotes a transient process that has been created
 * as a side-effect of the remote call on the remote machine.
 * Usually <code>Invocations</code> are created when asynchronous
 * methods are called (ie. methods that take callback parameters, or
 * methods that implement the publisher / subscriber pattern).
 * Invocation is a transient tree node object. It is contained by the
 * object that owned the operation which created the invocation.
 * Invocations must be destroyed as a side-effect of the container
 * destruction. If the method <code>isControllable()</code> returns
 * <code>false</code> the GUI need not add the node to the tree and
 * <b>must not</b> use this object as an <code>Introspectable</code> -
 * i.e. must not query for operations, attributes, invoke operations etc.
 * 
 */
public abstract class Invocation extends OETreeNode implements SimpleIntrospectable {
	private RemoteResponseCallback cb = null;
	private boolean destroyed = false;
	private boolean requestDestroy = false;
/**
 * Constructs a new instance of invocation object.
 *
 * @param type used internally by OE and engine to specify node types.
 *		  Types are defined by the engine and left unchanged by OE.
 * @param name name of the invocation transient object, used by GUI display
 * @param data data determined by the engine that is stored in the invocation node, can
 *        be null at construction time if the value is provided later
 * @param cb RemoteResponseCallback object associated with this invocation. This object 
 *        will deliver asynchronous responses to the GUI.
 * @param tree tree that will be the node container
 */
public Invocation(short type, String name, RemoteCall data, RemoteResponseCallback cb, OETree tree) {
	super(type, name, data, tree);
	if (cb == null) throw new NullPointerException("cb");
	this.cb = cb;
	
}
/**
 * If the invocation is controllable, this method is called after the node
 * representing the invocation is removed from the tree. Sets the
 * <code>destroyed</code> flag to <code>true</code>.
 * 
 */
public synchronized void destroy() {
	destroyed = true;
}
/**
 * Request destruction.
 * 
 */
public synchronized void requestDestroy() {
	requestDestroy = true;
}
/**
 * Request destruction.
 * 
 */
public synchronized boolean isDestroyRequested() {
	return requestDestroy;
}
/**
 * Returns a callback object associated with this invocation.
 * Callback is the object which receives asynchronous
 * responses for the server. GUI is responsible for instantiating
 * callbacks and passing them to the <code>invokeAsync()</code>
 * method of the <code>Operation</code> instance, which, in
 * response, instantiates this invocation object.
 * 
 * @return callback that delivers data for this invocation
 */
public RemoteResponseCallback getCallback() {
	return cb;
}
/**
 * Returns the remote call data structure which
 * identifies the asynchronous remote call that
 * caused this invocation to start on the remote
 * machine.
 *
 * @return call that initiated this invocation
 */
public RemoteCall getInvocationRequest() {
	return (RemoteCall)getUserObject();
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
public abstract boolean isControllable();
/**
 * Returns <code>true</code> if this controllable invocation
 * has already been destroyed.
 * 
 * @return boolean destroy flag
 */
public synchronized boolean isDestroyed() {
	return destroyed;
}
}

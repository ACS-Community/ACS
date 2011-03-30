package si.ijs.acs.objectexplorer.engine.BACI;



import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Properties;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.JOptionPane;

import org.omg.CORBA.Any;
import org.omg.CORBA.AttributeDescription;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.Contained;
import org.omg.CORBA.InterfaceDef;
import org.omg.CORBA.InterfaceDefHelper;
import org.omg.CORBA.NVList;
import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.ORB;
import org.omg.CORBA.OperationDef;
import org.omg.CORBA.OperationDefHelper;
import org.omg.CORBA.OperationDescription;
import org.omg.CORBA.OperationMode;
import org.omg.CORBA.ParameterDescription;
import org.omg.CORBA.ParameterMode;
import org.omg.CORBA.Repository;
import org.omg.CORBA.RepositoryHelper;
import org.omg.CORBA.Request;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import si.ijs.acs.objectexplorer.NotificationBean;
import si.ijs.acs.objectexplorer.OETreeNode;
import si.ijs.acs.objectexplorer.TreeHandlerBean;
import si.ijs.acs.objectexplorer.engine.Attribute;
import si.ijs.acs.objectexplorer.engine.DataException;
import si.ijs.acs.objectexplorer.engine.DataStruct;
import si.ijs.acs.objectexplorer.engine.Introspectable;
import si.ijs.acs.objectexplorer.engine.IntrospectionInconsistentException;
import si.ijs.acs.objectexplorer.engine.Invocation;
import si.ijs.acs.objectexplorer.engine.NonStickyComponentReleased;
import si.ijs.acs.objectexplorer.engine.NonStickyConnectFailedRemoteException;
import si.ijs.acs.objectexplorer.engine.Operation;
import si.ijs.acs.objectexplorer.engine.RemoteAccess;
import si.ijs.acs.objectexplorer.engine.RemoteException;
import si.ijs.acs.objectexplorer.engine.RemoteResponseCallback;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJNullPointerEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.UTCUtility;
import alma.maciErrType.CannotDeactivateComponentEx;
import alma.maciErrType.ComponentDeactivationFailedEx;
import alma.maciErrType.ComponentDeactivationUncleanEx;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotDeactivateComponentEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;
import alma.objexpErrType.wrappers.AcsJObjectExplorerConnectEx;
import alma.objexpErrType.wrappers.AcsJObjectExplorerInterfaceRepositoryAccessEx;
import alma.objexpErrType.wrappers.AcsJObjectExplorerReportEx;

/**
 * Insert the type's description here.
 * Creation date: (1.11.2000 13:00:27)
 * @author rbertoncelj
 */
public class BACIRemoteAccess implements Runnable, RemoteAccess {

	/************* INNER CLASSES BEGIN HERE ****************/
	private class ServerMessageHolder {
		public static final short MSG = 0;
		public static final short ERR = 1;

		public boolean shutdown = false;

		public short code = MSG;
		public String message = null;
		public ServerMessageHolder(
			short code,
			String message,
			boolean shutdown) {
			this.code = code;
			this.shutdown = shutdown;
			this.message = message;
		}
	}
	private class ServerMessage extends Thread {
		private ServerMessageHolder hldr = null;
		public ServerMessage(ServerMessageHolder hldr) {
			this.hldr = hldr;
		}
		public void run() {
			if (hldr == null)
				return;
			else if (hldr.code == ServerMessageHolder.MSG) {
				notifier.reportMessage(hldr.message);
			} else if (hldr.code == ServerMessageHolder.ERR) {
				notifier.reportError(hldr.message);

			}
		}
	}
	private class CBTimer extends Thread {
		private BACIInvocation invoc = null;

		public CBTimer(BACIInvocation invoc) {
			if (invoc == null)
				throw new NullPointerException("Invoc");
			this.invoc = invoc;
		}
		public void run() {
			try {
				Thread.sleep(POLL_TIMEOUT);
			} catch (InterruptedException ie) {
			}
			invoc.destroyDueToTimeout();
		}
	}

	private class ClientImpl extends ClientPOA {
	    private final long startTimeUTClong = UTCUtility.utcJavaToOmg(System.currentTimeMillis());
	    private long executionId = -1; 

		public void disconnect() {
			//		new DebugEntry(ESOprototypeConnectorInitializer.this, "Disconnected by the manager.").dispatchError();
			new ServerMessage(
				new ServerMessageHolder(
					ServerMessageHolder.ERR,
					"Disconnect requested by the manager. Destroying RemoteAccess.",
					true))
				.start();
			BACIRemoteAccess.this.destroy();
		}
		public String name() {
			return "ObjectExplorer";
		}
		public AuthenticationData authenticate(long execution_id, String question) {
	        // keep old executionId if it exists
	        if (executionId < 0) {
	        	executionId = execution_id;
	        }
	        
	        AuthenticationData ret = new AuthenticationData(
	        		"C", 
	        		ClientType.CLIENT_TYPE, 
	        		ImplLangType.JAVA, 
	        		false, 
	        		startTimeUTClong, 
	        		executionId);
	        return ret;
	    }
		public void message(short code, String text) {
			//	new DebugEntry(ESOprototypeConnectorInitializer.this, "Message from the manager: " + text).dispatch();
			new ServerMessage(
				new ServerMessageHolder(
					ServerMessageHolder.MSG,
					"Manager message: '" + text + "' (code " + code + ").",
					false))
				.start();
		}
		public void taggedmessage(short arg0, short arg1, String arg2) {
			// TODO Auto-generated method stub
			
		}
		public void components_unavailable(String[] cob_names) {
		}
		public void components_available(ComponentInfo[] cobs) {
		}
		public boolean ping() {
			return true;
		}
	}
	
	//2010.02.05 panta@naoj, for sorting components, baci properties, etc
	class MyComparator implements Comparator<String> {
	  public int compare(String strA, String strB) {
	    return strA.compareToIgnoreCase(strB);
	  }
	}
	//--
	private class CallbackImpl
		extends org.omg.PortableServer.DynamicImplementation {
		private String[] allIDs = new String[1];
		private RemoteResponseCallback cb = null;
		private InterfaceDef def = null;
		private String callbackID = null;
		private BACIInvocation invoc = null;

		public static final int MAX_CB_HASH = 1000;

		public CallbackImpl(String callbackID, RemoteResponseCallback cb) {
			super();
			if (callbackID == null)
				throw new NullPointerException("callbackID");
			if (cb == null)
				throw new NullPointerException("cb");

			this.callbackID = callbackID;
			this.cb = cb;
			allIDs[0] = callbackID;

			def = (InterfaceDef) descriptions.get(callbackID);
			if (def == null) {
				synchronized (descriptions) {
					if (descriptions.size() > MAX_CB_HASH)
						descriptions.clear();
				}
				def = InterfaceDefHelper.narrow(rep.lookup_id(callbackID));
				if (def == null)
					throw new RemoteException(
						"Cannot lookup IR record for callback '"
							+ callbackID
							+ "'.");
				synchronized (descriptions) {
					notifier.reportDebug(
						"BACIRemoteAccess$CallbackImpl::CallbackImpl",
						"Added 'InterfaceDef' for '"
							+ callbackID
							+ "' to cache.");
					descriptions.put(callbackID, def);
				}
			}
		}

		public void setInvocation(BACIInvocation invoc) {
			if (invoc == null)
				throw new NullPointerException("invoc");
			this.invoc = invoc;
		}
		public java.lang.String[] _all_interfaces(POA arg1, byte[] arg2) {
			return allIDs;
		}

		private String createHashKey(String callbackID, String operation) {
			return callbackID + "#" + operation;
		}

		public void invoke(org.omg.CORBA.ServerRequest request) {
			if (request == null) {
				getNotifier().reportError(
					"Callback implementation has received a 'null' ServerRequest object from CORBA DSI skeleton.");
				return;
			}
			String op = request.operation();

			/*
			notifier.reportDebug(
				"BACIRemoteAccess$CallbackImpl::invoke",
				"Received callback invocation '" + op + "'.");
			*/

			String opKey = createHashKey(callbackID, op);

			OperationDef odef =
				(OperationDef) descriptions.get(opKey);

			if (odef == null) {
				try {
					notifier.reportDebug(
						"BACIRemoteAccess$CallbackImpl::invoke",
						"Looking up operation definition for '"
							+ def.name()
							+ "."
							+ request.operation()
							+ "()' from IR.");
					odef =
						OperationDefHelper.narrow(
							def.lookup(request.operation()));
					ParameterDescription[] pdesc = odef.params();
					for (int i = 0; i < pdesc.length; i++) {
						if (pdesc[i].mode != ParameterMode.PARAM_IN) {
							getNotifier().reportError(
								"Operation '"
									+ callbackID
									+ "::"
									+ odef.name()
									+ "()' declares other parameters than CORBA IN. BACI does not allow such callbacks. Skipping.");
							return;
						}
					}
				} catch (Exception e) {
					getNotifier().reportError(
						"Lookup for operation '"
							+ op
							+ "' failed in Interface Repository.",
						e);
					return;
				}
				synchronized (descriptions) {
					notifier.reportDebug(
						"BACIRemoteAccess$CallbackImpl::invoke",
						"Added 'OperationDef' for '"
							+ callbackID
							+ "::"
							+ op
							+ "()' to cache.");
					descriptions.put(opKey, odef);
				}
			}

			/*
			notifier.reportDebug(
				"BACIRemoteAccess$CallbackImpl::invoke",
				"Unpacking callback arguments...");
			*/

			String[] names = null;
			java.lang.Object[] data = null;
			boolean errorResponse = false;
			try {

				// NVList list HAS to be synchronized,
				// since it is a container for values
				synchronized(operationListDescriptions)
				{

				        NVList list =
					    (NVList) operationListDescriptions.get(opKey);

					if (list == null) {
					    list = orb.create_operation_list((org.omg.CORBA.Object) odef);
					    synchronized (operationListDescriptions) {
						notifier.reportDebug(
								     "BACIRemoteAccess$CallbackImpl::invoke",
								     "Added 'NVList' for '"
								     + callbackID
								     + "::"
								     + op
								     + "()' to cache.");
						operationListDescriptions.put(opKey, list);
					    }
					}

					request.arguments(list);
					int size = list.count();
					data = new java.lang.Object[size];
					names = new String[size];
					for (int i = 0; i < size; i++) {
						//					if (list.item(i) == null) System.out.println("Null");
						// NOTE: values is COPIED from list.item(i).value()
						// and this makes it thread safe
						data[i] =
							getIntrospector().extractAny(list.item(i).value());
						names[i] = list.item(i).name();
					}

					// check for error-type ACSCompletion-s
					for (int i = 0; i < size; i++)
						errorResponse |= checkFromACSCompletion(data[i]);

				}

			} catch (Bounds b) {
				getNotifier().reportError(
					"The callback parameter list returned by the server and the IR data do not agree in length. Skipping.",
					b);
				return;
			} catch (Exception e) {
				getNotifier().reportError(
					"Exception while unpacking callback parameter list.",
					e);
			}
			TypeCode result_tc = orb.get_primitive_tc(TCKind.tk_void);
			Any result_any = orb.create_any();
			result_any.type(result_tc);
			request.set_result(result_any);
			BACIRemoteResponse response =
				new BACIRemoteResponse(invoc, op, names, data);
			response.setErrorResponse(errorResponse);
			response.cb = cb;
			if (baciIntrospector.isInvocationDoneMethod(op) || invoc.isDestroyRequested()) {
				response.destroy = true;
				org.omg.PortableServer.POA poa = _default_POA();
				try {
					byte[] id = poa.servant_to_id(this);
					poa.deactivate_object(id);
					notifier.reportDebug(
						"BACIRemoteAccess$CallbackImpl::invoke",
						"Destroyed callback implementation.");
				} catch (Exception e) {
					notifier.reportError(
						"POA cannot deactivate callback: " + e);
				}
			}
			/*
			notifier.reportDebug(
				"BACIRemoteAccess$CallbackImpl::invoke",
				"Inserted remote response into dispatcher queue.");
			*/
			getDispatcher().add(response);
		}
	}

	public class Dispatcher extends Thread {
		private static final int MAX_QUEUE = 100;
		private LinkedList queue = new LinkedList();
		private boolean working = true;

		public Dispatcher() {
			super("ObjectExplorerDispatcher");
			setPriority(Thread.NORM_PRIORITY - 2);
		}

		public void add(BACIRemoteResponse res) {
			synchronized (queue) {
				boolean found = false;
				if (queue.size() >= MAX_QUEUE) {
					for (int i = 0; i < queue.size(); i++) {
						if (((BACIRemoteResponse) queue.get(i)).getInvocation()
							== res.getInvocation()) {
							queue.set(i, res);
							found = true;
							break;
						}
					}
				}
				if (!found)
					queue.addLast(res);
				queue.notify();
			}
		}

		public void destroy() {
			synchronized (queue) {
				working = false;
				queue.notify();
			}
		}

		public void run() {
			while (working) {
			    try {
				BACIRemoteResponse res = null;
				synchronized (queue) {

					while (queue.isEmpty()) {
						try {
							queue.wait(100);
							if (!working) {
								notifier.reportDebug(
									"BACIRemoteAccess$Dispatcher::run",
									"Dispatcher thread returning gracefully.");
								return;
							}
						} catch (InterruptedException ie) {
						}
					}
					if (!working)
						return;
					res = (BACIRemoteResponse) queue.getFirst();
					if (res.getInvocation().getInvocationRequest() == null) {
						queue.removeFirst();
						queue.addLast(res);
						try {
							if (queue.size() == 1)
								sleep(25);
							yield();
						} catch (InterruptedException ie) {
						}
						continue;
					} else
						res = (BACIRemoteResponse) queue.removeFirst();
				}
				res.cb.responseReceived(res);
				if (res.destroy) {
					res.cb.invocationDestroyed();
					synchronized (invocations) {
						invocations.remove(res.getInvocation());
					}
				}
			    } catch (Exception e)
				{
				    e.printStackTrace();
				}
			}
		}
	}
	private TreeHandlerBean parent = null;
	private BACIMenu baciEngineMenu = null;
	private NotificationBean notifier = null;
	private ArrayList<BACIInvocation> invocations = new ArrayList<BACIInvocation>();
	private Dispatcher dispatcher = null;
	private ArrayList<String> connected = new ArrayList<String>();
	private BACIIntrospector baciIntrospector = new BACIIntrospector(this);
	private boolean ORBdebug = false;
	private String managerLoc = null;
	private ORB orb = null;
	private Thread orbThread = null;
	private Manager manager = null;
	private int handle = 0;
	private ComponentInfo[] infos = null;
	private ClientImpl client = null;
	private Repository rep = null;
	public static final short DOMAIN = 0;
	public static final short TYPE = 1;
	public static final short DEVICE = 2;
	public static final short PROPERTY = 3;
	public static final short ATTRIBUTE = 4;
	public static final short TRANSIENT = 5;
	public static final short DUMMY = 6;
	// default is 5s
	public static /*final*/ int POLL_TIMEOUT = 5000;
	public static final int POLL_SLEEP = 50;
	public static final String MANAGER_CORBALOC = "ACS.manager";
	public static final String IR_CORBALOC = "ACS.repository";
	public static final String PROPERTY_POOL_TIMEOUT = "objexp.pool_timeout";
	public static final String CONNECT_NON_STICKY_FLAG = "objexp.connect_non_sticky";
	public static final String strict = "false";
	/* for CallbackImpl, bookkeeping of static members outside nonstatic inner class */
	private static HashMap descriptions = new HashMap();
	private static HashMap operationListDescriptions = new HashMap();
	private HashMap interfaceDescriptions = new HashMap();
	private HashMap attributeIntrospected = new HashMap();
	private HashMap operationsIntrospected = new HashMap();
	private boolean bufferDescs = true;
	private Hashtable devices = null;
	private boolean destroyed = false;
	private boolean connectNonSticky = false;
	
	
	/**
	 * ESORemoteAccess constructor comment.
	 */
	public BACIRemoteAccess(
		TreeHandlerBean parent,
		NotificationBean notifier,
		Hashtable devices) {
		super();
		if (parent == null)
			throw new NullPointerException("treeHandler*");
		if (notifier == null)
			throw new NullPointerException("notifier");
		if (devices == null)
			throw new NullPointerException("devices");
		this.parent = parent;
		this.notifier = notifier;
		this.devices = devices;
		notifier.reportDebug(
			"BACIRemoteAccess::BACIRemoteAccess",
			"Constructed remote access.");

	}

	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 * @param i si.ijs.acs.objectexplorer.engine.Introspectable
	 */
	synchronized void connect(Introspectable target) {
		if (target == null)
			throw new NullPointerException("target");

			
		BACIRemoteNode baciNode = null;
		try {
			baciNode = (BACIRemoteNode) target;
			if (baciNode.getCORBARef() != null)
				return;
			if (!(baciNode.getParent() instanceof Introspectable)) {
				internalManagerConnect(baciNode);
				synchronized (connected) {
					// connected is being use to have a list of all nodes to be released at logout
					if (!baciNode.isNonSticky() && baciNode.getUserObject() instanceof String)
						connected.add((String)baciNode.getUserObject());
				}
			} else {
				internalParentConnect(baciNode);
			}
			if (baciNode.getCORBARef() == null)
				throw new AcsJNullPointerEx();
		} catch (AcsJObjectExplorerConnectEx e) {
			// do not make panic if non-sticky mode
			if (!connectNonSticky)
			{
			    logACSException(e);
				throw new RemoteException(
						"Failed to connect to '" + baciNode + "'");
			}
			else
			{
				throw new NonStickyConnectFailedRemoteException(e.getMessage());
			}
		} catch (AcsJNullPointerEx e) {
		    logACSException(e);
		    /**
		     * @todo GCH 2006.10.18
		     *       Here we throw a local exception.
		     *       If we do not do this, objexp does not work properly any more.
		     *       But things should be different.
		     *       The problem is that I did not understand what happens
		     *       upstream this call if I do not throw the exception.
		     *       In practice the only bad effect of throwing the exception 
		     *       are error traces in the console that probably should not be there.
		     */
			throw new RemoteException(
					"Failed to connect to '" + baciNode + "'");
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 */
	public synchronized void destroy() {
		if (destroyed)
			return;
		destroyed = true;
		synchronized (invocations) {
			for (BACIInvocation invoc : invocations) {
				try {
					if (invoc.isControllable())
						BACIIntrospector.destroyInvocation(invoc);
				} catch (Exception e) {
					// try others
				}
			}
			invocations.clear();
		}
		notifier.reportMessage("Logging out from the Manager.");

		synchronized (connected) {
			notifier.reportDebug(
				"BACIRemoteAccess::destroy",
				"Releasing Components on Manager, " + connected.size() + " Components total.");
			for (Iterator<String> iter = connected.iterator(); iter.hasNext();) {
				String compName = iter.next();
				try {
					manager.release_component(handle, compName);
				} catch (Exception ex) {
					notifier.reportError("Cannot release component " + compName, ex);
				}
			}
			connected.clear();
		}
		synchronized (descriptions) {
			descriptions.clear();
			notifier.reportDebug(
				"BACIRemoveAccess::destroy",
				"Cleared IF descriptions list for CallbackImpl.");
		}
		synchronized (operationListDescriptions) {
			operationListDescriptions.clear();
			notifier.reportDebug(
				"BACIRemoveAccess::destroy",
				"Cleared IF operation descriptions list for CallbackImpl.");
		}
		synchronized (interfaceDescriptions) {
			interfaceDescriptions.clear();
		}
		synchronized (operationsIntrospected) {
			operationsIntrospected.clear();
		}
		synchronized (attributeIntrospected) {
			attributeIntrospected.clear();
		}
		if (manager != null) {
			try {
				manager.logout(handle);
			} catch (Exception e) {
				// try others
			}
			notifier.reportDebug(
				"BACIRemoteAccess::destroy",
				"ObjectExplorer logout OK.");
		}
		/* deactivate the client object */
		org.omg.PortableServer.POA poa = client._default_POA();
		try {
			byte[] id = poa.servant_to_id(client);
			poa.deactivate_object(id);
		} catch (Exception e) {
			notifier.reportError("Error while deactivating Client servant.", e);
		}

		orb.destroy();
		notifier.reportDebug(
			"BACIRemoteAccess::destroy",
			"ORB shutdown complete.");
		notifier.reportMessage("Shutting down CORBA.");
		notifier.reportDebug(
			"BACIRemoteAccess::destroy",
			"Sending dispatcher termination signal.");
		dispatcher.destroy();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 * @param i si.ijs.acs.objectexplorer.engine.Introspectable
	 */
	synchronized void disconnect(Introspectable target) {
		if (target == null)
			throw new NullPointerException("target");

		BACIRemoteNode baciNode = (BACIRemoteNode) target;
		if (baciNode.getCORBARef() == null)
		{
			// already disconnected
			return;
		}
		notifier.reportMessage(
			"Disconnecting from '" + target.getName() + "'.");
		java.util.Enumeration e = baciNode.children();
		while (e.hasMoreElements()) {
			java.lang.Object next = e.nextElement();
			if (next instanceof Introspectable) {
				((Introspectable) next).disconnect();
			}
		}
		synchronized (invocations) {
			ArrayList<BACIInvocation> removal = new ArrayList<BACIInvocation>();
			for (BACIInvocation invoc : invocations) {
				if (invoc.getInvocationRequest().getIntrospectable() == target) {
					BACIIntrospector.destroyInvocation(invoc);
					removal.add(invoc);
				}
			}
			invocations.removeAll(removal);
		}
		synchronized (connected) {
			if (baciNode.getUserObject() instanceof String) {
				connected.remove(baciNode.getUserObject());
			}
		}
		// todo: This looks like a very dirty patch.
		//       we have to find out a clean way to rule out things that are not DOs and
		//       also not Components.
	    //if (baciIntrospector.isDevice(baciNode.getCORBARef())) {
		if (baciNode.isDevice() && !baciNode.isNonSticky()) {
			if (manager == null)
				resolveManager();
			notifier.reportDebug(
				"BACIRemoteAccess::disconnect",
				"Releasing component '" + target.getName() + "'.");
			try {
				manager.release_component(handle, (String) baciNode.getUserObject());
				notifier.reportDebug(
						"BACIRemoteAccess::disconnect",
						"Component '" + target.getName() + "' released.");
				}
			catch (NoPermissionEx npe) {
				notifier.reportError("No permission to release component", npe);
			} 
			catch (Exception ex) {
				notifier.reportError("Failed to release component", ex);
			}
		}
		else
		{
//			notifier.reportDebug(
//				"BACIRemoteAccess::disconnect",
//				"===> NOT DISCONNECTING  " + target.getName() + "'.");
		}
		notifier.reportMessage(
				"Disconnected from '" + target.getName() + "'.");
	}

	/**
	 * Explodes the device node.
	 * Creation date: (1.11.2000 17:01:09)
	 * @return si.ijs.acs.objectexplorer.engine.OETreeNode[]
	 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
	 */
	private synchronized OETreeNode[] explodeDeviceNode(BACIRemoteNode node) {
		if (node == null)
			throw new NullPointerException("node");
		if (manager == null)
			resolveManager();

		BACIRemoteNode baciNode = (BACIRemoteNode) node;
		connect(baciNode);

		AttributeDescription[] props2 =
			baciIntrospector.getProperties(baciNode.getIFDesc().attributes);
		BACIRemoteNode[] retVal = new BACIRemoteNode[props2.length];
			for (int i = 0; i < props2.length; i++) {
				retVal[i] = new BACIRemoteNode(
						ATTRIBUTE,
						props2[i].name,
						props2[i],
						parent.getTree(),
						this);
			}
			java.util.Arrays.sort(retVal);
			notifier.reportDebug(
			"BACIRemoteAccess::explodeDeviceNode",
			"Processing for node '" + node + "' complete.");
			java.util.Arrays.sort(retVal); //2010.02.05 panta@naoj
		return retVal;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 18:08:31)
	 * @return si.ijs.acs.objectexplorer.engine.BACI.BACITreeDataNode[]
	 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACITreeDataNode
	 */
	private synchronized BACITreeDataNode[] explodeDomainNode(BACITreeDataNode n) {
		if (n == null)
			throw new NullPointerException("node");
		if (n.childrenHolder == null)
			return new BACITreeDataNode[0];

		//TreeMap temp = new TreeMap();
		ComponentInfo[] infos = new ComponentInfo[n.childrenHolder.size()];
		n.childrenHolder.toArray(infos);

		TreeMap tempTypes = new TreeMap();
		TreeMap tempDomains = new TreeMap();
		for (int i = 0; i < infos.length; i++) {
			String cmp = null;
			String domain = BACICURLResolver.resolveDomain(infos[i].name);
			if (domain.endsWith("/"))
				cmp = domain;
			else
				cmp = domain + "/";

			if (n.domainRemainder.equals(cmp)) {
				BACITreeDataNode node =
					(BACITreeDataNode) tempTypes.get(infos[i]);
				if (node != null) {
					//				node.childrenHolder.add(infos[i].cob_curl);
					node.childrenHolder.add(infos[i].name);
				} else {
					try {
						node =
							new BACITreeDataNode(
								TYPE,
								BACIIntrospector.fullTypeToType(infos[i].type),
								BACIIntrospector.fullTypeToType(infos[i].type),
								parent.getTree(),
								getIcon(DOMAIN));

					} catch (IntrospectionInconsistentException iie) {
						notifier.reportError(
							"Invalid IDL type '" + infos[i].type + "'.",
							iie);
						continue;
					}
					node.childrenHolder = new ArrayList();
					//				node.childrenHolder.add(infos[i].cob_curl);
					node.childrenHolder.add(infos[i].name);
					tempTypes.put(infos[i].type, node);
				}
			} else {
				if (domain.startsWith("/"))
					domain = domain.substring(1);
				if (!domain.endsWith("/"))
					domain = domain + "/";
				String dpart =
					domain.substring(
						n.domainRemainder.length(),
						domain.indexOf('/'));

				BACITreeDataNode node =
					(BACITreeDataNode) tempDomains.get(dpart);
				if (node != null) {
					node.childrenHolder.add(infos[i]);
				} else {


					node =
						new BACITreeDataNode(
							DOMAIN,
							dpart,
							infos[i],
							parent.getTree(),
							getIcon(DOMAIN));
					node.childrenHolder = new ArrayList();
					node.childrenHolder.add(infos[i]);
					node.domainRemainder = n.domainRemainder + dpart + "/";
					tempDomains.put(dpart, node);
				}
			}
		}
		BACITreeDataNode[] arrayTypes = new BACITreeDataNode[tempTypes.size()];
		tempTypes.values().toArray(arrayTypes);
		BACITreeDataNode[] arrayDomains =
			new BACITreeDataNode[tempDomains.size()];
		tempDomains.values().toArray(arrayDomains);
		BACITreeDataNode[] retVal =
			new BACITreeDataNode[arrayTypes.length + arrayDomains.length];
		System.arraycopy(arrayDomains, 0, retVal, 0, arrayDomains.length);
		System.arraycopy(
			arrayTypes,
			0,
			retVal,
			arrayDomains.length,
			arrayTypes.length);
		notifier.reportDebug(
			"BACIRemoteAccess::explodeDomainNode",
			"Processing for node '" + n + "' complete.");
		java.util.Arrays.sort(retVal); //2010.02.05 panta@naoj
		return retVal;

	}

	private synchronized BACITreeDataNode[] explodeDomainNodeByName(BACITreeDataNode n) {
		if (n == null)
			throw new NullPointerException("node");
		if (n.childrenHolder == null)
			return new BACITreeDataNode[0];
		BACITreeDataNode[] retVal = new BACITreeDataNode[n.childrenHolder.size()];
		for (int i = 0; i < retVal.length; i++) {
			retVal[i] = (BACITreeDataNode)n.childrenHolder.get(i);
		}
		notifier.reportDebug(
			"BACIRemoteAccess::explodeDomainNodeByName",
			"Processing for node '" + n + "' complete.");
		//2010.02.05 panta@naoj
		java.util.Arrays.sort(retVal);
		return retVal;

	}

	/**
	 * Explodes the root node and groups the devices by their types.
	 * Creation date: (1.11.2000 17:00:46)
	 * @return si.ijs.acs.objectexplorer.engine.OETreeDataNode[]
	 */
	private synchronized OETreeNode[] explodeRootNodeByType() {
		if (manager == null)
			resolveManager();

		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByType",
			"Querying manager for all instances of all types...");
		long time1 = System.currentTimeMillis();
		int[] handles = new int[0];
		try {
	   	   infos = manager.get_component_info(handle, handles, "*", "*", false);
  		}
		catch (NoPermissionEx npe) {
		   notifier.reportError("Nopermission to get component info", npe);
		}
		long time2 = System.currentTimeMillis();
		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByType",
			"Query OK. Completed in " + (time2 - time1) + " ms.");
		TreeMap tempTypes = new TreeMap();
		TreeMap tempDomains = new TreeMap();


		devices.clear();
		/*
		 * Loop trough all the received devices and add any new devices that
		 * are not yet stored in Hashtable.
		 */
		for (int i = 0; i< infos.length; i++) {
			String curl = infos[i].name;
			RemoteNodeCouple rnc = (RemoteNodeCouple)devices.get(curl); //check if RemoteNodeCouple already exists for this device
			if (rnc != null) continue; //skip the device if it does (that should not ever happen actually...)
			String cob = BACICURLResolver.resolveName(curl);
			//System.out.println("DEBUG DEVICE: CURL: "+curl);
			BACIRemoteNode device = new BACIRemoteNode(
					DEVICE,
					cob, //cob2,
					curl,
					parent.getTree(),
					this);
			device.childrenHolder = new ArrayList();
			
			rnc = new RemoteNodeCouple(device, null);  //if not, create a new one and add it to Hashtable devices
			String[] cobs = cob.split("/");
			String cob2 = cobs[cobs.length-1];
			rnc.deviceByName = new DelegateRemoteNode(cob2, parent, device);
			devices.put(curl, rnc);
		}

		/*
		 * now loop trough all the devices again and add them to the tree
		 * TODO refacture to have only one "for loop" - add to hashtable and tree at the same time
		 */
		for (int i = 0; i < infos.length; i++) {
			String domain = BACICURLResolver.resolveDomain(infos[i].name);
			if (domain.equals(BACICURLResolver.ROOT_DOMAIN)) {
				BACITreeDataNode node =
					(BACITreeDataNode) tempTypes.get(infos[i].type);
				if (node != null) {
					//				node.childrenHolder.add(infos[i].cob_curl);
					node.childrenHolder.add(infos[i].name);
				} else {
					try {
						node =
							new BACITreeDataNode(
								TYPE,
								BACIIntrospector.fullTypeToType(infos[i].type),
								BACIIntrospector.fullTypeToType(infos[i].type),
								parent.getTree(),
								getIcon(TYPE));
					} catch (IntrospectionInconsistentException iie) {
						notifier.reportError(
							"Invalid IDL type '" + infos[i].type + "'.",
							iie);
						continue;
					}
					node.childrenHolder = new ArrayList();
					//				node.childrenHolder.add(infos[i].cob_curl);
					node.childrenHolder.add(infos[i].name);
					tempTypes.put(infos[i].type, node);
				}
			} else {
				if (domain.startsWith("/"))
					domain = domain.substring(1);
				int index = domain.indexOf('/');
				String dpart = null;
				if (index != -1)
					dpart = domain.substring(0, domain.indexOf('/'));
				else
					dpart = domain;
				BACITreeDataNode node =
					(BACITreeDataNode) tempDomains.get(dpart);
				if (node != null) {
					node.childrenHolder.add(infos[i]);
				} else {
					node =
						new BACITreeDataNode(
							DOMAIN,
							dpart,
							infos[i],
							parent.getTree(),
							getIcon(DOMAIN));
					node.childrenHolder = new ArrayList();
					node.childrenHolder.add(infos[i]);
					if (index != -1)
						node.domainRemainder = dpart + "/";
					else
						node.domainRemainder = dpart;
					tempDomains.put(dpart, node);
				}
			}
		}
		BACITreeDataNode[] arrayTypes = new BACITreeDataNode[tempTypes.size()];
		tempTypes.values().toArray(arrayTypes);
		BACITreeDataNode[] arrayDomains =
			new BACITreeDataNode[tempDomains.size()];
		tempDomains.values().toArray(arrayDomains);
		BACITreeDataNode[] retVal =
			new BACITreeDataNode[arrayTypes.length + arrayDomains.length];
		System.arraycopy(arrayDomains, 0, retVal, 0, arrayDomains.length);
		System.arraycopy(
			arrayTypes,
			0,
			retVal,
			arrayDomains.length,
			arrayTypes.length);
		java.util.Arrays.sort(retVal, new java.util.Comparator() { 
			public int compare(Object obj1, Object obj2)
			{
			   return obj1.toString().compareTo(obj2.toString());
			}
		});
		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByType",
			"Root nodes processing complete.");

		return retVal;
	}

	/**
	 * Explodes the root node and groups the devices by their parent devices.
	 * Warning: explodeRootNodeByType() has to be called at least once before this method is called!
	 * @return
	 */
	public synchronized OETreeNode[] explodeRootNodeByName() {
		if (manager == null)
			resolveManager();

		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByName",
			"Querying manager for all instances of all types...");
		long time1 = System.currentTimeMillis();
		int[] handles = new int[0];
		try {
	   	   infos = manager.get_component_info(handle, handles, "*", "*", false);
  		}
		catch (NoPermissionEx npe) {
		   notifier.reportError("Nopermission to get component info", npe);
		}
		long time2 = System.currentTimeMillis();
		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByName",
			"Query OK. Completed in " + (time2 - time1) + " ms.");
		TreeMap tempDomains = new TreeMap();
		TreeMap tempDummies = new TreeMap();
		Vector rootDummies = new Vector();

		for (int i = 0; i < infos.length; i++) {
			String curl = infos[i].name;
			String domain = BACICURLResolver.resolveDomain(curl);
			String cob = BACICURLResolver.resolveName(curl);
			RemoteNodeCouple rnc = (RemoteNodeCouple)devices.get(curl);
			if (rnc == null) {
				notifier.reportError(
						"BACIRemoteAccess::explodeRootNodeByName - Unexpected null pointer (rnc).");
				continue;
			}
			if (rnc.deviceByName == null) {
				notifier.reportError(
						"BACIRemoteAccess::explodeRootNodeByName - Unexpected null pointer (rnc.deviceByName).");
				continue;
			}
			
			String[] names = cob.split("/", 2);

			if (domain.equals(BACICURLResolver.ROOT_DOMAIN)) {
				BACITreeDataNode dummyNode = (BACITreeDataNode) tempDummies.get(names[0]);
				if (names.length > 1) {
					boolean doHierarchy = dummyNode instanceof BACIRemoteNode;
					if (dummyNode == null || doHierarchy) {
						BACITreeDataNode oldDummy = dummyNode;
						dummyNode = new BACITreeDataNode(DUMMY, names[0], BACICURLResolver.getFirstLevelCurl(curl), parent.getTreeByName(), getIcon(DOMAIN));
						dummyNode.childrenHolder = new ArrayList();
						tempDummies.put(names[0], dummyNode);
						rootDummies.add(dummyNode);
						if (doHierarchy)
							dummyNode.childrenHolder.add(oldDummy);	
					} 
					String[] arrNames = names[1].split("/");
					//System.out.println("DEBUG "+ names[1]);
					getTreeForName(dummyNode, 0, arrNames);
				} else {
					if (dummyNode == null) {
						tempDummies.put(names[0], rnc.deviceByName);
					} else {
						dummyNode.childrenHolder.add(0, rnc.deviceByName);
					}
				}
			} else {
				//TODO: Domain part not yet tested - should set up test environment with domains and do extensive testing.
				if (domain.startsWith("/"))
					domain = domain.substring(1);
				int index = domain.indexOf('/');
				String dpart = null;

				if (index != -1)
					dpart = domain.substring(0, index);
				else
					dpart = domain;

				BACITreeDataNode node =
					(BACITreeDataNode) tempDomains.get(dpart);
				if (node == null) {
					node =
						new BACITreeDataNode(
							DOMAIN,
							dpart,
							infos[i],
							parent.getTreeByName(),
							getIcon(DOMAIN));
					node.childrenHolder = new ArrayList();
					if (index != -1)
						node.domainRemainder = dpart + "/";
					else
						node.domainRemainder = dpart;
					tempDomains.put(dpart, node);
				}

				BACITreeDataNode dummyNode = (BACITreeDataNode) tempDummies.get(names[0]);
				if (names.length > 1) {
					if (dummyNode == null) {
						dummyNode = new BACITreeDataNode(DUMMY, names[0], BACICURLResolver.getFirstLevelCurl(curl), parent.getTree(), getIcon(DOMAIN));
						dummyNode.childrenHolder = new ArrayList();
						node.childrenHolder.add(0, rnc.deviceByName);
						rootDummies.add(dummyNode);
					}
					String[] arrNames = names[1].split("/");
					//System.out.println("DEBUG "+ names[1]);
					getTreeForName(dummyNode, 0, arrNames);
				} else {
					if (dummyNode == null) {
						node.childrenHolder.add(0, rnc.deviceByName);
					} else {
						dummyNode.childrenHolder.add(0, rnc.deviceByName);
					}
				}
			}
		}
		for (int i = 0; i < rootDummies.size(); i++) {
			BACITreeDataNode tmpNode = (BACITreeDataNode)rootDummies.get(i);
			for (int j = 0; j < tmpNode.childrenHolder.size(); j++) {
				if (tmpNode.childrenHolder.get(j) instanceof BACIRemoteNode) {
					continue;
				} else {
					removeSingleDeviceDummies(tmpNode, j, (BACITreeDataNode)tmpNode.childrenHolder.get(j));
				}
			}
		}
		BACITreeDataNode[] arrayDummies = new BACITreeDataNode[tempDummies.size()];
		tempDummies.values().toArray(arrayDummies);
		BACITreeDataNode[] arrayDomains =
			new BACITreeDataNode[tempDomains.size()];
		tempDomains.values().toArray(arrayDomains);
		BACITreeDataNode[] retVal = new BACITreeDataNode[arrayDummies.length + arrayDomains.length];
		System.arraycopy(arrayDomains, 0, retVal, 0, arrayDomains.length);
		System.arraycopy(
				arrayDummies,
			0,
			retVal,
			arrayDomains.length,
			arrayDummies.length);
		notifier.reportDebug(
			"BACIRemoteAccess::explodeRootNodeByName",
			"Root nodes processing complete.");

		return retVal;
	}

	/**
	 * recursive function that builds the tree for the given dummy node's name.
	 * @param root			dummy node to which the subtree will be attached
	 * @param pathIndex		the subtree of which dummy should be found or created if not found
	 * @param names			final dummy's name <code>"dummy1/dummy2/.../finaldummy"</code>, broken into array
	 * 						<code>{"dummy1", "dummy2", ..., "finaldummy"}</code>
	 * @return
	 */
	public BACITreeDataNode getTreeForName(BACITreeDataNode root, int pathIndex, String[] names) {
		if (names == null) return null; //invalid name - do not continue
		if (pathIndex == names.length) {
			return root; //we're done processing because we reached the end of the names
		}

		/*
		 * Loop trough all the children of the root node and process each one that is not already a device
		 */
		for (int i = 0; i < root.childrenHolder.size(); i++) {
			BACITreeDataNode node = (BACITreeDataNode)root.childrenHolder.get(i);
			if (node instanceof BACIRemoteNode) continue;
			if (node.getName().compareTo(names[pathIndex]) == 0) {
				return getTreeForName(node, ++pathIndex, names); //we found a node that corresponds to the name[pathIndex], let's process it
			}
		}

		/*
		 * if the for loop exits, it means that the dummy node for the name[pathIndex] does not exist, so we must create one
		 */
		String curl = (String)root.getUserObject() + "/" + names[pathIndex]; //we calculate the curl for the new dummy node
		RemoteNodeCouple rnc = (RemoteNodeCouple)devices.get(curl); //get a device with the same curl if it exists

		//add our new dummy node to it's parent
		BACITreeDataNode newDummy = new BACITreeDataNode(DUMMY, names[pathIndex], curl, parent.getTreeByName(), getIcon(DOMAIN));
		newDummy.childrenHolder = new ArrayList();

		if (rnc != null && rnc.deviceByName != null) newDummy.childrenHolder.add(rnc.deviceByName); //also add the device node if it exists
		root.childrenHolder.add(newDummy);

		return getTreeForName(newDummy, ++pathIndex, names); //continue processing
	}

	/**
	 * Replaces dummy nodes which have one device node in them with the device node itself
	 * @param parent
	 * @param index
	 * @param node
	 */
	public void removeSingleDeviceDummies(BACITreeDataNode parent, int index, BACITreeDataNode node) {
		if (node.childrenHolder.size() > 1) {
			for (int i = 0; i < node.childrenHolder.size(); i++) {
				BACITreeDataNode tmpNode = (BACITreeDataNode)node.childrenHolder.get(0);
				if (tmpNode instanceof BACIRemoteNode) {
					continue;
				} else {
					removeSingleDeviceDummies(node, i, tmpNode);
				}
			}
		} else if (node.childrenHolder.size() == 1) {
			BACITreeDataNode tmpNode = (BACITreeDataNode)node.childrenHolder.get(0);
			if (tmpNode instanceof BACIRemoteNode) {
				parent.childrenHolder.remove(index);
				parent.childrenHolder.add(index, tmpNode);
				return;
			} else {
				removeSingleDeviceDummies(node, 0, tmpNode);
				return;
			}
		}
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 15:17:19)
	 * @param node si.ijs.acs.objectexplorer.engine.OETreeDataNode
	 */
	public OETreeNode[] explodeTreeNode(OETreeNode node) {
		if (node == null) // root node - search for types
			{
			notifier.reportMessage("Querying root nodes.");
			return explodeRootNodeByType();
		} else {
				switch (node.getNodeType()) {
				case DOMAIN :
					notifier.reportMessage(
						"Querying domain node children of '"
							+ node.getName()
							+ "'.");
					return explodeDomainNode((BACITreeDataNode) node);
				case TYPE :
					notifier.reportMessage(
						"Querying type node children of '"
							+ node.getName()
							+ "'.");
					return explodeTypeNode((BACITreeDataNode) node);
				case DEVICE :
					notifier.reportMessage(
						"Querying device node children of '"
							+ node.getName()
							+ "'.");
					return explodeDeviceNode((BACIRemoteNode) node);
				case DUMMY :
					notifier.reportMessage(
						"Querying device node children of '"
							+ node.getName()
							+ "'.");
					return explodeDummyNode((BACITreeDataNode) node);
				default :
					return new OETreeNode[0];
				}
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 17:00:09)
	 * @return si.ijs.acs.objectexplorer.engine.OETreeDataNode[]
	 */
	private synchronized OETreeNode[] explodeTypeNode(BACITreeDataNode node) {
		if (node == null)
			throw new NullPointerException("node");

		if (node.childrenHolder == null || node.childrenHolder.size() == 0)
			return new BACITreeDataNode[0];
		BACIRemoteNode[] retVal = new BACIRemoteNode[node.childrenHolder.size()];
		//-------------------------------------------
		//2010.02.05 panta@naoj
		MyComparator myComp = new MyComparator();
		//java.util.Collections.sort(node.childrenHolder); 
		java.util.Collections.sort(node.childrenHolder, myComp);
		//---------
		for (int i = 0; i < retVal.length; i++) {
			String curl = (String) node.childrenHolder.get(i);
			String cob = BACICURLResolver.resolveName(curl);
			if (cob == null)
				throw new IllegalArgumentException("component name is null " + curl);
			RemoteNodeCouple rnc = (RemoteNodeCouple)devices.get(curl);
			if (rnc != null) {
				retVal[i] = rnc.deviceByType;
			} else {
				retVal[i] = null;
				notifier.reportError(
				"BACIRemoteAccess::explodeTypeNode - Unexpected null pointer (rnc).");
				continue;
			}
		}
		notifier.reportDebug(
			"BACIRemoteAccess::explodeTypeNode",
			"Processing for node '" + node + "' complete.");
		return retVal;
	}

	public synchronized OETreeNode[] explodeDummyNode(BACITreeDataNode node) {
		if (node == null)
			throw new NullPointerException("node");
		if (node.childrenHolder == null) return new BACITreeDataNode[0];
		
		BACITreeDataNode[] retVal = new BACITreeDataNode[node.childrenHolder.size()];
		for (int i = 0; i < retVal.length; i++) {
			retVal[i] = (BACITreeDataNode)node.childrenHolder.get(i);
		}
		notifier.reportDebug(
			"BACIRemoteAccess::explodeDummyNode",
			"Processing for node '" + node + "' complete.");
		
		//2010.02.05 panta@naoj
		java.util.Arrays.sort(retVal);
		return retVal;
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (7.11.2000 22:32:06)
	 * @return si.ijs.acs.objectexplorer.engine.Attribute[]
	 */
	Attribute[] getAttributes(BACIRemote target) {

		String id = ((BACIRemote) target).getIFDesc().id;
		Attribute[] retVal = null;
		if (bufferDescs)
		    retVal = (Attribute[]) attributeIntrospected.get(id);
		if (retVal == null) {
			notifier.reportMessage(
				"Analysing attributes for '" + target.getName() + "'.");
			retVal = baciIntrospector.getAttributes(target);
			if (bufferDescs)
			    attributeIntrospected.put(id, retVal);
		} else {
			notifier.reportDebug(
				"BACIRemoteAccess::getAttributes",
				"Found attribute descriptions for '"
					+ target.getName()
					+ "' in attribute cache...");
		}
		Attribute[] rv = new Attribute[retVal.length];
		for (int i = 0; i < rv.length; i++) {
			if (retVal[i].getIntrospectable() == target)
				rv[i] = retVal[i];
			else
				rv[i] = new BACIAttribute((BACIAttribute) retVal[i], target);
		}
		Arrays.sort(rv, 0, rv.length, ATTRIBUTE_COMPARATOR);
		return rv;
	}

	private static final class BACIAttributeNameComparator implements Comparator<Attribute> {
		public int compare(Attribute o1, Attribute o2) {
			return o1.toString().compareTo(o2.toString()); 
		}
	}
	private static final Comparator<Attribute> ATTRIBUTE_COMPARATOR = new BACIAttributeNameComparator();
	
	/**
	 * Insert the method's description here.
	 * Creation date: (6/29/2001 11:01:21 AM)
	 * @return boolean
	 */
	public boolean getCaching() {
		return bufferDescs;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13.11.2000 22:30:17)
	 * @return si.ijs.acs.objectexplorer.engine.BACI.Dispatcher
	 */
	Dispatcher getDispatcher() {
		return dispatcher;
	}
	/**
	 * @see si.ijs.acs.objectexplorer.engine.RemoteAccess
	 */
	public javax.swing.JMenu getEngineMenu() {
		javax.swing.JMenu menu = new BACIMenu(this);
		menu.setVisible(true);
		baciEngineMenu = (BACIMenu)menu;
		return menu;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (24.4.2001 0:14:35)
	 * @return javax.swing.Icon
	 */
	Icon getIcon(short type) {
		javax.swing.Icon icon = null;

		java.net.URL url = null;
		if (type == DOMAIN || type == TYPE)
			url = getClass().getClassLoader().getResource("domain.gif");
		if (type == DEVICE)
			url = getClass().getClassLoader().getResource("device.gif");
		if (type == PROPERTY)
			url = getClass().getClassLoader().getResource("property.gif");
		if (type == TRANSIENT)
			url = getClass().getClassLoader().getResource("invocation.gif");

		if (url != null)
			icon = new javax.swing.ImageIcon(url);
		return icon;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (6.5.2001 14:59:46)
	 * @return org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
	 * @param id java.lang.String
	 */
	private FullInterfaceDescription getIFDesc(String id) {
	         FullInterfaceDescription fid = null;
		 if (bufferDescs)
		     fid = (FullInterfaceDescription) interfaceDescriptions.get(id);
		if (fid == null) {
			notifier.reportDebug(
				"BACIRemoteAccess::getIFDesc",
				"Querying IR for full interface description for id '"
					+ id
					+ "'.");
			InterfaceDef idef = InterfaceDefHelper.narrow(rep.lookup_id(id));
			if (idef == null)
				throw new IllegalStateException(
					"ID '" + id + "' was not found in the Repository.");
			fid = idef.describe_interface();

			if (bufferDescs)
			    interfaceDescriptions.put(id, fid);

		} else {
			notifier.reportDebug(
				"BACIRemoteAccess::getIFDesc",
				"Full interface description for id '"
					+ id
					+ "' was found in interface cache.");
		}
		return fid;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13.11.2000 18:18:34)
	 * @return si.ijs.acs.objectexplorer.engine.BACI.BACIIntrospector
	 */
	BACIIntrospector getIntrospector() {
		return baciIntrospector;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 15:02:45)
	 * @return si.ijs.acs.objectexplorer.engine.Invocation[]
	 */
	public Invocation[] getInvocations() {
		synchronized (invocations) {
			Invocation[] retVal = new Invocation[invocations.size()];
			invocations.toArray(retVal);
			return retVal;
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (13.11.2000 20:32:30)
	 */
	NotificationBean getNotifier() {
		return notifier;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 * @return si.ijs.acs.objectexplorer.engine.Operation[]
	 * @param i si.ijs.acs.objectexplorer.engine.Introspectable
	 */
	Operation[] getOperations(BACIRemote target) {
		if (target == null)
			throw new NullPointerException("target");

		String id = ((BACIRemote) target).getIFDesc().id;
		Operation[] retVal = null;
		if (bufferDescs)
		    retVal = (Operation[]) operationsIntrospected.get(id);
		if (retVal == null) {
			notifier.reportMessage(
				"Analysing operations for '" + target.getName() + "'.");
			retVal = baciIntrospector.getOperations(target);
			if (bufferDescs)
			    operationsIntrospected.put(id, retVal);
		} else {
			notifier.reportDebug(
				"BACIRemoteAccess::getOperations",
				"Found operation descriptions for '"
					+ target.getName()
					+ "' in operation cache...");
		}
		Operation[] rv = new Operation[retVal.length];
		for (int i = 0; i < rv.length; i++) {
			if (retVal[i].getIntrospectable() == target)
				rv[i] = retVal[i];
			else
				rv[i] = new BACIOperation((BACIOperation) retVal[i], target);
		}
		
		Arrays.sort(rv, 0, rv.length, OPERATION_COMPARATOR);
		return rv;
	}

	private static final class BACIOperationNameComparator implements Comparator<Operation> {
		public int compare(Operation o1, Operation o2) {
			return o1.getName().compareTo(o2.getName()); 
		}
	}
	private static final Comparator<Operation> OPERATION_COMPARATOR = new BACIOperationNameComparator();
	
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 21:18:02)
	 * @return si.ijs.acs.objectexplorer.engine.OETreeDataNode[]
	 */
	public OETreeNode[] getTreeRoots() {
		return explodeTreeNode(null);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 */
	public void initialize() {
		notifier.reportMessage("Starting engine initialization...");

		Properties props = System.getProperties();
		managerLoc = props.getProperty(MANAGER_CORBALOC);
		String IRloc = props.getProperty(IR_CORBALOC);
		if (managerLoc == null || IRloc == null) {
			CorbalocDialog dialog = new CorbalocDialog();
			if (managerLoc != null)
				dialog.setManagerFieldText(managerLoc);
			if (IRloc != null)
				dialog.setRepositoryFieldText(IRloc);
			dialog.show();
			if (dialog.isOKed()) {
				managerLoc = dialog.getManagerFieldText();
				IRloc = dialog.getRepositoryFieldText();
				ORBdebug = dialog.getDebugSelected();
			}
		}

		if (IRloc == null || "".equals(IRloc))
			throw new IllegalStateException(
				"'" + IR_CORBALOC + "' property is not set. Aborting...");
		if (managerLoc == null || "".equals(managerLoc))
			throw new IllegalStateException(
				"'" + MANAGER_CORBALOC + "' property is not set. Aborting...");

		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Startup using '" + MANAGER_CORBALOC + "' = '" + managerLoc + "'.");
		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Startup using '" + IR_CORBALOC + "' = '" + IRloc + "'.");

		String poolTime = props.getProperty(PROPERTY_POOL_TIMEOUT);

		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Startup using '" +PROPERTY_POOL_TIMEOUT + "' = '" + poolTime + "'.");

		// check it POOL_TIME has to be oveeriden
		if (poolTime != null)
		{
			try
			{
				int value = Integer.parseInt(poolTime);
				if (value >= POLL_SLEEP)
					POLL_TIMEOUT = value;
			}
			catch (Exception ex)
			{
				notifier.reportDebug(
					"BACIRemoteAccess::initialize",
					"Failed to parse '" + PROPERTY_POOL_TIMEOUT + "' property value '" + poolTime + "' as integer.");
			}
		}

		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Configuration: STRICT flag = " + strict);

		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Configuration: POOL_TIMEOUT = " + POLL_TIMEOUT + " ms.");

		// ORB stanza
		java.util.Properties orbprops = java.lang.System.getProperties();

		// to make code completely independed, properties have to be set using JVM -D mechanism

		// ORBacus
		//orbprops.put("org.omg.CORBA.ORBClass", "com.ooc.CORBA.ORB");
		//orbprops.put("org.omg.CORBA.ORBSingletonClass", "com.ooc.CORBA.ORBSingleton");

		// JacORB
		//orbprops.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
		//orbprops.put("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");

		// Java JDK (does not work)

		//if (ORBdebug)
		//       what to do here

		orb = org.omg.CORBA.ORB.init(new String[0], orbprops);

		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"ORB initialized.");

		// POA stanza -- use RootPOA
		POA rootPOA = null;
		try {
			rootPOA =
				POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		} catch (org.omg.CORBA.ORBPackage.InvalidName in) {
			throw new IllegalStateException("Cannot resolve RootPOA: " + in);
		}
		POAManager manager = rootPOA.the_POAManager();
		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"POA initialized.");
		try {
			manager.activate();
			orbThread = new Thread(this);
			orbThread.start();
		} catch (Exception e) {
			throw new IllegalStateException(
				"POAManager activation failed: " + e);
		}

		// resolve IR
		org.omg.CORBA.Object repRef = null;
		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Trying to resolve 'Repository'.");
		try {
			repRef = orb.string_to_object(IRloc);
			//	repRef = orb.resolve_initial_references("DefaultRepository");
		} catch (Exception e) {
			throw new IllegalStateException("Cannot access orb initial reference 'InterfaceRepository'.");
		}
		if (repRef == null)
			throw new IllegalStateException("Cannot resolve Interface Repository");
		rep = RepositoryHelper.narrow(repRef);
		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Obtained reference to 'Repository'.");
		notifier.reportMessage("Obtained reference to 'Repository'.");

		// clear callback descriptions list, because it is static!!
		synchronized (descriptions) {
			descriptions.clear();
			notifier.reportDebug(
				"BACIRemoveAccess::destroy",
				"Cleared IF descriptions list for CallbackImpl.");
		}

		// clear callback operation descriptions list, because it is static!!
		synchronized (operationListDescriptions) {
			operationListDescriptions.clear();
			notifier.reportDebug(
				"BACIRemoveAccess::destroy",
				"Cleared IF operation descriptions list for CallbackImpl.");
		}

		// start dispatcher
		dispatcher = new Dispatcher();
		dispatcher.start();
		notifier.reportDebug(
			"BACIRemoteAccess::initialize",
			"Started callback dispatcher thread.");

		// resolve manager
		resolveManager();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (2.11.2000 18:08:47)
	 * @return si.ijs.acs.objectexplorer.engine.Invocation
	 * @param target si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
	 * @param op si.ijs.acs.objectexplorer.engine.Operation
	 */
	private Invocation internalInvokeInvocation(
		//TODO update secondary tree???
		BACIRemote target,
		BACIOperation op,
		java.lang.Object[] params,
		RemoteResponseCallback cb) {
		int callbackIndex = baciIntrospector.getCallbackLocation(op);
		if (callbackIndex == -1)
			throw new IntrospectionInconsistentException(
				"Asynchronous operation '"
					+ op
					+ "' must take exactly one callback parameter.");

		/* create callback */
		CallbackImpl cbImpl = null;
		try {
			cbImpl =
				new CallbackImpl(
					op.getOperationDesc().parameters[callbackIndex].type.id(),
					cb);
		} catch (org.omg.CORBA.TypeCodePackage.BadKind bk) {
			throw new RemoteException(
				"Error while trying to analyze typecode: " + bk);
		}
		org.omg.CORBA.Object cbIF = cbImpl._this_object(orb);
		notifier.reportDebug(
			"BACIRemoteAccess::internalInvokeInvocation",
			"Activated callback object for '" + op + "'.");
		DataStruct desc = new DataStruct("IDL:alma/ACS/CBDescIn:1.0");
		desc.add("normal_timeout", new Long(1000000));
		desc.add("negotiable_timeout", new Long(5000000));
		desc.add("id_tag",new Integer(0));
		params[callbackIndex] = cbIF;
		if (callbackIndex == params.length - 1) {
			if (!isStrict())
				notifier.reportDebug(
					"BACIRemoteAccess::internalInvokeInvocation",
					"Non-strict BACI invocation signature: callback is not followed by a descriptor.");
			else
				throw new IntrospectionInconsistentException("Callback cannot be the last argument, because it must be followed by the descriptor.");
		} else
			params[callbackIndex + 1] = desc;

		BACIInvocation invoc =
			new BACIInvocation(
				TRANSIENT,
				op.getName(),
				null,
				cb,
				parent.getTree(),
				this);
		cbImpl.setInvocation(invoc);
		invoc.setRemoteCall(internalInvokeTrivial(target, op, params));
		synchronized (invocations) {
			invocations.add(invoc);
		}
		notifier.reportDebug(
			"BACIRemoteAccess::internalInvokeInvocation",
			"Remote call ended, constructed Invocation. Returning...");
		return invoc;
	}
	
	private static BACIRemoteNode getDeviceFromTarget(Object target)
	{
		if (target instanceof BACIRemoteNode)
		{
			BACIRemoteNode remoteNode = (BACIRemoteNode)target;
			if (remoteNode.isDevice())
				return remoteNode;
			else
				return getDeviceFromTarget(remoteNode.getParent());
			
		}
		else if (target instanceof BACIInvocation)
		{
			BACIInvocation invocation = (BACIInvocation)target;
			return getDeviceFromTarget(invocation.getParent());
		}
		else
			return null;
	}
	
	
	/**
	 * Insert the method's description here.
	 * Creation date: (2.11.2000 18:08:01)
	 * @return si.ijs.acs.objectexplorer.engine.Invocation
	 * @param target si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
	 * @param op si.ijs.acs.objectexplorer.engine.Operation
	 */
	private BACIRemoteCall internalInvokeTrivial(
		BACIRemote target,
		Operation op,
		java.lang.Object[] params) {
		if (target.getCORBARef() == null)
			throw new RemoteException(
				"Cannot invoke operation '"
					+ op.getName()
					+ "' on object '"
					+ target.getName()
					+ "' because it is not connected.");
		notifier.reportDebug(
			"BACIRemoteAccess::internalInvokeTrivial",
			"Preparing DII parameters for '"
				+ target.getName()
				+ "."
				+ op.getName()
				+ "()'.");
		java.lang.Object[] allArguments =
			baciIntrospector.prepareDIIparameters(
				((BACIOperation) op).getOperationDesc(),
				params);
		org.omg.CORBA.Object remote = target.getCORBARef();
		OperationDescription desc = ((BACIOperation) op).getOperationDesc();
		if (allArguments.length != desc.parameters.length)
			throw new IllegalStateException(
				"BACI introspector returned an array of values the length of which does not match CORBA parameter list, object = '"
					+ target.getName()
					+ "', operation = '"
					+ op.getName()
					+ "'.");
		StringBuffer buf = new StringBuffer(200);
		for (int i = 0; i < params.length; i++) {
			buf.append("'");
			buf.append(op.getParameterNames()[i]);
			buf.append("' = '");
			buf.append(params[i]);
			buf.append("' ");
		}
		notifier.reportDebug(
			"BACIRemoteAccess::internalInvokeTrivial",
			"Parameters for '" + op.getName() + "': " + buf.toString());
		notifier.reportMessage(
			"Invoking '"
				+ target.getName()
				+ "."
				+ op.getName()
				+ "()', parameters: "
				+ buf.toString());
		/* begin DII stanza */
		Request req = remote._request(op.getName());
		req.set_return_type(desc.result);

		/* set exceptions */
		org.omg.CORBA.ExceptionList exceptions = req.exceptions();

		org.omg.CORBA.ExceptionDescription[] exceptionsDesc = desc.exceptions;
		for (int i = 0; i < exceptionsDesc.length; i++)
		{
			// TAO IFR bug workaround
			if (exceptionsDesc[i].type.kind().value() != TCKind._tk_except)
			{
			        // System.out.println("--> Invalid user exception kind, fixing...");
				Class c = null;
				String className = null;
				try
				{
					className =	baciIntrospector.IDtoClassName(exceptionsDesc[i].type.id())	+ "Helper";
					c = Class.forName(className);
				} catch (Exception e)
				{
					throw new JavaIDLIntrospectionException(
						"Failed to load class '"
							+ className
							+ "'. Introspection failed on typedef argument: "
							+ e);
				}

				Class[] paramTypes = { };
				java.lang.Object[] paramVals = { };
				try
				{
					java.lang.Object retVal = c.getMethod("type", paramTypes).invoke(null, paramVals);
					exceptions.add((TypeCode) retVal);
				} catch (Exception e1)
				{
					throw new JavaIDLIntrospectionException(
						"Dynamic invocation of 'internalInvokeTrivial' failed on a typedef argument. Class instance: "
							+ c.getName()
							+ ". Exception:"
							+ e1);
				}
			}
			else
				exceptions.add(exceptionsDesc[i].type);
		}


		for (int i = 0; i < allArguments.length; i++) {
			Any argument = orb.create_any();
			argument.type(desc.parameters[i].type);
			if (desc.parameters[i].mode != ParameterMode.PARAM_OUT) {
				argument = baciIntrospector.insertAny(desc.parameters[i].type,argument,allArguments[i]);
				//baciIntrospector.displayAny(argument);
			}
			//org.omg.CORBA.ParameterMode.PARAM_xxx is defined in [0-2] and in org.jacorb.orb.ARG_xxx is defined in [1-3].
			req.arguments().add_value(desc.parameters[i].name, argument, desc.parameters[i].mode.value()+1);
		}

		// invoke request
		if (desc.mode == OperationMode.OP_ONEWAY) {
			notifier.reportDebug(
				"BACIRemoteAccess::internalInvokeTrivial",
				"Sending oneway request '"
					+ target.getName()
					+ "."
					+ op.getName()
					+ "()'...");
			try {
				req.send_oneway();
				return new BACIRemoteCall(
					target,
					(BACIOperation) op,
					params,
					null,
					null);
			} catch (Exception e) {
				notifier.reportError(
					"Exception during oneway remote invocation.",
					e);
				return new BACIRemoteCall(
					target,
					(BACIOperation) op,
					params,
					e);
			}

		} else {
			int time = 0;
			boolean errorResponse = false;
			notifier.reportDebug(
				"BACIRemoteAccess::internalInvokeTrivial",
				"Sending deferred request '"
					+ target.getName()
					+ "."
					+ op.getName()
					+ "()'.");
			try {
				req.send_deferred();
				while (!req.poll_response()) {
					try {
						Thread.sleep(POLL_SLEEP);
						time += POLL_SLEEP;
					} catch (InterruptedException ie) {
					}
					if (time > POLL_TIMEOUT) {
						notifier.reportError(
							"Timeout ("
								+ POLL_TIMEOUT
								+ " ms) while polling for response from '"
								+ op.getName()
								+ "' on '"
								+ target.getName()
								+ "'.");
						return new BACIRemoteCall(
							target,
							(BACIOperation) op,
							params,
							true);
					}
				}

				// check exception
				checkException(target, req);

				notifier.reportDebug(
					"BACIRemoteAccess::internalInvokeTrivial",
					"Received response for '"
						+ target.getName()
						+ "."
						+ op.getName()
						+ "()'.");
				//	req.get_response();
				Any anyRet = req.return_value();
				java.lang.Object oRet = null;
				if (anyRet != null)
					oRet = baciIntrospector.extractAny(anyRet);
				java.lang.Object[] outs =
					baciIntrospector.extractOuts(req, desc);

				// check for error-type ACSCompletion-s
				errorResponse = checkFromACSCompletion(oRet);
				for (int i = 0; i < outs.length; i++)
					errorResponse |= checkFromACSCompletion(outs[i]);


				if (target instanceof Invocation
					&& baciIntrospector.isInvocationDestroyMethod(op.getName()))
					new CBTimer((BACIInvocation) target).start();
				notifier.reportDebug(
					"BACIRemoteAccess::internalInvokeTrivial",
					"Successfully unpacked response for '"
						+ target.getName()
						+ "."
						+ op.getName()
						+ "()'.");

				BACIRemoteCall remoteCall = new BACIRemoteCall(
					target,
					(BACIOperation) op,
					params,
					oRet,
					outs);
				remoteCall.setErrorResponse(errorResponse);
				return remoteCall;
			} catch (Exception e) {
				notifier.reportError(
					"Exception during deferred remote invocation.",
					e);
				return new BACIRemoteCall(
					target,
					(BACIOperation) op,
					params,
					e);
			}
		}
		/* end DII stanza */

	}

	/**
	 * Logs ACSException.
	 * @param exceptionThrown
	 */
	private void logACSException(Exception exceptionThrown) {
		/* all user exception is expected to be using ACS Error System */
		if (exceptionThrown instanceof DataException) {
			DataException de = (DataException) exceptionThrown;
			/*
			 * This is just to probe if we have an ACS Exception.
			 * All applications should deal with ACS Exception and we want 
			 * to issue a warning if this is not the case.
			 * In this case we get an exception and the trap will log the warning.
			 */
			if(de.get("errorTrace") != null) {
				AcsJObjectExplorerReportEx objexpEx = new AcsJObjectExplorerReportEx(exceptionThrown);
				objexpEx.log(BACILogger.getLogger());
			} else {
				AcsJObjectExplorerReportEx notAnAcsEx = new AcsJObjectExplorerReportEx(exceptionThrown);
				notAnAcsEx.setContext("Logging a User Exception that is NOT an ACS Exception");
				notAnAcsEx.log(BACILogger.getLogger());
				
				notifier.reportDebug(
					"BACIRemoteAccess::logACSExcpetion",
					"Failed to wrap user exception into native ACS Error System exception"
						+ " for '" + de.id() + "'.");
			}
		} /* End if org.omg.CORBA.UserException */
		else if (exceptionThrown instanceof alma.acs.exceptions.AcsJException)
		{
			AcsJObjectExplorerReportEx objexpEx = new AcsJObjectExplorerReportEx(exceptionThrown);
			objexpEx.log(BACILogger.getLogger());
		}
		else if (exceptionThrown instanceof org.omg.CORBA.SystemException)
		{
			org.omg.CORBA.SystemException corbaSys = (org.omg.CORBA.SystemException)exceptionThrown;
			AcsJCORBAProblemEx corbaProblemEx = new AcsJCORBAProblemEx(exceptionThrown);
			corbaProblemEx.setMinor(corbaSys.minor);
			corbaProblemEx.setCompletionStatus(corbaSys.completed.value());
			corbaProblemEx.setInfo(corbaSys.toString());
			AcsJObjectExplorerReportEx objexpEx = new AcsJObjectExplorerReportEx(corbaProblemEx);
			objexpEx.log(BACILogger.getLogger());
		}
		else
		{
			AcsJUnexpectedExceptionEx unexpectedEx = new AcsJUnexpectedExceptionEx(exceptionThrown);
			AcsJObjectExplorerReportEx objexpEx = new AcsJObjectExplorerReportEx(unexpectedEx);
			objexpEx.log(BACILogger.getLogger());
			notifier.reportDebug(
					"BACIRemoteAccess::logACSException",
					"Received an unexpected exception: "
						+ " '"
						+ exceptionThrown.getClass().getName()
						+ "'.");
		}
	}

	/**
	 * Check if returned (out) parameter is type of ACSCompletion and
	 * if error log it
	 * @param param
	 */
	private boolean checkFromACSCompletion(java.lang.Object param) {
		// check if retVal is a completion
		if (param instanceof alma.ACSErr.Completion)
		{
			alma.ACSErr.Completion completion = (alma.ACSErr.Completion)param;
			// error completion
			if (completion.type != 0)
			{
				AcsJCompletion jcompletion = AcsJCompletion.fromCorbaCompletion(completion);
				if (jcompletion.getAcsJException() != null)
				{
					jcompletion.getAcsJException().log(BACILogger.getLogger());
					return true;
				}
			}
		}

		return false;
	}
	
	private int choiceForNonStickyComponentConnection(String message) {
		String[] choices = new String[] {
				"get a \"sticky references\" for just this component",
				"switch to \"sticky mode\" from now on",
				"cancel the operation"
		};
		int ans = JOptionPane.showOptionDialog(notifier.getParent(), 
				message, "Non-Sticky reference", JOptionPane.DEFAULT_OPTION,
				JOptionPane.WARNING_MESSAGE, null, choices, choices[0]);
		return ans;
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (2.11.2000 0:34:52)
	 * @param node si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
	 */
	private void internalManagerConnect(BACIRemoteNode baciNode) throws AcsJObjectExplorerConnectEx {
		/* we are connecting directly to the Manager to obtain the object reference */
		//System.out.println("DEBUG: imc "+baciNode);
		String curl = (String)baciNode.getUserObject();
		notifier.reportDebug(
			"BACIRemoteAccess::internalManagerConnect",
			"Requesting component: '" + curl + "', activate = true");
		notifier.reportMessage("Connecting to '" + curl + "'.");

		baciNode.setNonSticky(connectNonSticky);
		org.omg.CORBA.Object obj = null;
		if (connectNonSticky) {
			try {
				obj = manager.get_component_non_sticky(handle, curl);
			} catch (Throwable e) {
				String message = "Connection to component '" + curl + "' failed. \n" + 
								 "'Connect as non-sticky' mode is enabled: in this mode component will not be activated by ObjectExplorer,\n" +
								 "only already activated components can be accessed.";
				int ans = choiceForNonStickyComponentConnection(message);
				switch (ans) {
					case 0:
						baciNode.setNonSticky(false);
						System.out.println("BACIRemoteAccess.internalManagerConnect setNonSticky to " + baciNode.isNonSticky() + " @ " + baciNode.hashCode());
						break;
					case 1:
						baciEngineMenu.setNonSticky(false);
						baciNode.setNonSticky(false);
						break;
					case 2: {
					    AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx(e);
					    acsjex.setCurl(curl);
					    throw acsjex;
					}
					default : {
						// should not happen.
						notifier.reportError("Unexpected choice.");
						AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx(e);
						acsjex.setCurl(curl);
						throw acsjex;
					}
				}
			}
		}

		try {
			if (obj == null) {
				obj = manager.get_component(handle, curl, true);
			}
		    notifier.reportDebug("BACIRemoteAccess::internalManagerConnect",
					 "Manager returns OK");
		}
        /* 
         * We wrap into a specific exception and report up
         */
		catch(Throwable e)
		{
			notifier.reportError("Connection to component '" + curl + "' failed.");

		    AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx(e);
		    acsjex.setCurl(curl);
		    throw acsjex;
		}

		String irfid = null;
		try {
			notifier.reportDebug(
				"BACIRemoteAccess::internalManagerConnect",
				"Querying component '" + curl + "' for CORBA type id.");

			ComponentInfo[] cobInfos =
				manager.get_component_info(handle, new int[0], curl, "*", true);
			if (cobInfos.length != 1) {
			    AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx();
			    acsjex.setReason(
					"Manager did not return valid ComponentInfo for '"
						+ curl
						+ "'.");
			    throw acsjex;
			}
			irfid = cobInfos[0].type;

			notifier.reportDebug(
				"BACIRemoteAccess::internalManagerConnect",
				"component '" + curl + "' has id: " + irfid);
		}
		catch( Exception e)
		{
			notifier.reportError("Cannot retrieve Interface Repository ID for component '" 
					+ curl + "'", e);
		    AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx(e);
		    acsjex.setCurl(curl);
		    throw acsjex;
		}
		
//		baciNode.setNonSticky(connectNonSticky);
		baciNode.setCORBARef(obj);
		try {
			baciNode.setIFDesc(getIFDesc(irfid));
			if (baciNode.getIFDesc() != null)
				notifier.reportDebug(
					"BACIRemoteAccess::internalManagerConnect",
					"IR Query OK.");
			else
			{
			    AcsJObjectExplorerConnectEx acsjex = new AcsJObjectExplorerConnectEx();
			    acsjex.setCurl(curl);
			    acsjex.setReason(
					"Cannot retrieve Interface Repository description for component for '"
						+ curl
						+ "'.");
			    throw acsjex;
			}
		} catch (Exception e) {
			baciNode.setCORBARef(null);
			notifier.reportError(
				"Failed to retrieve interface description from IR, releasing component on Manager, if needed.",
				e);
			AcsJException releaseCompEx = null;
			try {
				if (manager != null && obj != null && !baciNode.isNonSticky()) {
					manager.release_component(handle, curl);
				}
			} catch (NoPermissionEx npe) {
				releaseCompEx = AcsJNoPermissionEx.fromNoPermissionEx(npe);
			} catch (CannotDeactivateComponentEx ex) { // @TODO remove this catch once we remove this ex from maci.idl
				releaseCompEx = AcsJCannotDeactivateComponentEx.fromCannotDeactivateComponentEx(ex);
			} catch (ComponentDeactivationUncleanEx ex) {
				releaseCompEx = AcsJComponentDeactivationUncleanEx.fromComponentDeactivationUncleanEx(ex);
			} catch (ComponentDeactivationFailedEx ex) {
				releaseCompEx = AcsJComponentDeactivationFailedEx.fromComponentDeactivationFailedEx(ex);
			}
			if (releaseCompEx != null) {
				notifier.reportError("Failed to release component", releaseCompEx);
				logACSException(releaseCompEx);
			}
		    AcsJObjectExplorerInterfaceRepositoryAccessEx acsjex = 
		    	new AcsJObjectExplorerInterfaceRepositoryAccessEx(e);
		    acsjex.setCurl(curl);
		    acsjex.setIRid(irfid);    
		    AcsJObjectExplorerConnectEx acsjex2 = new AcsJObjectExplorerConnectEx(acsjex);
		    acsjex2.setCurl(curl);
		    throw acsjex2;
		}	
		notifier.reportMessage("Connected to '" + curl + "'.");
	}

	public void synchronizeInternalParentConnect(BACIRemoteNode baciNode) {
		internalParentConnect(baciNode, false);
	}
	private void internalParentConnect(BACIRemoteNode baciNode) {
		internalParentConnect(baciNode, true);
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (2.11.2000 0:35:23)
	 * @param baciNode si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode
	 */
	private void internalParentConnect(BACIRemoteNode baciNode, boolean doSync) {
		/* we are using the parent to query the object as either an IDL attribute or
			   Java property accessor design pattern (returns Object type, takes no parameters)
		*/
		//	System.out.println("IPC");
		String targetName = null;
		TypeCode retType = null;
		String id = null;
		if (baciNode.getNodeType() == ATTRIBUTE) {
			AttributeDescription ad =
				(AttributeDescription) baciNode.getUserObject();
			try {
				id = ad.type.id();
			} catch (org.omg.CORBA.TypeCodePackage.BadKind bk) {
				throw new RemoteException(
					"IDL: BadKind thrown on type id lookup for PROPERTY child of the remote node: "
						+ bk);
			}
			targetName = BACIIntrospector.attributeNameToMethodName(ad.name);
			retType = ad.type;
			notifier.reportDebug(
				"BACIRemoteAccess::internalParentConnect",
				"Obtaining IDL attribute reference for '" + ad.name + "'.");
		} else if (baciNode.getNodeType() == PROPERTY) {
			OperationDescription od =
				(OperationDescription) baciNode.getUserObject();
			try {
				id = od.result.id();
			} catch (org.omg.CORBA.TypeCodePackage.BadKind bk) {
				throw new RemoteException(
					"IDL: BadKind thrown on type id lookup for PROPERTY child of the remote node: "
						+ bk);
			}
			targetName = od.name;
			retType = od.result;
			notifier.reportDebug(
				"BACIRemoteAccess::internalParentConnect",
				"Obtaining reference to contained object through property accessor design pattern for '"
					+ od.name
					+ ".");
		} else
			throw new IntrospectionInconsistentException(
				"Devices can contain objects only as IDL attributes or property accessor design patterns. Failed on '"
					+ baciNode
					+ "'.");
		BACIRemoteNode parentNode = (BACIRemoteNode) baciNode.getParent();
		if (parentNode.getCORBARef() == null) {
			parentNode.connect();
			if (parentNode.getCORBARef() == null)
				throw new RemoteException(
					"Child node is accessible although the parent node CORBA reference is null. Failed on '"
						+ baciNode
						+ ".");
		}

		/* begin DII stanza */
		Request req = parentNode.getCORBARef()._request(targetName);
		req.set_return_type(retType);
		notifier.reportDebug(
			"BACIRemoteAccess::internalParentConnect",
			"Invoking remote call...");
		req.invoke();
		Any returnValue = req.return_value();
		if (returnValue.type().kind() != TCKind.tk_objref)
			throw new IntrospectionInconsistentException(
				"Return type of '"
					+ targetName
					+ "' is not of type object reference, expected object reference because of BACI containment specifications.");
		baciNode.setCORBARef(returnValue.extract_Object());
		//

		baciNode.setIFDesc(getIFDesc(id));
		//
		if (baciNode.getCORBARef() != null)
			notifier.reportDebug(
				"BACIRemoteAccess::internalParentConnect",
				"Connection to contained object OK.");
		else
			notifier.reportError(
				"Reference returned when resolving contained object '"
					+ targetName
					+ "' is null.");
		/* end DII stanza */
	}

	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 13:00:27)
	 * @return si.ijs.acs.objectexplorer.engine.Invokation
	 * @param o si.ijs.acs.objectexplorer.engine.Operation
	 */
	java.lang.Object invoke(
		BACIRemote node,
		BACIOperation op,
		java.lang.Object[] explicitParams,
		RemoteResponseCallback cb) {
		if (node == null)
			throw new NullPointerException("node");
		if (op == null)
			throw new NullPointerException("op");
		if (explicitParams == null)
			throw new NullPointerException("explicitParams: must not be null, for void argument type return new Object[0]");

		if (!op.isInvocation())
			return internalInvokeTrivial(node, op, explicitParams);
		else {
			if (cb == null)
				throw new NullPointerException("cb");
			return internalInvokeInvocation(node, op, explicitParams, cb);
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (7.11.2000 22:52:30)
	 * @return si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteCall
	 * @param att si.ijs.acs.objectexplorer.engine.BACI.BACIAttribute
	 */
	BACIRemoteCall invokeAccessor(BACIAttribute att) {
		if (att == null)
			throw new NullPointerException("att");

		BACIRemote target = (BACIRemote) att.getIntrospectable();
		if (target.getCORBARef() == null)
			throw new RemoteException(
				"Cannot invoke attribute accessor '"
					+ att
					+ "' on object '"
					+ target.getName()
					+ "' because it is not connected.");

		org.omg.CORBA.Object remote = target.getCORBARef();
		AttributeDescription desc = att.getAttributeDesc();

		/* begin DII stanza */
		Request req =
			remote._request(
				BACIIntrospector.attributeNameToMethodName(desc.name));
		req.set_return_type(desc.type);
		java.lang.Object[] params = new java.lang.Object[0];

		int time = 0;
		notifier.reportMessage(
			"Invoking accessor '" + target.getName() + "." + att + "()'.");
		notifier.reportDebug(
			"BACIRemoteAccess::invokeAccessor",
			"Sending deferred attribute accessor '"
				+ target.getName()
				+ "."
				+ att
				+ "()'.");
		try {
			req.send_deferred();
			while (!req.poll_response()) {
				try {
					Thread.sleep(POLL_SLEEP);
					time += POLL_SLEEP;
				} catch (InterruptedException ie) {
				}
				if (time > POLL_TIMEOUT) {
					notifier.reportError(
						"Timeout ("
							+ POLL_TIMEOUT
							+ " ms) while polling for response from '"
							+ att
							+ "' on '"
							+ target.getName()
							+ "'.");
					return new BACIRemoteCall(
						target,
						att,
						params,
						null,
						true,
						null);
				}
			}
			
			// check exception
			checkException(target, req);
			
			notifier.reportDebug(
				"BACIRemoteAccess::invokeAccessor",
				"Received response for '"
					+ target.getName()
					+ "."
					+ att
					+ "()'.");
			//	req.get_response();
			Any anyRet = req.return_value();
			java.lang.Object oRet = null;
			if (anyRet != null)
				oRet = baciIntrospector.extractAny(anyRet);
			notifier.reportDebug(
				"BACIRemoteAccess::invokeAccessor",
				"Successfully unpacked response for '"
					+ target.getName()
					+ "."
					+ att
					+ "()'.");
			return new BACIRemoteCall(target, att, params, oRet, false, null);
		} catch (Exception e) {
			notifier.reportError(
				"Exception during deferred remote invocation.",
				e);
			return new BACIRemoteCall(target, att, params, null, false, e);
		}

		/* end DII stanza */
	}

	/**
	 * @param req
	 * @throws Exception
	 */
	private void checkException(Object target, Request req) throws Exception {
		Exception exceptionThrown = req.env().exception();
		if (exceptionThrown != null)
		{
			BACIRemoteNode device = getDeviceFromTarget(target);
			if (device != null && device.isNonSticky() && exceptionThrown instanceof OBJECT_NOT_EXIST)
			{
				// disconnect and provice nice error message
				try {
					device.disconnect();
				} catch (Throwable th) { /* noop, still report */ th.printStackTrace(); }
				throw new NonStickyComponentReleased("Non-sticky component released, disconnecting it automatically.");
			}
			else if (exceptionThrown instanceof org.omg.CORBA.UnknownUserException)
			{
				// without ID int the CDROutputStream (value field)
				Any exceptionValue = ((org.omg.CORBA.UnknownUserException)exceptionThrown).except;
				java.lang.Object userException = baciIntrospector.extractAny(exceptionValue);

				exceptionThrown = (Exception) userException;
			}

			// log ACS exception
			logACSException(exceptionThrown);

			throw exceptionThrown;
		}
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (7.11.2000 22:52:49)
	 * @return si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteCall
	 * @param att si.ijs.acs.objectexplorer.engine.BACI.BACIAttribute
	 */
	BACIRemoteCall invokeMutator(BACIAttribute att) {
		notifier.reportError(
			"Attribute mutators are not implemented yet: BACI does not specify any design pattern with mutable attributes.");
		return null;
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.12.2000 13:08:24)
	 * @return boolean
	 */
	boolean isStrict() {
		return "true".equals(strict);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (29.11.2000 0:36:08)
	 * @return org.omg.CORBA.InterfaceDef
	 * @param id java.lang.String
	 */
	public Contained lookupId(String id) {
		if (id == null)
			throw new NullPointerException("id");
		return rep.lookup_id(id);
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (1.11.2000 14:29:17)
	 */
	private void resolveManager() {
		if (managerLoc == null)
			throw new IllegalStateException("Cannot resolve manager corbaloc when it is null.");
		try {
			org.omg.CORBA.Object object = orb.string_to_object(managerLoc);
			notifier.reportDebug(
				"BACIRemoteAccess::resolveManager",
				"Manager reference string_to_object OK with managerLoc = '"
					+ managerLoc
					+ "'.");
			manager = ManagerHelper.narrow(object);
			notifier.reportDebug(
				"BACIRemoteAccess::resolveManager",
				"Manager reference narrowing OK.");
			notifier.reportMessage("Obtained reference to 'Manager'.");
			if (manager == null)
				throw new NullPointerException("Manager is null after ManagerHelper.narrow()");
		} catch (Exception e) {
			throw new RemoteException(
				"Could not resolve manager reference: " + e);
		}
		try {
			client = new ClientImpl();
			Client cIF = client._this(orb);
			notifier.reportDebug(
				"BACIRemoteAccess::resolveManager",
				"Instantiated Client servant.");
			ClientInfo info = manager.login(cIF);
			if (info == null)
				throw new Exception("Failed to login to the manager when returned ClientInfo is null.");
			handle = info.h;
			notifier.reportDebug(
				"BACIRemoteAccess::resolveManager",
				"Manager login OK.");
		} catch (Exception e1) {
			throw new RemoteException("Cannot login to the manager: " + e1);
		}

		new Thread(new Runnable() {
			public void run()
			{
				notifier.reportDebug(
						"BACIRemoteAccess::resolveManager",
						"Initializing remote logging...");
				ClientLogManager.getAcsLogManager().initRemoteLogging(orb, manager, handle, true);
				notifier.reportDebug(
						"BACIRemoteAccess::resolveManager",
						"Remote logging initialized.");
			}
		}, "InitRemoteLogging").start();
	}
	/**
	 * When an object implementing interface <code>Runnable</code> is used
	 * to create a thread, starting the thread causes the object's
	 * <code>run</code> method to be called in that separately executing
	 * thread.
	 * <p>
	 * The general contract of the method <code>run</code> is that it may
	 * take any action whatsoever.
	 *
	 * @see     java.lang.Thread#run()
	 */
	public void run() {
		notifier.reportDebug("BACIRemoteAccess::run", "Starting ORB thread...");
		orb.run();
	}
	/**
	 * Insert the method's description here.
	 * Creation date: (6/29/2001 10:59:00 AM)
	 */
	public void setCaching(boolean value) {
		bufferDescs = value;
	}

	/**
	 * @param connectNonSticky the connectNonSticky to set
	 */
	public void setConnectNonSticky(boolean connectNonSticky) {
		this.connectNonSticky = connectNonSticky;
	}
	
	public org.omg.DynamicAny.DynAnyFactory getDynFact() {
		try {
			return org.omg.DynamicAny.DynAnyFactoryHelper.narrow(orb.resolve_initial_references("DynAnyFactory"));
		}catch (org.omg.CORBA.ORBPackage.InvalidName e) {
			e.printStackTrace();
		}
		return null;
	}
}

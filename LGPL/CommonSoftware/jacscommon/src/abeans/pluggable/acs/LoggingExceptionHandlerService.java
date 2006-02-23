/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.logging.Level;

import javax.swing.tree.TreeNode;

import abeans.core.Component;
import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.CoreException;
import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.PolicyList;
import abeans.core.QoSProvider;
import abeans.core.ResourceLocation;
import abeans.core.Root;
import abeans.core.defaults.AbeansExceptionListener;
import abeans.core.defaults.DebuggableSupport;
import abeans.core.defaults.EHSExceptionTree;
import abeans.core.defaults.EHSReportLevel;
import abeans.core.defaults.EHSStackTrace;
import abeans.core.defaults.EHSWaitTimeForTree;
import abeans.core.defaults.ExceptionHandlerService;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.core.defaults.ExceptionTreeNode;
import abeans.core.defaults.MessageLog;
import abeans.core.defaults.MessageLogEntry;
import abeans.core.defaults.ResourceConstants;
import abeans.core.defaults.ThreadPoolService;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.util.CommonThrowable;
import com.cosylab.util.NameValueList;
import com.cosylab.util.ObjectList;

/**
 * TODO replace this implementation with DefaultExceptionHandlerService and handle logging via notification callback
 *
 * The default ACS implementation of exception handler service
 * heavily beased on <code>abeans.core.defaults.DefaultExceptionHandlerService</code>.
 * This implementation forwards all exceptions to <code>MessageLog</code> service.
 * 
 * This implementation of exception handler service
 * stores the exceptions in an array list in the order in which they
 * were reported to the service. This <code>java.util.List</code> also
 * constitutes the state of this service, that may be exchanged in the
 * state transfer with other services. The default processing of this
 * service is to integrate the reports and send them to:
 * <p>1. Resource location "Errors" if it is available.</p>
 * <p>2. Normal message log, if it is available.</p>
 * If none of these services is available when exception handler is
 * initializing, the installation of this service will fail. 
 * <p>
 * This service intercepts all instances of either
 * <code>AssertionFailed</code> or <code>CoreException</code>, because
 * they report to this service (if available) in their constructors.
 * Whenever an exception is constructed, this service waits for a certain
 * time interval (typically of the order of several seconds) and listens
 * for other exceptions. All exceptions caught in such time interval are
 * examined for interrelations (by traversing their <code>getCause()</code>
 * lists) and for correlated exceptions, a single exception report will be 
 * created. 
 * </p>
 * <p>
 * Note that this service returns all exceptions as instances of 
 * <code>Throwable</code> and not more specific class (for example
 * <code>CoreException</code>). The reason is the following: in the case
 * where one exception causes another, the first exception may be, for
 * example, one of the standard Java exceptions (like <code>IllegalStateException</code>).
 * The users of this service (for example visualization tools) should, on each
 * exception, try <code>instanceof com.cosylab.util.CommonThrowable</code> test before
 * casting. If the test succeeds, a lot of additional information can be
 * obtained through <code>CommonThrowable</code> interface.
 * </p>
 * <p>
 * This service implementation should not create excessive logs to prevent
 * recursion in log generation in case exceptions are thrown in the logging
 * service.
 * </p>
 * <p>
 * For integrating exceptions in a separate thread this service will use 
 * <code>ThreadPoolService</code> if it is available; otherwise new thread 
 * will be launched for each exception.
 * </p>
 * 
 * @author		Gasper Tkacik (gasper.tkacik@cosylab.com)
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		abeans.core.CoreException
 * @see		abeans.core.AssertionFailed
 */
public class LoggingExceptionHandlerService extends ComponentSupport implements ExceptionHandlerService , QoSProvider
{
	
	private ComponentDescriptor cdesc = null;
	private ArrayList rootExceptions = new ArrayList();
	private HashMap exceptionToNode = new HashMap();
	private SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");
	private ObjectList exceptionListeners = new ObjectList(AbeansExceptionListener.class);
	// defualts for policies
	private long    waitTimeForExceptionTree = 2000;
	private int     reportLevel = 1; // like verbose level 
	private boolean showStackTrace = true;
	private boolean showExceptionTree = false;
	private Class[] supportedPolicies = { EHSReportLevel.class, EHSStackTrace.class, EHSWaitTimeForTree.class, EHSExceptionTree.class };

	/**
	 * A simple implementation of the <code>Enumeration</code> interface.
	 * Used by the <code>NodeImpl</code>. Iterates over an array of objects.
	 */
	private class ArrayEnum implements Enumeration
	{
		private Object[] array = null;
		private int index = 0;
		
		/**
		 * Constructs a new iteration for the specified array argument.
		 * 
		 * @param array the array to be iterated over
		 */	
		public ArrayEnum(Object[] array)
		{
			this.array = array;
		}
		
		/**
		 * Returns the next element of the array.
		 * 
		 * @return							next element
		 * @throws	NoSuchElementException	if no next element is available, because end of 
		 * 									the array has been reached
		 */
		public Object nextElement()
		{
			if (array == null || index >= array.length) throw new NoSuchElementException();
			Object retVal = array[index];
			index++;
			return retVal;
		}
		
		/**
		 * Returns <code>true</code> iff there are more elements in the array.
		 * On <code>true</code>, <code>nextElement()</code> can be called without
		 * raising an exception.
		 * 
		 * @return <code>true</code> iff there are more elements available
		 */
		public boolean hasMoreElements()
		{
			if (array == null) return false;
			if (index >= array.length) return false;
			else return true;
		}
	}
	
	/**
	 * A simple implementation of the tree node that, as a data item, 
	 * contains an throwable instance, and stores its children in an
	 * array.
	 */
	private class NodeImpl implements ExceptionTreeNode
	{
		private NodeImpl[] nodes = new NodeImpl[0];
		private NodeImpl parent = null;
		private Throwable exception = null;
		
		/**
		 * Used by the enclosing class to determine if the exception report for
		 * this node has already been created.
		 */
		boolean processed = false;
		
		/**
		 * Timer thread used by the enclosing class that times this integrating
		 * period for this exception node.
		 */
		Object timer = null;
		
		
		/**
		 * Creates a new exception tree node.
		 * 
		 * @param parent		the parent node of this node, contains the parent exception, 
		 * 						can be <code>null</code> if the node has no parent
		 * @param exception	data carried by this node, non-<code>null</code>
		 */
		public NodeImpl(NodeImpl parent, Throwable exception)
		{
			assert (exception != null);
			
			this.parent = parent;
			this.exception = exception;
		}

		/**
		 * Inserts a new node child into this node.
		 * 
		 * @param node the new node to be inserted, non-<code>null</code>, not <code>this</code>
		 */
		public void addChild(NodeImpl node)
		{
			assert (node != null);
		//	assert (node.getParent() != this);
			
			synchronized(nodes)
			{
				NodeImpl[] nnodes = new NodeImpl[nodes.length + 1];
				System.arraycopy(nodes, 0, nnodes, 0, nodes.length);
				nnodes[nodes.length] = node;
				nodes = nnodes;
			}
		}
		
		/**
		 * Returns <code>false</code>, all nodes can have children.
		 * 
		 * @return <code>false</code>
		 */
		public boolean isLeaf()
		{
			return false;
		}
		
		/**
		 * Returns the enumeration of this node's children.
		 * 
		 * @return enumeration
		 */
		public synchronized Enumeration children()
		{
			return new ArrayEnum(nodes);
		}
		
		/**
		 * Returns the parent node specified as the constructor argument.
		 * 
		 * @return the parent tree node of this
		 */
		public TreeNode getParent()
		{
			return parent;
		}
		
		/**
		 * Returns the number of children of this node.
		 * 
		 * @return the number of children
		 */
		public synchronized int getChildCount()
		{
			return nodes.length;
		}
		
		/**
		 * Returns <code>true</code>.
		 *
		 * @return <code>true</code>
		 */
		public boolean getAllowsChildren()
		{
			return true;
		}
		
		/**
		 * Returns the data carried by this node, which is the instance of throwable.
		 * 
		 * @return the data item of this node
		 */
		public Throwable getException()
		{
			return exception;
		}
		
		/**
		 * Returns the child at a given position in this node. 
		 * 
		 * @param	index	integer between 0 and <code>getChildCount()</code>, specifying
		 * 					the child of this node
		 * @return			the node with a given index 
		 * @throws	ArrayIndexOutOfBoundsException
		 * 					if the index is invalid
		 */
		public synchronized TreeNode getChildAt(int index)
		{
			return nodes[index];
		}
		
		/**
		 * Searches the array of child nodes for a specified node to obtain its index.
		 * 
		 * @param node	the node to search
		 * @return 	the index of the node or -1 if the node is not contained in this 
		 * 				instance
		 */
		public synchronized int getIndex(TreeNode node)
		{
			for (int i = 0; i < nodes.length; i++)
			{
				if (nodes[i] == node) return i;
			}
			return -1;
		}
		
        /**
         * @see abeans.core.defaults.ExceptionTreeNode#isIgnored()
         */
        public boolean isIgnored()
        {
            return false;
        }
	}
	
	/**
	 * Default constructor as prescribed by component interface.
	 * Delegates to super constructor.
	 */
	public LoggingExceptionHandlerService() 
	{
		super("LoggingExceptionHandlerService", "ACSExHSrv", Identifier.CORE);
		cdesc = new ComponentDescriptor(getClass(), ExceptionHandlerService.class, 1, "Exception handler collects exceptions and generates appropriate log.", false, true, null);
		
	}
	
	/**
	 * Creates the exception report by extracting the data from the
	 * exception. Additional information may be appended by the
	 * handler service, for instance information concerning the username,
	 * current host, current application etc. The report will be
	 * delivered either to the "Error" resource location or to the 
	 * message log.
	 * 
	 * @param 	ex	exception for which the report will be generated, non-<code>null</code>
	 */
	public String createExceptionReport(Throwable ex)
	{
		assert (ex != null);
		
		if (isDebug()) new MessageLogEntry(this, "createExceptionReport", new Object[] { ex }).dispatch();
		
		String retVal = createSingleExceptionReport(ex);
		
		if (isDebug()) new MessageLogEntry(this, "createExceptionReport", "Exiting.", Level.FINEST).dispatch();
		
		return retVal;
	}
	
	/**
	 * Prefixes each line stored in the string buffer array with the
	 * string specified by <code>prefix</code>.
	 * 
	 * @param sb		the array in which each line (delimeted by newline character)
	 * 					will be prefixed
	 * @param prefix	the string inserted after each newline character into string
	 * 					buffer
	 */
	private StringBuffer insertPrefix(StringBuffer sb, String prefix)
	{
		for (int i = 0; i < sb.length() - 1; i++)
		{
			char current = sb.charAt(i);
			if (current == '\n') sb.insert(i + 1, prefix.toCharArray());
		}
		return sb;
	} 
	
	/**
	 * Creates an exception report.
	 * 
	 * @param t	exception for which the report will be generated, non-<code>null</code>
	 */
	private String createSingleExceptionReport(Throwable t) 
	{
		assert (t != null);

		if (!(t instanceof CommonThrowable))
		{
			StringBuffer sbuf = new StringBuffer(100);
			sbuf.append(t.getMessage());

			sbuf.append("\n\tType: ");
			sbuf.append(t.getClass().getName());
			if(showStackTrace) {
				StringWriter sw = new StringWriter();
				PrintWriter pw = new PrintWriter(sw);
				t.printStackTrace(pw);
				return new String("\t" + insertPrefix(sw.getBuffer(), "\t"));
			}
			return new String(sbuf);
		}
		else
		{
			CommonThrowable cx = (CommonThrowable)t;
			StringBuffer sbuf = new StringBuffer(2500);
			
			sbuf.append(cx.getMessage());

			sbuf.append("\n\tType: ");
			sbuf.append(cx.getClass().getName());
			sbuf.append("\n\tTimestamp: ");
			sbuf.append(sdf.format(new Date(cx.getTimestamp())));

			if (cx.getCaughtIn()!=null && cx.getCaughtIn().length()>0)
			{
				sbuf.append("\n\tCaught in: ");
				sbuf.append(cx.getCaughtIn());
			}

			Map keyValueMap = cx.getValues();
			Iterator keysIter = cx.getValues().keySet().iterator();
			while (keysIter.hasNext())
			{
				try
				{
					Object key = keysIter.next();
					sbuf.append("\n\t");
					sbuf.append(key);
					sbuf.append(" = ");
					Object vl = keyValueMap.get(key);
					sbuf.append(vl);
				} catch (Exception e)
				{
					sbuf.append("Exception while invoking 'toString()'");
				}
			}
			
            if( reportLevel > 1 ) {
				sbuf.append("\n");
				sbuf.append("\tHost: ");
				sbuf.append(cx.getHost());
				sbuf.append("\n");
				sbuf.append("\tThread: ");
				sbuf.append(cx.getThread().getName());
				sbuf.append("\n");
				sbuf.append("\tInstance: ");
				if (cx.getInstance() instanceof Identifiable)
					sbuf.append(((Identifiable)cx.getInstance()).getIdentifier().getQualifiedLongName());	
				else
					sbuf.append(cx.getInstance());
				sbuf.append("\n");
				sbuf.append("\tValues: \n");
				Iterator i = cx.getValues().keySet().iterator();
				while (i.hasNext())
				{
					String key = (String)i.next();
					Object vl = cx.getValues().get(key);
					if (vl instanceof DebuggableSupport)
					{
						sbuf.append("\n\t\tInternal state:\n\t\t");
						try
						{
							sbuf.append(((DebuggableSupport)vl).toDebugString());
						} catch (Exception e)
						{
							sbuf.append("Exception while invoking 'toDebugString()'");
						}
						sbuf.append("\n");
					}
				}
            }
            if( showStackTrace ) {
				StringWriter sw = new StringWriter();
				PrintWriter pw = new PrintWriter(sw);
				sbuf.append("\n\tStack trace: ");
				if (cx instanceof Exception) ((Exception)cx).printStackTrace(pw);
				sbuf.append(insertPrefix(sw.getBuffer(), "\t\t"));
			}
			sbuf.append("\n");
			return new String(sbuf);
		}
	}
	
	/**
	 * Creates an exception report.
	 * 
	 * @param t	exception for which the report will be generated, non-<code>null</code>
	 * @return		timestamp of generated exception (if not deteminate, closesTimeStamp should be returned)
	 */
	private long generateSingleExceptionMessageLog(Throwable t, long closestTimeStamp, String stackId, int level) 
	{
		assert (t != null);
	


		if (!(t instanceof CommonThrowable))
		{
			MessageLogEntry mle = new MessageLogEntry(this, t.getMessage(), t, LoggingLevel.ERROR);

			Map params = new HashMap();
	
			// no line info
			// map.put("Line", new Long(0));
	
			params.put("StackId", stackId);
			params.put("StackLevel", new Long(level));

			Map data = new NameValueList(1);
			data.put("exceptionType", t.getClass().toString());
			
			params.put("Data", data);

			mle.setParameters(new Object[] { params });
	
			mle.setMillis(closestTimeStamp);
			mle.setThreadID(-1);
			mle.setSourceClassName(null);
			mle.setSourceMethodName(null);
			
			mle.dispatch();
			
			return closestTimeStamp;
		}
		else
		{
			CommonThrowable cx = (CommonThrowable)t;
			
			Identifiable source = this;
			if (cx.getInstance() instanceof Identifiable)
				source = (Identifiable)cx.getInstance();
			
			MessageLogEntry mle = new MessageLogEntry(source, cx.getMessage(), t, LoggingLevel.ERROR);
			Map params = new HashMap();
	
			// no line info
			// map.put("Line", new Long(0));
	
			params.put("StackId", stackId);
			params.put("StackLevel", new Long(level));
	
			if (cx.getThread()!=null &&
				cx.getThread().getName()!=null &&
				cx.getThread().getName().length() > 0)
				params.put("ThreadName", cx.getThread().getName());

			Map data = new HashMap();

			if (cx.getValues() != null)
				data.putAll(cx.getValues());
			
			data.put("exceptionType", t.getClass().toString());
			
			params.put("Data", data);

			mle.setParameters(new Object[] { params });

			mle.setMillis(cx.getTimestamp());
			mle.setThreadID(-1);		// thread name is used instead

			boolean caughtInSet = false;
			String caughtIn = cx.getCaughtIn();
			if (caughtIn != null)
			{
				// expected format: "className::methodName"
				int firstColon = caughtIn.indexOf(':');
				if (firstColon>0 && caughtIn.charAt(firstColon+1)==':' && firstColon+2<caughtIn.length())
				{
					mle.setSourceClassName(caughtIn.substring(0, firstColon));
					mle.setSourceMethodName(caughtIn.substring(firstColon+2));
					caughtInSet = true;
				}
			}
			
			if (!caughtInSet)
			{
				mle.setSourceClassName(null);
				mle.setSourceMethodName(null);
			}
			mle.dispatch();
			
			return cx.getTimestamp();
			
		}
	}

	/**
	 * This method should be invoked by the exception in its constructor
	 * after the internal state of the exception has been set. In response, the
	 * exception handler may query the exception immediately for the report
	 * generation, or it may store the exception for later processing. Do not
	 * generate log entries, because this may cause infinite loops if the exception
	 * is instantiated in the loggers.
	 *
	 * @param ex 	exception that has been instantiated, must extend
	 * 				<code>Throwable</code>, non-<code>null</code>
	 */
	public void exceptionInstantiated(CommonThrowable ex) 
	{
		assert (ex != null);
		if (!(ex instanceof Throwable)) throw new IllegalArgumentException("ex does not extend 'Throwable'.");
	
		if (isDebug()) new MessageLogEntry(this, "exceptionInstantiated", new Object[] { ex }).dispatch();
		
		NodeImpl node = (NodeImpl)exceptionToNode.get(ex);
		if (node != null)
		{
			if (isDebug()) new MessageLogEntry(this, "exceptionInstantiated", "Exiting.", Level.FINEST).dispatch();
			return;
		}
		Throwable parent = ex.getParent();
		
		/* recursion boundary conditions */
		if (parent == null)
		{
			if (exceptionToNode.get(ex) == null)
			{
				NodeImpl tnode = new NodeImpl(null, (Throwable)ex);
				synchronized(rootExceptions)
				{
					rootExceptions.add(tnode);
				}
				synchronized(exceptionToNode)
				{
					exceptionToNode.put(ex, tnode);
				}
				resetTimer(tnode);
			}
			if (isDebug()) new MessageLogEntry(this, "exceptionInstantiated", "Exiting.", Level.FINEST).dispatch();
			return;
		}
		if (!(parent instanceof CoreException) && exceptionToNode.get(parent) == null)
		{
			NodeImpl tnode = new NodeImpl(null, parent);
			synchronized(rootExceptions)
			{
				rootExceptions.add(tnode);
			}
			synchronized(exceptionToNode)
			{
				exceptionToNode.put(parent, tnode);
			}
			resetTimer(tnode);
		}
		else if (exceptionToNode.get(parent) == null)
		{
			// recurse
			exceptionInstantiated((CommonThrowable)parent);
		}
	
		/* all parents have now been added, add this */
		NodeImpl parentNode = null;
		synchronized(exceptionToNode)
		{
			parentNode = (NodeImpl)exceptionToNode.get(parent);
		}
		NodeImpl newNode = new NodeImpl(parentNode, (Throwable)ex);
		synchronized(exceptionToNode)
		{
			exceptionToNode.put(ex, newNode);
		}
		parentNode.addChild(newNode);
		resetTimer(newNode);
		if (isDebug()) new MessageLogEntry(this, "exceptionInstantiated", "Exiting.", Level.FINEST).dispatch();
	}
	
	
	/**
	 * Finds the root node for a given node.
	 * 
	 * @param	node	the node for which the parent is to be found, non-<code>null</code>
	 * @return			the parent of the <code>impl</code> parameter, or <code>null</code>
	 * 					if the parent does not exist
	 */
	private NodeImpl findParent(NodeImpl node) 
	{
		assert (node != null);
		
		NodeImpl recursive = node;
		while (recursive.getParent() != null)
		{
			recursive = (NodeImpl)recursive.getParent();
		}
		return recursive;
	}
	
	/**
	 * Returns an instance of the <code>ComponentDescriptor</code>
	 * that parametrizes this component. The component should
	 * create an immutable descriptor instance only once and always
	 * return the same instance.
	 *
	 * @return a description of this component
	 */
	public ComponentDescriptor getComponentDescriptor() 
	{
		return cdesc;
	}
	
	/**
	 * The service returns all of the exceptions that have
	 * been reported through <code>exceptionInstantiated()</code> since
	 * the installation of the service. Notice that this also includes
	 * exceptions created, thrown, caught and handled.
	 * 
	 * @return	a list of all exceptions thrown during the lifetime of this
	 *         	service
	 */
	public Throwable[] getExceptions() 
	{
		synchronized(exceptionToNode)
		{
			Throwable[] retVal = new Throwable[exceptionToNode.keySet().size()];
			exceptionToNode.keySet().toArray(retVal);
			return retVal;
		}
	}
	
	/**
	 * Returns all "first" exceptions, i.e. exceptions that do not
	 * declare any parent throwable (see <code>CoreException</code>
	 * documentation for details). It is possible then to
	 * browse the exception tree by doing traversals of the tree nodes
	 * and hence discovering which further exceptions have been caused
	 * by the <code>this</code> exception tree node.
	 *
	 * @return a list of root "first" exceptions
	 */
	public ExceptionTreeNode[] getRootExceptions() 
	{
		synchronized(rootExceptions)
		{
			ExceptionTreeNode[] retVal = new ExceptionTreeNode[rootExceptions.size()];
			rootExceptions.toArray(retVal);
			return retVal;
		}
	}
	
	/**
	 * Initializes the component. This includes examination of other installed 
	 * components to determine if message log or error resource location are available.
	 * If none are availabe, the installation fails.
	 * 
	 * @param	manager	manager into which this component will be instantiated, non-<code>null</code>
	 * @param	state	must be <code>null</code>, this component does not support state transfer
	 * @param	cdesc	must be <code>null</code>, this component does not support state transfer
	 * @throws	IllegalComponentStateException
	 * 					when the state or descriptor parameters are not <code>null</code>
	 * @throws	ComponentInitializationException
	 * 					when neither the message log nor error resource location are present
	 */
	public void initialize(ComponentManager manager, Object state, ComponentDescriptor cdesc) throws ComponentInitializationException, IllegalComponentStateException {
		if (manager == null) throw new ComponentInitializationException(this, "Parameter 'manager' passed to initialize() was null.");
		if (/*cdesc != null ||*/ state != null) 
		{
			IllegalComponentStateException ce = new IllegalComponentStateException(this, "Cannot interpret a non-null component state.");
			ce.getValues().put("cdesc", cdesc);
			ce.getValues().put("state", state);
			throw ce;
		}
		Component log = Root.getComponentManager().getComponent(MessageLog.class);
		ResourceLocation resLoc = Root.getLoaderManager().getResourceLocation(ResourceConstants.ERROR_RESOURCE_LOCATION);
		
		if (log == null && resLoc == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Initialization of 'LoggingExceptionHandlerService' failed because both 'MessageLog' and '" + ResourceConstants.ERROR_RESOURCE_LOCATION + "' are missing.");
			cie.putValue("manager", manager);
			cie.setHelpID("exception_default_exhandler_no_target");
			throw cie;
		}
		
		// install AFTER checking for exceptions, so that the exceptions thrown above are not
		// forwarded to this instance, if it is improperly installed
		setParent(manager);
	}
	
	/**
	 * Starts the timer from when the exception is thrown until the timer expires.
	 * During this time exceptions are integrated and correlated. After the timer
	 * expires, the exception report is created and delivered to target (message
	 * log or error resource location).
	 * 
	 * @param	node	the node which is to be timed, non-<code>null</code>
	 */
	private void resetTimer(NodeImpl node) 
	{
		class Timer implements Runnable
		{
			NodeImpl node = null;
			
			public Timer(NodeImpl node)
			{
				assert (node != null);
				this.node = node;
			}
			public void run()
			{
				
				try
				{
					Thread.sleep(waitTimeForExceptionTree);
				} catch (InterruptedException ie)
				{
				}
				NodeImpl root = findParent(node);
				if (root.timer == this)
				{
					notifyListeners( node );		
				}
			}
		}
		
		assert (node != null);
		
		// try to obtain thread pool service
		if (Root.isInitialized())
		{
			ThreadPoolService tps = (ThreadPoolService)Root.getComponentManager().getComponent(ThreadPoolService.class);
			if (tps != null)
			{
				NodeImpl parent = findParent(node);
				Timer t = new Timer(node);
				parent.timer = t;
				try
				{
					tps.execute(t);
					return;
				} catch (InterruptedException ie)
				{
					// do nothing, will be processed by a separate thread anyway
				}
			}
		}
		
		// else execute by launching a separate thread
		Timer t = new Timer(node);
		Thread tt = new Thread(t, "LoggingExceptionHandlerService");
		NodeImpl parent = findParent(node);
		parent.timer = t;
		tt.start();
	}
	
	public void addExceptionListener(AbeansExceptionListener listener) 
	{
		if (listener == null) return;
		
		exceptionListeners.add(listener);
	}

	public void removeExceptionListener(AbeansExceptionListener listener) 
	{
		if (listener == null) return;
		
		exceptionListeners.remove(listener);
	}

	public AbeansExceptionListener[] getExceptionListeners() 
	{
		return (AbeansExceptionListener[])exceptionListeners.toArray();
	}
	
	public void notifyListeners(NodeImpl node) {

		NodeImpl root = findParent(node);

		// cleanup
		NodeImpl temp = root;
		while (temp != null)
		{
			// remove node
			synchronized(rootExceptions)
			{
				rootExceptions.remove(temp);
			}
			synchronized(exceptionToNode)
			{
				exceptionToNode.remove(temp.exception);
			}

			if( temp.getChildCount() == 0 )
				break;
			temp = (NodeImpl)temp.getChildAt(0);
		}

		// if this exception is handled then do nothing
		if( node.getException() instanceof ExceptionIgnorer ) {
			if (isDebug())
				new MessageLogEntry(this, "Handled exception", root.getException().getMessage(), Level.FINEST).dispatch();
			return;
		}
					
		// generic java exceptions do not provide timestamp information, try to guess it
		long closestTimeStamp = System.currentTimeMillis() - waitTimeForExceptionTree;

		temp = root;

		// create report as it is defined by installed policies
		String result = "";
		int count = 0;
		while (temp != null)
		{
			result = result + "-------------------->( " + count + " )<--------------------\n";
			result = result + createExceptionReport(temp.getException());
			if( temp.getChildCount() == 0 )
				break;
			temp = (NodeImpl)temp.getChildAt(0);

			// generate logs
			closestTimeStamp = generateSingleExceptionMessageLog(temp.getException(), closestTimeStamp, String.valueOf(System.currentTimeMillis()), count);

			count++;
			if( !showExceptionTree )
				break;
		}
		result = result + "--------------------------------------------------\n";

		// if we are destroyed then user will never see any message that is still in queue
		if( isDestroyed() ) {
			System.err.println( result );
		}
		else
			// log but w/ special logging level  
			new MessageLogEntry(LoggingExceptionHandlerService.this, "resetTimer", "Exception report:\n" + result, root.getException(), ExceptionReportLevel.EXCEPTION).dispatch();

		// listeners
		AbeansExceptionListener[] listeners = getExceptionListeners();
		for( int i=0; i<listeners.length; i++ ) {
			listeners[i].reportException(node.getException());
		}
	}
	
	/**
	 * Returns a short summary about this instance.
	 * 
	 * @return internal state of this
	 */
	public String toString()
	{
		return "LoggingExceptionHandlerService = { rootExceptions.size()=" + rootExceptions.size() + " } " + super.toString();
	}
	
	/**
	 * No not work in debug mode at all (generates to much overhead).
	 * 
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

	/**
	 * @see abeans.core.QoSProvider#getSupportedPolicies()
	 */
	public Class[] getSupportedPolicies() {
		return supportedPolicies;
	}

	/**
	 * @see abeans.core.QoSProvider#installPolicies(abeans.core.PolicyList)
	 */
	public PolicyList installPolicies(PolicyList policies) {
		assert (policies != null);
		
		PolicyList retVal = new PolicyList();
		
		EHSReportLevel levelPolicy = (EHSReportLevel)policies.getPolicy(EHSReportLevel.NAME);
		if (levelPolicy != null)
		{
			reportLevel = levelPolicy.getValue(); 
			retVal.addPolicy(levelPolicy);
		}

		EHSStackTrace stPolicy = (EHSStackTrace)policies.getPolicy(EHSStackTrace.NAME);
		if (stPolicy != null)
		{
			showStackTrace = stPolicy.getValue();
			retVal.addPolicy(stPolicy);
		}

		EHSWaitTimeForTree wtPolicy = (EHSWaitTimeForTree)policies.getPolicy(EHSWaitTimeForTree.NAME);
		if (wtPolicy != null)
		{
			waitTimeForExceptionTree = wtPolicy.getValue();
			retVal.addPolicy(wtPolicy);
		}

		EHSExceptionTree etPolicy = (EHSExceptionTree)policies.getPolicy(EHSExceptionTree.NAME);
		if (etPolicy != null)
		{
			showExceptionTree = etPolicy.getValue();
			retVal.addPolicy(etPolicy);
		}

		return retVal;
	}

	/**
	 * @see abeans.core.QoSProvider#refreshHierarchicalPolicies()
	 */
	public void refreshHierarchicalPolicies() {
		EHSReportLevel levelPolicy = (EHSReportLevel)Root.getPolicyManager().getEffectivePolicy(this, EHSReportLevel.NAME);
		if (levelPolicy != null) reportLevel = levelPolicy.getValue();

		EHSStackTrace stPolicy = (EHSStackTrace)Root.getPolicyManager().getEffectivePolicy(this, EHSStackTrace.NAME);
		if (stPolicy != null) showStackTrace = stPolicy.getValue();

		EHSWaitTimeForTree wtPolicy = (EHSWaitTimeForTree)Root.getPolicyManager().getEffectivePolicy(this, EHSWaitTimeForTree.NAME);
		if (wtPolicy != null) waitTimeForExceptionTree = wtPolicy.getValue();

		EHSExceptionTree etPolicy = (EHSExceptionTree)Root.getPolicyManager().getEffectivePolicy(this, EHSExceptionTree.NAME);
		if (etPolicy != null) showExceptionTree = etPolicy.getValue();
	}
}

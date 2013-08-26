package si.ijs.acs.objectexplorer.engine;

/**
 * Data structure that describes the operation declared
 * by a given introspectable. Note that objects of this
 * type do not describe a specific remote call (an instance
 * of the call), but rather syntax of an operation. In that
 * way they are analogous to Java <code>java.lang.reflect.Method</code>
 * instances.
 * A note about the convention on how parameters to the operation
 * are described: each parameter is described by a name, a type
 * and a mask field. These data are stored in arrays of equal length,
 * corresponding indexes describing the same parameter. Only
 * parameters with mask set to true should be processed by the GUI,
 * others are used by the engine to hold "context" parameters, such
 * as callbacks, parameters passed by reference (pointers) to the remote
 * objects, parameters declared as OUT in CORBA etc. GUI implementations
 * must not modify any parameter values that declare <code>false</code>
 * in the mask.
 */
public abstract class Operation {
	private String name = null;
	private SimpleIntrospectable introspectable = null;
	private String[] parameterNames = null;
	private DataType[] parameterTypes = null;
	private boolean[] mask = null;
	private boolean special = false;
	private boolean invocation = false;
	private DataType returnType=null;
/**
 * Constructs a new instance of an operation.
 *
 * @param name the name of the operation to be displayed in GUI
 * @param introspectable the remote object that declares this operation
 * @param parameterNames an array of string names of all parameters
 *		  that the operation declares
 * @param parameterTypes an array of <code>DataType</code> objects identifying
 * 		  the parameter types in the same sequence as in the names
 *	 	  array
 * @param mask an array of booleans that indicates which parameters should
 *		  be requested by the GUI to be supplied by the user, when the
 *		  method is invoked. The GUI should only ask for parameters that
 *		  have <code>true</code> flag set in this array. The sequence is
 *		  the same as in types and names arrays.
 * @param invocation <code>true</code> iff this method is an asynchronous
 *		  method. If so, it must be invoked with <code>invokeAsync()</code> method
 *		  and will, as a response return an invocation object.
 * @param special <code>true</code> iff the operation should be treated as
 *		  special in the GUI (ie. displayable only if certain checkbox is checked)
 */
public Operation(String name, SimpleIntrospectable introspectable, DataType returnType, String[] parameterNames, DataType[] parameterTypes, boolean[] mask, boolean invocation, boolean special) {
	super();
	if (name == null) throw new NullPointerException("name");
	if (introspectable == null) throw new NullPointerException("introspectable");
	if (parameterNames == null) throw new NullPointerException("parameterNames");
	if (parameterTypes == null) throw new NullPointerException("parameterTypes");
	if (mask == null) throw new NullPointerException("mask");
	if (mask.length != parameterNames.length) throw new IllegalArgumentException("parameterNames.length != mask.length");
	if (mask.length != parameterTypes.length) throw new IllegalArgumentException("mask.length != parameterTypes.length");
	this.name = name;
	this.mask = mask;
	this.introspectable = introspectable;
	this.parameterNames = parameterNames;
	this.parameterTypes = parameterTypes;
	this.special = special;
	this.invocation = invocation;
	this.returnType=returnType;
}
/**
 * Returns the introspectable instance that declares this method.
 * Returns the same value as was passed to the constructor.
 * 
 * @return introspectable instance
 */
public SimpleIntrospectable getIntrospectable() {
	return introspectable;
}
/**
 * Returns the mask value for each argument / parameter to this
 * method. GUIs should only process (query the user for values)
 * only those parameters that declare <code>true</code>. Other
 * parameters should be left unmodified.
 * 
 * @return a mask array
 */
public boolean[] getMask() {
	return mask;
}
/**
 * Returns the name of this operation, as it should be displayed
 * to the user.
 * 
 * @return operation name
 */
public String getName() {
  return name;
}
/**
 * Returns the parameter names array. This is the same
 * value as was passed to the constructor.
 * 
 * @return parameter names
 */
public String[] getParameterNames() {
	return parameterNames;
}
/**
 * Returns the array of parameter types for each parameter
 * to this operation. Note that if parameters are of complex
 * types and it is desired for the GUI to be able to query
 * users for their values and construct instances described
 * by this classes, the classes must declare a public constructor
 * that takes all parameters that have to be supplied to
 * instantiate a type.
 * 
 * @return an array of parameter types
 */
public DataType[] getParameterTypes() {
	return parameterTypes;
}
/**
 * Returns the array of parameter types for each parameter
 * to this operation. Note that if parameters are of complex
 * types and it is desired for the GUI to be able to query
 * users for their values and construct instances described
 * by this classes, the classes must declare a public constructor
 * that takes all parameters that have to be supplied to
 * instantiate a type.
 * 
 * @return a return value DataType
 */
public DataType getReturnType() {
	return returnType;
}
/**
 * Returns short name of the given DataType object
 * Creation date: (13.5.2001 12:09:49)
 * @return java.lang.String
 * @param param java.lang.Class 
 */
private String getShortParamName(DataType param) {
  return param.getName().substring(param.getName().lastIndexOf(".")+1);
}
/**
 * Returns the parameter types of this operation as they should be displayed
 * by the GUI.
 * 
 * @return parameter names
 */
private String getStringParamTypes() {
	StringBuffer result=new StringBuffer(" ");
	for (int i=0;i<parameterTypes.length;i++) {
	  if (i>0) result.append(", ");
	  if (!mask[i]) result.append("<");
  	  if (parameterTypes[i].isArray()) {
	  	    result.append(getShortParamName(parameterTypes[i].getComponentType()));
	  	    for (int j=0;j<=(parameterTypes[i].getName().lastIndexOf("[")-parameterTypes[i].getName().indexOf("[")); j++ ) {
		  	   result.append("[ ]");
		    }
  	    }
  	  else result.append(getShortParamName(parameterTypes[i]));
	  if (!mask[i]) result.append(">");
	}
	result.append(" ");
	return result.toString();
}
/**
 * Invokes this operation. The method returns a remote call
 * data struture that will pack the input parameters, return
 * values and possible exceptions of the call.
 * <b>Note: this method should only be used to invoke
 *    synchronous operations, ie. operations that do not
 *	  create <code>Invocations</code> as a side-effect.</b>.
 * The method must block for the duration of the call,
 * although it can terminate with a timeout, if this
 * condition can be detected.
 *
 * @param data parameters to be passed to the remote call. It is
 *		  the responsibility of the GUI to initialize this array
 *		  in accordance to the guidelines set forth in the documentation
 *		  of this class
 * 
 * @return remote call data structure that packs the results
 *		   of the remote operation call
 */
public abstract RemoteCall invoke(Object[] data);
/**
 * Invokes an asynchronous operation on the introspectable instane.
 * The method produces an invocation instance as a side-effect. Note
 * that because invocations are asynchronous, a callback parameter is
 * required to receive the asynchronous responses. Use this model for
 * monitors, alarms, event notifications and similar designs.
 *
 * @return invocation instance that represents the started process on
 *		   the remote machine; this instance contains also the familiar
 *		   remote call data structure that describes the call that
 *		   generated the invocation
 * @param data parameters to be passed to the remote call. It is
 *		  the responsibility of the GUI to initialize this array
 *		  in accordance to the guidelines set forth in the documentation
 *		  of this class
 * @param cb the callback that will receive the async notifications. Must
 *		  not be <code>null</code>.
 */
public abstract Invocation invokeAsync(Object[] data, RemoteResponseCallback cb);
/**
 * Returns <code>true</code> if this object represents an
 * operation, that is asynchronous. If so, an invocation of
 * such operation will produce an invocation object as a side
 * effect. Asynchronous operations <b>must</b> be invoked with
 * <code>invokeAsync()</code> method. Therefore the GUI should
 * always check first the return value of this call and decide
 * which mode of invocation (sync or async) to use.
 * 
 * @return <code>true</code> if the method should be invoked
 		   through the <code>invokeAsync()</code> method.
 */
public boolean isInvocation() {
	return invocation;
}
/**
 * Returns <code>true</code> if this operation is "special". GUI
 * decides how to handle special operations. The preferred way
 * is not to display them until the user has checked a special
 * checkbox allowing the GUI to display "advanced" or "dangerous"
 * features.
 * 
 * @return <code>true</code> if this operation is special
 */
public boolean isSpecial() {
	return special;
}
/**
 * Returns the display name of this operation with its 
 * parameter types in brackets.
 * 
 * @return name of <code>this</code>.
 */
public String toString() {
	return (name+" ("+getStringParamTypes()+")");
}
}

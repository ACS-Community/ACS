package si.ijs.acs.objectexplorer.engine.BACI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;

import si.ijs.acs.objectexplorer.engine.*;
import org.omg.CORBA.*;


/**
 * Insert the type's description here.
 * Creation date: (1.11.2000 20:57:03)
 * @author: 
 */
public class BACIIntrospector {

	/*********** INNER CLASSES BEGIN HERE ***************/	
	public class InvocationObjectHolder
	{
		public org.omg.CORBA.Object ref = null;
		public String id = null;

		public InvocationObjectHolder(org.omg.CORBA.Object ref, String id)
		{
			this.id = id;
			this.ref = ref;
		}
	}
	private BACIRemoteAccess ra = null;
	private HashMap IDLtoJavaMapping = new HashMap();
	public static final String ID_CORBA_OBJECT ="IDL:omg.org/CORBA/Object:1.0";
	public static final String ID_PROPERTY = "IDL:alma/ACS/Property:1.0";
	public static final String ID_CALLBACK = "IDL:alma/ACS/Callback:1.0";
	public static final String ID_DEVICE = "IDL:alma/ACS/ACSComponent:1.0";
	public static final String ID_SUBSCRIPTION = "IDL:alma/ACS/Subscription:1.0";
	public static final String ID_CBDESCIN = "IDL:alma/ACS/CBDescIn:1.0";
	public static final String METHOD_DONE = "done";
	public static final String METHOD_DESTROY = "destroy";
	public static final String IDL_PACKAGE_PREFIX = "alma.";
	public static final String BACI_PACKAGE_PREFIX = "alma.ACS.";
/**
 * BACIIntrospector constructor comment.
 */
public BACIIntrospector(BACIRemoteAccess ra) {
	super();
	if (ra == null) throw new NullPointerException("ra");
	this.ra = ra;
}
/**
 * Insert the method's description here.
 * Creation date: (13.11.2000 0:58:40)
 * @return java.lang.String
 * @param arg java.lang.String
 */
public static String addIDLPackagePrefix(String arg) {
	if (arg == null) throw new NullPointerException("arg");
	
	//return IDL_PACKAGE_PREFIX + arg;
	return arg;
}
/**
 * Insert the method's description here.
 * Creation date: (17.11.2000 2:02:35)
 * @return java.lang.String
 * @param name java.lang.String
 */
public static String addJavaPackagePrefix(String name) {
	if (name == null) throw new NullPointerException("name");
	
	//return BACI_PACKAGE_PREFIX + name;
	return name;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 0:04:34)
 * @return java.lang.String
 * @param propertyName java.lang.String
 */
public static String attributeNameToMethodName(String propertyName) {
	if (propertyName == null) throw new NullPointerException("propertyName");
	
	return "_get_" + propertyName;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 21:47:04)
 * @param invoc si.ijs.acs.objectexplorer.engine.Invocation[]
 */
public static void destroyInvocation(BACIInvocation invoc) {
	if (invoc == null) throw new NullPointerException("invoc");

	if (invoc.isDestroyed()) return;
	Operation[] ops = invoc.getOperations();
	Operation dest = null;
	for (int i = 0; i < ops.length; i++)
	{
		if (METHOD_DESTROY.equals(ops[i].getName()))
		{
			dest = ops[i];
			if (dest.getParameterTypes().length != 0) throw new IntrospectionInconsistentException("Operation " + METHOD_DESTROY + "' on a 'Subscription' instance '" + invoc + "' must take exactly 0 parameters.");
			break;
		}
	}
	if (dest == null) throw new IntrospectionInconsistentException("'Subscription' instance for invocation '" + invoc + "' does not declare a 'destroy()' method.");
	invoc.ra.invoke(invoc, (BACIOperation)dest, new java.lang.Object[0], null);
}

/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:38:21)
 */
public java.lang.Object extractAny(Any argument) {
	if (argument == null) throw new NullPointerException("argument");

	return extractAny(argument.type(), argument);
}

/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:38:21)
 */
public java.lang.Object extractAny(TypeCode argumentType, Any argument) {
	if (argumentType == null) throw new NullPointerException("argumentType");
	if (argument == null) throw new NullPointerException("argument");

	switch(argumentType.kind().value())
	{
		case TCKind._tk_any:
			return extractAny(argument.extract_any());
		case TCKind._tk_void:
		case TCKind._tk_null:
			return null;
		case TCKind._tk_objref:
			return argument.extract_Object();
		case TCKind._tk_struct:
		case TCKind._tk_except:
		//case TCKind._tk_enum:
		case TCKind._tk_alias:
			return extractTypedef(argument);
/*
		case TCKind._tk_alias:
		        try
			{
			    return extractAny(argument.type().content_type(), argument);
			} catch(org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
			        throw new RemoteException("Exception while analyzing alias typecode (getting typecode name raises exception). Exception: " + bk);
			}
*/
		case TCKind._tk_enum:
			java.lang.Object enumObj = extractTypedef(argument);
			java.lang.Class type = enumObj.getClass();

			try
			{
				java.lang.reflect.Field[] fields = type.getDeclaredFields();
				for (int j = 0; j < fields.length; j++) {
					if (fields[j].getType() == type)
					{
						try {
							if (fields[j].get(enumObj).equals(enumObj))
								return fields[j].getName();
						} catch (Throwable th) { /* noop */ }
					}
				}
				return enumObj;
			}
			catch (Throwable th)
			{
				// failed to extract the enum name, return object
				return enumObj;
			}
		
		case TCKind._tk_double:
			return new Double(argument.extract_double());
		case TCKind._tk_float:
			return new Float(argument.extract_float());
		case TCKind._tk_octet:
			//return new Byte(argument.extract_octet());
			// use short to get unsigned value 
			return new Short((short)(argument.extract_octet() & 0xFF));
		case TCKind._tk_longlong:
			return new Long(argument.extract_longlong());
		case TCKind._tk_ulonglong:
			return new Long(argument.extract_ulonglong());
		case TCKind._tk_long:
			return new Integer(argument.extract_long());
		case TCKind._tk_ulong:
			return new Integer(argument.extract_ulong());
		case TCKind._tk_short:
			return new Short(argument.extract_short());
		case TCKind._tk_ushort:
			return new Short(argument.extract_ushort());
		case TCKind._tk_string:
			return argument.extract_string();
		case TCKind._tk_char:
			return new Character(argument.extract_char());
		case TCKind._tk_boolean:
			return new Boolean(argument.extract_boolean());
		default:
			throw new IllegalArgumentException("Argument typecode '" + argumentType.kind().value() + "' is not supported.");
	}
}
/**
 * Insert the method's description here.
 * Creation date: (10.11.2000 0:07:59)
 * @return org.omg.CORBA.Object
 * @param call si.ijs.acs.objectexplorer.engine.RemoteCall
 */
public InvocationObjectHolder extractInvocationObject(RemoteCall call) {
	if (call == null) throw new NullPointerException("call");
	
	if (!(call.getSyncReturnValue() instanceof org.omg.CORBA.Object))
		return new InvocationObjectHolder(null, null);
	org.omg.CORBA.Object retVal = (org.omg.CORBA.Object)call.getSyncReturnValue();
	if (!retVal._is_a(ID_SUBSCRIPTION))
		throw new IntrospectionInconsistentException("Remote call '" + call.getSN() + "' that produced an 'Invocation' returns a CORBA object, which does not implement 'Subscription' interface.");
	try
	{
		return new InvocationObjectHolder(retVal, ((BACIOperation)call.getOperation()).getOperationDesc().result.id());
	} catch (org.omg.CORBA.TypeCodePackage.BadKind bk)
	{
		throw new RemoteException("CORBA BadKind exception thrown while analysing typecode for 'Subscription' descendant. Should be a valid IDL operation.");
	}
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 13:25:37)
 * @return java.lang.Object[]
 * @param req org.omg.CORBA.Request
 */
public java.lang.Object[] extractOuts(Request req, OperationDescription desc) {
	if (req == null) throw new NullPointerException("req");
	if (desc == null) throw new NullPointerException("desc");
	
	java.lang.Object[] retVal = new java.lang.Object[desc.parameters.length];

	for (int i = 0; i < desc.parameters.length; i++)
	{
		if (!(desc.parameters[i].mode == ParameterMode.PARAM_IN))
		{
			try
			{
				retVal[i] = extractAny(req.arguments().item(i).value());
			} catch (Bounds be)
			{
				throw new IntrospectionInconsistentException("Request object and operation description object for operation '" + desc.name + "' declare different number of arguments to remote function.");
			}
		}
		else retVal[i] = null;
	}
	return retVal;
}

/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 1:00:12)
 * @return java.lang.Object
 * @param argumentType org.omg.CORBA.TypeCode
 * @param argument org.omg.CORBA.Any
 */
public java.lang.Object extractTypedef(Any argument) {
	if (argument == null) throw new NullPointerException("argument");

	return extractTypedef(argument.type(), argument);
}

/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 1:00:12)
 * @return java.lang.Object
 * @param argument org.omg.CORBA.Any
 */
public java.lang.Object extractTypedef(TypeCode argumentType, Any argument) {
	if (argumentType == null) throw new NullPointerException("argumentType");
	if (argument == null) throw new NullPointerException("argument");
	
	Class c = null;
	String className = null;
	try
	{
		className = IDtoClassName(argumentType.id()) + "Helper";
		c = Class.forName(className);
	} catch (Exception e)
	{
		throw new JavaIDLIntrospectionException("Failed to load class '" + className + "'. Introspection failed on typedef argument: " + e);
	}
	Class[] paramTypes = { org.omg.CORBA.Any.class };
	java.lang.Object[] params = { argument };
	try
	{
		return c.getMethod("extract", paramTypes).invoke(null, params);
	} catch (Exception e1)
	{
		throw new JavaIDLIntrospectionException("Dynamic invocation of 'extractAny()' failed on a typedef argument. Class instance: " + c.getName() + ". Exception:" + e1);
	}	
}
/**
 * Insert the method's description here.
 * Creation date: (13.11.2000 18:22:59)
 * @return java.lang.Class
 * @param tc org.omg.CORBA.TypeCode
 */
private Class extractTypeFromTC(TypeCode tc) {
	if (tc == null) throw new NullPointerException("tc");
	
	switch(tc.kind().value())
	{
		case TCKind._tk_objref:
		case TCKind._tk_struct:
		case TCKind._tk_enum:
			try
			{
				try
				{
					return Class.forName(IDtoClassName(tc.id()));
				} catch (Exception e)
				{
					throw new JavaIDLIntrospectionException("Java introspection (Class.forName()) failed for type '" + IDtoClassName(tc.id()) + "'. Exception: " + e);
				}
			} catch (org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
				throw new RemoteException("Exception while analyzing enum, objref or struct typecode (getting typecode name raises exception). Exception: " + bk);
			}
		case TCKind._tk_alias:
			try
			{
				return extractTypeFromTC(tc.content_type());
//				else throw new IllegalArgumentException("Do not know how to handle alias typecodes other than those that represent sequences and have names ending with 'Seq'. Typecode being processed was '" + tc.id());
			} catch(org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
				throw new RemoteException("Exception while analyzing alias typecode (getting typecode name raises exception). Exception: " + bk);
			}
		case TCKind._tk_sequence:
		{
			try
			{
				Class content = extractTypeFromTC(tc.content_type());
				return java.lang.reflect.Array.newInstance(content, 0).getClass();
			}  catch(org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
				throw new RemoteException("Exception while analyzing sequence typecode (getting typecode name raises exception). Exception: " + bk);
			}
		}
		case TCKind._tk_array:
		{
			try
			{
				Class content = extractTypeFromTC(tc.content_type());
				return java.lang.reflect.Array.newInstance(content, 0).getClass();
			}  catch(org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
				throw new RemoteException("Exception while analyzing array typecode (getting typecode name raises exception). Exception: " + bk);
			}
		}
		case TCKind._tk_double:
			return Double.TYPE;
		case TCKind._tk_float:
			return Float.TYPE;
		case TCKind._tk_octet:
			return Byte.TYPE;
		case TCKind._tk_longlong:
			return Long.TYPE;
		case TCKind._tk_ulonglong:
			return Long.TYPE;
		case TCKind._tk_long:
			return Integer.TYPE;
		case TCKind._tk_ulong:
			return Integer.TYPE;
		case TCKind._tk_short:
			return Short.TYPE;
		case TCKind._tk_ushort:
			return Short.TYPE;
		case TCKind._tk_string:
			return String.class;
		case TCKind._tk_char:
			return Character.TYPE;
		case TCKind._tk_boolean:
			return Boolean.TYPE;
		case TCKind._tk_void:
			return Void.TYPE;
		default:
		    throw new IllegalArgumentException("Argument typecode '" + tc.kind().value() + "' is not supported.");
	}
}
/**
 * Insert the method's description here.
 * Creation date: (17.3.2001 19:36:18)
 * @return java.lang.String
 * @param fullType java.lang.String
 */
public  static String fullTypeToType(String fullType) {
	if (fullType == null) throw new NullPointerException("fullType");
	int index1 = fullType.lastIndexOf('/');
	int index2 = fullType.lastIndexOf(':');
	if (index1 == -1 || index2 == -1) throw new IntrospectionInconsistentException("Type returned by the Manager is not a valid IDL ID, beginning with IDL: and ending with :1.0");
	return fullType.substring(index1 + 1, index2);
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:47:14)
 * @return si.ijs.acs.objectexplorer.engine.BACI.BACIAttribute[]
 * @param desc org.omg.CORBA.AttributeDescription[]
 */
public BACIAttribute[] getAttributes(BACIRemote target) {
	if (target == null) throw new NullPointerException("target");

	AttributeDescription[] desc = target.getIFDesc().attributes;
	ArrayList temp = new ArrayList();
	for (int i = 0; i < desc.length; i++)
	{
		if (isProperty(desc[i])) continue;
		Class type = extractTypeFromTC(desc[i].type);
		temp.add(new BACIAttribute(ra, target, desc[i], type));
	}
	BACIAttribute[] retVal = new BACIAttribute[temp.size()];
	temp.toArray(retVal);
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 0:23:33)
 * @return java.lang.Object[]
 * @param params java.lang.Object[]
 * @param cb si.ijs.acs.objectexplorer.engine.RemoteResponseCallback
 */
public int getCallbackLocation(Operation op) {
	if (op == null) throw new NullPointerException("op");
	
	Class[] args = op.getParameterTypes();
	int count = -1;
	for (int i = 0; i < args.length; i++)
	{
		if (alma.ACS.Callback.class.isAssignableFrom(args[i]))
		{
			if (count != -1) throw new IntrospectionInconsistentException("Operation '" + op + "' declares more than one parameter that extends 'Callback'");
			count = i;
			if (i == args.length-1 && ra.isStrict()) throw new IntrospectionInconsistentException("Operation '" + op + "' takes callback as its last parameter, but takes no 'CBDescIn' parameter.");
			if (ra.isStrict() && !args[i+1].equals(alma.ACS.CBDescIn.class)) throw new IntrospectionInconsistentException("Operation '" + op + "' does not take a 'CBDescIn' parameter after 'Callback' parameter.");
		}
	}
	return count;
}
/**
 * Insert the method's description here.
 * Creation date: (2.11.2000 0:44:52)
 */
public BACIOperation[] getOperations(BACIRemote target) {
	if (target == null) throw new NullPointerException("target");

	OperationDescription[] operations = target.getIFDesc().operations;	
	ArrayList tempList = new ArrayList();
	for (int i = 0; i < operations.length; i++)
	{
		ra.getNotifier().reportDebug("BACIIntrospector::getOperations", "Analysing operation '" + operations[i].name + "'.");
		ParameterDescription[] ps = operations[i].parameters;
		String[] names = new String[ps.length];
		Class[] types = new Class[ps.length];
		boolean[] mask = new boolean[ps.length];
		int cb = -1;
		boolean unsupportedOperation = false;
		for (int j = 0; j < ps.length; j++)
		{
		    try
		    {
			names[j] = ps[j].name;
			types[j] = extractTypeFromTC(ps[j].type);
			if (isOfType(ps[j].type, ID_CALLBACK)) 
			{
				if (cb != -1) throw new IntrospectionInconsistentException("Operation '" + operations[i].name + "' declares more than one callback parameter.");
				cb = j;
				if (cb == ps.length-1 && ra.isStrict()) throw new IntrospectionInconsistentException("A callback parameter for operation '" + operations[i].name + "' must be followed by a 'CBDescIn' parameter.");
			}
			if (ps[j].mode == ParameterMode.PARAM_OUT || cb == j)
			{
				mask[j] = false;
			}
			else
				mask[j] = true;

			if (cb != -1 && j == cb+1)
			{
				try
				{
					if (ID_CBDESCIN.equals(ps[j].type.id()))
					{
						mask[j] = false;
					}
					else if (ra.isStrict()) throw new IntrospectionInconsistentException("A callback parameter for opration '" + operations[i].name + "' must be followed by a 'CBDescIn' parameter.");
				} catch (Exception bk)
				{
					throw new RemoteException("Exception while analyzing typecode (getting typecode name raises exception). Exception: " + bk);
				}
			}

		    }
		    catch (Exception e)
		    {
			ra.getNotifier().reportDebug("BACIIntrospector::getOperations", "Failed to analyse parameter '" + ps[j].name + "' for operation '" + operations[i].name + "'. Removing it from operation list... Exception:" + e);
			e.printStackTrace();
		        unsupportedOperation = true;
			break;
		    }
		}
		if (!unsupportedOperation)
		    tempList.add(new BACIOperation(ra, operations[i], target, names, types, mask, !(cb == -1), false));
	}
	BACIOperation[] retVal = new BACIOperation[tempList.size()];
	tempList.toArray(retVal);
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (1.11.2000 22:47:50)
 * @return java.lang.String[]
 * @param descs org.omg.CORBA.AttributeDescription[]
 */
public AttributeDescription[] getProperties(AttributeDescription[] attributes) {
	if (attributes == null) throw new NullPointerException("attributes");
	
	ArrayList tempList = new ArrayList();
	for (int i = 0; i < attributes.length; i++)
	{
		if (isProperty(attributes[i]))
		{
			tempList.add(attributes[i]);
		}
	}
	AttributeDescription[] retVal = new AttributeDescription[tempList.size()];
	tempList.toArray(retVal);
	return retVal;
}
/**
 * Insert the method's description here.
 * Creation date: (13.11.2000 0:59:30)
 * @return java.lang.String
 * @param ID java.lang.String
 */
public String IDtoClassName(String ID) {
	if (ID == null) throw new NullPointerException("ID");

	String retVal = (String)IDLtoJavaMapping.get(ID);
	if (retVal != null) return retVal;
	else
	{
	
		int index1 = 0;
		int index2 = 0;
		index1 = ID.indexOf(':');
		index2 = ID.lastIndexOf(':');
		if (index1 == index2) throw new IntrospectionInconsistentException("IDL ID '" + ID + "' is not well-formed because it contains only one ':' character");
		String partial = ID.substring(index1 + 1, index2);
		index1 = partial.lastIndexOf('/');
		index2 = partial.indexOf('/');
		if (index1 == -1 || index2 == -1) throw new IntrospectionInconsistentException("IDL ID '" + ID + "' is not well-formed because it does not contain module separators '/'.");
		if (index1 == index2) return addIDLPackagePrefix(partial.replace('/', '.'));
		String lookup = "IDL:" + partial.substring(0, index1) + ":1.0";
		
		if (index1 != index2) 
		{
			// reverse order of pragma prefix
			final String delimiter = ".";
			StringTokenizer stringTokenizer = new StringTokenizer(partial.substring(0, index2), delimiter);
			String reversedPragma = stringTokenizer.nextToken();
			while (stringTokenizer.hasMoreTokens())
				reversedPragma = stringTokenizer.nextToken() + delimiter + reversedPragma;
		
			partial = reversedPragma + partial.substring(index2);		
		}
		ra.getNotifier().reportDebug("BACIIntrospector::IDtoClassName", "Analysing IDL to Java mapping of type '" + ID + "'. Querying IR for parent container '" + lookup + "'.");
		IRObject ctd = ra.lookupId(lookup);
		if (ctd == null) throw new IntrospectionInconsistentException("Repository does not contain container '" + lookup + "' of child '" + ID + "'.");
		if (ctd.def_kind() == DefinitionKind.dk_Module)
			retVal = addIDLPackagePrefix(partial.replace('/', '.'));
		else
			retVal = addIDLPackagePrefix(partial.substring(0, index1).replace('/', '.') + "Package." + partial.substring(index1 + 1));
		IDLtoJavaMapping.put(ID, retVal);
		return retVal;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:28:16)
 * @param desc org.omg.CORBA.ParameterDescription
 */
public void insertTypedef(ParameterDescription desc, java.lang.Object o, Any any) {
	if (any == null) throw new NullPointerException("any");
	if (o == null) throw new NullPointerException("o");


	String className = null;
	try
	{
		if (!o.getClass().isArray())
			className = o.getClass().getName() + "Helper";
		else
		{
			className = IDtoClassName(desc.type.id()) + "Helper";
		}
		if (className.startsWith("java.lang.")) className = "org.omg.CORBA." + className.substring("java.lang.".length());
		Class c = Class.forName(className);
		Class[] paramTypes = { org.omg.CORBA.Any.class, o.getClass() };
		java.lang.Object[] params = { any, o };
		//System.out.println("Invoking 'insert' on class '"+c.getName()+"' with (org.omg.CORBA.Any, "+o.getClass().getName()+").");
		c.getMethod("insert", paramTypes).invoke(null, params);
	} catch (Exception e)
	{
		throw new JavaIDLIntrospectionException("Error while dynamically inserting a typedef value into 'Any' by loading helper class '" + className + "' (inserting class '"+o.getClass().getName()+"'). Exception: " + e);
	}
	
}
/**
 * Insert the method's description here.
 * Creation date: (1.11.2000 21:26:59)
 * @return boolean
 * @param remote org.omg.CORBA.Object
 */
public boolean isDevice(org.omg.CORBA.Object remote) {
	if (remote == null) throw new NullPointerException("remote");
	
	return remote._is_a(ID_DEVICE);
}
/**
 * Insert the method's description here.
 * Creation date: (14.11.2000 0:34:32)
 * @return boolean
 * @param operation java.lang.String
 */
public boolean isInvocationDestroyMethod(String operation) {
	if (operation == null) throw new NullPointerException("operation");
	
	return METHOD_DESTROY.equals(operation);
}
/**
 * Insert the method's description here.
 * Creation date: (14.11.2000 0:34:32)
 * @return boolean
 * @param operation java.lang.String
 */
public boolean isInvocationDoneMethod(String operation) {
	if (operation == null) throw new NullPointerException("operation");
	
	return METHOD_DONE.equals(operation);
}
/**
 * Insert the method's description here.
 * Creation date: (13.11.2000 18:44:35)
 * @return boolean
 * @param tc org.omg.CORBA.TypeCode
 * @param baciType java.lang.String
 */
private boolean isOfType(TypeCode tc, String type) {
	switch(tc.kind().value())
	{
		case TCKind._tk_objref: break;
		case TCKind._tk_alias: 
			try
			{
				return isOfType(tc.content_type(), type);
			} catch (org.omg.CORBA.TypeCodePackage.BadKind bk)
			{
				throw new RemoteException("Exception while analyzing typecode (getting typecode name raises exception). Exception: " + bk);
			}
		default: return false;
	}
	try
	{
		ra.getNotifier().reportDebug("BACIIntrospector::isOfType", "Checking if '" + tc.id() + "' implements '" + type + "'.");

		if (tc.id().equals(ID_CORBA_OBJECT))
			if (type.equals(ID_CORBA_OBJECT))
				return true;
			else
				return false;
				
	/*	Class c = Class.forName(IDtoClassName(tc.id()));
		Class c1 = null;
		try
		{ 
			c1 = Class.forName(addJavaPackagePrefix(baciType));
		} catch (Exception e)
		{
			throw new JavaIDLIntrospectionException("Introspection error while loading property class '" + addJavaPackagePrefix(baciType) + "'.");
		}
		if (c1.isAssignableFrom(c)) return true;*/
		
		InterfaceDef idef = InterfaceDefHelper.narrow(ra.lookupId(tc.id()));
		
		if (idef.is_a(type)) return true;
	} catch (org.omg.CORBA.TypeCodePackage.BadKind bk)
	{
		throw new RemoteException("Exception while analyzing typecode (getting typecode name raises exception). Exception: " + bk);
	} catch (Exception e)
	{
		e.printStackTrace();
		return false;
	}
	return false;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 1:20:04)
 * @return boolean
 * @param desc org.omg.CORBA.AttributeDescription
 */
private boolean isProperty(AttributeDescription desc) {
	if (desc == null) throw new NullPointerException("desc");
	
	if (desc.mode == AttributeMode.ATTR_NORMAL) return false;
	
	if (desc.type.kind() == TCKind.tk_objref && isOfType(desc.type, ID_PROPERTY))
	{
		return true;
	}
	return false;
}
/**
 * Insert the method's description here.
 * Creation date: (3.11.2000 0:25:31)
 * @return java.lang.Object[]
 * @param desc org.omg.CORBA.OperationDescription
 */
public java.lang.Object[] prepareDIIparameters(OperationDescription desc, java.lang.Object[] params) {
	return params;
}
}

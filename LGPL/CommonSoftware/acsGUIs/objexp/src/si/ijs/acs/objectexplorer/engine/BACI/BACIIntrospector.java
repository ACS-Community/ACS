package si.ijs.acs.objectexplorer.engine.BACI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;
import java.util.StringTokenizer;

import org.omg.CORBA.Any;
import org.omg.CORBA.AttributeDescription;
import org.omg.CORBA.AttributeMode;
import org.omg.CORBA.Bounds;
import org.omg.CORBA.Contained;
import org.omg.CORBA.DefinitionKind;
import org.omg.CORBA.EnumDef;
import org.omg.CORBA.EnumDefHelper;
import org.omg.CORBA.IRObject;
import org.omg.CORBA.InterfaceDef;
import org.omg.CORBA.InterfaceDefHelper;
import org.omg.CORBA.OperationDescription;
import org.omg.CORBA.ParameterDescription;
import org.omg.CORBA.ParameterMode;
import org.omg.CORBA.Request;
import org.omg.CORBA.StructDef;
import org.omg.CORBA.StructDefHelper;
import org.omg.CORBA.StructMember;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCode;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynArray;
import org.omg.DynamicAny.DynEnum;
import org.omg.DynamicAny.DynSequence;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.NameDynAnyPair;

import si.ijs.acs.objectexplorer.engine.DataElement;
import si.ijs.acs.objectexplorer.engine.DataEnum;
import si.ijs.acs.objectexplorer.engine.DataException;
import si.ijs.acs.objectexplorer.engine.DataStruct;
import si.ijs.acs.objectexplorer.engine.DataType;
import si.ijs.acs.objectexplorer.engine.IntrospectionInconsistentException;
import si.ijs.acs.objectexplorer.engine.Operation;
import si.ijs.acs.objectexplorer.engine.RemoteCall;
import si.ijs.acs.objectexplorer.engine.RemoteException;

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
	private Stack structs;
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
	structs = new Stack();
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

	public Any insertAny(Any argument, java.lang.Object obj) {
		if (argument == null) throw new NullPointerException("argument");
		if (obj == null) throw new NullPointerException("obj");
		return insertAny(argument.type(),argument,obj);
	}

	public Any insertAny(TypeCode tc, Any argument, java.lang.Object obj) {
		if (tc == null) throw new NullPointerException("tc");
		if (argument == null) throw new NullPointerException("argument");
		if (obj == null) throw new NullPointerException("obj");
		int value = tc.kind().value();
		
		switch (value) {
			case TCKind._tk_objref :
				argument.insert_Object((org.omg.CORBA.Object) obj);
				break;
			case TCKind._tk_struct :
				return insertTypedef(tc, obj);
			case TCKind._tk_enum :
				DynEnum en;
				try {
					en = (DynEnum) ra.getDynFact().create_dyn_any_from_type_code(tc);
				} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
					e.printStackTrace();
					return null;
				}
				//return en.get_as_string();
				try {
					en.set_as_ulong(((DataEnum)obj).get());
				} catch(org.omg.DynamicAny.DynAnyPackage.InvalidValue e) {
					e.printStackTrace();
					return null;
				}
				return en.to_any();
			case TCKind._tk_double :
				argument.insert_double(((Double) obj).doubleValue());
				break;
			case TCKind._tk_float :
				argument.insert_float(((Float) obj).floatValue());
				break;
			case TCKind._tk_octet :
				argument.insert_octet(((Byte) obj).byteValue());
				break;
			case TCKind._tk_longlong :
				argument.insert_longlong(((Long) obj).longValue());
				break;
			case TCKind._tk_ulonglong :
				argument.insert_ulonglong(((Long) obj).longValue());
				break;
			case TCKind._tk_long :
				argument.insert_long(((Integer) obj).intValue());
				break;
			case TCKind._tk_ulong :
				argument.insert_ulong(((Integer) obj).intValue());
				break;
			case TCKind._tk_short :
				argument.insert_short(((Short) obj).shortValue());
				break;
			case TCKind._tk_ushort :
				argument.insert_ushort(((Short) obj).shortValue());
				break;
			case TCKind._tk_string :
				argument.insert_string((String) obj);
				break;
			case TCKind._tk_char :
				argument.insert_char(((Character) obj).charValue());
				break;
			case TCKind._tk_boolean :
				argument.insert_boolean(((Boolean) obj).booleanValue());
				break;
			case TCKind._tk_sequence :
			case TCKind._tk_array :
				return insertSequence(tc, argument, obj);
			case TCKind._tk_alias :
				try {
					return insertAny(tc.content_type(), argument, obj);
				} catch (org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				}
			default :
				throw new IllegalArgumentException("Argument typecode '"+value+"' is not supported.");
		}
		return argument;
	}

	public Any insertTypedef(TypeCode tc, java.lang.Object obj) {
		DynAny dany;
		try {
			dany = ra.getDynFact().create_dyn_any_from_type_code(tc);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
			return null;
		}
		Class cl = getClassType(tc);
		DynStruct str = (DynStruct) dany;
		NameDynAnyPair[] mems = str.get_members_as_dyn_any();
		try {
			if(tc.kind() == TCKind.tk_struct) {
				DataStruct ds = (DataStruct) obj;
				for(int i = 0; i < mems.length; i++) {
					mems[i].value.from_any(insertAny(mems[i].value.to_any(), ds.get(mems[i].id)));
				}
			} else {
				DataException de = (DataException) obj;
				for(int i = 0; i < mems.length; i++) {
					mems[i].value.from_any(insertAny(mems[i].value.to_any(), de.get(mems[i].id)));
				}
			}
			str.set_members_as_dyn_any(mems);
		} catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch e) {
			e.printStackTrace();
			return null;
		} catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue e) {
			e.printStackTrace();
			return null;
		}
		//displayAny(dany.to_any());
		return dany.to_any();
	}

	public Any insertSequence(TypeCode tc, Any argument, java.lang.Object obj) {
		if (argument == null) throw new NullPointerException("argument");
		if (obj == null) throw new NullPointerException("obj");
		if (!obj.getClass().isArray()) throw new IllegalArgumentException("The parameter obj has to be a sequence");

		java.lang.Object[] objs;
		if (obj.getClass().getComponentType().isPrimitive())
			objs = com.cosylab.gui.components.r2.DataFormatter.convertPrimitiveArray(obj);
		else
			objs = (java.lang.Object[]) obj;
		DynAny dany;
		try {
			dany = ra.getDynFact().create_dyn_any_from_type_code(tc);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
			return null;
		}
		Any[] els = new Any[objs.length];
		try {
			for(int i = 0; i < els.length; i++) {
				DynAny dany2;
				try {
					dany2 = ra.getDynFact().create_dyn_any_from_type_code(tc.content_type());
				} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
					e.printStackTrace();
					return null;
				}
				els[i] = dany2.to_any();
				els[i] = insertAny(tc.content_type(),els[i],objs[i]);
			}
			if(tc.kind() == TCKind.tk_sequence) {
				((DynSequence) dany).set_elements(els);
				((DynSequence) dany).set_length(els.length);
			}
			else if(tc.kind() == TCKind.tk_array)
				((DynArray) dany).set_elements(els);
		} catch(org.omg.DynamicAny.DynAnyPackage.TypeMismatch e) {
			e.printStackTrace();
		} catch(org.omg.DynamicAny.DynAnyPackage.InvalidValue e) {
			e.printStackTrace();
		} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
			e.printStackTrace();
		}
		return dany.to_any();
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
	public java.lang.Object extractAny(TypeCode tc, Any argument) {
		if (argument == null) throw new NullPointerException("argument");
		if (tc == null) throw new NullPointerException("tc");
		switch(tc.kind().value())
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
				return extractTypedef(argument);
			case TCKind._tk_alias:
				try {
					return extractAny(tc.content_type(),argument);
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				}
			case TCKind._tk_enum:
				DynEnum en;
				try {
					en = (DynEnum) ra.getDynFact().create_dyn_any(argument);
				} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
					e.printStackTrace();
					return null;
				}
				//return en.get_as_string();
				DataEnum de = (DataEnum) getDef(tc);
				de.set(en.get_as_ulong());
				return de;
			case TCKind._tk_sequence:
			case TCKind._tk_array:
				return extractSequence(tc, argument);
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
				throw new IllegalArgumentException("Argument typecode '" + argument.type().kind().value() + "' is not supported.");
		}
	}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 1:00:12)
 * @return java.lang.Object
 * @param argument org.omg.CORBA.Any
 */
	public java.lang.Object extractTypedef(TypeCode tc, Any argument) {
		if (argument == null) throw new NullPointerException("argument");
		DynAny dany;
		try {
			dany = ra.getDynFact().create_dyn_any(argument);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
			return null;
		}
		DynStruct str = (DynStruct) dany;
		try {
			if(tc.kind() == TCKind.tk_struct) {
				DataStruct ds = new DataStruct(dany.type().id());
				for(int i = 0; i < str.component_count(); i++) {
					ds.add(str.current_member_name(), extractAny(str.current_component().to_any()));
					str.next();
				}
				return ds;
			} else {
				DataException de = new DataException(dany.type().id());
				for(int i = 0; i < str.component_count(); i++) {
					de.add(str.current_member_name(), extractAny(str.current_component().to_any()));
					str.next();
				}
				return de;
			}
		} catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch e) {
			e.printStackTrace();
			return null;
		} catch (org.omg.DynamicAny.DynAnyPackage.InvalidValue e) {
			e.printStackTrace();
			return null;
		} catch (org.omg.CORBA.TypeCodePackage.BadKind e) {
			e.printStackTrace();
			return null;
		}		
	}

	public java.lang.Object extractSequence(TypeCode tc, Any argument) {
		if (tc == null) throw new NullPointerException("tc");
		if (argument == null) throw new NullPointerException("argument");
		DynAny dany;
		try {
			dany = ra.getDynFact().create_dyn_any(argument);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
			return null;
		}
		Class cl = getClassType(argument.type());
		Any[] els;
		if(tc.kind() == TCKind.tk_sequence)
			els = ((DynSequence) dany).get_elements();
		else if(tc.kind() == TCKind.tk_array)
			els = ((DynArray) dany).get_elements();
		else {
			return null;
		}
		java.lang.Object array = java.lang.reflect.Array.newInstance(cl.getComponentType(), els.length);
		java.lang.Object[] objs;
		if (cl.getComponentType().isPrimitive())
			objs = com.cosylab.gui.components.r2.DataFormatter.convertPrimitiveArray(array);
		else
			objs = (java.lang.Object[]) array;
		for(int i = 0; i < els.length; i++) {
			objs[i] = extractAny(els[i]);
		}
		return objs;
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
 * Creation date: (17.3.2001 19:36:18)
 * @return java.lang.String
 * @param fullType java.lang.String
 */
public  static String fullTypeToType(String fullType) {
	if (fullType == null) throw new NullPointerException("fullType");
	int index1 = fullType.lastIndexOf('/');
	int index2 = fullType.lastIndexOf(':');
	if (index1 == -1 || index2 == -1 || index1 > index2) throw new IntrospectionInconsistentException("Type '" + fullType + "' is not a valid IDL ID, beginning with IDL: and ending with :1.0");
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
		Class type = getClassType(desc[i].type);
		temp.add(new BACIAttribute(ra, target, desc[i], new BACIDataType(type)));
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
	
	DataType[] args = op.getParameterTypes();
	int count = -1;
	for (int i = 0; i < args.length; i++)
	{
		if (alma.ACS.Callback.class.isAssignableFrom(args[i].getType()))
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
		//Class[] types = new Class[ps.length];
		DataType[] types = new DataType[ps.length];
		boolean[] mask = new boolean[ps.length];
		int cb = -1;
		boolean unsupportedOperation = false;
		for (int j = 0; j < ps.length; j++)
		{
			try
			{
			names[j] = ps[j].name;
			types[j] = new BACIDataType(getClassType(ps[j].type));
			types[j].setElement(getDef(ps[j].type));
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

public String classNameToId(String name) {
	String prefix = "IDL:alma/";
	String suffix = ":1.0";
	String cName = name.substring(name.lastIndexOf(".")+1).replace("AcsJ","");
	String module = name.substring(name.indexOf(".")+1);
	module = module.substring(0,module.indexOf("."));
	String id = prefix + module + "/" + cName + suffix;
	return id;
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

	public Class getClassType(TypeCode tc) {
		switch(tc.kind().value()) {
			case TCKind._tk_objref:
				try {
					return Class.forName(IDtoClassName(tc.id()));
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				} catch(java.lang.ClassNotFoundException e) {
					return org.omg.CORBA.Object.class;
				}
			case TCKind._tk_enum:
				//return String.class;
				return DataEnum.class;
			case TCKind._tk_struct:
			case TCKind._tk_except:
				//Special
				try {
					//return Class.forName(IDtoClassName(tc.id()));
					return Class.forName("holi"+IDtoClassName(tc.id()));
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				} catch(java.lang.ClassNotFoundException e) { //Class doesn't exist, try to create it.
					if(tc.kind().value() == TCKind._tk_struct)
						return DataStruct.class;
					else
						return DataException.class;
				}
			case TCKind._tk_sequence:
			case TCKind._tk_array:
				Class content;
				try {
					content = getClassType(tc.content_type());
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					return null;
				}
				return java.lang.reflect.Array.newInstance(content, 0).getClass();
			case TCKind._tk_alias:
				try {
					return getClassType(tc.content_type());
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					return null;
				}
			case TCKind._tk_void:
				return Void.TYPE;
			case TCKind._tk_short:
				return Short.TYPE;
			case TCKind._tk_long:
				return Integer.TYPE;
			case TCKind._tk_ushort:
				return Short.TYPE;
			case TCKind._tk_ulong:
				return Integer.TYPE;
			case TCKind._tk_float:
				return Float.TYPE;
			case TCKind._tk_double:
				return Double.TYPE;
			case TCKind._tk_boolean:
				return Boolean.TYPE;
			case TCKind._tk_char:
			case TCKind._tk_wchar:
				return Character.TYPE;
			case TCKind._tk_octet:
				return Byte.TYPE;
			case TCKind._tk_string:
			case TCKind._tk_wstring:
				return String.class;
			case TCKind._tk_longlong:
				return Long.TYPE;
			case TCKind._tk_ulonglong:
				return Long.TYPE;
			case TCKind._tk_union:
			case TCKind._tk_longdouble:
			case TCKind._tk_fixed:
			case TCKind._tk_value:
			case TCKind._tk_value_box:
			case TCKind._tk_native:
			case TCKind._tk_TypeCode:
			case TCKind._tk_Principal:
			case TCKind._tk_abstract_interface:
			case TCKind._tk_any:
			case TCKind._tk_null:
			default:
				throw new IllegalArgumentException("Argument typecode '" + tc.kind().value() + "' is not supported.");
		}
	}

	private DataElement getDef(TypeCode tc) {
		int value = tc.kind().value();
		switch(value) {
			case TCKind._tk_struct:
				try {
					if(structs.search(tc.id()) != -1)
						return null;
					structs.push(tc.id());
					DataElement ret = getStructDef(tc);
					structs.pop();
					return ret;
				} catch (org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				}
			case TCKind._tk_enum:
				return getEnumDef(tc);
			case TCKind._tk_alias:
			case TCKind._tk_sequence:
			case TCKind._tk_array:
				try {
					return getDef(tc.content_type());
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
					return null;
				}
			default:
				//Other types should have no Definition DataElement.
				//System.out.println("Definition not supported: "+value);
				return null;
		}
	}
	private DataStruct getStructDef(TypeCode tc) {
		if(tc.kind() != TCKind.tk_struct)
			return null;
		try {
			DataStruct ds = new DataStruct(tc.id());
			Contained ctd = ra.lookupId(tc.id());
			StructDef sd = StructDefHelper.narrow(ctd);
			StructMember[] mems = sd.members();
			for(int i = 0; i < mems.length; i++) {
				DataType dt = new BACIDataType(getClassType(mems[i].type));
				dt.setElement(getDef(mems[i].type));
				ds.add(mems[i].name,dt);
			}
			return ds;
		} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
			e.printStackTrace();
			return null;
		}
	}
	private DataEnum getEnumDef(TypeCode tc) {
		if(tc.kind() != TCKind.tk_enum)
			return null;
		try {
			DataEnum de = new DataEnum(tc.id());
			Contained ctd = ra.lookupId(tc.id());
			EnumDef ed = EnumDefHelper.narrow(ctd);
			String[] mems = ed.members();
			for(int i = 0; i < mems.length; i++)
				de.add(i,mems[i]);
			return de;
		} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
			e.printStackTrace();
			return null;
		}
	}

	public void displayAny(Any argument) {
		if (argument == null) throw new NullPointerException("argument");
		displayAny(argument.type(), argument);
	}

	public void displayAny(TypeCode tc, Any argument) {
		if (argument == null) throw new NullPointerException("argument");
		switch(tc.kind().value())
		{
			case TCKind._tk_any:
				displayAny(argument.extract_any());
				break;
			case TCKind._tk_void:
			case TCKind._tk_null:
				System.out.println("NULL!!!");
				break;
			case TCKind._tk_objref:
				System.out.println(argument.extract_Object());
				break;
			case TCKind._tk_struct:
			case TCKind._tk_except:
				System.out.println("Struct!!!");
				displayTypedef(tc,argument);
				break;
			case TCKind._tk_alias:
				try {
					displayAny(tc.content_type(),argument);
				} catch(org.omg.CORBA.TypeCodePackage.BadKind e) {
					e.printStackTrace();
				}
				break;
			case TCKind._tk_enum:
				DynEnum en;
				try {
					en = (DynEnum) ra.getDynFact().create_dyn_any(argument);
					System.out.println(en.get_as_string());
				} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
					e.printStackTrace();
				}
				break;
			case TCKind._tk_sequence:
			case TCKind._tk_array:
				System.out.println("Sequence!!!");
				displaySequence(tc,argument);
				break;
			case TCKind._tk_double:
				System.out.println(new Double(argument.extract_double()));
				break;
			case TCKind._tk_float:
				System.out.println(new Float(argument.extract_float()));
				break;
			case TCKind._tk_octet:
				//return new Byte(argument.extract_octet());
				// use short to get unsigned value 
				System.out.println(new Short((short)(argument.extract_octet() & 0xFF)));
				break;
			case TCKind._tk_longlong:
				System.out.println(new Long(argument.extract_longlong()));
				break;
			case TCKind._tk_ulonglong:
				System.out.println(new Long(argument.extract_ulonglong()));
				break;
			case TCKind._tk_long:
				System.out.println(new Integer(argument.extract_long()));
				break;
			case TCKind._tk_ulong:
				System.out.println(new Integer(argument.extract_ulong()));
				break;
			case TCKind._tk_short:
				System.out.println(new Short(argument.extract_short()));
				break;
			case TCKind._tk_ushort:
				System.out.println(new Short(argument.extract_ushort()));
				break;
			case TCKind._tk_string:
				System.out.println(argument.extract_string());
				break;
			case TCKind._tk_char:
				System.out.println(new Character(argument.extract_char()));
				break;
			case TCKind._tk_boolean:
				System.out.println(new Boolean(argument.extract_boolean()));
				break;
			default:
				throw new IllegalArgumentException("Argument typecode '" + argument.type().kind().value() + "' is not supported.");
		}
	}
	public void displayTypedef(TypeCode tc, Any argument) {
		if (argument == null) throw new NullPointerException("argument");
		if (tc == null) throw new NullPointerException("tc");
		DynAny dany = null;
		try {
			dany = ra.getDynFact().create_dyn_any(argument);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
		}
		DynStruct str = (DynStruct) dany;
		try {
			if(tc.kind() == TCKind.tk_struct) {
				for(int i = 0; i < str.component_count(); i++) {
					displayAny(str.current_component().to_any());
					str.next();
				}
			} else {
				for(int i = 0; i < str.component_count(); i++) {
					displayAny(str.current_component().to_any());
					str.next();
				}
			}
		} catch (org.omg.DynamicAny.DynAnyPackage.TypeMismatch e) {
			e.printStackTrace();
		}		
	}
	public void displaySequence(TypeCode tc, Any argument) {
		if (tc == null) throw new NullPointerException("tc");
		if (argument == null) throw new NullPointerException("argument");
		DynAny dany = null;
		try {
			dany = ra.getDynFact().create_dyn_any(argument);
		} catch(org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode e) {
			e.printStackTrace();
		}
		Class cl = getClassType(argument.type());
		Any[] els;
		if(tc.kind() == TCKind.tk_sequence)
			els = ((DynSequence) dany).get_elements();
		else if(tc.kind() == TCKind.tk_array)
			els = ((DynArray) dany).get_elements();
		else
			els = new Any[0];
		for(int i = 0; i < els.length; i++) {
			displayAny(els[i]);
		}
	}
}

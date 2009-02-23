package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.HashMap;

import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynEnum;
import org.omg.DynamicAny.DynSequence;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.NameValuePair;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import alma.acs.eventbrowser.model.EventModel;

public class DynAnyParser {
	
	private final DynAnyFactory daFactory;
	private DynAny dynAny = null;
	private ArrayList<ParsedAnyData> pdlist = new ArrayList<ParsedAnyData>(10);
	private HashMap<String, Object> propertyValues = new HashMap<String, Object>(10);

	public DynAnyParser(Any anyToParse) {
		daFactory = EventModel.getDynAnyFactory();
		try {
			dynAny = daFactory.create_dyn_any(anyToParse);
		} catch (InconsistentTypeCode e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassCastException e) {
			e.printStackTrace();
		}
	}

	public ParsedAnyData[] getParsedResults() {
		// TODO Auto-generated method stub
		parseEventAny(dynAny, "");
		return pdlist.toArray(new ParsedAnyData[0]);
	}

	private ParsedAnyData[] parseEventAny(DynAny dynAny2, String path) {
		DynAny da = dynAny2;
		String tcKindString = "";
		int tcKind = da.type().kind().value();
		ParsedAnyData entry = new ParsedAnyData(path,"","");
		try {
//			String daName = da.type().name();
//			String qualName = path+daName;
			switch (tcKind) {

			case TCKind._tk_short:
				entry.setType("short");
				entry.setValue((new Short(da.get_short())).toString());
//				pdlist.add(new ParsedAnyData(path,"short",(new Short(da.get_short())).toString()));
				break;
			case TCKind._tk_long:
				entry.setType("long");
				entry.setValue(""+da.get_long());
//				propertyValues.put(path, new Integer(da.get_long()));
				break;
			case TCKind._tk_longlong:
				entry.setType("longlong");
				entry.setValue(""+da.get_longlong());
//				propertyValues.put(path, new Long(da.get_longlong()));
				break;
			case TCKind._tk_ulonglong:
				entry.setType("ulonglong");
				entry.setValue(""+da.get_ulonglong());
//				propertyValues.put(path, new Long(da.get_ulonglong()));
				break;
			case TCKind._tk_string:
				entry.setType("string");
				entry.setValue(da.get_string());
				break;
			case TCKind._tk_boolean:
				entry.setType("boolean");
				entry.setValue(""+da.get_boolean());
				break;
			case TCKind._tk_float:
				propertyValues.put(path, new Float(da.get_float()));
				break;
			case TCKind._tk_double:
				propertyValues.put(path, new Double(da.get_double()));
				break;
			case TCKind._tk_enum:
				propertyValues.put(path, ((DynEnum)da).get_as_string());
				break;
			case TCKind._tk_array:
				System.out.println("Array found at: "+path);
				System.out.println("   with size: "+da.component_count());
				int numDisplayElements = Math.min(da.component_count(),5);
				int elementType = da.type().content_type().kind().value();
				switch (elementType) {
				case TCKind._tk_double:
					for (int j = 0; j < numDisplayElements; j++) {
						String dname = path+"["+j+"]";
						double value = da.current_component().get_double();
						pdlist.add(new ParsedAnyData(dname,"double",(new Double(value)).toString()));
						System.out.println("dname = "+dname+" value = "+value);
						da.next();
					}
					break;
					default:
						propertyValues.put(path, "Unimplemented type for array: "+elementType);
				}
				System.out.println("   content type: "+elementType);
				if (elementType == TCKind._tk_double) System.out.println("...and it's a double!");
				break;
			case TCKind._tk_struct:
			case TCKind._tk_except:
				DynStruct ds = (DynStruct)da;
				String structName = path+ds.type().name();
// TODO				pdlist.add(new TextPropertyDescriptor(path+"struct name", "Struct Name"));
				propertyValues.put(path+"struct name", structName);
				for (int i = 0; i < ds.component_count(); i++) {
					String dname = path+ds.current_member_name();
// TODO					pdlist.add(new TextPropertyDescriptor(dname, dname));
					parseEventAny(ds.current_component(), dname);
					ds.next();
				}

				NameValuePair[] nvp = ds.get_members();
				for (int i = 0; i < nvp.length; i++) {
					System.out.println(nvp[i].id+" "+ nvp[i].value);
				}
				break;
				
			case TCKind._tk_sequence:
				DynSequence dsq = (DynSequence)da;
				String seqName = path+dsq.type().name();
				propertyValues.put(path, "IDL sequences not implemented yet; found: "+seqName);

			default:
				propertyValues.put(path, "Unimplemented type: " + da.type().kind().toString());
				break;
			}
		} catch (TypeMismatch e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InvalidValue e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BadKind e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

}

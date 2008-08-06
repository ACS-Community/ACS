package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.HashMap;

import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.IPropertySource;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.DynamicAny.DynAny;
import org.omg.DynamicAny.DynAnyFactory;
import org.omg.DynamicAny.DynStruct;
import org.omg.DynamicAny.NameValuePair;
import org.omg.DynamicAny.DynAnyFactoryPackage.InconsistentTypeCode;
import org.omg.DynamicAny.DynAnyPackage.InvalidValue;
import org.omg.DynamicAny.DynAnyPackage.TypeMismatch;

import alma.acs.eventbrowser.model.EventModel;

public class EventPropertySource implements IPropertySource {
	
	private final EventData eventData;
	private final DynAnyFactory daFactory;
	private DynAny dynAny = null;
	private ArrayList<PropertyDescriptor> pdlist = new ArrayList<PropertyDescriptor>(10);
	private HashMap<String, Object> propertyValues = new HashMap<String, Object>(10);

	public EventPropertySource(EventData adaptableObject) {
		eventData=adaptableObject;
		daFactory = EventModel.getDynAnyFactory();
		try {
			dynAny = daFactory.create_dyn_any(eventData.getEventAny());
		} catch (InconsistentTypeCode e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public Object getEditableValue() {
		// TODO Auto-generated method stub
		return this;
	}

	@Override
	public IPropertyDescriptor[] getPropertyDescriptors() {
		// TODO Auto-generated method stub
		parseEventAny(dynAny, "");
		return pdlist.toArray(new PropertyDescriptor[0]);
	}

	@Override
	public Object getPropertyValue(Object id) {
		if (propertyValues.keySet().contains(id)) return propertyValues.get(id);
		return null;
	}

	@Override
	public boolean isPropertySet(Object id) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void resetPropertyValue(Object id) {
		// TODO Auto-generated method stub

	}

	@Override
	public void setPropertyValue(Object id, Object value) {
	}
	
	private PropertyDescriptor[] parseEventAny(DynAny dynAny2, String path) {
		DynAny da = dynAny2;
		int tcKind = da.type().kind().value();
		try {
//			String daName = da.type().name();
//			String qualName = path+daName;
			switch (tcKind) {

			case TCKind._tk_short:
				propertyValues.put(path, new Short(da.get_short()));
				break;
			case TCKind._tk_long:
				propertyValues.put(path, new Integer(da.get_long()));
				break;
			case TCKind._tk_ulonglong:
				propertyValues.put(path, new Long(da.get_ulonglong()));
				break;
			case TCKind._tk_string:
				propertyValues.put(path, da.get_string());
				break;
			case TCKind._tk_boolean:
				
				break;
			case TCKind._tk_float:
				
				break;
			case TCKind._tk_double:
				
				break;
			case TCKind._tk_struct:
			case TCKind._tk_except:
				DynStruct ds = (DynStruct)da;
				String structName = path+ds.type().name();
				pdlist.add(new TextPropertyDescriptor(path+"struct name", "Struct Name"));
				propertyValues.put(path+"struct name", structName);
				for (int i = 0; i < ds.component_count(); i++) {
					String dname = path+ds.current_member_name();
					pdlist.add(new TextPropertyDescriptor(dname, dname));
					parseEventAny(ds.current_component(), dname);
					ds.next();
				}

				NameValuePair[] nvp = ds.get_members();
				for (int i = 0; i < nvp.length; i++) {
					System.out.println(nvp[i].id+" "+ nvp[i].value);
				}
				break;

			default:
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

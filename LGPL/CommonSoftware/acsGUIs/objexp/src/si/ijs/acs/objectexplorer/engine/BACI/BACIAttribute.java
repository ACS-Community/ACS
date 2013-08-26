package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.*;
import org.omg.CORBA.AttributeDescription;
import org.omg.CORBA.AttributeMode;

/**
 * Insert the type's description here.
 * Creation date: (7.11.2000 22:38:17)
 * @author: 
 */
public class BACIAttribute extends Attribute {
	private BACIRemoteAccess ra = null;
	private AttributeDescription desc = null;
/**
 * Insert the method's description here.
 * Creation date: (28.6.2001 20:38:15)
 * @param template si.ijs.acs.objectexplorer.engine.BACI.BACIAttribute
 * @param parent si.ijs.acs.objectexplorer.engine.SimpleIntrospectable
 */
public BACIAttribute(BACIAttribute template, SimpleIntrospectable parent) {
	this(template.ra, (BACIRemote)parent, template.desc, template.getAttributeType());
}
/**
 * BACIAttribute constructor comment.
 */
public BACIAttribute(BACIRemoteAccess ra, BACIRemote introspectable, AttributeDescription desc, DataType type) {
	super(desc.name, introspectable, type, (desc.mode == AttributeMode.ATTR_READONLY));
	
	if (desc == null) throw new NullPointerException("desc");
	if (ra == null) throw new NullPointerException("ra");
	
	this.desc = desc;
	this.ra = ra;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:54:56)
 */
public AttributeDescription getAttributeDesc() {
	return desc;
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:38:17)
 * @return java.lang.Object
 */
public RemoteCall invokeAccessor() {
	return ra.invokeAccessor(this);
}
/**
 * Insert the method's description here.
 * Creation date: (7.11.2000 22:38:17)
 * @param value java.lang.Object
 */
public RemoteCall invokeMutator(Object value) {
	return ra.invokeMutator(this);
}
}

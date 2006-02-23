package si.ijs.acs.objectexplorer.engine.BACI;

import si.ijs.acs.objectexplorer.engine.*;
import org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription;
/**
 * Insert the type's description here.
 * Creation date: (9.11.2000 21:41:46)
 * @author: 
 */
public interface BACIRemote extends SimpleIntrospectable {
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:42:50)
 * @return java.lang.Object
 */
org.omg.CORBA.Object getCORBARef();
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 21:43:48)
 * @return org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
FullInterfaceDescription getIFDesc();
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:51:48)
 * @param ref org.omg.CORBA.Object
 */
void setCORBARef(org.omg.CORBA.Object ref);
/**
 * Insert the method's description here.
 * Creation date: (9.11.2000 23:55:13)
 * @param desc org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription
 */
void setIFDesc(FullInterfaceDescription desc);
}

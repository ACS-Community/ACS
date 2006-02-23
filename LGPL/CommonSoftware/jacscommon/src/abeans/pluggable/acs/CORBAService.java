/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;

import abeans.core.Component;

/**
 * Abeans service which provides access to the CORBA
 * Object Request Broker (ORB) object and its
 * root Portable Object Adapter (POA) object.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface CORBAService extends Component
{

	/**
	 * Returns Object Request Broker (ORB) object.
	 * @return		Object Request Broker (ORB) object
	 */
	public ORB getORB();

	/**
	 * Returns root Portable Object Adapter (POA) object.
	 * @return		root Portable Object Adapter (POA) object
	 */
	public POA getRootPOA();

}

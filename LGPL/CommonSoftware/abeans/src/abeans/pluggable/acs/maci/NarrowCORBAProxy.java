/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import abeans.core.Identifiable;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteException;

/**
 * Defines an interface narrowing all CORBA requests.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface NarrowCORBAProxy extends Proxy, Identifiable {

	/**
	 * Method narrowing all the CORBA interfaces.
	 * @param method		name of the method to be invoked
	 * @param parameters	array of parameters
	 * @return				returned value
	 * @throws RemoteException
	 */
	public Object invoke(String method, Object[] parameters) throws RemoteException;

	/**
	 * Returns the delegate CORBA object on which all methods are invoked.
	 * @return				delegate CORBA object object.
	 */
	public org.omg.CORBA.Object getDelegate();

	/**
	 * Set owner of the proxy (optional).
	 * @param object owner of the proxy
	 */
	public void setOwner(java.lang.Object object);

	/**
	 * Returns the owner of the proxy (optional).
	 * @return				owner of this proxy object.
	 */
	public java.lang.Object getOwner();
}

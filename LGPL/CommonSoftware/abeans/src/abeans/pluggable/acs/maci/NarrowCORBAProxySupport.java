/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.pluggable.RemoteException;

/**
 * Support implementation of NarrowCORBAProxy interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public abstract class NarrowCORBAProxySupport implements NarrowCORBAProxy {

	/**
	 * Identifier.
	 */
	private transient Identifier id;

	/**
	 * Owner (optional but required properties).
	 */
	private java.lang.Object owner;

	/**
	* Default constructor.
	* @param delegate delegate CORBA object object.
	*/
	public NarrowCORBAProxySupport(org.omg.CORBA.Object delegate)
	{
		// noop
	}

	/**
	 * Method narrowing all the CORBA interfaces.
	 * @param method		name of the method to be invoked
	 * @param parameters	array of parameters
	 * @return				returned value
	 * @throws RemoteException
	 * @see abeans.pluggable.acs.maci.NarrowCORBAProxy#invoke(java.lang.String, java.lang.Object[])
	 */
	public abstract Object invoke(String method, Object[] parameters) throws RemoteException;

	/**
	 * Returns the delegate CORBA object on which all methods are invoked.
	 * @return				delegate CORBA object object.
	 * @see abeans.pluggable.acs.maci.NarrowCORBAProxy#getDelegate()
	 */
	public abstract org.omg.CORBA.Object getDelegate();
	
	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier() {
		if (id == null)
			id = new IdentifierSupport(getClass().getName(), getClass().getName(), Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug() {
		return false;
	}

	/**
	 * Return owner of the proxy (optional).
	 * @return owner of the proxy
	 */
	public java.lang.Object getOwner() {
		return owner;
	}

	/**
	 * Set owner of the proxy (optional).
	 * @param object owner of the proxy
	 */
	public void setOwner(java.lang.Object object) {
		owner = object;
	}

}

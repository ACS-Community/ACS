package abeans.models.acs.baci;

import abeans.core.InitializationException;
import abeans.models.Family;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteInfo;

/*
 * @@COPYRIGHT@@
 */

/**
 * Support class implementing <code>ProxyContainer</code> interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ProxyContainerSupport implements ProxyContainer {

	/**
	 * Proxy object.
	 */
	protected Proxy proxy;
	
	/**
	 * Default donstructor.
	 */
	public ProxyContainerSupport()
	{
		// noop
	}

	/**
	 * Constructor.
	 * @param parent parent family of this component.
	 * @param info	 remote info od this component.
	 * @throws InitializationException
	 */
	public ProxyContainerSupport(Family parent, RemoteInfo info)
		throws InitializationException
	{
		// noop
	}

	/**
	 * @see abeans.models.acs.baci.ProxyContainer#getProxy()
	 */
	public Proxy getProxy() {
		return proxy;
	}

	/**
	 * @see abeans.models.acs.baci.ProxyContainer#setProxy(abeans.pluggable.Proxy)
	 */
	public void setProxy(Proxy proxy) {
		this.proxy = proxy;
	}

}

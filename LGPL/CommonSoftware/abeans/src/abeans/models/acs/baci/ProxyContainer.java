package abeans.models.acs.baci;

/*
 * @@COPYRIGHT@@
 */

import abeans.pluggable.Proxy;

/**
 * Interface defining an object containing an proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface ProxyContainer {

	/**
	 * Get proxy of the object. 
	 * @return proxy.
	 */
	public Proxy getProxy();

	/**
	 * Set proxy of the object. 
	 * @param	proxy	proxy to be set.
	 */
	public void setProxy(Proxy proxy);
}

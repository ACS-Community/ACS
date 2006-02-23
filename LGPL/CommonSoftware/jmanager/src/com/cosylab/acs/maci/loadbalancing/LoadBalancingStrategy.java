/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.loadbalancing;

import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.ContainerInfo;

/**
 * Interface defining load balancing strategy.
 * NOTE: this is not a final interface - the implementation should have an
 * option of monitoring containers all the time (simply registering as an adminstrator?).
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public interface LoadBalancingStrategy {

	/**
	 * Select the most appropriate (i.e. the least loaded) container.
	 * @param requestor		client (or component) requiring component.
	 * @param containers	all registered containers, can be <code>null</code> when no containers are logged in.
	 * @return	container name, <code>null</code> if failed.
	 */
	public String selectContainer(ClientInfo requestor, ContainerInfo[] containers);
}

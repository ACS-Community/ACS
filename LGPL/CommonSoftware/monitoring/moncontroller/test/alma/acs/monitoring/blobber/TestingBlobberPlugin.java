/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

/** 
 * @author  rtobar   
 * @version $Id: TestingBlobberPlugin.java,v 1.1 2011/09/21 17:02:37 rtobar Exp $
 * @since    
 */

package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.MonitorDAO;

public class TestingBlobberPlugin extends BlobberPlugin {

	public TestingBlobberPlugin(ContainerServices containerServices) {
		super(containerServices);
	}

	@Override
	public boolean isProfilingEnabled() {
		return false;
	}

	@Override
	public int getCollectorIntervalSec() {
		return 5;
	}

	@Override
	public List<MonitorDAO> createMonitorDAOs()
			throws AcsJCouldntCreateObjectEx {
		return new ArrayList<MonitorDAO>(Arrays.asList(new MonitorDAO[] { new MonitorDAO() {
			@Override
			public void store(ComponentData inData) throws Exception {
			}

			// no-ops
			@Override
			public void openTransactionStore() throws AcsJException {
			}
			
			@Override
			public void closeTransactionStore() throws AcsJException {
			}
			
			@Override
			public void close() {
			}
		}}));
	}

}

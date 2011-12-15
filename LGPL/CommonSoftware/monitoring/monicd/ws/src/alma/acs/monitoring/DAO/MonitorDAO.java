/*
 * ALMA - Atacama Large Millimiter Array
 * Copyright (c) European Southern Observatory, 2011 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */


/** MonitorDAO is the interface that defines the interaction
 * between Blobbers and DAO Persistence Layer.
 *
 * @author Pablo Burgos
 * @since ACS-8_0_0-B Jun2009
 * @version "@(#) $Id: MonitorDAO.java,v 1.6 2011/12/15 13:35:09 mmora Exp $
 */
package alma.acs.monitoring.DAO;

import alma.acs.exceptions.AcsJException;
import alma.acs.monitoring.blobber.BlobberPlugin;

/**
 * This interface connects the upper layers of the blobber components,
 * which belong to ACS and deal with infrastructural concerns and general data aggregation tasks,
 * with the lower layers contributed from outside of ACS, 
 * which may have knowledge about some special devices
 * and store the data in DBs, files etc. 
 */
public interface MonitorDAO {

    /**
     * Passes data from the upper layers to this DAO.
     */
    public void store(ComponentData inData) throws Exception;

    /**
     * If the DAO supports transactions (e.g. for DB insertion), 
     * it should open a transaction here.
     * Currently a transaction contains the data from all device components from one container.
     * @see #closeTransactionStore()
     */
    public void openTransactionStore() throws AcsJException;

    /**
     * If the DAO supports transactions (e.g. for DB insertion), 
     * it should close the currently open transaction here.
     * @see #openTransactionStore()
     */
    public void closeTransactionStore() throws AcsJException;
    
    /**
     * Called by the BlobberPlugin when the DAO is no longer needed,
     * typically shortly before the component gets unloaded.
     * The DAO should clean up resources.
     * After this call, the DAO should no longer be used.
     * @see BlobberPlugin#cleanUp()
     */
    public void close();
}

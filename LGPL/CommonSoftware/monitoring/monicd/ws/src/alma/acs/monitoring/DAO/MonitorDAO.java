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
 * @version "@(#) $Id: MonitorDAO.java,v 1.4 2011/07/31 04:19:24 hsommer Exp $
 */
package alma.acs.monitoring.DAO;

import alma.acs.exceptions.AcsJException;

public interface MonitorDAO {

    public void store(ComponentData inData) throws Exception;

    public void close();

    public void openTransactionStore() throws AcsJException;

    public void closeTransactionStore() throws AcsJException;
}

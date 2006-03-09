/*
 * ALMA - Atacama Large Millimiter Array (c) Associated Universities Inc., 2002
 * (c) European Southern Observatory, 2002 Copyright by ESO (in the framework of
 * the ALMA collaboration), All rights reserved
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
 * 
 * CommonNC.java
 * 
 * Created on March 6, 2003, 11:02 AM
 */
// //////////////////////////////////////////////////////////////////////////////
package alma.acs.nc;

import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;

// //////////////////////////////////////////////////////////////////////////////
/**
 * This provides common functionality to both Consumer and Supplier.
 * 
 * @author dfugate
 */
public interface CommonNC {
   /**
    * Supplier and Consumer both need a Helper which provides access to the
    * naming service among other things.
    * 
    * @return Reference to the object's event channel Helper.
    */
   public alma.acs.nc.Helper getHelper();

   /**
    * Not too useful right now but it has to be specified.
    */
   static final InterFilterGroupOperator IFGOP = InterFilterGroupOperator.AND_OP;
   // //////////////////////////////////////////////////////////////////////////
}

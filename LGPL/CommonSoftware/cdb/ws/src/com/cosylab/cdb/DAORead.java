package com.cosylab.cdb;

/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
import org.omg.CORBA.*;
import com.cosylab.CDB.*;

import java.net.InetAddress;

import alma.acs.util.ACSPorts;
import java.util.logging.Logger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogLevel;

public class DAORead {
	static	int indent = 0;

	public static void main(String args[]) {
		Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DAORead", true);
		try {
			String strIOR = null;

			if (args.length < 2) {
				System.out.println("Usage: cmd curl field [-d ior]");
				return;
			}
			String curl = args[0];
			String field = args[1];
			
			// test for IOR in cmd line
			for (int i = 2; i < args.length; i++) {
				if (args[i].equals("-d")) {
					if (i < args.length - 1) {
						strIOR = args[++i];
					}
				}
				if (args[i].equals("-h") || args[i].equals("-help")) {
					System.out.println("Usage: cmd curl [-d ior]");
					return;
				}
			}
			if (strIOR == null) {
				// use default
				strIOR = "corbaloc::" + InetAddress.getLocalHost().getHostName() + ":" + ACSPorts.getCDBPort() + "/CDB";
			}

			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			DAL dal = DALHelper.narrow(orb.string_to_object(strIOR));
			
			DAO dao = dal.get_DAO_Servant(curl);
			
			m_logger.log(AcsLogLevel.INFO, curl + "/" + field + " = " + dao.get_field_data(field));

		}
		/*catch (XMLerror e) {
			System.out.println("XMLerror : " + e.msg );
			e.printStackTrace(System.out);
		}*/
		catch (Exception e) {
			m_logger.log(AcsLogLevel.NOTICE, "ERROR : " + e); 
			e.printStackTrace(System.out);
		}
	}
}

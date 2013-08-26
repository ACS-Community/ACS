package com.cosylab.cdb;

/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2006
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
 * @author cparedes
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
import org.omg.CORBA.*;
import com.cosylab.CDB.*;

import java.util.logging.Logger;
import java.net.InetAddress;

import alma.acs.util.ACSPorts;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogLevel;

/**
 * Modify the component attributes, like Container and Code, given the name.
 * Assumes the curl of the components = "MACI/Components"
 * @author cparedes
 */
public class CDBChangeDeployment {
	static	int indent = 0;
	private static Logger m_logger;
	public static void main(String args[]) {
		try {
			if (args.length != 3) {
				System.out.println("Usage: cmd instance_name newcontainer newcode");
				return;
			}
			m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("CDBChangeDeployment", true);
			String in_name = args[0];
			String new_container = args[1];
			String new_code = args[2];
			String curl = "MACI/Components"; 

			String strIOR = "corbaloc::" + InetAddress.getLocalHost().getHostName() + ":" + ACSPorts.getCDBPort() + "/CDB";

			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			WDAL wdal = WDALHelper.narrow(orb.string_to_object(strIOR));
			
			try{
				WDAO wdao = wdal.get_WDAO_Servant(curl);
				wdao.set_string(in_name + "/Container", new_container);
				wdao.set_string(in_name + "/Code", new_code);
			}catch(Exception e){
				WDAO wdao = wdal.get_WDAO_Servant(curl+"/"+in_name);
				wdao.set_string( "Container", new_container);
				wdao.set_string( "Code", new_code);
			}
		}
		catch (CDBXMLErrorEx e) {
			 AcsJCDBXMLErrorEx je =
				AcsJCDBXMLErrorEx.fromCDBXMLErrorEx(e);
			m_logger.log(AcsLogLevel.NOTICE, "CDBXMLErrorEx Error", e);
			je.log(m_logger);
			e.printStackTrace(System.out);
		}
		catch (Exception e) {
			m_logger.log(AcsLogLevel.NOTICE, "Error", e);
			e.printStackTrace(System.out);
		}
	}
}

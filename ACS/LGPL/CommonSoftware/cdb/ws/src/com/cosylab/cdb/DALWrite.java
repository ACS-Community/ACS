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
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;

public class DALWrite {
	static	int indent = 0;

	public static void main(String args[]) {
		try {
			String strIOR = null;

			if (args.length < 3) {
				System.out.println("Usage: cmd curl field value [-d ior -h]");
				return;
			}
			String curl = args[0];
			String field = args[1];
			String value = args[2];

			// test for IOR in cmd line
			for (int i = 3; i < args.length; i++) {
				if (args[i].equals("-d")) {
					if (i < args.length - 1) {
						strIOR = args[++i];
					}
				}
				if (args[i].equals("-h") || args[i].equals("-help")) {
					System.out.println("Usage: cmd curl field value [-d ior -h]");
					return;
				}
			}
			if (strIOR == null) {
				// use default
				strIOR = "corbaloc::" + InetAddress.getLocalHost().getHostName() + ":" + ACSPorts.getCDBPort() + "/CDB";
			}

			// create and initialize the ORB
			ORB orb = ORB.init(args, null);

			WDAL wdal = WDALHelper.narrow(orb.string_to_object(strIOR));

			WDAO wdao = wdal.get_WDAO_Servant(curl);
			
			System.out.println("Reading '" + curl + "/" + field + "'='" + wdao.get_string(field) + "'");

			wdao.set_string(field, value);

			System.out.println("Setting '" + curl + "/" + field + "'='" + value + "'");
		}
		catch (CDBXMLErrorEx e) {
			AcsJCDBXMLErrorEx e2 = new AcsJCDBXMLErrorEx(e);
			System.out.println("XMLerror : " + e2.getCurl() );
			e.printStackTrace(System.out);
		}
		catch (Exception e) {
			System.out.println("ERROR : " + e);
			e.printStackTrace(System.out);
		}
	}
}

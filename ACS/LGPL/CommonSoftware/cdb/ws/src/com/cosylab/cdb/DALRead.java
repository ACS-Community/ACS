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
import com.cosylab.cdb.jdal.XMLHandler;
import com.cosylab.cdb.jdal.XMLTreeNode;

import java.io.*;
import java.net.InetAddress;
import javax.xml.parsers.*;

import org.xml.sax.*;
import java.util.Iterator;

import java.util.logging.Logger;

import alma.acs.util.ACSPorts;

import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.cdbErrType.wrappers.AcsJCDBRecordDoesNotExistEx;
import alma.cdbErrType.CDBXMLErrorEx;
import alma.cdbErrType.CDBRecordDoesNotExistEx;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogLevel;

public class DALRead {
	static	int indent = 0;

	public static void main(String args[]) {

	        Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DALRead", false);

		try {
			String curl;
			String strIOR = null;
			boolean rawOutput = false;

			if (args.length < 1) {
				System.out.println("Usage: cmd curl [-d ior -raw -h]");
				return;
			}
			curl = args[0];

			// test for IOR in cmd line
			for (int i = 0; i < args.length; i++) {
				if (args[i].equals("-d")) {
					if (i < args.length - 1) {
						strIOR = args[++i];
					}
				}
				if (args[i].equals("-raw")) {
					rawOutput = true;
				}
				if (args[i].equals("-h")){
					System.out.println("Usage: cmd curl [-d ior -raw -h]");
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

			String xml = dal.get_DAO(curl);
			if( rawOutput ) {
				m_logger.log(AcsLogLevel.INFO, "Curl data:\n" + xml);
				return;
			}

			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			XMLHandler xmlSolver = new XMLHandler(false, m_logger);
			saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
			if (xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				AcsJCDBXMLErrorEx xmlErr = new AcsJCDBXMLErrorEx();
				System.err.println(info);
				throw xmlErr;
			}
			System.out.println( "Env " + System.getProperty("HOMEPATH") );
			// dump contents
			System.out.println("________________________________________________________");
			walk(xmlSolver.m_rootNode);
			System.out.println("________________________________________________________");
		}
		catch (AcsJCDBXMLErrorEx e) {
			e.printStackTrace();
			e.log(m_logger);
		}
		catch (CDBXMLErrorEx e) {
		        AcsJCDBXMLErrorEx je = 
			    AcsJCDBXMLErrorEx.fromCDBXMLErrorEx(e);
			String smsg = "XML Error \tCURL='" + je.getCurl()+"'\n\t\tFilename='"+je.getFilename()+"'\n\t\tNodename='"+je.getNodename()+"'\n\t\tMSG='"+je.getErrorString()+"'";
			je.log(m_logger);
			m_logger.log(AcsLogLevel.NOTICE, smsg, je);	
		}
		catch (CDBRecordDoesNotExistEx e) {
		        AcsJCDBRecordDoesNotExistEx je = 
			    AcsJCDBRecordDoesNotExistEx.fromCDBRecordDoesNotExistEx(e);
			String smsg = "Record does not exist \tCURL='" + je.getCurl() + "'";
			je.log(m_logger);
			m_logger.log(AcsLogLevel.NOTICE, smsg, je);	
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}
	public static void walk(XMLTreeNode node) {

		Iterator<String> i = node.getFieldMap().keySet().iterator();
		if( i.hasNext() ) {
			outputIndentation(indent);
			System.out.print("Node " + node.getName());
			System.out.println();
		}
		while (i.hasNext()) {
			String key = i.next();
			String value = node.getFieldMap().get(key);
			System.out.print("    " + key + "=\"" + value + "\"");
			System.out.println();
		}
		indent++;
		Iterator nodesIter = node.getNodesMap().keySet().iterator();
		while (nodesIter.hasNext()) {
			String key = (String) nodesIter.next();
			XMLTreeNode value = (XMLTreeNode) node.getNodesMap().get(key);
			walk(value);
		}
		indent--;
	}
	private static void outputIndentation(int indent) {
		for (int i = 0; i < indent; i++) {
			System.out.print("  ");
		}
	}
}

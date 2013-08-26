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
 */
import java.io.StringReader;
import java.net.InetAddress;
import java.util.Iterator;
import java.util.logging.Logger;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.omg.CORBA.ORB;
import org.xml.sax.InputSource;

import com.cosylab.CDB.WDAL;
import com.cosylab.CDB.WDALHelper;
import com.cosylab.CDB.WDAO;
import alma.cdbErrType.wrappers.AcsJCDBXMLErrorEx;
import alma.cdbErrType.CDBXMLErrorEx;
import com.cosylab.cdb.jdal.XMLHandler;
import com.cosylab.cdb.jdal.XMLTreeNode;

import alma.acs.util.ACSPorts;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.AcsLogLevel;


/**
 * Allow switching the default component of a given IDL type. 
 * This is useful if there is only one component instance per type, but the CDB contains two entries, 
 * one for the real component, and one for a simulator. 
 * The client (test code or real component in the system) is then not affected by this switching, 
 * because it always uses the IDL type to retrieve the component.
 * 
 * Sets Default=true the given component and sets Default=false the other components with the same
 * type.
 * Assumes the curl of the components = "MACI/Components"
 * 
 * @author cparedes
 */
public class CDBDefault {
	static String strIOR;
	static String curl;
	static String curl_allComponents;
	static ORB orb;
	private static Logger m_logger;
	
	public static void main(String args[]) {
		try {
			strIOR=null;
			if (args.length < 2) {
				System.out.println("Usage: cmd <idl_type> <instance_name> [-d ior -h]");
				return;
			}
			
 			m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("CDBDefault", true);
			String in_type = args[0];
			String in_name = args[1];
		
			curl_allComponents = "MACI/Components";
			curl = curl_allComponents + "/" + in_name;

			for (int i = 0; i < args.length; i++) {
				if (args[i].equals("-d")) {
					if (i < args.length - 1) {
						strIOR = args[++i];
					}
				}
				if (args[i].equals("-h")){
					System.out.println("Usage: cmd idl_type instance_name [-d ior -h]");
					return;
				}
			}
			if (strIOR == null) {
				strIOR = "corbaloc::" + InetAddress.getLocalHost().getHostName() + ":" + ACSPorts.getCDBPort() + "/CDB";
			}
			// create and initialize the ORB
			orb = ORB.init(new String[0], null);

			WDAL wdal = WDALHelper.narrow(orb.string_to_object(strIOR));

			String xml = wdal.get_DAO(curl_allComponents);

			SAXParserFactory factory = SAXParserFactory.newInstance();
			SAXParser saxParser = factory.newSAXParser();
			XMLHandler xmlSolver = new XMLHandler(false, m_logger);
			saxParser.parse(new InputSource(new StringReader(xml)), xmlSolver);
			
			if (xmlSolver.m_errorString != null) {
				String info = "XML parser error: " + xmlSolver.m_errorString;
				AcsJCDBXMLErrorEx cdbxmlErr =  new AcsJCDBXMLErrorEx();
				//cdbxmlErr.setFileN
				//XMLerror xmlErr = new XMLerror(info);
				throw cdbxmlErr;
			}		
			setDefault(xmlSolver.m_rootNode, in_type, in_name);
		}
		catch (AcsJCDBXMLErrorEx e) {
			m_logger.log(AcsLogLevel.NOTICE, "Xml Error", e);
			e.printStackTrace();
		}
		catch (Exception e) {
			m_logger.log(AcsLogLevel.NOTICE, "Error", e);
			e.printStackTrace();
		}
	}
    /**
     * Recursively go throw the xml finding nodes with the same Type. If the type match,
     * compares the Name. If is the same name, sets the Default=true, in the other case
     * sets the Default attribute to false.
     * @param node_root the root of all components xml .
     * @param in_type  the type of the component.
     * @param in_name the name of the component to set up default.
     *                             
     */
	public static void setDefault(XMLTreeNode node_root, String in_type, String in_name){
		try{
			Iterator<String> nodesIter = node_root.getNodesMap().keySet().iterator();
			WDAL wdal = WDALHelper.narrow(orb.string_to_object(strIOR));
			while (nodesIter.hasNext()) {
				String key = nodesIter.next();
				XMLTreeNode node = node_root.getNodesMap().get(key);
		
				String name = node.getFieldMap().get("Name");
				String type = node.getFieldMap().get("Type");
				String isDefault = node.getFieldMap().get("Default");

				String strTrue = "true";
				if(in_type.equals(type)){
					if(strTrue.equals(isDefault)){
						if(in_name.equals(name)) return;
						else{
							//write Default = false
							try{
								//System.out.println("1-"+curl_allComponents+"\t"+name);
								WDAO wdao = wdal.get_WDAO_Servant(curl_allComponents);
								wdao.set_string(name +"/Default", "false");
							}catch(Exception e){
								//System.out.println("2-"+curl_allComponents+ "/" + name);
								WDAO wdao = wdal.get_WDAO_Servant(curl_allComponents + "/" + name);
								wdao.set_string("Default", "false");
							}
						}
					}else if(in_name.equals(name)){
//						write Default = true
						try{
							//System.out.println("3-"+curl_allComponents+ "\t" + name);
							WDAO wdao = wdal.get_WDAO_Servant(curl_allComponents);
							wdao.set_string(name + "/Default", "true");
						}catch(Exception e){
							//System.out.println("4-"+curl);
							WDAO wdao = wdal.get_WDAO_Servant(curl);
							wdao.set_string("Default", "true");
						}
					}
				}
				XMLTreeNode value = node_root.getNodesMap().get(key);
				setDefault(value, in_type,in_name);
			}
		}catch(CDBXMLErrorEx e){
			m_logger.log(AcsLogLevel.NOTICE, "Xml Error", e);
			e.printStackTrace();
		}
		catch(Exception e){
			m_logger.log(AcsLogLevel.NOTICE, "Error", e);
			e.printStackTrace();
		}
		
	}
}

package acs.ifr;

/*
 *        JacORB - a free Java ORB
 *
 *   Copyright (C) 1997-2004 Gerald Brose.
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Library General Public
 *   License as published by the Free Software Foundation; either
 *   version 2 of the License, or (at your option) any later version.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Library General Public License for more details.
 *
 *   You should have received a copy of the GNU Library General Public
 *   License along with this library; if not, write to the Free
 *   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

import java.io.*;
import alma.acs.util.ACSPorts;
import com.cosylab.CDB.JDALHelper;
import org.jacorb.ir.IdlWriter;

public class ContainerInterfacesRetriever
{
	String strIOR = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";
	com.cosylab.CDB.DAL dal;
	org.omg.CORBA.Repository ir;
	long totalIFRTime;
	public static void main( String[] args )
	{
		ContainerInterfacesRetriever cir = new ContainerInterfacesRetriever(args);
	}

	public ContainerInterfacesRetriever(String[] args) {
		totalIFRTime = 0;
		if( args.length != 1 ) {
			System.err.println("Usage: retrieveContainer <ContainerName>");
			System.exit(1);
		}
		
		try {
			org.omg.CORBA.ORB orb = org.omg.CORBA.ORB.init( args, null );
			ir = org.omg.CORBA.RepositoryHelper.narrow(orb.resolve_initial_references("InterfaceRepository"));
			if( ir == null ) {
				System.out.println( "Could not find IR.");
				System.exit(1);
			}  
			dal = JDALHelper.narrow(orb.string_to_object(strIOR));
			if( dal == null ) {
				System.out.println( "Could not find CDB.");
				System.exit(1);
			}  
			long initTime = System.nanoTime();
			checkComponentInterface("MACI/Components", args[0]);
			long endTime = System.nanoTime();
			System.out.println(((endTime-initTime)/1000000.0)+"[ms] "+args[0]);
			System.out.println((totalIFRTime/1000000.0)+"[ms] "+ "IFR Time on "+args[0]);
			   
		} catch ( Exception e) {
			e.printStackTrace();
		}
	}

	public void checkComponentInterface(String curl, String container) {
		String nodeList = dal.list_nodes(curl).trim();
		String daoList = dal.list_daos(curl).trim();
		String[] nodes = nodeList.split(" ");
		for(String node: nodes) {
			if(node.compareTo("") != 0) {
				daoList = daoList.replaceFirst(" "+node+" "," ");
				daoList = daoList.replaceFirst("^"+node+" ","");
				daoList = daoList.replaceFirst(" "+node+"$","");
				daoList = daoList.replaceFirst("^"+node+"$","");
				checkComponentInterface(curl+"/"+node, container);
			}
		}
		String[] daos = daoList.split(" ");
		for(String daoe: daos) {
			if(daoe.compareTo("") == 0)
				continue;
			org.omg.CORBA.Contained c = null;
			try {
				com.cosylab.CDB.DAO dao = dal.get_DAO_Servant(curl+"/"+daoe);
				if(dao.get_string("Container").compareTo(container) == 0) {
					long t1 = System.nanoTime();
					long mt1,mt2;
					mt1 = System.nanoTime();
					c = ir.lookup_id(dao.get_string("Type"));
					mt2 = System.nanoTime();
					System.out.println(((mt2-mt1)/1000000.0)+"[ms] lookup "+dao.get_string("Type"));
					mt1 = System.nanoTime();
					org.omg.CORBA.InterfaceDef d = org.omg.CORBA.InterfaceDefHelper.narrow(c);
					mt2 = System.nanoTime();
					System.out.println(((mt2-mt1)/1000000.0)+"[ms] narrow "+dao.get_string("Type"));
					if( d != null )
					{
						mt1 = System.nanoTime();
						org.omg.CORBA.InterfaceDefPackage.FullInterfaceDescription fd = d.describe_interface();
						mt2 = System.nanoTime();
						System.out.println(((mt2-mt1)/1000000.0)+"[ms] descri "+dao.get_string("Type"));
						mt1 = System.nanoTime();
						String[] bases = new String[fd.base_interfaces.length+1];
						int i;
						for(i = 0; i < fd.base_interfaces.length; i++)
							bases[i] = fd.base_interfaces[i];
						bases[i] = fd.id;
						mt2 = System.nanoTime();
						System.out.println(((mt2-mt1)/1000000.0)+"[ms] basesi "+dao.get_string("Type"));
					} 
					else 
						System.out.println( dao.get_string("Name")+": "+dao.get_string("Type") + " not found in IR.");
					long t2 = System.nanoTime();
					totalIFRTime += t2 - t1;
					System.out.println(((t2-t1)/1000000.0)+"[ms] "+dao.get_string("Type"));
				}
			} catch(alma.cdbErrType.CDBFieldDoesNotExistEx e) {
				System.out.println(curl+"/"+daoe);
				e.printStackTrace();
				System.exit(1);
			} catch(Exception e) {
				System.out.println(curl+"/"+daoe);
				e.printStackTrace();
			}
		}
	}
}

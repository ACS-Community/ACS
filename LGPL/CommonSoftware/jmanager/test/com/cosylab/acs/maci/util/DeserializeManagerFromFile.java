package com.cosylab.acs.maci.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;

import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.manager.HandleDataStore;
import com.cosylab.acs.maci.manager.ManagerImpl;

public class DeserializeManagerFromFile {

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Throwable
	{
		 File f = new File(args[0]);
		 ObjectInputStream obj = new ObjectInputStream(new FileInputStream(f));
		 
		 // we cast directly to the implementation
		 ManagerImpl manager = (ManagerImpl)obj.readObject();
		 
		 {
			//
			// list all active components
			//
			HandleDataStore components = manager.getComponents();
			System.out.println("Capacity of handle data store: " + components.capacity());
			System.out.println(components.size() + " component(s) stored:");
			int h = components.first();
			while (h != 0)
			{
				ComponentInfo componentInfo = (ComponentInfo)components.get(h);
			
				System.out.println("\tName            : " + componentInfo.name);
				System.out.println("\tHandle          : " + componentInfo.h + ", " + HandleHelper.toString(componentInfo.h));
				System.out.println("\tType            : " + componentInfo.type);
				System.out.println("\tCode            : " + componentInfo.code);
				System.out.println("\tContainer name  : " + componentInfo.container_name);
				System.out.println("\tContainer handle: " + HandleHelper.toString(componentInfo.container));
				System.out.println("\tClients         : count = " + componentInfo.clients.length);
				for (int j = 0; j < componentInfo.clients.length; j++)
					System.out.println("\t             \t" + componentInfo.clients[j]);
				System.out.println("\t-------------------------------");
				
				h = components.next(h);
			}

		 }
		 
		System.out.println();
		System.out.println();
		System.out.println();
		
		{
			//
			// list all active containers
			//
			HandleDataStore containers = manager.getContainers();
			System.out.println("Capacity of handle data store: " + containers.capacity());
			System.out.println(containers.size() + " container(s) returned:");
			int h = containers.first();
			while (h != 0)
			{
				ContainerInfo containersInfo = (ContainerInfo)containers.get(h);
			
				System.out.println("\tName            : " + containersInfo.name);
				System.out.println("\tHandle          : " + containersInfo.h + ", " + HandleHelper.toString(containersInfo.h));
				System.out.println("\tComponents      : count = " + containersInfo.components.length);
				for (int j = 0; j < containersInfo.components.length; j++)
					System.out.println("\t             \t" + containersInfo.components[j]);
				System.out.println("\t-------------------------------");
			}

		}
		
		System.out.println();
		System.out.println();
		System.out.println();

		{
			//
			// list all active clients
			//
			
			HandleDataStore clients = manager.getClients();
			System.out.println("Capacity of handle data store: " + clients.capacity());
			System.out.println(clients.size() + " clients(s) returned:");
			int h = clients.first();
			while (h != 0)
			{
				ClientInfo clientsInfo = (ClientInfo)clients.get(h);
				System.out.println("\tName            : " + clientsInfo.name);
				System.out.println("\tHandle          : " + clientsInfo.h + ", " + HandleHelper.toString(clientsInfo.h));
				System.out.println("\tComponents      : count = " + clientsInfo.components.length);
				for (int j = 0; j < clientsInfo.components.length; j++)
					System.out.println("\t             \t" + clientsInfo.components[j]);
				System.out.println("\t-------------------------------");
			}
		}

		
		System.out.println();
		System.out.println();
		System.out.println();
		
		{
			//
			// list all active administrators
			//
			
			HandleDataStore clients = manager.getAdministrators();
			System.out.println("Capacity of handle data store: " + clients.capacity());
			System.out.println(clients.size() + " administrators(s) returned:");
			int h = clients.first();
			while (h != 0)
			{
				ClientInfo clientsInfo = (ClientInfo)clients.get(h);
				System.out.println("\tName            : " + clientsInfo.name);
				System.out.println("\tHandle          : " + clientsInfo.h + ", " + HandleHelper.toString(clientsInfo.h));
				System.out.println("\tComponents      : count = " + clientsInfo.components.length);
				for (int j = 0; j < clientsInfo.components.length; j++)
					System.out.println("\t             \t" + clientsInfo.components[j]);
				System.out.println("\t-------------------------------");
			}
		}
		
		System.out.println();
		System.out.println();
		System.out.println();

		System.out.println("# of unavailable components in a map: " + manager.getUnavailableComponents().size());
		System.out.println("# of default components in a map: " + manager.getDefaultComponents().size());
		System.out.println("# of active alarms in a map: " + manager.getActiveAlarms().size());
		System.out.println("# of released handles in a map: " + manager.getReleasedHandles().size());
	}
}

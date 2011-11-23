/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import java.util.Hashtable;
import java.util.concurrent.LinkedBlockingQueue;


/**
* This is a manager class designed to transport information from one thread to
* an other. The comunication is based in channels, each channel has its own
* LinkedBlockingQueue to gurantee data consistency. Once you get a channel to
* eficiently extract the data the darinTo(Collection c) meber is prefered, as it
* will move the data from the queue to the colection in an atomic action. 
*/
public class ThreadCommunicator {
	private static ThreadCommunicator _instance;
	Hashtable<String,LinkedBlockingQueue<DataItem>> channels = null;
	

	/**
	* Constructor that mantains the singleton consistency for each Communicator.
	* @return An instance of ThreadCommunicator
	*/
	public static synchronized ThreadCommunicator getInstance() {
		if (_instance==null){
			_instance = new ThreadCommunicator();
		}
		return _instance;
	}

	/**
	* Constructs a new HashTable for the Communicator
	*/
	private ThreadCommunicator(){
		channels = new Hashtable<String,LinkedBlockingQueue<DataItem>>();
	}

	/**
	* Checks if a channel already exist, if it does, throws an Exception, else, adds a new Queue to the channel and assigns a name to it.
	* @param cName Name of the Channel to be created
	* @return A new BlockingQueue with the given parametres as values 
	*/
	public LinkedBlockingQueue<DataItem> createChannel(String cName){
		if(channels.get(cName)!=null)
			throw new IllegalArgumentException("Channel "+ cName + "already exists");
		LinkedBlockingQueue<DataItem> c = new LinkedBlockingQueue<DataItem>();
		channels.put(cName,c);
		return c;
	}

	/**
	* Getter for a channel given it's name
	* @return The found channel. (null if it doesn't exists)
	*/
	public LinkedBlockingQueue<DataItem> getChannel(String cName){
		return channels.get(cName);
	}

	/**
	* Removes a channel of the Queue.
	* @param cName Name of the channel to remove
	*/
	public void removeChannel(String cName){
		channels.remove(cName);
	}
		
}

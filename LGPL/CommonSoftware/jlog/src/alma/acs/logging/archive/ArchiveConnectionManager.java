/*
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.archive;

import javax.swing.ImageIcon;

import com.cosylab.logging.LoggingClient;

import java.lang.reflect.Constructor;
import java.util.logging.Logger;

/**
 * A class to connect and interact with the archive
 * 
 * The connection with the database is not always available.
 * There are 2 possibilities:
 * 	1. the code to connect to the DB is not available (it comes from
 * 	   a module in ARCHIVE)
 *  2. the code is availbale but for some reason something in the
 *     communication with the DB doesn't work as expected
 *     
 * @author acaproni
 *
 */
public class ArchiveConnectionManager {
	
	// The possible states of the database
	private final int DATABASE_OK = 0;
	private final int DATABASE_NOP = 1;
	private final int DATABASE_WORKING =2;
	
	// The status of the connection with the DB
	// It can be DATABASE_OK, DATABASE_NOP, ...
	private int status;
	
	// The class to talk to the archive
	private Object archive = null;
	
	LoggingClient logging;
	
	// The icons showed in the main window
	ImageIcon[] databaseStatusIcons = new ImageIcon[] {
			new ImageIcon(ArchiveConnectionManager.class.getResource("/databaseLink.png")),
			new ImageIcon(ArchiveConnectionManager.class.getResource("/databaseNOP.png")),
			new ImageIcon(ArchiveConnectionManager.class.getResource("/databaseBusy.png"))
	};
	
	public ArchiveConnectionManager(LoggingClient loggingClient) {
		if (loggingClient==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient reference");
		}
		logging = loggingClient;
		archive = loadArchiveClass();
		if (archive==null) {
			// The code for the archive is not availbale
			status = DATABASE_NOP;
			showDBStatus("Database connection not available");
		} else {
			status = DATABASE_OK;
			showDBStatus("Database ready");
		}
	}
	
	/**
	 * Load the class to talk with the archive
	 * 
	 * @return An object to submit wueries to the database
	 *         null if something went wrong loading the class
	 */
	private Object loadArchiveClass() {
		try {
			  Thread t = Thread.currentThread();
			  ClassLoader loader = t.getContextClassLoader();
			  Class cl =loader.loadClass("alma.archive.logging.ArchiveLoggingQuery");
			  Class[] classes = { Class.forName("java.util.logging.Logger")};
			  Constructor constructor = cl.getConstructor(classes);
			  return constructor.newInstance((Logger)null);
		  } catch (Throwable t) {
			  System.out.println("Database connection not available "+t.getMessage());
			  t.printStackTrace();
			  return null;
		  }
	}
	
	/**
	 * Notify the LoggingClient about the status of the connection with the database
	 * 
	 * The icon in the LC is set depending on the actual state of the connection.
	 * The tooltip of the icon is set equal to the msg parameter
	 * 
	 * @param msg A message to show as tool tip in the icon of the status
	 *            of the DB connection
	 */
	private void showDBStatus(String msg) {
		logging.showDBStatus(databaseStatusIcons[status],msg);
	}

}

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
package alma.alarmsystem.source.test;

import java.io.File;
import java.io.FileWriter;

/**
 * A class containing a set of useful methods needed by the tests
 * 
 * NOTE:
 * In this test I am interested in checking if the alarms are sent to the log.
 * CurrentlyI have the following message in the stdout (I think I'll mask that
 * in the TestList.grep):
 * failed to flush logging queue because remote logging service has not been made available.
 * 
 * This message is here because I have not initialized correctly the log and the remote logging
 * is disabled.
 * To fix that I should define this class as a Client, log into the manager and initialize the log.
 * This would require a lot of time because I don't have the SimpleClient available at this point:
 * everything should be done by hand.
 * Ok... I don't care about the error because if the message is in the stdout it means that 
 * everything is working.
 * 
 * @author acaproni
 *
 */
public class TestUtil {
	
	private static String XMLHead[] = {
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n",
			"<alarm-system-configuration xmlns=\"urn:schemas-cosylab-com:acsalarm-alarmservice:1.0\" \n\t\t",
			"xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> \n",
			"\t<configuration-property name=\"Implementation\">"
	};
	
	private static String XMLTail = "</configuration-property>\n</alarm-system-configuration>\n";
	
	/**
	 * Remove the Alarm branch from the CDB
	 * 
	 * @param CDBFolder The directory of the CDB 
	 */
	public static void deleteAlarmBranch(String CDBFolder) {
		String CDBdir = CDBFolder+"/CDB/Alarms";
		File alarmsDir = new File(CDBdir);
		if (!alarmsDir.exists()) {
			return;
		}
		String configDirName = CDBdir+"/Administrative/AlarmSystemConfiguration";
		String configFileName = configDirName+"/AlarmSystemConfiguration.xml";
		File configDir = new File(configDirName);
		File configFile = new File(configFileName);
		if (configFile.exists()) {
			configFile.delete();
		}
		if (configDir.exists()) {
			configDir.delete();
		}
		alarmsDir.delete();
	}
	
	/**
	 * Rewrite the Alarm branch of the CDB.
	 * 
	 * @param CDBFolder The directory of the CDB 
	 * @param ASImplementation The value of the implementation property of the CDB 
	 */
	public static void setupAlarmBranch(String CDBFolder,String ASImplementation) {
		deleteAlarmBranch(CDBFolder);
		String CDBdir = CDBFolder+"/CDB/Alarms";
		String configDirName = CDBdir+"/Administrative/AlarmSystemConfiguration";
		String configFileName = configDirName+"/AlarmSystemConfiguration.xml";
		try {
			File alarmsDir = new File(CDBdir);
			alarmsDir.mkdir();
			File configDir = new File(configDirName);
			configDir.mkdir();
			FileWriter writer = new FileWriter(configFileName);
			for (int t=0; t<XMLHead.length; t++) {
				writer.write(XMLHead[t]);
			}
			writer.write(ASImplementation);
			writer.write(XMLTail);
			writer.flush();
			writer.close();
		} catch (Exception e) {
			System.out.println("Error setting up the Alarm branch of the CDB: ");
			e.printStackTrace();
		}
	}

}


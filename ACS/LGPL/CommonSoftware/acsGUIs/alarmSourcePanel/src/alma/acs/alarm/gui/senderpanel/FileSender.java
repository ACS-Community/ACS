/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2012
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.gui.senderpanel;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import javax.swing.JFileChooser;

import alma.acs.container.ContainerServices;


/**
 * Send the alarms read from a file by extending {@link BaseAlarmsSender}.
 * <P>
 * <EM>Format of the file of alarms.</EM><BR>
 * The file has one line for each alarm with the format triplet properties.
 * 
 * Where:
 * <UL>
 *   <LI> triplet is <CODE>FaultFamily,FaultMember,FaultCode</CODE>
 *   <LI> properties is a list of key=value: <CODE>key1=value1, key2=value2,...</CODE>
 * </UL>
 * The properties string is optional.
 * <BR>Comments starts with a '#' character.
 * 
 * @author acaproni
 */
public class FileSender extends BaseAlarmsSender {
	
	/**
	 * The start of a comment
	 */
	private static final String commentMarker ="#";
	
	/**
	 * The name of the file selected by the user
	 */
	private String fileName=null;
	
	/**
	 * The file selected by the user (can be <code>null</code>)
	 */
	private File selectedFile=null;
	
	/**
	 * Constructor
	 * 
	 * @param parent the parent component of the dialog
	 * @param contSvcs The ContainerServices
	 * @param sender The object to send alarms
	 */
	public FileSender(SenderPanel parent,ContainerServices contSvcs, ParallelAlarmSender sender) {
		super(parent, contSvcs,  sender,FileSender.class.getName()+"_");
	}

	/**
	 * Select the file of alarms offering a dialog to the user.
	 * <P>
	 * {@link #selectFile()} does not check if the content of the file is well
	 * formed but checks if it is readable.
	 * 
	 * @return The name of the file or
	 *  	   <code>null</code> if the file is not readable
	 */
	public synchronized String selectFile() {
		JFileChooser fileChooser = new JFileChooser();
		if (fileChooser.showOpenDialog(panel)==JFileChooser.APPROVE_OPTION) {
			selectedFile=fileChooser.getSelectedFile();
		} else if (fileChooser.showOpenDialog(panel)==JFileChooser.CANCEL_OPTION) {
			return fileName;
		} else {
			selectedFile=null;
		}
		if (selectedFile!=null && selectedFile.canRead() ) {
			fileName=selectedFile.getName();
		} else {
			fileName=null;
		}
		try {
			buildAlarmsFromFile();
		} catch (Throwable t) {
			t.printStackTrace();
		} finally {
			notifyAlarmsRead();
			 // Dump the alarms in the stdout. It is more for debugging but can be
			 // useful while testing to let the user knows what to expect
			 dumpAlarms();	
		}
		return fileName;
	}

	/**
	 * 
	 * @return The name of the file selected by the user (can be <code>null</code>)
	 */
	public synchronized String getFileName() {
		return fileName;
	}
	
	/**
	 * Read the alarms from the file selected by the user.
	 * <P>
	 * The alarms read from the file are stored in memory ready to be sent
	 * when the user presses one of the control buttons of the panel.
	 * 
	 * @throws Exception in case of error reading the file
	 */
	private void buildAlarmsFromFile() throws Exception {
		alarms.clear();
		if (selectedFile==null) {
			return;
		}
		FileReader fReader = new FileReader(selectedFile);
		BufferedReader reader = new BufferedReader(fReader);
		String strLine;
		 while ((strLine = reader.readLine()) != null)   {
			 // Print the content on the console
			 parseLine(strLine.trim());
		 }
	}
	
	/**
	 * Build an alarm from the passed line.
	 * 
	 * @param line The line read from the file
	 * @return The alarm defined in the line or <code>null</code>
	 *         if the line does not define an alarm
	 */
	private AlarmRead parseLine(String line) {
		if (line==null || line.isEmpty()) {
			return null;
		}
		// Remove comments
		String str=removeComment(line).trim();
		boolean isAlarm=SenderPanelUtils.isAnAlarm(str);
		if (isAlarm) {
			String[] parts=SenderPanelUtils.propertyRegExp.split(str);
			String theTriplet=parts[0];
			parts = SenderPanelUtils.alarmRegExp.split(str);
			String theProperties=str.substring(theTriplet.length()).trim();
			try {
				alarms.add(new AlarmRead(theTriplet, theProperties));
			} catch (Throwable t) {
				System.out.println("Alarm discarded. Triplet = "+theTriplet+", Properties="+theProperties);
				System.out.println("\tTriplet and properties have been generated by parsing ["+str+"]");
				t.printStackTrace();
			}
		} else {
			if (!str.isEmpty()) {
				System.out.println("["+str+"] discarded. Is it a malformed alarm?");
			}
		}
		return null;
	}
	
	/**
	 * Return a copy of the passed string without comments.
	 * <P>
	 * A comment starts with {@value #commentMarker} and terminates at the end of the line
	 * 
	 * @param str The string to check
	 * @return The string without comments
	 */
	private String removeComment(String str) {
		if (str==null) {
			throw new IllegalArgumentException("A null String is invalid");
		}
		int pos = str.indexOf(commentMarker);
		if (pos==-1) {
			// No comment in the string
			return str;
		}
		if (pos==0) {
			return "";
		}
		return str.substring(0,pos).trim();
	}
}

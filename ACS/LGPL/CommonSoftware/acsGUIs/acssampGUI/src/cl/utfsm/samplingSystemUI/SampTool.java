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
package cl.utfsm.samplingSystemUI;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JOptionPane;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.container.ContainerServices;
import cl.utfsm.samplingSystemUI.core.AcsInformation;
import cl.utfsm.samplingSystemUI.core.AcsInformationException;
import cl.utfsm.samplingSystemUI.core.SamplingManagerUITool;

public class SampTool extends SamplingManagerUITool {
	private static SamplingSystemGUI window;
	
	public static final String NAME="TheNewSampTool";

	private static final String SAMP_MAN_IFACE = "IDL:alma/acssamp/Samp:1.0";
	
	protected static ContainerServices cServices;
	private static String[] compList = null;
	private static LinkedList<String> sampManList = new LinkedList<String>();
	private static LinkedList<ComponentDescriptor> cDescriptorList = new LinkedList<ComponentDescriptor>();
	private static LinkedList<List<String>> propList = new LinkedList<List<String>>();
	private static String managerLoc = System.getProperty("ACS.manager");
	
	public static String[] getComponents(){		
		return compList;
	}

	public static String[] getSamplingManagers() {
		String tmp[] = null;
		tmp = new String[sampManList.size()];
		sampManList.toArray(tmp);
		Arrays.sort(tmp);
		return tmp;
	}

	public static void initializeComponents() throws AcsJContainerEx, AcsInformationException{

		info=AcsInformation.getInstance(NAME);
		
		/* Get all component names and descriptors */
		List<String> listTmp = new ArrayList<String>();
		compList  = info.getCManager().getComponentsName();
		cServices = info.getContainerServices();
		
		for(int i=0; i<compList.length; i++){
			try{
				ComponentDescriptor c = cServices.getComponentDescriptor(compList[i]);
				
				/* Check which of them are Sampling Managers */
				if(c.getType().equals(SAMP_MAN_IFACE)) {
					sampManList.add(c.getName());
					info.getContainerServices().getLogger().info("Found Sampling Manager " + c.getName());
				}
				/* If they are not, then add them to the component list */
				else {
					listTmp.add(compList[i]);
					cDescriptorList.add(c);
					propList.add(null);
				}
			} catch(AcsJContainerServicesEx e){
				e.getMessage();
			}
		}

		compList = new String[listTmp.size()];
		compList = listTmp.toArray(compList);
		Arrays.sort(compList);
	}

	public static void main(String[] args) throws ParserConfigurationException, SAXException, IOException{
		boolean check = false;
		window = new SamplingSystemGUI();
		try {
			initializeComponents();
			if(sampManList.size()==0){
				JOptionPane.showMessageDialog(null,
					    "There is no Sampling Manager defined in the current CDB.\n" +
					    "(The component type should be 'IDL:alma/acssamp/Samp:1.0')\n" +
					    "Please check your CDB and restart Sampling System GUI",
					    "Critical Error",
					    JOptionPane.ERROR_MESSAGE);
				System.exit(1);
			}
			//spinUp(NAME,sampManList.getFirst());
			
			// -f option
			if (args.length != 0){
				window.specialReadStatusFile(args[1]);
				check = true;
			}
			
				
		} catch (AcsInformationException e) {
			e.printStackTrace();
		} catch (AcsJContainerEx e) {
			JOptionPane.showMessageDialog(null,
				    "Coudln't connect to Manager '" + managerLoc + "'\n" +
				    "Please check that the Manager is running, and that your Manager\n" +
				    "reference is well formed",
				    "Critical Error",
				    JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}
		window.loadWindow(check);
		window.fillWidgets(getComponents(), propList);


		try {
			tearDown();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static LinkedList<List<String>> getProps(){
		return propList;
	}

	public static List<String> getPropsForComponent(String comp) {
		return info.getCManager().getComponentProperties(comp);
	}
}

package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.container.ContainerServices;
import cl.utfsm.samplingSystemUI.core.AcsInformationException;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;
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

	public static void initializeComponents(){
		
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

	public static void main(String[] args){
		window = new SamplingSystemGUI();
		try {
			spinUp(NAME,window.MAN_NAME);
			initializeComponents();
		} catch (AcsInformationException e) {
			e.printStackTrace();
		} catch (SamplingManagerException e) {
			e.printStackTrace();
		}
		window.loadWindow();
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
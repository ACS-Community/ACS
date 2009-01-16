package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JOptionPane;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.container.ContainerServices;
import cl.utfsm.samplingSystemUI.core.AcsInformation;
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
		try {
			info=AcsInformation.getInstance(NAME);
		} catch (AcsInformationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
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
				
			spinUp(NAME,sampManList.getFirst());
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
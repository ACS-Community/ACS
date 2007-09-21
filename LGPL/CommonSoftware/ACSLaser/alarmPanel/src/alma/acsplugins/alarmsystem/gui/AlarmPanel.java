/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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

/** 
 * @author  acaproni   
 * @version $Id: AlarmPanel.java,v 1.1.1.1 2007/09/21 09:10:11 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import alma.alarmsystem.AlarmServiceHelper;
import alma.alarmsystem.Category;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.AlarmService;

import java.awt.BorderLayout;

import alma.acs.gui.util.panel.IPanel;

/**
 * 
 * The panel showing alarms
 *
 */
public class AlarmPanel extends JScrollPane implements IPanel {
	
	// The container services
    private ContainerServices contSvc=null;
	
	// The table of logs and its model
	private AlarmTableModel model = new AlarmTableModel();
	private AlarmTable alarmTable = new AlarmTable(model);
	
	// The name of the AlarmSrvice component
	private String alarmName;
	
	// The alarm service component and its IDL
	private final String alarmServiceIDL = "*/AlarmService:*";
	private AlarmService alarm;
	
	// The category root topic
	private String categoryRootTopic;
	
	// The window that shows this panel
    private JFrame frame=null;
    
	/**
	 * The categories 
	 * Each category is a notifcation channel we have to listen to.
	 * The list of the categories is read from the AlarmServise componet
	 */
	private Category[] categories;
	
	private CategorySubscriber[] consumers;
	
	/**
	 * Constructor 
	 *
	 */
	public AlarmPanel() {
		super();
		initialize();
	}
	
	/**
	 * Constructor 
	 * 
	 * @param frame The window that owns this panel
	 */
	public AlarmPanel(JFrame frame) {
		if (frame==null) {
			throw new IllegalArgumentException("Invalid null frame in constructor");
		}
		this.frame=frame;
		initialize();
	}
	
	/**
	 * Init the GUI
	 *
	 */
	private void initialize() {
		setViewportView(alarmTable);
		setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
		alarmTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
	}
	
	/**
	 * @see IpauseResume
	 */
	public void pause() throws Exception {
		
	}
	
	/**
	 * @see IPauseResume
	 */
	public void resume() throws Exception {
		
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void start() throws Exception {
		// Check if the CS have been set
		if (contSvc==null) {
			throw new Exception("PluginContainerServices not set");
		}
		// Get the AlarmService
		try {
			getAlarmServiceComponent();
		} catch (Exception e) {
			releaseAlarmServiceComponent();
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Error getting the AlarmSystem component: "+e.getMessage());
			throw new Exception("Error getting the AlarmSystem component: "+e.getMessage(),e);
		}
		if (alarmName==null || alarm==null) {
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Unknown error getting the AlarmService");
			throw new Exception("Unknown error getting the AlarmService");
		}
		// Read the available categories
		try {
			categories=getCategories();
		} catch (Exception e) {
			releaseAlarmServiceComponent();
			contSvc.getLogger().log(AcsLogLevel.ERROR,"Error getting the categories from AlarmService");
			throw new Exception("Error getting the categories from AlarmService",e);
		}
		dumpCategories();
		if (categories==null || categories.length==0) {
			contSvc.getLogger().log(AcsLogLevel.INFO,"No categories to subscribe to");
		}
		createConsumers();
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void stop() throws Exception {
		releaseAlarmServiceComponent();
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void setServices (ContainerServices ctrl) {
		contSvc=ctrl;
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public boolean runRestricted (boolean restricted) throws Exception {
		return restricted;
	}
	
	/**
	 * Connect the AlarmSrevice component
	 */
	private void getAlarmServiceComponent() throws Exception {
		String[] names  = contSvc.findComponents("*",alarmServiceIDL);
		if (names==null || names.length==0) {
			// Nothing has been found
			throw new Exception("No available alarm component");
		}
		if (names.length>1) {
			contSvc.getLogger().log(AcsLogLevel.WARNING,"More then one AlarmService component found");
			
		}
		contSvc.getLogger().log(AcsLogLevel.INFO,"Getting "+names[0]);
		// Get a reference to the first component
		alarm=AlarmServiceHelper.narrow(contSvc.getComponent(names[0]));
		alarmName=names[0];
		contSvc.getLogger().log(AcsLogLevel.INFO,names[0]+" connected");
	}
	
	/**
	 * Release the alarm component
	 *
	 */
	private void releaseAlarmServiceComponent() {
		try {
			if (alarmName!=null) {
				contSvc.getLogger().log(AcsLogLevel.INFO,"Releasing "+alarmName);
				contSvc.releaseComponent(alarmName);
				contSvc.getLogger().log(AcsLogLevel.INFO,alarmName+" released");
			}
		} catch (Throwable t) {
			System.err.println("Error releasing the AlarmService: "+t.getMessage());
		}
		alarmName=null;
		alarm=null;
		categories=null;
	}
	
	/**
	 * Read the categories and the category root topic from the component
	 * 
	 * @return The categories
	 */
	private Category[] getCategories() throws Exception {
		if (alarm==null) {
			throw new IllegalStateException("No alarm component connected");
		}
		categoryRootTopic=alarm.getCategoryRootTopic();
		return alarm.getCategories();
	}
	
	/**
	 * Create the consumers for the categories
	 *
	 */
	private void createConsumers() {
		if (categories==null ||categories.length==0) {
			contSvc.getLogger().log(AcsLogLevel.INFO,"No categories channels to connect to");
		}
		consumers=new CategorySubscriber[categories.length];
		for (int t=0; t<categories.length; t++) {
			try {
				consumers[t]=new CategorySubscriber(contSvc,categoryRootTopic,categories[t].path,model);
			} catch (Exception jmse) {
				contSvc.getLogger().log(AcsLogLevel.ERROR,"Error subscribing to "+categoryRootTopic+"."+categories[t].path);
			}
		}
	}
	
	/**
	 * Dumps the category
	 *
	 */
	private void dumpCategories() {
		if (categories==null) {
			System.out.println("Categories null");
		}
		if (categories.length==0) {
			System.out.println("Categories empty");
		}
		System.out.println("Category root topic="+categoryRootTopic);
		for (Category cat: categories) {
			System.out.println("Category name="+cat.name);
			System.out.println("\tpath="+cat.path);
			System.out.println("\tdescription="+cat.description);
		}
	}
	
	/**
	 * Set the ContainerServices
	 * 
	 * @see alma.acs.gui.util.panel.IPanel
	 */
	public void setACSContainerServices(ContainerServices cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		contSvc=cs;	
	}
	
	/**
	 * Return true if the panel is running inside OMC
	 * 
	 * @see alma.acs.gui.util.panel.IPanel
	 */
	public boolean isOMCPlugin() {
		return frame==null;
	}
}

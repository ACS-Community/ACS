package cl.utfsm.samplingSystemUI;

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;

/**
 * Main Widget class, and starting point for the SSG Software. Controls the main flow of the software.
 * <p>
 * SamplingSystemGUI ask for a Sampling Manager, then tries to load a status file, and then presents
 * a window with the Components and Properties available for sampling.<br>
 * If there was a status file, SamplingSystemGUI will load it, and open the Sampling Groups as recorded.
 * 
 * @author Jorge Avarias <javarias@inf.utfsm.cl>
 * @author Rodrigo Tobar <rtobar@inf.utfsm.cl>
 * @author Alejandro Baltra <abaltra@alumnos.inf.utfsm.cl>
 * @author Arturo Hoffstadt <ahoffsta@inf.utfsm.cl>
 */
public class SamplingSystemGUI extends JFrame {

	private static final long serialVersionUID = 1L;
	
	public String MAN_NAME = "SAMP1";
	private final static String DEFAULT_STATUS_FILENAME = "default.ssgst";
	
	private LinkedList<List<String>> propList;
	private String[] compList = null;
	private ArrayList<SerializableProperty> status;
	private ArrayList<BeanGrouper> BeanGrouperList;
	
	// Menu Widgets
	private JMenuBar SSGMenuBar = null;
	private JMenu FileMenu = null;
	private JMenuItem FileMenuSelectSMButton = null;
	private JMenuItem FileMenuLoadStatusButton = null;
	private JMenuItem FileMenuSaveStatusButton = null;	
	private JMenuItem FileMenuExitButton = null;
	private JMenu HelpMenu = null;
	private JMenuItem HelpMenuAboutButton = null;
	
	// Component and Property Selection and Adding
	private JPanel PropertyAddPanel = null;
	private JLabel componentLabel = null;
	private JComboBox ComponentComboBox = null;
	private JLabel propertyLabel = null;
	private JComboBox PropertyComboBox = null;
	private JLabel groupLabel = null;
	private JTextField groupTextField = null;
	private JButton addSampleButton = null;
	
	/**
	 * This is the default constructor. It start the initialization of the window.
	 */
	public SamplingSystemGUI() {
		super();
		BeanGrouperList = new ArrayList<BeanGrouper>();
		initialize();
	}
	
	/**
	 * This method initializes this GUI 
	 * @return void
	 */
	private void initialize() {

		this.setLocation(0, 0);
		this.setJMenuBar(getSSGMenuBar());
		this.setLayout( new GridLayout(1,1,10,10) );
		this.setContentPane( this.getPropertyAddPanel() );
		this.setTitle("Sampling System GUI");
		this.setLocationRelativeTo(null);
		this.pack();

	}
	
	/**
	 * This method initializes SSGMenuBar
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getSSGMenuBar() {
		if (SSGMenuBar == null) {
			SSGMenuBar = new JMenuBar();
			SSGMenuBar.add(getFileMenu());
			SSGMenuBar.add(getHelpMenu());
		}
		return SSGMenuBar;
	}

	/**
	 * This method initializes FileMenu	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getFileMenu() {
		if (FileMenu == null) {
			FileMenu = new JMenu();
			FileMenu.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			FileMenu.setName("FileMenu");
			FileMenu.setText("File");
			FileMenu.add(getFileMenuSelectSMButton());
			FileMenu.add(getFileMenuLoadStatusButton());
			FileMenu.add(getFileMenuSaveStatusButton());
			FileMenu.add(getFileMenuExitButton());
		}
		return FileMenu;
	}

	/**
	 * This method initializes FileMenuSelectSMButton	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getFileMenuSelectSMButton() {
		if (FileMenuSelectSMButton == null) {
			FileMenuSelectSMButton = new JMenuItem();
			FileMenuSelectSMButton.setText("Choose Sampling Manager");
			FileMenuSelectSMButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					String s = (String)JOptionPane.showInputDialog(SamplingSystemGUI.this,
							"Please Select a Sampling Manager",
							"Sampling Manager Selection",
							JOptionPane.PLAIN_MESSAGE,
							null,
							(Object[])SampTool.getSamplingManagers(),
							MAN_NAME);
					if( s != null && !s.trim().equals("") )
						SamplingSystemGUI.this.MAN_NAME = s;
				}
			});
		}
		return FileMenuSelectSMButton;
	}

	/**
	 * This method initializes FileMenuLoadStatusButton	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getFileMenuLoadStatusButton() {
		if (FileMenuLoadStatusButton == null) {
			FileMenuLoadStatusButton = new JMenuItem();
			FileMenuLoadStatusButton.setText("Load GUI Status");
			FileMenuLoadStatusButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
				    JFileChooser chooser = new JFileChooser();
				    Filter filter = new Filter();
				    chooser.setFileFilter(filter);
				    int returnVal = chooser.showOpenDialog(cl.utfsm.samplingSystemUI.SamplingSystemGUI.this);
				    if(returnVal == JFileChooser.APPROVE_OPTION) {
				    	readStatusFile(chooser.getSelectedFile().getAbsolutePath(),false);
				    }
				}
			});
		}
		return FileMenuLoadStatusButton;
	}

	/**
	 * This method initializes FileMenuSaveStatusButton	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getFileMenuSaveStatusButton() {
		if (FileMenuSaveStatusButton == null) {
			FileMenuSaveStatusButton = new JMenuItem();
			FileMenuSaveStatusButton.setText("Save GUI status");
			FileMenuSaveStatusButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
				    JFileChooser chooser = new JFileChooser();
				    int returnVal = chooser.showSaveDialog(cl.utfsm.samplingSystemUI.SamplingSystemGUI.this);
				    if(returnVal == JFileChooser.APPROVE_OPTION) {
				    	writeStatusFile(chooser.getSelectedFile().getAbsolutePath() + ".ssgst");
				    }
				}
			});
		}
		return FileMenuSaveStatusButton;
	}
	
	/**
	 * This method initializes FileMenuExitButton	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getFileMenuExitButton() {
		if (FileMenuExitButton == null) {
			FileMenuExitButton = new JMenuItem();
			FileMenuExitButton.setText("Exit");
			FileMenuExitButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					System.exit(0);
				}
			});
		}
		return FileMenuExitButton;
	}
	
	/**
	 * This method initializes HelpMenu	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getHelpMenu() {
		if (HelpMenu == null) {
			HelpMenu = new JMenu();
			FileMenu.setName("HelpMenu");
			HelpMenu.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			HelpMenu.setText("Help");
			HelpMenu.add(getHelpMenuAboutButton());
		}
		return HelpMenu;
	}
	
	/**
	 * This method initializes FileMenuSelectSMButton	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getHelpMenuAboutButton() {
		if (HelpMenuAboutButton == null) {
			HelpMenuAboutButton = new JMenuItem();
			HelpMenuAboutButton.setText("About...");
			HelpMenuAboutButton.addActionListener(new java.awt.event.ActionListener() {
				
				private JFrame aboutWindow;
				
				private JFrame getAboutWindow() {
					
					if( aboutWindow == null ) {
						String url = "http://alma.inf.utfsm.cl/twiki4/bin/view/ACS/SamplingSystem";
						String message = "<html>Sampling System GUI v1.1<br>" +
						   "This software is released under <b>LGPL</b> license.<br>" +
					       "SSG was developed by the ALMA-UTFSM Team.</html>";
						String messageUrl = "<html>Please refer to <u><font color=#0000ff>" + url + "</font></u><br>" +
					       "for more information.</html>";
						
						aboutWindow = new JFrame("About");
						JLabel aboutLabel = new JLabel(message);
						JLabel urlLabel   = new JLabel(messageUrl);
						JLabel imageLabel = new JLabel(new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/alma-utfsm.png")));
						JButton closeButton = new JButton("Close");
						
						closeButton.addActionListener(new java.awt.event.ActionListener(){

							public void actionPerformed(ActionEvent e) {
								aboutWindow.setVisible(false);
								aboutWindow.dispose();
							}
							
						});
						
						aboutWindow.getContentPane().setLayout(new GridBagLayout());
						GridBagConstraints constraints = new GridBagConstraints();
						constraints.insets = new Insets(0,10,0,10);
						constraints.anchor = GridBagConstraints.WEST;
						aboutWindow.getContentPane().add(aboutLabel,constraints);
						constraints.anchor = GridBagConstraints.EAST;
						aboutWindow.getContentPane().add(imageLabel,constraints);
						constraints.gridwidth=2;
						constraints.gridy = 2;
						constraints.insets.top = 10;
						constraints.insets.bottom = 10;
						aboutWindow.getContentPane().add(urlLabel,constraints);
						constraints.gridy = 3;
						constraints.anchor = GridBagConstraints.EAST;
						constraints.insets.top = 0;
						aboutWindow.getContentPane().add(closeButton,constraints);
						
					}
					
					return aboutWindow;
				};
				
				public void actionPerformed(java.awt.event.ActionEvent e) {
					aboutWindow = getAboutWindow();
					aboutWindow.setMinimumSize( new Dimension(550,250) );
					aboutWindow.setSize( getContentPane().getSize() );
					aboutWindow.setLocationRelativeTo(null);
					aboutWindow.setResizable(false);
					aboutWindow.setVisible(true);
				}
			});
		}
		return HelpMenuAboutButton;
	}
	
	/**
	 * This method initializes PropertyAddPanel	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getPropertyAddPanel() {
		if (PropertyAddPanel == null) {
			PropertyAddPanel = new JPanel();
			PropertyAddPanel.setLayout(new GridBagLayout());
			//PropertyAddPanel.setLayout( new GridLayout( 4,2, 10, 10 ) );
			PropertyAddPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			//PropertyAddPanel.setSize( new Dimension(310, 180) );
			GridBagConstraints c = new GridBagConstraints();
			c.gridheight = 1;
			c.gridwidth = 1;
			c.insets = new Insets(5,5,5,5);
			
			c.anchor = GridBagConstraints.EAST;
			c.gridx = 0;
			c.gridy = 0;
			PropertyAddPanel.add( getComponentLabel(), c);
			c.gridx = 1;
			c.anchor = GridBagConstraints.WEST;
			PropertyAddPanel.add( getComponentComboBox(), c);
			c.anchor = GridBagConstraints.EAST;
			c.gridy = 1;
			c.gridx = 0;
			PropertyAddPanel.add( getPropertyLabel(), c);
			c.gridx = 1;
			c.anchor = GridBagConstraints.WEST;
			PropertyAddPanel.add( getPropertyComboBox(), c);
			c.anchor = GridBagConstraints.EAST;
			c.gridy = 2;
			c.gridx = 0;
			PropertyAddPanel.add( getGroupLabel(), c );
			c.gridx = 1;
			c.anchor = GridBagConstraints.WEST;
			PropertyAddPanel.add( getGroupTextField(), c);
			c.anchor = GridBagConstraints.EAST;
			c.gridy = 3;
			c.gridx = 0;
			c.gridwidth = 2;
			PropertyAddPanel.add( getAddSampleButton(), c);
			PropertyAddPanel.validate();
		}
		return PropertyAddPanel;
	}

	/**
	 * This method initializes ComponentComboBox	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getComponentComboBox() {
		if (ComponentComboBox == null) {
			ComponentComboBox = new JComboBox();
			ComponentComboBox.setPreferredSize(new Dimension(270, 24));
			ComponentComboBox.setSize(ComponentComboBox.getPreferredSize());
			ComponentComboBox.addItemListener(new java.awt.event.ItemListener() {
				public void itemStateChanged(java.awt.event.ItemEvent e) {
					PropertyComboBox.setEnabled(true);
					String comp = e.getItem().toString();
					PropertyComboBox.removeAllItems();
					for(int i=0; i<compList.length;i++){
						/* We find the component. We show the
						 * properties for it. If we do not have them,
						 * we go and find them */
						if(compList[i].compareTo(comp)==0){
							
							if( propList.get(i) == null )
								propList.add(i,SampTool.getPropsForComponent(compList[i]));

							try{
								fillPropertyComboBox(propList.get(i));
							}catch(IndexOutOfBoundsException ex){
								PropertyComboBox.removeAllItems();
								PropertyComboBox.setEnabled(false);
							}
					//System.out.println(comp);
						}
					}
					if(PropertyComboBox.getItemCount()==0)addSampleButton.setEnabled(false);
					else addSampleButton.setEnabled(true);
				}
			});
		}
		return ComponentComboBox;
	}

	/**
	 * This method initializes PropertyComboBox	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getPropertyComboBox() {
		if (PropertyComboBox == null) {
			PropertyComboBox = new JComboBox();
			PropertyComboBox.setPreferredSize(new Dimension(270, 24));
			PropertyComboBox.setSize(PropertyComboBox.getPreferredSize());
		}
		return PropertyComboBox;
	}

	/**
	 * This method initializes addSampleButton	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddSampleButton() {
		if (addSampleButton == null) {
			addSampleButton = new JButton();
			addSampleButton.setPreferredSize(new Dimension(120, 24));
			addSampleButton.setText("Add Sample");
			addSampleButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(groupTextField.getText().trim().equalsIgnoreCase("")) return;
					String component = ComponentComboBox.getSelectedItem().toString();
					String property = PropertyComboBox.getSelectedItem().toString();
					String group = groupTextField.getText();
					
					SerializableProperty p = new SerializableProperty();
					p.setComponent(component);
					p.setProperty(property);
					p.setSamplingGroup(group);
					if(status==null)
						status = new ArrayList<SerializableProperty>();
					
					/* Only add it if it was really added to the panel */
					if( addToSampling(component, property, group) )
						status.add(p);
				}
			});
		}
		return addSampleButton;
	}
	
	/**
	 * This method initializes groupLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getGroupLabel() {
		if (groupLabel == null) {
			groupLabel = new JLabel("Sampling group:");
		}
		return groupLabel;
	}

	/**
	 * This method initializes groupLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getComponentLabel() {
		if (componentLabel == null) {
			componentLabel = new JLabel("Component:");
		}
		return componentLabel;
	}
	
	/**
	 * This method initializes groupLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getPropertyLabel() {
		if (propertyLabel == null) {
			propertyLabel = new JLabel("Property:");
		}
		return propertyLabel;
	}
	
	/**
	 * This method initializes groupTextField
	 * @return javax.swing.JTextField	
	 */
	private JTextField getGroupTextField() {
		if (groupTextField == null) {
			Dimension d = new Dimension(100, 19);
			groupTextField = new JTextField();
			groupTextField.setPreferredSize(d);
			groupTextField.setSize(d);
			groupTextField.setMinimumSize(d);
			groupTextField.setToolTipText("Sampling Group where to add the new Sample. Only alphanumeric and underscore characters.");
			groupTextField.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			groupTextField.setHorizontalAlignment(JTextField.LEFT);
			groupTextField.setText("SampGroup");
			groupTextField.addFocusListener(new FocusListener() {
				public void focusGained(FocusEvent e) {
					// Do nothing special :)
				}
				public void focusLost(FocusEvent e) {
					if( !groupTextField.getText().matches("^([a-z]|[A-Z]|[0-9]|_)+$") ) {
						JOptionPane.showConfirmDialog(SamplingSystemGUI.this,
								"Group name '" + groupTextField.getText() + "' is invalid.\nPlease use only alphanumeric characters and/or underscores.",
								"Invalid group name",
								JOptionPane.PLAIN_MESSAGE,JOptionPane.WARNING_MESSAGE);
						groupTextField.setText("");
						groupTextField.grabFocus();
					}
				}
			}
			);
			}
		return groupTextField;
	}
	
	/**
	 * Adds a Property to a SamplingGroup, creating its BeanGrouper
	 * @param component Component that contains the Property to be sampled.
	 * @param property Property to be sampled.
	 * @param group SamplingGroup at which the property is to be added.
	 * @return Whether or not the property was added to the sampling group.
	 */
	private boolean addToSampling(String component, String property, String group){
		boolean added;
		BeanGrouper bg = groupExists(group);

		/* If there is no group with this name, we create it */
		if (bg == null){
			bg = new BeanGrouper(this,group);
			bg.setGroupName(group);
			bg.addSamp(component, property);
			BeanGrouperList.add(bg);
			added = true;
			bg.setVisible(true);
		}
		/* Else, we check if component/property is already added */
		else {
			if( bg.checkIfExists(component, property) ) {
				JOptionPane.showMessageDialog(this,  
						"Component " + component + " with property " + property +
						"\nhas been already added to the sample list for group " + bg.getGroupName(), 
						"Already added",
						JOptionPane.WARNING_MESSAGE );
				added = false;
			}
			else {
				bg.addSamp(component, property);
				added = true;
			}
		}
		return added;
	}
	
	/**
	 * Checks if a BeanGrouper with the groupName exists
	 * @param groupName Name of the SamplingGroup to be searched.
	 * @return Whether or not the SamplingGroup exits.
	 */
	private BeanGrouper groupExists(String groupName){
		if( BeanGrouperList.isEmpty() )
			return null;
		else{
			for( BeanGrouper bg : BeanGrouperList ){
				if( bg.getGroupName().toString().equalsIgnoreCase(groupName)){
					return bg; 
				}
			}
		}
		return null;
	}

	public void loadWindow(){
		/* Select the sampling manager */
		System.out.println(SampTool.getSamplingManagers().length);
		String s = (String)JOptionPane.showInputDialog(this,
				"Please Select a Sampling Manager",
				"Sampling Manager Selection",
				JOptionPane.PLAIN_MESSAGE,
				null,
				(Object[])SampTool.getSamplingManagers(),
				MAN_NAME);
		if( s != null && !s.trim().equals("") )
			this.MAN_NAME = s;
		else
			System.exit(0);
		
		/* Show the main window */
		this.setVisible(true);
		this.validate();
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				if(status!=null){
					writeStatusFile();
				}
			}
		});
		readStatusFile(true);
	}
	
	public void fillWidgets(String[] components, LinkedList<List<String>> properties){
		this.propList = properties;
		this.compList = components;
		fillComponentComboBox(components);
	}
	
	private void fillComponentComboBox(String[] components){
		ComponentComboBox.removeAllItems();
		for(int i=0;i<components.length;i++)
			ComponentComboBox.addItem(components[i]);
	}
	
	private void fillPropertyComboBox(List<String> prop){
		PropertyComboBox.removeAllItems();
		PropertyComboBox.setEnabled(true);
		addSampleButton.setEnabled(true);
		for(String s:prop)
			PropertyComboBox.addItem(s);
	}
	
	@SuppressWarnings("unchecked")
	private void readStatusFile(String filename,boolean startup){
		FileInputStream fis = null;
		ObjectInputStream in = null;
		try {
			fis = new FileInputStream(filename);
			in = new ObjectInputStream(fis);
			if(startup){
				int n = JOptionPane.showConfirmDialog(this,
						"An old status file has been found\nWould you like to load it?", 
						"Old status file found", 
						JOptionPane.YES_NO_OPTION, 
						JOptionPane.INFORMATION_MESSAGE
						);
				if(n!=0){
					in.close();
					return;
				}
			}
			status = (ArrayList<SerializableProperty>)in.readObject();
			in.close();
			for(SerializableProperty p: status){
				addToSampling(p.getComponent(), p.getProperty(), p.getSamplingGroup());
			}
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
		} catch (ClassNotFoundException e) {
		}
	}
	
	private void readStatusFile(boolean startup){
		readStatusFile(DEFAULT_STATUS_FILENAME,startup);
	}
	
	private void writeStatusFile(String filename){
		FileOutputStream fos = null;
		ObjectOutputStream out = null;
		if( status.size() == 0 )
			return;
		try {
			fos = new FileOutputStream(filename);
			out = new ObjectOutputStream(fos);
			out.writeObject(status);
			out.close();
		} catch (FileNotFoundException e) {
		} catch (IOException e) {
		}
	}
	
	private void writeStatusFile(){
		writeStatusFile(DEFAULT_STATUS_FILENAME);
	}

	/**
	 * Removes from the local list of Component/Properties that are being
	 * sampled a given set of sampler <code>samplers</code> for a given
	 * sampling group <code>group</code>
	 * @param samplers The given sampler list
	 * @param group The belonging sampling group for the given samplers
	 */
	public synchronized void deleteBeanGrouper(ArrayList<DataPrinter> samplers, String group) {
		for(DataPrinter dp : samplers )
			for(SerializableProperty sp : status ) {
				if( sp.getComponent().equals(dp.component) &&
					sp.getProperty().equals(dp.property)   &&
					sp.getSamplingGroup().equals(group) ) {
					status.remove(sp);
					break;
				}
		}
		for( BeanGrouper bg : BeanGrouperList ){
			if( bg.getGroupName().toString().equalsIgnoreCase(group)){
				bg.setVisible(false);
				BeanGrouperList.remove(bg);
				bg.dispose();
				break;
			}
		}
	}
	
	public static void main(String[] args) {
		SwingUtilities.invokeLater( new Runnable(){
			public void run(){
				SamplingSystemGUI thisClass = new SamplingSystemGUI();
				thisClass.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				thisClass.setVisible(true);
			}
		});
	}
}

class Filter extends FileFilter {

	@Override
	public boolean accept(File f) {
		if(f.isDirectory()) return true;
		String name = f.getName();
		if(name==null) return false;
		int index = name.lastIndexOf(".");
		if (index <0) return false;
		String ext = name.substring(index);
		if(ext.equalsIgnoreCase(".ssgst")) return true;
		return false;
	}

	@Override
	public String getDescription() {
		return "Sample System GUI status File (*.ssgst)";
	}
	
}


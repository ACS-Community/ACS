package cl.utfsm.samplingSystemUI;

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
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
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.filechooser.FileFilter;

import net.miginfocom.swing.MigLayout;

public class SamplingSystemGUI extends JFrame {

	private static final long serialVersionUID = 1L;
	
	public String MAN_NAME = "SAMP1";
	private final static String DEFAULT_STATUS_FILENAME = "default.ssgst";
	
	private JPanel jContentPane = null;

	private JMenuBar SSGMenuBar = null;
	private JMenu FileMenu = null;
	private JMenu AboutMenu = null;
	private JMenuItem FileMenuExitButton = null;

	private JSplitPane ComponentListerContainer = null;

	private JPanel jPanel = null;

	private JComboBox ComponentBox1 = null;

	private JComboBox PropertyBox1 = null;

	private LinkedList<List<String>> propList;
	private String[] compList = null;

	private JButton addSampleButton = null;
	
	private JLabel groupLabel = null;

	private JScrollPane jScrollPane = null;

	private JPanel jPanel21 = null;

	private JTextField jTextField = null;
	
	private ArrayList<SerializableProperty> status;

	private JMenuItem jMenuItem = null;

	private JMenuItem jMenuItem1 = null;

	private JMenuItem jMenuItem2 = null;
	
	/**
	 * This method initializes SSGMenuBar	
	 * 	
	 * @return javax.swing.JMenuBar	
	 */
	private JMenuBar getSSGMenuBar() {
		if (SSGMenuBar == null) {
			SSGMenuBar = new JMenuBar();
			SSGMenuBar.add(getFileMenu());
			SSGMenuBar.add(getAboutMenu());
		}
		return SSGMenuBar;
	}

	/**
	 * This method initializes FileMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getFileMenu() {
		if (FileMenu == null) {
			FileMenu = new JMenu();
			FileMenu.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			FileMenu.setName("UpperMenu");
			FileMenu.setText("File");
			FileMenu.add(getJMenuItem1());
			FileMenu.add(getJMenuItem());
			FileMenu.add(getJMenuItem2());
			FileMenu.add(getFileMenuExitButton());
		}
		return FileMenu;
	}

	/**
	 * This method initializes AboutMenu	
	 * 	
	 * @return javax.swing.JMenu	
	 */
	private JMenu getAboutMenu() {
		if (AboutMenu == null) {
			AboutMenu = new JMenu();
			AboutMenu.setText("About");
		}
		return AboutMenu;
	}

	/**
	 * This method initializes FileMenuExitButton	
	 * 	
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
	 * This method initializes ComponentListerContainer	
	 * 	
	 * @return javax.swing.JSplitPane	
	 */
	private JSplitPane getComponentListerContainer() {
		if (ComponentListerContainer == null) {
			ComponentListerContainer = new JSplitPane();
			ComponentListerContainer.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
			ComponentListerContainer.setRightComponent(getJScrollPane());
			ComponentListerContainer.setLeftComponent(getJPanel());
			ComponentListerContainer.setDividerLocation(310);
		}
		return ComponentListerContainer;
	}

	/**
	 * This method initializes jPanel	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	private JPanel getJPanel() {
		if (jPanel == null) {
			jPanel = new JPanel();
			jPanel.setLayout(new MigLayout("","","[]10[]10[]10[]"));
			jPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			jPanel.setPreferredSize(new Dimension(180, 34));
			jPanel.add(getComponentBox1(),"wrap, span 2");
			jPanel.add(getPropertyBox1(),"wrap, span 2");
			jPanel.add(getgroupLabel());
			jPanel.add(getJTextField(), "wrap");
			jPanel.add(getAddSampleButton(), "wrap, span 2");
		}
		return jPanel;
	}

	/**
	 * This method initializes groupLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getgroupLabel() {
		if (groupLabel == null) {
			groupLabel = new JLabel("Sampling group:");
		}
		return groupLabel;
	}

	/**
	 * This method initializes ComponentBox1	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getComponentBox1() {
		if (ComponentBox1 == null) {
			ComponentBox1 = new JComboBox();
			ComponentBox1.setPreferredSize(new Dimension(270, 24));
			ComponentBox1.setSize(ComponentBox1.getPreferredSize());
			ComponentBox1.addItemListener(new java.awt.event.ItemListener() {
				public void itemStateChanged(java.awt.event.ItemEvent e) {
					PropertyBox1.setEnabled(true);
					String comp = e.getItem().toString();
					PropertyBox1.removeAllItems();
					for(int i=0; i<compList.length;i++){
						/* We find the component. We show the
						 * properties for it. If we do not have them,
						 * we go and find them */
						if(compList[i].compareTo(comp)==0){
							
							if( propList.get(i) == null )
								propList.add(i,SampTool.getPropsForComponent(compList[i]));

							try{
								fillPropertyBox1(propList.get(i));
							}catch(IndexOutOfBoundsException ex){
								PropertyBox1.removeAllItems();
								PropertyBox1.setEnabled(false);
							}
					//System.out.println(comp);
						}
					}
					if(PropertyBox1.getItemCount()==0)addSampleButton.setEnabled(false);
					else addSampleButton.setEnabled(true);
				}
			});
		}
		return ComponentBox1;
	}

	/**
	 * This method initializes PropertyBox1	
	 * 	
	 * @return javax.swing.JComboBox	
	 */
	private JComboBox getPropertyBox1() {
		if (PropertyBox1 == null) {
			PropertyBox1 = new JComboBox();
			PropertyBox1.setPreferredSize(new Dimension(270, 24));
			PropertyBox1.setSize(PropertyBox1.getPreferredSize());
		}
		return PropertyBox1;
	}

	/**
	 * This method initializes addSampleButton	
	 * 	
	 * @return javax.swing.JButton	
	 */
	private JButton getAddSampleButton() {
		if (addSampleButton == null) {
			addSampleButton = new JButton();
			addSampleButton.setPreferredSize(new Dimension(120, 24));
			addSampleButton.setText("Add Sample");
			addSampleButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(jTextField.getText().trim().equalsIgnoreCase("")) return;
					String component = ComponentBox1.getSelectedItem().toString();
					String property = PropertyBox1.getSelectedItem().toString();
					String group = jTextField.getText();
					
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

	
	private boolean addToSampling(String component, String property, String group){
		
		boolean added;
		BeanGrouper bg = groupExists(group);

		/* If there is no group with this name, we create it */
		if (bg == null){
			bg = new BeanGrouper(this,group);
			bg.setCheckName(group);
			bg.addSamp(component, property);
			jPanel21.add(bg,"growx, dock north");
			added = true;
		}
		/* Else, we check if component/property is already added */
		else {
			if( bg.checkIfExists(component, property) ) {
				JOptionPane.showMessageDialog(this,  
						"Component " + component + " with property " + property +
						"\nhas been already added to the sample list for group " + bg.getCheckName(), 
						"Already added",
						JOptionPane.WARNING_MESSAGE );
				added = false;
			}
			else {
				bg.addSamp(component, property);
				added = true;
			}
		}
		
		jPanel21.validate();
		jScrollPane.validate();
		return added;
	}

	/**
	 * This method initializes jScrollPane	
	 * 	
	 * @return javax.swing.JScrollPane	
	 */
	private JScrollPane getJScrollPane() {
		if (jScrollPane == null) {
			jScrollPane = new JScrollPane();
			jScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			jScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
			jScrollPane.setViewportView(getJPanel21());
		}
		return jScrollPane;
	}

	/**
	 * This method initializes jPanel21	
	 * 	
	 * @return javax.swing.JPanel	
	 */
	 private JPanel getJPanel21() {
		if (jPanel21 == null) {
			jPanel21 = new JPanel();
			jPanel21.setLayout(new MigLayout("","[grow]",""));
			jPanel21.setBackground(new java.awt.Color(100,100,100));
			jPanel21.setVisible(true);
		}
		return jPanel21;
	}

	/**
	 * This method initializes jTextField	
	 * 	
	 * @return javax.swing.JTextField	
	 */
	private JTextField getJTextField() {
		if (jTextField == null) {
			jTextField = new JTextField();
			jTextField.setPreferredSize(new Dimension(100, 19));
			jTextField.setToolTipText("Sampling Group where to add the new Sample. Only alphanumeric and underscore characters.");
			jTextField.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			jTextField.setHorizontalAlignment(JTextField.LEFT);
			jTextField.setText("SampGroup");
			jTextField.addFocusListener(new FocusListener() {

				public void focusGained(FocusEvent e) {
					// Do nothing special :)
				}

				public void focusLost(FocusEvent e) {
					if( !jTextField.getText().matches("^([a-z]|[A-Z]|[0-9]|_)+$") ) {
						JOptionPane.showConfirmDialog(SamplingSystemGUI.this,
								"Group name '" + jTextField.getText() + "' is invalid.\nPlease use only alphanumeric characters and/or underscores.",
								"Invalid group name",
								JOptionPane.PLAIN_MESSAGE,JOptionPane.WARNING_MESSAGE);
						jTextField.setText("");
						jTextField.grabFocus();
					}
				}
			}
			);
			}
		return jTextField;
	}

	/**
	 * This method initializes jMenuItem	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getJMenuItem() {
		if (jMenuItem == null) {
			jMenuItem = new JMenuItem();
			jMenuItem.setText("Save GUI status");
			jMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
				    JFileChooser chooser = new JFileChooser();
				    int returnVal = chooser.showSaveDialog(cl.utfsm.samplingSystemUI.SamplingSystemGUI.this);
				    if(returnVal == JFileChooser.APPROVE_OPTION) {
				    	writeStatusFile(chooser.getSelectedFile().getAbsolutePath() + ".ssgst");
				    }
				}
			});
		}
		return jMenuItem;
	}

	/**
	 * This method initializes jMenuItem1	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getJMenuItem1() {
		if (jMenuItem1 == null) {
			jMenuItem1 = new JMenuItem();
			jMenuItem1.setText("Choose Sampling Manager");
			jMenuItem1.addActionListener(new java.awt.event.ActionListener() {
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
		return jMenuItem1;
	}

	/**
	 * This method initializes jMenuItem2	
	 * 	
	 * @return javax.swing.JMenuItem	
	 */
	private JMenuItem getJMenuItem2() {
		if (jMenuItem2 == null) {
			jMenuItem2 = new JMenuItem();
			jMenuItem2.setText("Load GUI Status");
			jMenuItem2.addActionListener(new java.awt.event.ActionListener() {
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
		return jMenuItem2;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				SamplingSystemGUI thisClass = new SamplingSystemGUI();
				thisClass.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
				thisClass.setVisible(true);
			}
		});
	}

	/**
	 * This is the default constructor
	 */
	public SamplingSystemGUI() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		this.setSize(900, 400);
		this.setJMenuBar(getSSGMenuBar());
		this.setContentPane(getJContentPane());
		this.setTitle("Sampling System GUI");
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
	
	/**
	 * This method initializes jContentPane
	 * 
	 * @return javax.swing.JPanel
	 */
	private JPanel getJContentPane() {
		if (jContentPane == null) {
			GridBagConstraints gridBagConstraints5 = new GridBagConstraints();
			gridBagConstraints5.fill = GridBagConstraints.BOTH;
			gridBagConstraints5.gridy = 0;
			gridBagConstraints5.weightx = 1.0;
			gridBagConstraints5.weighty = 1.0;
			gridBagConstraints5.gridx = 0;
			jContentPane = new JPanel();
			jContentPane.setLayout(new GridBagLayout());
			jContentPane.add(getComponentListerContainer(), gridBagConstraints5);
		}
		return jContentPane;
	}
	
	public void fillWidgets(String[] components, LinkedList<List<String>> properties){
		this.propList = properties;
		this.compList = components;
		fillComponentBox1(components);
	}
	
	private void fillComponentBox1(String[] components){
		ComponentBox1.removeAllItems();
		for(int i=0;i<components.length;i++)
			ComponentBox1.addItem(components[i]);
	}
	
/*	private void fillComponentBox2(String[] components){
		ComponentBox2.removeAllItems();
		for(int i=0;i<components.length;i++)
			ComponentBox2.addItem(components[i]);
	}*/
	
	private void fillPropertyBox1(List<String> prop){
		PropertyBox1.removeAllItems();
		/*if (prop.isEmpty()){
			ComponentBox2.removeAllItems();
			PropertyBox2.removeAllItems();
			disableAll();
			return;
		}*/
		PropertyBox1.setEnabled(true);
		addSampleButton.setEnabled(true);
		for(String s:prop)
			PropertyBox1.addItem(s);
	}
	/*
	private void fillPropertyBox2(List<String> prop){
		PropertyBox2.removeAllItems();
		if (prop.isEmpty()){
			jButton1.setEnabled(false);
			return;
		}
		PropertyBox2.setEnabled(true);
		jButton1.setEnabled(true);
		for(String s:prop)
			PropertyBox2.addItem(s);
	}
	
	private void disableAll(){
		PropertyBox1.setEnabled(false);
		addSampleButton.setEnabled(false);
		
		ComponentBox2.setEnabled(false);
		PropertyBox2.setEnabled(false);
		jButton1.setEnabled(false);
;	}*/
	
	private BeanGrouper groupExists(String groupName){
		for(int i=0; i<jPanel21.getComponentCount();i++)
			if (((BeanGrouper)jPanel21.getComponent(i)).getCheckName().toString().equalsIgnoreCase(groupName)) return ((BeanGrouper)jPanel21.getComponent(i));
		return null;
	}
	
	@SuppressWarnings("unchecked")
	private void readStatusFile(String filename,boolean startup){
		FileInputStream fis = null;
		ObjectInputStream in = null;
		try {
			fis = new FileInputStream(filename);
			in = new ObjectInputStream(fis);
			if(startup){
				int n = JOptionPane.showConfirmDialog(this, "An old status file has been found\nWould you like to load it?", "Old status file found", JOptionPane.YES_NO_OPTION, JOptionPane.INFORMATION_MESSAGE);
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
	public void deleteFromStatus(ArrayList<DataPrinter> samplers, String group) {
		
		for(DataPrinter dp : samplers )
			for(SerializableProperty sp : status ) {
				if( sp.getComponent().equals(dp.component) &&
					sp.getProperty().equals(dp.property)   &&
					sp.getSamplingGroup().equals(group) ) {
					status.remove(sp);
					break;
				}
		}
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


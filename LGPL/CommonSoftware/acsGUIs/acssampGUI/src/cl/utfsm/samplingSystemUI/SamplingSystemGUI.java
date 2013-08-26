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

import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.HashSet;
import java.util.Vector;
import java.util.concurrent.ExecutionException;

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
import javax.swing.SwingWorker;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.filechooser.FileFilter;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Result;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import cl.utfsm.samplingSystemUI.core.SamplingManager;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;

/**
 * Main Widget class, and starting point for the SSG Software. Controls the
 * main flow of the software. <p> SamplingSystemGUI ask for a Sampling Manager,
 * then tries to load a status file, and then presents a window with the
 * Components and Properties available for sampling.<br/> If there was a status
 * file, SamplingSystemGUI will load it, and open the Sampling Groups as
 * recorded.
 * 
 * @author Jorge Avarias <javarias@inf.utfsm.cl>
 * @author Rodrigo Tobar <rtobar@inf.utfsm.cl>
 * @author Alejandro Baltra <abaltra@alumnos.inf.utfsm.cl>
 * @author Arturo Hoffstadt <ahoffsta@inf.utfsm.cl>
 * @author Cristián Maureria <cmaureir@inf.utfsm.cl>
 */
public class SamplingSystemGUI extends JFrame {

	private static final long serialVersionUID = 1L;

	public String MAN_NAME = "";
	public boolean correctManName = false;

	private LinkedList<List<String>> propList;
	private String[] compList = null;
	private ArrayList<SerializableProperty> status;
	private HashSet<String> samplingGroups;
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
	private JPanel ComponentPanel = null;
	private JPanel PropertyPanel = null;
	private JLabel componentLabel = null;
	private JLabel filterComponentLabel = null;
	private JTextField filterComponentTextField = null;
	private JComboBox ComponentComboBox = null;
	private JLabel propertyLabel = null;
	private JComboBox PropertyComboBox = null;
	private JLabel filterPropertyLabel = null;
	private JTextField filterPropertyTextField = null;
	private JLabel groupLabel = null;
	private JTextField groupTextField = null;
	private JButton addSampleButton = null;

	private StatusIcon statusIcon;
	private Vector<Object> property_sampled;

	/**
	 * This is the default constructor. It starts the initialization of the
	 * window.
	 */
	public SamplingSystemGUI() {
		super();
		BeanGrouperList = new ArrayList<BeanGrouper>();
		property_sampled = new Vector<Object>();
		initialize();
	}

	/**
	 * This method initializes this GUI
	 * @return void
	 */
	private void initialize() {

		this.setLocation(0, 0);
		this.setJMenuBar(getSSGMenuBar());
		this.setLayout(new GridLayout(1, 1, 10, 10));
		this.setContentPane(this.getPropertyAddPanel());
		this.setTitle("Sampling System GUI");
		this.setLocationRelativeTo(null);
		this.pack();
	}

	/**
	 * Method to check if a component and a property was previously sampled. The
	 * argument comp_prop is equal to component:property <br>
	 */
	public boolean isAlreadySampled(String comp_prop){
		for(int i=0; i< property_sampled.size(); i++) {
			if(((String)property_sampled.elementAt(i)).compareTo(comp_prop)==0) {
				return true;
			}
		}
		return false;
	}

	/** Add the component:property to the property_sampled vector */
	public void addToSampled(String comp_prop) {
		if(!isAlreadySampled(comp_prop)) {
			property_sampled.add((Object)comp_prop);
		}
	}

	/** removes the componente:property from the property_sampled vector */
	public void delFromSampled(String comp_prop){
		if(isAlreadySampled(comp_prop)){
			property_sampled.remove((Object)comp_prop);
		}

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
					checkSamplingManager();
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
						try {
							readStatusFile(chooser.getSelectedFile().getAbsolutePath(),false);
						} catch (ParserConfigurationException e1) {
							e1.printStackTrace();
						} catch (SAXException e1) {
							e1.printStackTrace();
						} catch (IOException e1) {
							e1.printStackTrace();
						}
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
						try {
							String file = chooser.getSelectedFile().getAbsolutePath();
							if(file.endsWith(".ssgst")){
								writeStatusFile(file);
							}else{
								writeStatusFile(file + ".ssgst");
							}
						} catch (ParserConfigurationException e1) {
							e1.printStackTrace();
						} catch (TransformerException e1) {
							e1.printStackTrace();
						} catch (FileNotFoundException e1) {
							e1.printStackTrace();
						}
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
					askClose();
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
						String message = "<html>Sampling System GUI Version 2.0<br>"
							+ "This software is released under <b>LGPL</b> license.<br>"
							+ "SSG was developed by the ALMA-UTFSM Team.</html>";
						String messageUrl = "<html>Please refer to <u><font color=#0000ff>" + url + "</font></u><br>"
							+ "for more information.</html>";

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
						constraints.insets = new Insets(0, 10, 0, 10);
						constraints.anchor = GridBagConstraints.WEST;
						aboutWindow.getContentPane().add(aboutLabel,constraints);
						constraints.anchor = GridBagConstraints.EAST;
						aboutWindow.getContentPane().add(imageLabel,constraints);
						constraints.gridwidth = 2;
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
					aboutWindow.setMinimumSize(new Dimension(550, 250));
					aboutWindow.setSize(getContentPane().getSize());
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
			PropertyAddPanel.setMinimumSize(new Dimension(450,350));
			PropertyAddPanel.setLayout(new GridBagLayout());
			PropertyAddPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			PropertyAddPanel.setPreferredSize(new Dimension(450,350));

			ComponentPanel = new JPanel();
			PropertyPanel = new JPanel();

			ComponentPanel.setLayout(new GridBagLayout());
			ComponentPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			ComponentPanel.setBorder(new javax.swing.border.TitledBorder("Components"));

			PropertyPanel.setLayout(new GridBagLayout());
			PropertyPanel.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			PropertyPanel.setBorder(new javax.swing.border.TitledBorder("Properties"));

			GridBagConstraints comp = new GridBagConstraints();
			comp.gridheight = 1;
			comp.gridwidth = 1;
			comp.insets = new Insets(15, 5, 5, 5);

			comp.anchor = GridBagConstraints.EAST;
			comp.gridx = 0;
			comp.gridy = 0;
			ComponentPanel.add(getFilterComponentLabel(), comp);

			comp.gridx = 1;
			comp.gridy = 0;
			comp.anchor = GridBagConstraints.WEST;
			ComponentPanel.add(getFilterComponentTextField(), comp);

			comp.anchor = GridBagConstraints.EAST;
			comp.gridx = 0;
			comp.gridy = 1;
			ComponentPanel.add(getComponentLabel(), comp);

			comp.gridx = 1;
			comp.gridy = 1;
			comp.anchor = GridBagConstraints.WEST;
			ComponentPanel.add(getComponentComboBox(), comp);

			comp.anchor = GridBagConstraints.EAST;
			comp.gridx = 0;
			comp.gridy = 0;
			PropertyPanel.add(getFilterPropertyLabel(), comp);

			comp.gridx = 1;
			comp.gridy = 0;
			comp.anchor = GridBagConstraints.WEST;
			PropertyPanel.add(getFilterPropertyTextField(), comp);			

			comp.anchor = GridBagConstraints.EAST;
			comp.gridx = 0;
			comp.gridy = 1;
			PropertyPanel.add(getPropertyLabel(), comp);

			comp.gridx = 1;
			comp.gridy = 1;
			comp.anchor = GridBagConstraints.WEST;
			PropertyPanel.add(getPropertyComboBox(), comp);


			comp.gridx = 0;

			GridBagConstraints c = new GridBagConstraints();
			c.gridheight = 1;
			c.gridwidth = 1;
			c.insets = new Insets(15, 5, 5, 5);

			c.anchor = GridBagConstraints.EAST;
			c.gridx = 0;
			c.gridy = 0;
			c.gridwidth = 2;
			PropertyAddPanel.add(ComponentPanel, c);

			c.anchor = GridBagConstraints.EAST;
			c.gridy = 1;
			c.gridx = 0;
			c.gridwidth = 2;
			PropertyAddPanel.add(PropertyPanel, c);

			c.anchor = GridBagConstraints.EAST;
			c.gridy = 2;
			c.gridx = 0;
			c.gridwidth = 1;
			PropertyAddPanel.add(getGroupLabel(), c);

			c.gridx = 1;
			c.gridy = 2;
			c.gridwidth = 1;
			c.anchor = GridBagConstraints.WEST;
			PropertyAddPanel.add(getGroupTextField(), c);

			c.anchor = GridBagConstraints.EAST;
			c.gridy = 3;
			c.gridx = 0;
			c.gridwidth = 2;
			PropertyAddPanel.add(getAddSampleButton(), c);
			PropertyAddPanel.validate();

			// This is a new status icon that should reflect the status of the SSG
			c.anchor = GridBagConstraints.WEST;
			c.gridy = 3;
			c.gridx = 0;
			PropertyAddPanel.add(getStatusIcon(), c);
			
			this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
			this.addWindowListener(new WindowAdapter(){
			     public void windowClosing(WindowEvent we){
			    	 askClose();    	 
			     }
			});	
		}
		return PropertyAddPanel;
	}

	/* This Method ask to the user, if really want to close a Sampling Group Window */
	private void askClose(){
		Object[] options = {"Yes","No"};
		int n = JOptionPane.showOptionDialog(this, "Exit Sampling System GUI?", "Warning",
			JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[1]);
		if (n == JOptionPane.YES_OPTION) {
			System.exit(0);
		} else if (n == JOptionPane.NO_OPTION){
		}
	}

	/**
	 * This method initializes statusIcon
	 * 
	 * @return javax.swing.JLabel
	 */
	private StatusIcon getStatusIcon() {
		if (statusIcon == null) {
			statusIcon = new StatusIcon();
		}
		return statusIcon;
	}

	/**
	 * This method initializes ComponentComboBox
	 * @return javax.swing.JComboBox
	 */
	private JComboBox getComponentComboBox() {
		if (ComponentComboBox == null) {
			ComponentComboBox = new JComboBox();
			ComponentComboBox.setPreferredSize(new Dimension(320, 24));
			//ComponentComboBox.setSize(ComponentComboBox.getPreferredSize());
			ComponentComboBox.addItemListener(new java.awt.event.ItemListener() {
				public void itemStateChanged(java.awt.event.ItemEvent e) {
					if(e.getStateChange() == java.awt.event.ItemEvent.DESELECTED)
						return;

					PropertyComboBox.setEnabled(true);
					String comp = e.getItem().toString();
					PropertyComboBox.removeAllItems();
					for(int i=0; i<compList.length;i++){
						/* We find the component. We show the properties for it. If we do not
						 * have them, we go and find them */
						if(compList[i].compareTo(comp)==0){
							if(propList.get(i) == null) {
								/* If we can't get the list of properties for the interface,
								 * (cuased because of the IR not available or not
								 * having the interface definition), this should be
								 * notified to the user. */
								final int k = i;
								SwingWorker<List<String>, Object> sw = new SwingWorker<List<String>, Object>() {
									public List<String> doInBackground() {
										ComponentComboBox.setEnabled(false);
										PropertyComboBox.setEnabled(false);
										List<String> list_ = SampTool.getPropsForComponent(compList[k]);
										return list_;
									}
									public void done(){
										ComponentComboBox.setEnabled(true);
										PropertyComboBox.setEnabled(true);
									}
								};
								sw.execute();
								try {
									List<String> list = sw.get();
									if(list == null) {
										PropertyComboBox.removeAllItems();
										PropertyComboBox.setEnabled(false);
										ComponentComboBox.hidePopup();
										JOptionPane.showMessageDialog(PropertyComboBox.getParent().getParent(),
											"The interface definition for the component '" + comp +
											"' could not be found in the Interface Repository\n" +
											"Please check that you have the Interface Repository running " +
											"and that the interface is loaded into it",
											"IR error",
											JOptionPane.ERROR_MESSAGE);
									} else {
										propList.add(i, list);
									}
								} catch (InterruptedException e1) {
									// TODO Auto-generated catch block
									e1.printStackTrace();
								} catch (ExecutionException e1) {
									// TODO Auto-generated catch block
									e1.printStackTrace();
								}
							}
							try{
								if(propList.get(i) != null) {
									fillPropertyComboBox(propList.get(i));
									}
							}catch(IndexOutOfBoundsException ex){
								PropertyComboBox.removeAllItems();
								PropertyComboBox.setEnabled(false);
							}catch(Exception e1){
								System.out.println("Unknow exception " + e1);
							}
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
			PropertyComboBox.setPreferredSize(new Dimension(320, 24));
			//PropertyComboBox.setSize(PropertyComboBox.getPreferredSize());
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
					addToSampling(component, property, group);
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
	 * This method initializes filterComponentLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getFilterComponentLabel() {
		if (filterComponentLabel == null) {
			filterComponentLabel = new JLabel("filter:");
		}
		return filterComponentLabel;
	}

	/**
	 * This method initializes filterComponentTextField
	 * @return javax.swing.JTextField
	 */
	private JTextField getFilterComponentTextField() {
		if (filterComponentTextField == null) {
			Dimension d = new Dimension(100, 19);
			filterComponentTextField = new JTextField();
			filterComponentTextField.setPreferredSize(d);
			filterComponentTextField.setToolTipText("Write a word to find a particular component.");
			//filterComponentTextField.setSize(d);
			filterComponentTextField.setMinimumSize(d);
			filterComponentTextField.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			filterComponentTextField.setHorizontalAlignment(JTextField.LEFT);
			filterComponentTextField.getDocument().addDocumentListener(new DocumentListener(){

				public void applyFilter(){
					int total = compList.length;
					String text = filterComponentTextField.getText();

					if(!filterComponentTextField.getText().isEmpty()){
						ComponentComboBox.removeAllItems();
						for (int i = 0; i < total; i++) {
							if(compList[i].contains(text)){
								ComponentComboBox.addItem(compList[i]);
							}
						}
						ComponentComboBox.hidePopup();
						ComponentComboBox.showPopup();
					}
					else{
						ComponentComboBox.hidePopup();
						ComponentComboBox.removeAllItems();
						for(int j = 0; j < total;j++){
							ComponentComboBox.addItem(compList[j]);
						}
					}
					
					if(ComponentComboBox.getItemCount() == 0){
						PropertyComboBox.removeAllItems();
					}
					
				}
				public void changedUpdate(DocumentEvent e) {
				}
				public void insertUpdate(DocumentEvent e) {
					applyFilter();
				}
				public void removeUpdate(DocumentEvent e) {
					applyFilter();
				}
			});
		}
		return filterComponentTextField;
	}

	/**
	 * This method initializes filterPropertyLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getFilterPropertyLabel() {
		if (filterPropertyLabel == null) {
			filterPropertyLabel = new JLabel("filter:");
		}
		return filterPropertyLabel;
	}

	/**
	 * This method initializes filterPropertyTextField
	 * @return javax.swing.JTextField
	 */
	private JTextField getFilterPropertyTextField() {
		if (filterPropertyTextField == null) {
			Dimension d = new Dimension(100, 19);
			filterPropertyTextField = new JTextField();
			filterPropertyTextField.setPreferredSize(d);
			filterPropertyTextField.setToolTipText("Write a word to find a particular property.");
			//filterPropertyTextField.setSize(d);
			filterPropertyTextField.setMinimumSize(d);
			filterPropertyTextField.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
			filterPropertyTextField.setHorizontalAlignment(JTextField.LEFT);
			filterPropertyTextField.getDocument().addDocumentListener(new DocumentListener(){

				public void applyFilter(){
					String item = (String)ComponentComboBox.getSelectedItem();
					int i = -1;
					for (int j =0; j < compList.length; j++ ) {
						if(compList[j].compareTo(item)  == 0) {
							i = j;
							break;
						}
					}
					if (i == -1){
						PropertyComboBox.removeAll();
						return;
					}
					
					int total = propList.get(i).size();
					
					String text = filterPropertyTextField.getText();
					PropertyComboBox.removeAllItems();
					for(int j = 0; j < total;j++){
						PropertyComboBox.addItem(propList.get(i).get(j).toString());
					}
				
					PropertyComboBox.showPopup();
					
					if(!filterPropertyTextField.getText().isEmpty()){	
						PropertyComboBox.removeAllItems();
						for (int j = 0; j < total; j++) {
							if(propList.get(i).get(j).toString().contains(text)){
								PropertyComboBox.addItem(propList.get(i).get(j).toString());
							}
						}
					}
					else{
						PropertyComboBox.hidePopup();
					}					
				}
				public void changedUpdate(DocumentEvent e) {
				}
				public void insertUpdate(DocumentEvent e) {
					applyFilter();
				}
				public void removeUpdate(DocumentEvent e) {
					applyFilter();
				}
			});
		}
		return filterPropertyTextField;
	}

	/**
	 * This method initializes groupLabel
	 * @return javax.swing.JLabel
	 */
	private JLabel getPropertyLabel() {
		if (propertyLabel == null) {
			propertyLabel = new JLabel("     Property:");
		}
		return propertyLabel;
	}

	/**
	 * This method initializes groupTextField
	 * @return javax.swing.JTextField
	 */
	private JTextField getGroupTextField() {
		if (groupTextField == null) {
			Dimension d = new Dimension(150, 19);
			groupTextField = new JTextField();
			groupTextField.setPreferredSize(d);
			//groupTextField.setSize(d);
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
					if(!groupTextField.getText().matches("^([a-z]|[A-Z]|[0-9]|_)+$")) {
						JOptionPane.showConfirmDialog(SamplingSystemGUI.this,
							"Group name '" + groupTextField.getText() + "' is invalid.\nPlease use only alphanumeric characters and/or underscores.",
							"Invalid group name",
							JOptionPane.PLAIN_MESSAGE, JOptionPane.WARNING_MESSAGE);
						groupTextField.setText("");
						groupTextField.grabFocus();
					}
				}
			});
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
		if(isAlreadySampled(component + ":" + property)){
			JOptionPane.showMessageDialog(null, "Property Already Sampled in another Sampling Group", "Error", JOptionPane.ERROR_MESSAGE);
			return false;
		} else {
			boolean added;
			BeanGrouper bg = groupExists(group);

			/* If there is no group with this name, we create it */
			if (bg == null){
				bg = new BeanGrouper(this, group, getStatusIcon().getStatus());
				bg.setGroupName(group);
				bg.addSamp(component, property);
				BeanGrouperList.add(bg);
				added = true;
				bg.setVisible(true);
			}
			/* Else, we check if component/property is already added */
			else {
				if(bg.checkIfExists(component, property)) {
					JOptionPane.showMessageDialog(this,
						"Component '" + component + "' with property '" + property +
						"'\nhas been already added to the sample list for group " + bg.getGroupName(),
						"Already added",
						JOptionPane.WARNING_MESSAGE);
					added = false;
				} else {
					bg.addSamp(component, property);
					added = true;
				}
			}
			return added;
		}
	}

	/**
	 * Checks if a BeanGrouper with the groupName exists
	 * @param groupName Name of the SamplingGroup to be searched.
	 * @return Whether or not the SamplingGroup exits.
	 */
	private BeanGrouper groupExists(String groupName){
		if(BeanGrouperList.isEmpty())
			return null;
		else{
			for(BeanGrouper bg : BeanGrouperList){
				if(bg.getGroupName().toString().equalsIgnoreCase(groupName)){
					return bg;
				}
			}
		}
		return null;
	}

	public void loadWindow(boolean check) throws ParserConfigurationException, SAXException, IOException{
		if (!check){
			/* Select the sampling manager */
			String s = (String)JOptionPane.showInputDialog(this,
				"Please Select a Sampling Manager", "Sampling Manager Selection",
				JOptionPane.PLAIN_MESSAGE, null, (Object[])SampTool.getSamplingManagers(),
				MAN_NAME);
			if(s != null && !s.trim().equals(""))
				this.MAN_NAME = s;
			else
				System.exit(0);
		} else {
			// Setear el MAN_NAME
			// Verificar que es correcto
		}

		checkSamplingManager();

		/* Show the main window */
		this.setVisible(true);
		this.setResizable(false);
	}

	private void checkSamplingManager() {
		Thread t = new Thread(new Runnable(){

			public void run() {
				try {
					SamplingManager man = SamplingManager.getInstance(MAN_NAME);
					man.getSampReference();
				} catch (SamplingManagerException e) {
					statusIcon.setStatus(StatusIcon.CONNECTED_TO_MANAGER);
					for (BeanGrouper bg: BeanGrouperList) {
						bg.setStatus(StatusIcon.CONNECTED_TO_MANAGER);
					}
					JOptionPane.showMessageDialog(null, "Can't connect to Sampling Manager. Isn't posible start some sampling");
					return;
				}
				statusIcon.setStatus(StatusIcon.CONNECTED_TO_SAMPMANAGER);
				for (BeanGrouper bg: BeanGrouperList) {
					bg.setStatus(StatusIcon.CONNECTED_TO_SAMPMANAGER);
				}
			}

		});
		t.start();
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

	private boolean validateStatusFile(String filename){
		final String JAXP_SCHEMA_LANGUAGE = "http://java.sun.com/xml/jaxp/properties/schemaLanguage";
		final String JAXP_SCHEMA_SOURCE = "http://java.sun.com/xml/jaxp/properties/schemaSource";
		final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";
		final String SCHEMA_FILE = "status.xsd";
		final String XML_FILE = filename;
		boolean valid = false;

		DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		documentBuilderFactory.setValidating(true);
		try {
			documentBuilderFactory.setAttribute(JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);
			documentBuilderFactory.setAttribute(JAXP_SCHEMA_SOURCE, new File(SCHEMA_FILE));
			DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
			// documentBuilder.setErrorHandler(new ParserErrorHandler());
			Document parse = documentBuilder.parse(new File(XML_FILE));
			if (parse != null)
				valid = true;
		} catch (SAXException saxEx){
			System.out.println("SAXException: XML bad syntax");
		} catch (Exception ex) {
			System.out.println("Exception: undiscovered exception");
		}
		return valid;
	}

	public void specialReadStatusFile(String filename) throws ParserConfigurationException, SAXException, IOException{
		readStatusFile(filename, false);
	}

	private void readStatusFile(String filename, boolean startup) throws ParserConfigurationException, SAXException, IOException{
		try {
			if(startup){
				int n = JOptionPane.showConfirmDialog(this,
					"An old status file has been found\nWould you like to load it?",
					"Old status file found", JOptionPane.YES_NO_OPTION,
					JOptionPane.INFORMATION_MESSAGE);
				if(n == JOptionPane.NO_OPTION){
					return;
				}
			}
			if(validateStatusFile(filename)){
				DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
				DocumentBuilder docBuilder = docBuilderFactory.newDocumentBuilder();
				Document doc = docBuilder.parse(new File(filename));
				doc.getDocumentElement().normalize();
				NodeList listOfSamplingGroup = doc.getElementsByTagName("SamplingGroup");

				for(int s=0; s<listOfSamplingGroup.getLength(); s++){
					Node firstSamplingGroup = listOfSamplingGroup.item(s);
					if(firstSamplingGroup.getNodeType() == Node.ELEMENT_NODE){
						Element firstSamplingGroupElement = (Element)firstSamplingGroup;
						// -------
						NodeList nameList = firstSamplingGroupElement.getElementsByTagName("SamplingGroupName");
						Element nameElement = (Element)nameList.item(0);
						NodeList nameText = nameElement.getChildNodes();
						String samplingGroupName = ((Node)nameText.item(0)).getNodeValue().trim();

						// MAN_NAME
						NodeList manNameList = firstSamplingGroupElement.getElementsByTagName("SamplingManagerName");
						Element manNameElement = (Element)manNameList.item(0);
						NodeList manNameText = manNameElement.getChildNodes();
						String manName = ((Node)manNameText.item(0)).getNodeValue().trim();
						String[] managers = SampTool.getSamplingManagers();
						for (int i = 0; i < managers.length; i++) {
							if (managers[i].equals(manName)){
								correctManName = true;
							}
						}
						if (correctManName){
							MAN_NAME = manName;
						} else {
							System.out.println("ERROR: The Sampling Manager Name on the loaded file doesn't exist.");
							return;
						}
						//-------
						NodeList freqList = firstSamplingGroupElement.getElementsByTagName("Frequency");
						Element freqElement = (Element)freqList.item(0);
						NodeList freqText = freqElement.getChildNodes();
						double frequency = Double.parseDouble(((Node)freqText.item(0)).getNodeValue().trim());
						//-------
						NodeList stList = firstSamplingGroupElement.getElementsByTagName("SamplingTime");
						Element stElement = (Element)stList.item(0);
						NodeList stText = stElement.getChildNodes();
						int samplingtime = Integer.parseInt(((Node)stText.item(0)).getNodeValue().trim());
						//-------
						NodeList twList = firstSamplingGroupElement.getElementsByTagName("TimeWindow");
						Element twElement = (Element)twList.item(0);
						NodeList twText = twElement.getChildNodes();
						int timewindow = Integer.parseInt(((Node)twText.item(0)).getNodeValue().trim());
						//----- Properties
						NodeList listProperties = firstSamplingGroupElement.getElementsByTagName("Sample");

						for(int o=0; o<listProperties.getLength(); o++){
							Node firstProperty = listProperties.item(o);
							if(firstProperty.getNodeType() == Node.ELEMENT_NODE){
								Element firstPropertyElement = (Element)firstProperty;
								//-------
								NodeList compList = firstPropertyElement.getElementsByTagName("component");
								Element compElement = (Element)compList.item(0);
								NodeList compText = compElement.getChildNodes();
								String component = ((Node)compText.item(0)).getNodeValue().trim();
								//-------
								NodeList propList = firstPropertyElement.getElementsByTagName("property");
								Element propElement = (Element)propList.item(0);
								NodeList propText = propElement.getChildNodes();
								String property = ((Node)propText.item(0)).getNodeValue().trim();
								boolean checkStatus = checkComponentProperty(component,property);
								System.out.println("opening "+component+", "+property+", "+samplingGroupName);
								if (checkStatus){
									addToSampling(component, property, samplingGroupName);
									
								}
								else{
									JOptionPane.showMessageDialog(this,
											"The component or the property in:\n\n- "+component+"#"+property+"\n\n are invalid on the Sampling Group: '"
											+samplingGroupName+"'");
								}
							}
						}
						for(BeanGrouper bg: BeanGrouperList){
							if(bg.getGroupName().equals(samplingGroupName)){
								bg.loadConfiguration(frequency, timewindow, samplingtime);
								break;
							}
						}
					}
				}
			} else {
				JOptionPane.showMessageDialog(this, "Your XML file have a bad format, please check it");
			}
		} catch (SAXParseException err) {
			System.out.println("** Parsing error" + ", line " + err.getLineNumber() + ", uri " + err.getSystemId());
			System.out.println(" " + err.getMessage());
		} catch (SAXException e) {
			Exception x = e.getException();
			((x == null) ? e : x).printStackTrace();
		} catch (Throwable t) {
			t.printStackTrace();
		}

	}

	private boolean checkComponentProperty(String component, String property) {
		System.out.println("Checking for "+component+", "+property);
		int c = 0;
		// Component verification
		for (String comp :SampTool.getComponents()) {
			if( comp.compareTo(component) == 0){
				c++;
				break;
			}
		}
		System.out.println("Component found at index "+c);
		// Property verification
		for(String prop: SampTool.getPropsForComponent(component)){			
			if (prop.compareTo(property) == 0){
				c++;
				System.out.println("Prop found "+prop);
				break;
			}
		}
		System.out.println("Property found at index "+c);
		
		if(c == 2){
			return true;
		}
		else{
			return false;
		}
	}

	private void writeStatusFile(String filename) throws ParserConfigurationException, TransformerException, FileNotFoundException{
		status = new ArrayList<SerializableProperty>();
		samplingGroups = new HashSet<String>();
		// Nuevo XML
		// BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

		String root = "SamplingStatus";

		DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
		Document document = documentBuilder.newDocument();

		Element rootElement = document.createElement(root);
		rootElement.setAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
		rootElement.setAttribute("xsi:noNamespaceSchemaLocation", "status.xsd");
		document.appendChild(rootElement);

		int i = 0;
		for(BeanGrouper bg: BeanGrouperList){
			for(SerializableProperty sp: bg.getSerializableProperty()){
				status.add(i, sp);
				samplingGroups.add(sp.getSamplingGroup());
				i++;
			}
		}
		int index = 0;
		for(int l=0; l<samplingGroups.size();l++){

			for(i=0;i<status.size();i++){
				if(samplingGroups.toArray(new String[0])[l] == status.get(i).getSamplingGroup()){
					index = i;
					break;
				}
			}
			String groupName = "SamplingGroup";
			Element groupElement = document.createElement(groupName);
			rootElement.appendChild(groupElement);

			String name = "SamplingGroupName";
			String name_data = status.get(index).getSamplingGroup();
			Element name_element = document.createElement(name);
			name_element.appendChild(document.createTextNode(name_data));
			groupElement.appendChild(name_element);

			String manName = "SamplingManagerName";
			String manName_data = MAN_NAME;
			Element manName_element = document.createElement(manName);
			manName_element.appendChild(document.createTextNode(manName_data));
			groupElement.appendChild(manName_element);

			String freq = "Frequency";
			String freq_data = Double.toString(status.get(index).getFrequency());
			Element freq_element = document.createElement(freq);
			freq_element.appendChild(document.createTextNode(freq_data));
			groupElement.appendChild(freq_element);

			String st = "SamplingTime";
			String st_data = Integer.toString(status.get(index).getSamplingTime());
			Element st_element = document.createElement(st);
			st_element.appendChild(document.createTextNode(st_data));
			groupElement.appendChild(st_element);

			String tw = "TimeWindow";
			String tw_data = Integer.toString(status.get(index).getTimeWindow());
			Element tw_element = document.createElement(tw);
			tw_element.appendChild(document.createTextNode(tw_data));
			groupElement.appendChild(tw_element);

			for(i=0;i<status.size();i++){
				if(samplingGroups.toArray(new String[0])[l] == status.get(i).getSamplingGroup()){
					/* Properties */
					String propertiesName = "Sample";
					Element propertiesElement = document.createElement(propertiesName);
					groupElement.appendChild(propertiesElement);

					String comp = "component";
					String comp_data = status.get(i).getComponent();
					Element comp_element = document.createElement(comp);
					comp_element.appendChild(document.createTextNode(comp_data));
					propertiesElement.appendChild(comp_element);

					String prop = "property";
					String prop_data = status.get(i).getProperty();
					Element prop_element = document.createElement(prop);
					prop_element.appendChild(document.createTextNode(prop_data));
					propertiesElement.appendChild(prop_element);
				}
			}
		}

		TransformerFactory transformerFactory = TransformerFactory.newInstance();
		Transformer transformer = transformerFactory.newTransformer();
		transformer.setOutputProperty(OutputKeys.INDENT, "yes");
		transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

		if(status.size() == 0)
			return;

		File file = new File(filename);
		if(file.exists()) file.delete();

		DOMSource source = new DOMSource(document);
		Result result = new StreamResult(new FileOutputStream(filename));
		transformer.transform(source, result);

	}

	/**
	 * Removes from the local list of Component/Properties that are being
	 * sampled a given set of sampler <code>samplers</code> for a given sampling
	 * group <code>group</code>
	 * @param samplers The given sampler list
	 * @param group The belonging sampling group for the given samplers
	 */
	public synchronized void deleteBeanGrouper(ArrayList<DataPrinter> samplers, String group) {
		for(DataPrinter dp : samplers)
			if(status != null){
				for(SerializableProperty sp : status) {
					if(sp.getComponent().equals(dp.component) &&
						sp.getProperty().equals(dp.property) &&
						sp.getSamplingGroup().equals(group)) {
						status.remove(sp);
						break;
					}
				}
			}

		for(BeanGrouper bg : BeanGrouperList){
			if(bg.getGroupName().toString().equalsIgnoreCase(group)){
				bg.setVisible(false);
				BeanGrouperList.remove(bg);
				bg.dispose();
				break;
			}
		}
	}

	public void setStatus(int status) {
		statusIcon.setStatus(status);
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

class ScriptFilter extends FileFilter {
	@Override
	public boolean accept(File f) {
		if(f.isDirectory()) return true;
		String name = f.getName();
		if(name==null) return false;
		int index = name.lastIndexOf(".");
		if (index <0) return false;
		String ext = name.substring(index);
		if(ext.equalsIgnoreCase(".sh") || ext.equalsIgnoreCase(".py") || ext.equalsIgnoreCase(".pl") || ext.equalsIgnoreCase(".rb")) return true;
		return false;
	}

	@Override
	public String getDescription() {
		return "Script file(*.sh, *.py, *.pl, *.rb)";
	}
}

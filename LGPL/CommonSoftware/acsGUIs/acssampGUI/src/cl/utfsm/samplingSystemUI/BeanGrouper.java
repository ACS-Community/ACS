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

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SpinnerNumberModel;
import javax.swing.JComboBox;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;



/**
 * Displays the GUI for a Sampling Group, allowing plotting and control of its functions.
 * 
 * Class that works as a displayable container of sampling items. It has an internal state to check
 * if it's selected to be sampled (a check-box) and N samples, which will start parallel to one another 
 * once the "Start Sample(s)" button from the GUI is pressed. 
 * 
 * @author Alejandro Baltra <abaltra@alumnos.inf.utfsm.cl>
 * @author Rodrigo Tobar <rtobar@inf.utfsm.cl>
 * @author Jorge Avarias <javarias@inf.utfsm.cl>
 * @author Arturo Hoffstadt <ahoffsta@inf.utfsm.cl>
 */
public class BeanGrouper extends JFrame implements WindowListener {
	
	/**
	 * Generated serialVersionUID by Eclipse
	 */
	private static final long serialVersionUID = 6190720245608994272L;
	private SamplingSystemGUI ssg = null;
	
	//GUI Widgets
	private JComboBox jcombo = null;
	private JFrame frame = null;
	private JButton startButton = null;
	private JToggleButton pauseButton = null;
	private JButton stopButton = null;
	private JButton resetFrequencyButton = null;
	private JButton cleanButton = null;
	private JLabel frequencyLabel = null;
	private JSpinner freqSpinner = null;
	private JLabel timeSampLabel = null;
	private JSpinner timeSampSpinner = null;
	private JLabel timeWindowLabel = null;
	private JSpinner timeWindowSpinner = null;
	private JToggleButton saveButton = null;
	private JLabel fileNameLabel = null;
	private JScrollPane statusScrollBar = null;
	private DefaultListModel model = null;
	private JList statusList = null;
	private StatusIcon statusIcon;
	//Menu
	private JMenuItem addScriptMenuItem;
	private JMenuItem delScriptMenuItem;
	private JMenu scriptMenu;
	private JMenu editMenu;
	private JMenuItem delEditMenuItem;
	private JMenuBar beanGrouperMenuBar;
	
	//For program control
	private ArrayList<DataPrinter> samplers = null;
	private boolean ready2samp = false;
	private boolean isStopped=true;
	private String group;
	private boolean faultErrorAddedToStatusBox = false;
	private boolean pausedGraph = false;
	
	//For script execution
	private ScriptExecutor script = null;
	
	// It dumps the contents to the final file
	SamplingDataCorrelator _sdc;

	/**
	 * This is the overloaded constructor that allows to save the Sampling Group name
	 * @param ssg The Sampling Group this BeanGrouper is attached to.
	 * @param group The name of the Sampling Group this BeanGrouper is grouping.
	 */
	public BeanGrouper(SamplingSystemGUI ssg, String group, int status) {
		super();
		this.ssg = ssg;
		this.group = group;
		initialize();
		getStatusIcon().setStatus(status);
	}

	/**
	 * This method initializes the GUI, setting up the layout.
	 */
	private void initialize() {
		this.setMinimumSize(new Dimension(990,600));
		this.setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
		c.insets = new Insets(5,5,5,5);

		/* First row only has the Plot. It is filled 
		 * after the creation of this object, in the addSamp method */

		/* Second Row */
		c.anchor = GridBagConstraints.WEST;
		c.gridy = 1; c.gridx = 0;
		c.weighty = 0;
		c.weightx = 0;
		c.gridwidth = 1;
		c.fill = GridBagConstraints.NONE;
		c.anchor = GridBagConstraints.WEST;
		this.add(getStartButton(), c);
		c.gridx = 1;
		c.anchor = GridBagConstraints.WEST;
		this.add(getPauseButton(),c);
		c.gridx = 2;
		c.anchor = GridBagConstraints.WEST;
		this.add(getStopButton(), c);
		c.gridx = 3;
		c.anchor = GridBagConstraints.EAST;
		this.add(getFrequencyLabel(), c);
		c.gridx = 4;
		c.anchor = GridBagConstraints.CENTER;
		this.add(getFreqSpinner(), c);
		c.gridx = 5;
		c.anchor = GridBagConstraints.WEST;
		this.add(getResetFrequencyButton(), c);
		c.gridx = 6;
		c.anchor = GridBagConstraints.WEST;
		this.add(getCleanButton(), c);
		c.gridx = 7;
		c.anchor = GridBagConstraints.WEST;
		this.add(getTimeSampLabel(), c);
		c.gridx = 8;
		c.anchor = GridBagConstraints.WEST;
		this.add(getTimeSampSpinner(), c);
		c.gridx = 9;
		c.anchor = GridBagConstraints.WEST;
		this.add(getTimeWindowLabel(), c);
		c.gridx = 10;
		c.anchor = GridBagConstraints.WEST;
		this.add(getTimeWindowSpinner(), c);
		
		/* Third row */
		c.anchor = GridBagConstraints.WEST;
		c.gridy = 2; c.gridx = 0;
		this.add(getSaveButton(),c);
		
		c.anchor = GridBagConstraints.WEST;
		c.gridx = 1;
		c.gridwidth = 5;
		c.weightx = 1;
		this.add(getFileNameLabel(), c);

		c.gridx = 6;
		c.gridwidth = 4;
		c.weightx = 0;
		c.fill = 1;
		c.anchor = GridBagConstraints.WEST;
		this.add(getStatusScrollBar(),c);
		
		c.gridx = 10;
		c.gridwidth = 1;
		this.add(getStatusIcon(), c);
		this.getStopButton().setEnabled(false);
		this.getPauseButton().setEnabled(false);
		
		this.setTitle("Sampling Group: "+ group);
		this.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		//addWindowListener(this);
		
		//add menu
		this.setJMenuBar(getBeanGrouperMenuBar());
		samplers = new ArrayList<DataPrinter>();
		
		//question about close the graph window
		this.addWindowListener(new WindowAdapter(){
				     public void windowClosing(WindowEvent we){
				    	 askClose();    	 
				     }
		 		});	
		
	}

	/*
	 * This Method ask to the user, if really want to close a Samling Group Window
	 */
	private void askClose(){

	  Object[] options = {"Yes","No"};
  	  int n = JOptionPane.showOptionDialog(this,
			  	"Would you really want to close this Sampling Group?"+"\n\n"+
			  	"If you want to sampling this properties in another moment,"+"\n"+
			  	"you must add one by one again.",
			  	"Warning",
			  	JOptionPane.YES_NO_OPTION,
			  	JOptionPane.QUESTION_MESSAGE,
			  	null,
			  	options,
			  	options[1]);
	  
	  if (n == JOptionPane.YES_OPTION) {
		   this.setVisible(false);
		   this.dispose();
		   for(DataPrinter wp : samplers){
			   ssg.deleteBeanGrouper(samplers, this.group);
			   ssg.delFromSampled(wp.component+":"+wp.property);
		   }
		   stopSample();
      }
	}
	/**
	 * This method clean the graphic<br>
	 * This JButton when you click it remove all points in the trace<br>
	 * @return javax.swing.JButton Reference to the Clean Button.
	 */
	
	private JButton getCleanButton() {
		
		if (cleanButton == null) {
		
			cleanButton = new JButton();
			cleanButton.setText("Clean");
			cleanButton.addActionListener(new java.awt.event.ActionListener(){			
				public void actionPerformed(java.awt.event.ActionEvent e) {
					
					for(DataPrinter wp : samplers){
						wp.getWidget().resetSampleCount();
					}
					
				}
			});
		}
		return cleanButton;
	}
	

	private StatusIcon getStatusIcon() {
		if( statusIcon == null ) {
			statusIcon = new StatusIcon(StatusIcon.CONNECTED_TO_SAMPMANAGER);
		}
		return statusIcon;
	}

	/**
	 * This method pause the graphic in movement<br>
	 * This JButton when click do a pause of the graphic, but the sampling still running<br>
	 * @return javax.swing.JButton Reference to the Pause Button.
	 */
	private JToggleButton getPauseButton() {
		if (pauseButton == null) {
			pauseButton = new JToggleButton();
			pauseButton.setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/player_pause.png")) );
			pauseButton.setToolTipText("Pause the graphic.");
			pauseButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if (pausedGraph == false) {
						for (DataPrinter i : samplers) {
							i.pauseSampling(true);
						}
						pauseButton.setSelected(true);
						pausedGraph = true;
					}
					else {
						for (DataPrinter i : samplers) {
							i.pauseSampling(false);
						} 					
        				pausedGraph = false;
						pauseButton.setSelected(false);
					}
				}
			});
		}
		return pauseButton;
	}

	/**
	 * This method initializes startButton<br>
	 * This JButton when click do a lot of effects, among them:<br>
	 * - Enabling and Disabling the corresponding widgets in the GUI.<br>
	 * - Starts the Sample 	
	 * @return javax.swing.JButton Reference to the Start Button.
	 */
	private JButton getStartButton() {
		if (startButton == null) {
			startButton = new JButton();
			startButton.setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/player_play.png")) );
			startButton.setToolTipText("Start the sampling.");
			startButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					stopButton.setEnabled(true);
					pauseButton.setEnabled(true);
					startButton.setEnabled(false);
					//to change the freq dinamically
					//getFreqTextField().setEnabled(false);
					getTimeSampSpinner().setEnabled(false);
					getTimeWindowSpinner().setEnabled(false);
					getSaveButton().setEnabled(false);
					startSample();
					setTimeWindow();
				}
			});
		}
		return startButton;
	}
	
	
	/**
	 * Initializes the Stop Button, and also performs on click, the stop of the Sampling.
	 * @return javax.swing.JButton Reference to the Stop Button
	 */
	private JButton getStopButton() {
		
		if (stopButton == null) {
			stopButton = new JButton();
			stopButton.setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/player_stop.png")) );
			stopButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					// Stop the samples
					pauseButton.setEnabled(false);
					stopSample();
					
					//UnPause the graph, if is already paused
					if (pausedGraph == true) {
						for (DataPrinter i : samplers) {
							i.pauseSampling(false);
						} 					
        				pausedGraph = false;
						pauseButton.setSelected(false);
					}
								
					// Enable/disable buttons
					timeSampSpinner.setEnabled(true);
					freqSpinner.setEnabled(true);
					timeWindowSpinner.setEnabled(true);
					stopButton.setEnabled(false);
				}
			});
		}
		
		return stopButton;
	}

	/**
	 * Initializes the Label that says "Sampling time:"
	 * @return javax.swing.JLabel Reference to the Label.
	 */
	private JLabel getTimeSampLabel() {
		if(timeSampLabel==null){
			timeSampLabel=new JLabel();
			timeSampLabel.setText("Sampling time (min):");
			timeSampLabel.setHorizontalAlignment(JTextField.RIGHT);
			timeSampLabel.setToolTipText("How long, in minutes, will the sampling last. A value of 0 means non-stopping sample.");
		}
		return timeSampLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Sampling Time for the Sampling Group.<br>
	 * By default the value is 0, which means infinite time (or until stop button is pressed).<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */ 
	private JSpinner getTimeSampSpinner() {
		if(timeSampSpinner == null){
			timeSampSpinner = new JSpinner();
			timeSampSpinner.setModel(new SpinnerNumberModel(0, 0, 1000, 1));
			timeSampSpinner.setToolTipText("How long, in minutes, will the sampling last. A value of 0 means non-stopping sample.");
		}
		return timeSampSpinner;
	}
	
	/**
	 * Initializes the Label that says "Time Window:"
	 * @return javax.swing.JLabel Reference to the Label.
	 */
	private JLabel getTimeWindowLabel() {
		if(timeWindowLabel==null){
			timeWindowLabel=new JLabel();
			timeWindowLabel.setText("Time Window (sec):");
			timeWindowLabel.setHorizontalAlignment(JTextField.LEFT);
			timeWindowLabel.setToolTipText("How much data, expressed in minutes, will the trend present in the graph.");
		}
		return timeWindowLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Time Window for the Sampling Group.<br>
	 * By default the value is 10, which means 10 minutes.<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */ 
	private JSpinner getTimeWindowSpinner() {
		if(timeWindowSpinner == null){
			timeWindowSpinner = new JSpinner();
			timeWindowSpinner.setToolTipText("How much data, expressed in seconds, will the trend present in the graph.");
			timeWindowSpinner.setModel(new SpinnerNumberModel(5, 1, 900, 1));
			timeWindowSpinner.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					setTimeWindow();
				}

			});
		}
		return timeWindowSpinner;
	}

	/**
	 * Initializes the Frequency Label
	 * @return javax.swing.JLabel Reference to the Label says "Frequency".
	 */
	private JLabel getFrequencyLabel() {
		if(frequencyLabel==null){
			frequencyLabel=new JLabel();
			frequencyLabel.setText("Frequency (Hz): ");
		}
		return frequencyLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Frequency for the Sampling Group.<br>
	 * By default the value is 10 Hz.<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */
	private JSpinner getFreqSpinner() {
		if(freqSpinner == null){
			freqSpinner = new JSpinner();
			freqSpinner.setToolTipText("How often, in herz, will the sampling occur.");
			freqSpinner.setModel(new SpinnerNumberModel(1, 0.1, 20.0, 0.1));
			freqSpinner.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					for( DataPrinter dp: samplers){
						double freq = ((SpinnerNumberModel)freqSpinner.getModel()).getNumber().doubleValue();
						dp.setFrequency(freq);
					}
				}

			});
		}
		return freqSpinner;
	}
	
	/**
	*	This method initializes resetFrequencyButton<br>
	*     This JButton when click do a kind of "Reset" in the frequency of the Samp<br>
	*     @return javax.swing.JButton Reference to the Reset Frequency Button.
	*     
	*/
	private JButton getResetFrequencyButton() {
	
		if (resetFrequencyButton == null) {
		
			resetFrequencyButton = new JButton();
			resetFrequencyButton.setText("Set Freq");
			resetFrequencyButton.addActionListener(new java.awt.event.ActionListener(){			
				public void actionPerformed(java.awt.event.ActionEvent e) {
					// stop the samp (Thread)
					stopSample();
					timeSampSpinner.setEnabled(true);
					freqSpinner.setEnabled(true);
					stopButton.setEnabled(false);
					pauseButton.setEnabled(false);
					getSaveButton().setEnabled(true);
					
					// start (again) the Thread, with the new frequency
					stopButton.setEnabled(true);
					pauseButton.setEnabled(true);
					startButton.setEnabled(false);
					getTimeSampSpinner().setEnabled(false);
					getSaveButton().setEnabled(false);
					startSample();				
				}
			});
		}
		return resetFrequencyButton;
	}
	
	private JToggleButton getSaveButton(){
		if(saveButton == null ){
			saveButton = new JToggleButton();
			saveButton.setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/filesave.png")) );
			saveButton.setSelected(false);
			saveButton.setToolTipText("Press to start saving sampled data to file");
			
			saveButton.addChangeListener( new ChangeListener() {
			
				@Override
				public void stateChanged(ChangeEvent e) {
					if( saveButton.isSelected() == true ){
						saveButton.setToolTipText("Press to stop saving sampled data to file");
						for( DataPrinter dp: samplers){
							((PlotPrinter)dp).setDumpToFile(true);
						}
					}else{
						saveButton.setToolTipText("Press to start saving sampled data to file");
						for( DataPrinter dp: samplers){
							((PlotPrinter)dp).setDumpToFile(false);
						}
					}
			
				}
			});
		}
		return saveButton;
	}
	
	/**
	 * Initializes the Label that says "Sampling To:"
	 * @return javax.swing.JLabel Reference to the Label.
	 */
	private JLabel getFileNameLabel(){
		if(fileNameLabel == null){
			fileNameLabel = new JLabel();
			fileNameLabel.setText("Saving data to:");
			fileNameLabel.setToolTipText("Informs the name of the file in which the data is being saved.");
		}
		return fileNameLabel;
	}
	
	private JScrollPane getStatusScrollBar() {
		model = new DefaultListModel();
		if(statusList==null) {
			statusList = new JList(model);
			model.addElement("Status: Sampling Group ready to start.");
		}
		if(statusScrollBar==null) {
			statusScrollBar= new JScrollPane(statusList);
		}
		statusList.setVisibleRowCount(1);
		statusScrollBar.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		return statusScrollBar;
	}

	private void addToStatusList(String status) {
		model.addElement(status);
	}

	private void updateStatusList() {
		statusList.setSelectedIndex(statusList.getModel().getSize()-1);
		statusScrollBar.repaint();
	}

	private JMenuBar getBeanGrouperMenuBar() {
		if (beanGrouperMenuBar == null) {
			beanGrouperMenuBar = new JMenuBar();
			beanGrouperMenuBar.add(getScriptMenu());
			beanGrouperMenuBar.add(getEditMenu());
		}
		return beanGrouperMenuBar;
	}

	private JMenu getScriptMenu() {
		if (scriptMenu == null) {
			scriptMenu = new JMenu();
			scriptMenu.setText("Scripting");
			scriptMenu.add(getAddScriptMenuItem());
			scriptMenu.add(getDelScriptMenuItem());
			scriptMenu.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if(script==null){
						getDelScriptMenuItem().setEnabled(false);
					}else{
						getDelScriptMenuItem().setEnabled(true);
					}
				}
			});
		}
		return scriptMenu;
	}
	
	private JMenu getEditMenu(){
		if (editMenu == null) {
			editMenu = new JMenu();
			editMenu.setText("Edit");
			editMenu.add(getDelEditMenuItem());
		}
		return editMenu;
	}
	
	private JMenuItem getDelEditMenuItem() {
		if(delEditMenuItem == null) {
			delEditMenuItem = new JMenuItem();
			delEditMenuItem.setText("Delete Property");
			delEditMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					createDelPropWindow();
				}
			} );
		}
		return delEditMenuItem;
	}
	
	private void createDelPropWindow() {
		if(frame != null) {
			return;
		}
		JLabel delLabel = new JLabel("Select the Component:Property to delete:");
		jcombo      = new JComboBox();
		frame     = new JFrame("Delete");
		JPanel    panel     = new JPanel(new BorderLayout( 5, 5 ));

		JButton   delButton = new JButton("Delete");
		for(DataPrinter wp : samplers){
			jcombo.addItem(wp.component+":"+wp.property);
		}
		
		delButton.addActionListener( new java.awt.event.ActionListener(){
			public void actionPerformed(ActionEvent e) {
				String comProp[] = new String[2];
				comProp          = jcombo.getSelectedItem().toString().split(":");
				removeSamp(comProp[0],comProp[1]);
				jcombo.removeItemAt(jcombo.getSelectedIndex());
				addToStatusList(comProp[0] + "#" + comProp[1] + "removed");
				
			}
		});
		delLabel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
		panel.add(delLabel, BorderLayout.NORTH);
		jcombo.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
		panel.add(jcombo, BorderLayout.CENTER);
		delButton.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));
		panel.add(delButton, BorderLayout.EAST);
		//frame.setDefaultCloseOperation(EXIT_ON_CLOSE);
		frame.add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.addWindowListener(new java.awt.event.WindowListener(){

			@Override
			public void windowActivated(WindowEvent e) {}

			@Override
			public void windowClosed(WindowEvent e) {}

			@Override
			public void windowClosing(WindowEvent e) {}

			@Override
			public void windowDeactivated(WindowEvent e) {
				frame = null;
			}

			@Override
			public void windowDeiconified(WindowEvent e) {}

			@Override
			public void windowIconified(WindowEvent e) {}

			@Override
			public void windowOpened(WindowEvent e) {}
			
		});
	}

	private JMenuItem getDelScriptMenuItem() {
		if (delScriptMenuItem == null) {
			delScriptMenuItem = new JMenuItem();
			delScriptMenuItem.setText("Delete script");
			delScriptMenuItem.setEnabled(false);
			delScriptMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					script = null;
					delScriptMenuItem.setEnabled(false);
					getAddScriptMenuItem().setEnabled(true);
					getStartButton().setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/player_play.png")) );
					getStartButton().setToolTipText("Start the sampling, and also start the configurated script.\nThe sampling will end with the script.");
					getTimeSampSpinner().setEnabled(true);
				}
			});
		}
		return delScriptMenuItem;
	}

	private JMenuItem getAddScriptMenuItem() {
		if (addScriptMenuItem == null) {
			addScriptMenuItem = new JMenuItem();
			addScriptMenuItem.setText("Add Script ...");
			addScriptMenuItem.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					ScriptConfigGui scriptC = new ScriptConfigGui(script);
					script = scriptC.getScriptExec();
					System.out.println("Finished Script Configuration GUI: " + script);
					if( script != null ){
						getDelScriptMenuItem().setEnabled(true);
						addScriptMenuItem.setEnabled(false);
						getStartButton().setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/run.png")) );
						getStartButton().setToolTipText("Start the sampling.");
						getTimeSampSpinner().setEnabled(false);
					}
				}
			});
		}
		return addScriptMenuItem;
	}

	/**
	 * This method adds a new sample (represented as a {@link DataPrinter}) into this class.<br>
	 * This class should be able to hold N samples inside itself. <br>
	 * - First, the {@link BeanLister} widget contained by the {@link DataPrinter} is added to the jPanel<br>
	 * - Second, the {@link DataPrinter} is added to the list of samplers.
	 * @param w The Printer Object that is used to represent a sampling by its own.
	 */
	private void addSamp(DataPrinter w){
		
		/* There is only one plot per window. The other
		 * sampled properties are shown in the same plot */
		if( samplers.size() == 0 ){
			GridBagConstraints c = new GridBagConstraints();
			c.insets = new Insets(5,5,5,5);
			
			// First row only has the Plot
			c.anchor = GridBagConstraints.CENTER;
			c.gridx = 0; c.gridy = 0;
			c.fill = GridBagConstraints.BOTH;
			c.weighty = 1;
			c.weightx = 1;
			c.gridwidth = 12;
			this.add( (JPanel)w.getWidget(), c);
		}
		updateLabel();
		//w.getSamplingWidget().repaint();
		samplers.add(w);
	}

	/** This method removes a sample from the chart */
	
	private void removeSamp(String component, String property) {
		for(int i = 0; i < samplers.size() ; i++) {
			if(samplers.get(i).getComponent().equals(component) && samplers.get(i).getProperty().equals(property)){
				((PlotWidget)samplers.get(i).getSamplingWidget() ).removeTrace(component, property);//remove from GUI
				samplers.get(i).getSamplingWidget().repaint();
				samplers.remove(i);//remove from DataPrinter Arraylist
				ssg.delFromSampled(component + ":" + property);//remove from sampled vector
				break;	
			}
		}
	}
	
	/**
	 * This method adds to the Sampling Group, a new sample. This method is used by <br>
	 * {@link SamplingSystemGUI} to add Sampling to the Sampling Group, represented by {@link BeanGrouper}.<br>
	 * - Creates a {@link PlotPrinter}, which extends from {@link DataPrinter}.
	 * - Set the Components and Properties to the PlotPrinter
	 * - Adds this new Sample to the Sampling Group using the other addSamp method. 
	 * @param component Name of the Component containing the property to be sampled.
	 * @param property Name of the Property to be sampled.
	 */
	public void addSamp(String component, String property) {
		
		if( !isStopped ) {
			JOptionPane.showMessageDialog(ssg,
					"Cannot add a new Property to  '" + group + "':\n" +
					"the sampling group is in the middle of a sampling process.",
					"Running sampling group", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		//TODO: Check for presence of 1 or more, and reassign the BeanLister for each one.
		//TODO: Also assign a unique number to each one, to allow them to know their "line" in the plot
		PlotPrinter w;
		if( samplers.size() == 0){
			w = new PlotPrinter(ssg);
		}else{
			w = new PlotPrinter(ssg, (PlotWidget)samplers.get(0).getWidget() , samplers.size() );
		}
		w.setComponent(component);
		w.setProperty(property);
		w.setDumpToFile(getSaveButton().isSelected());			
		addSamp(w);
		ssg.addToSampled(component + ":" + property);
		if(frame != null)
			jcombo.addItem(component+":"+property);
	}

	public void updateLabel(){
	}
	
	public void runScript() {
		if(script == null) {
			//FIXME: Print warning window
			return;
		}
		startSample();
		script.run();
		stopSample();
	}
	
	/**
	 * Method to begin the sampling of all the samples contained in the class.
	 * It initializes the SamplingDataCorrelator Object, which is used to store data to file.<br>
	 */
	private void startSample(){
		double freq=0;
		int prev_status;
		
		freq = ((SpinnerNumberModel)freqSpinner.getModel()).getNumber().doubleValue();
		Date startTimestamp = new Date();
		if( getSaveButton().isSelected() ){
			_sdc = new SamplingDataCorrelator(group, ((SpinnerNumberModel)freqSpinner.getModel()).getNumber().intValue(), startTimestamp);
			getFileNameLabel().setText("Sampling to file: " + _sdc.getFilename());
			getFileNameLabel().setToolTipText("Saving data to: " + _sdc.getFilename() );
		}else{
		}
		
		prev_status = getStatusIcon().getStatus();
		model.clear();
		for(DataPrinter wp : samplers){
			wp.setFrequency(freq);
			try {
				wp.startSample();
				isStopped = false;
				addToStatusList("Status: Sampling of " + wp.getComponent() + "#" + wp.getProperty() + " started" );
				updateStatusList();
				setStatusIcon(StatusIcon.SAMPLING);
				ssg.setStatus(StatusIcon.CONNECTED_TO_SAMPMANAGER);
			} catch(alma.ACSErrTypeCommon.CouldntAccessComponentEx e) {
				wp.setComponentAvailable(false,"Cannot access component");
				addToStatusList("Status: Cannot access component " + wp.getComponent());
				updateStatusList();
				setStatusIcon(StatusIcon.SAMPLING_WARNING);
			} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
				wp.setComponentAvailable(false,"Type not supported");
				addToStatusList("Status: Type not supported " + wp.getComponent() + "#" + wp.getProperty());
				updateStatusList();
				setStatusIcon(StatusIcon.SAMPLING_WARNING);
			} catch(alma.ACSErrTypeCommon.CouldntAccessPropertyEx e) {
				wp.setComponentAvailable(false,"Cannot access property");
				addToStatusList("Status: Cannot access property " + wp.getProperty());
				updateStatusList();
				setStatusIcon(StatusIcon.SAMPLING_WARNING);
			} catch(cl.utfsm.samplingSystemUI.core.SamplingManagerException e) {
				
				wp.setComponentAvailable(false,"Sampling Manager fault");
				//to have only one "Status: Sampling Manager Fault" in the statusComboBox
				if(!faultErrorAddedToStatusBox) {				
					addToStatusList("Status: Sampling Manager fault");
					updateStatusList();
					faultErrorAddedToStatusBox = true;
				}
				getStatusIcon().setStatus(StatusIcon.DISCONNECTED);
				ssg.setStatus(StatusIcon.DISCONNECTED);
			}
		}

		if( isStopped ) {
			stopButton.setEnabled(false);
			pauseButton.setEnabled(false);
			startButton.setEnabled(true);
			freqSpinner.setEnabled(true);
			timeSampSpinner.setEnabled(true);
			getSaveButton().setEnabled(true);
			//setStatusIcon(prev_status);
			return;
		}
		
		if(script != null)
			new ScriptRunner(script).start();
			
		
		int mins = ((SpinnerNumberModel)this.getTimeSampSpinner().getModel()).getNumber().intValue();
		if(script == null && mins!=0 )
			new Watchdog(mins).start();
		
	}

	private void setStatusIcon(int status) {
		StatusIcon icon = getStatusIcon();
		icon.setStatus(status);
	}

	/**
	 * Method used by the {@link SamplingSystemGUI} to set the status icon when
	 * a new sampling manager is selected
	 * @param status
	 */
	public void setStatus(int status) {
		if( isStopped )
			setStatusIcon(status);
	}

	/**
	 * Getter for a boolean status check.
	 * @return True if the Group is selected for sampling, false in the other case
	 */
	public boolean Ready2Samp(){
		return ready2samp;
	}
	
	/**
	 * Generic setter for the Group name
	 * @param name Name of the group
	 */
	public void setGroupName(String name){
		this.group = name;
	}
	
	/**
	 * Generic getter for the group name
	 * @return The name of group.
	 */
	public String getGroupName(){
		return group;
	}

	public boolean checkIfExists(String component, String property) {
		
		for (Iterator<DataPrinter> iter = samplers.iterator(); iter.hasNext();) {
			DataPrinter element = (DataPrinter) iter.next();
			if( element.component == component &&
				element.property  == property )
				return true;
		}
		return false;
	}

	private void stopSample() {
		
		if(isStopped ) return;
		isStopped=true;
		stopButton.setEnabled(false);
		pauseButton.setEnabled(false);
		startButton.setEnabled(true);
		model.clear();
		getSaveButton().setEnabled(true);
		addToStatusList("Status: Sampling stoped");
		updateStatusList();
		setStatusIcon(StatusIcon.CONNECTED_TO_SAMPMANAGER);

		for(DataPrinter i : samplers) {
			i.stopSampling();
			if(getSaveButton().isSelected() == true){
				if( i.isComponentAvailable() == true ) {
					_sdc.addSamplingSet(((PlotPrinter)i).getFilename());
				}
			}

		}
		if(getSaveButton().isSelected() == true){
			_sdc.dumpToFile();
		}
		for(DataPrinter i : samplers) {
			if(getSaveButton().isSelected() == true){
				if( i.isComponentAvailable() == true ) {
					File temp = new File(((PlotPrinter)i).getFilename() );
					temp.delete();
				}
			}
			
		}
	}

	public ArrayList<SerializableProperty> getSerializableProperty(){
		ArrayList<SerializableProperty> spa = new ArrayList<SerializableProperty>();
		for( DataPrinter dp: samplers){
			SerializableProperty sp = dp.getSerializableProperty();
			sp.setSamplingGroup(getGroupName());
			sp.setSamplingTime( ((SpinnerNumberModel)this.getTimeSampSpinner().getModel()).getNumber().intValue() );
			sp.setTimeWindow( ((SpinnerNumberModel)this.getTimeWindowSpinner().getModel()).getNumber().intValue() );
			spa.add(sp);
		}
		return spa;
	}

	public void loadConfiguration(double frequency, int timeWindow, int samplingTime){
		this.getFreqSpinner().setValue(frequency);
		this.getTimeWindowSpinner().setValue(timeWindow);
		this.getTimeSampSpinner().setValue(samplingTime);		
	}

	public ScriptExecutor getScript() {
		return script;
	}

	public void setScript(ScriptExecutor script) {
		this.script = script;
	}

	/**
	 * Class in charge of stopping the Sampling for all the DataPrinter objects that composes the BeanGrouper<br />
	 * A parameters consisting in minutes has to be passed to the constructor, and after that time has passed <br />
	 * the thread stops the sampling.
	 */
	class Watchdog extends Thread{
		private long sleepTime;
		
		/**
		 * Sets the period of time in which this BeanGrouper will sample, and sets to maximum this thread priority.
		 * @param mins
		 */
		public Watchdog(int mins){
			this.setPriority(Thread.MAX_PRIORITY);
			sleepTime=mins*60*1000;
		}
		
		/**
		 * Starts the thread right away putting it to sleep for the minutes needed. Then, stops the sampling in the BeanGrouper.
		 */
		public void run() {
			try {
				Thread.sleep(sleepTime);
				if(!isStopped)
					stopSample();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	
	class ScriptRunner extends Thread{
		
		/**
		 * Sets the period of time in which this BeanGrouper will sample, and sets to maximum this thread priority.
		 * @param mins
		 */
		public ScriptRunner(ScriptExecutor script){
			this.setPriority(Thread.MAX_PRIORITY);
		}
		
		/**
		 * Starts the thread right away putting it to sleep for the minutes needed. Then, stops the sampling in the BeanGrouper.
		 */
		public void run() {
			script.run();
			if(!isStopped) stopSample();
		}
	}

	@Override
	public void windowActivated(WindowEvent e) { }

	@Override
	public void windowClosed(WindowEvent e) { }

	@Override
	public void windowDeactivated(WindowEvent e) { }

	@Override
	public void windowDeiconified(WindowEvent e) { }

	@Override
	public void windowIconified(WindowEvent e) { }

	@Override
	public void windowOpened(WindowEvent e) { }
	
	@Override
	public void windowClosing(WindowEvent e) {
        if(!isStopped)
        	stopSample();
		ssg.deleteBeanGrouper(samplers,getGroupName());
    }
	
	private void setTimeWindow() { //double frequency, int timeWindow){
		double frequency = ((SpinnerNumberModel)this.getFreqSpinner().getModel()).getNumber().doubleValue();
		int timeWindow = ((SpinnerNumberModel)this.getTimeWindowSpinner().getModel()).getNumber().intValue();
		for(int i=0;i<samplers.size();i++){
			samplers.get(i).getWidget().setTimeWindow(frequency, timeWindow);
		}
	}
}

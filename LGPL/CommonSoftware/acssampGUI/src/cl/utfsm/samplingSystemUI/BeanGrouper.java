package cl.utfsm.samplingSystemUI;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import javax.swing.JOptionPane;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;


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
public class BeanGrouper extends JFrame implements WindowListener{

	private static final long serialVersionUID = 1L;
	
	private SamplingSystemGUI ssg = null;
	
	//GUI Widgets
	private JPanel jPanel = null; // Contains the BeanWidgets.
	private JButton startButton = null;
	private JButton stopButton = null;
	private JLabel fileNameLabel = null;	
	private JLabel frecuencyLabel = null;
	private JTextField freqTextField = null;
	private JLabel timeSampLabel = null;
	private JTextField timeSampTextField = null;
	private JLabel timeWindowLabel = null;
	private JTextField timeWindowTextField = null;
	
	//For program control
	private ArrayList<DataPrinter> samplers = null;
	private boolean ready2samp = false;	
	private FileHelper toFile;
	private boolean isStopped=true;	
	private Date startTimestamp;
	private String group; 
	
	/**
	 * This is the default constructor
	 * @param ssg The Sampling Group this BeanGrouper is attached to.
	 */
	public BeanGrouper(SamplingSystemGUI ssg) {
		super();
		this.ssg = ssg;
		toFile=new FileHelper();
		initialize();
	}

	/**
	 * This is the overloaded constructor that allows to save the Sampling Group name
	 * @param ssg The Sampling Group this BeanGrouper is attached to.
	 * @param group The name of the Sampling Group this BeanGrouper is grouping.
	 */
	public BeanGrouper(SamplingSystemGUI ssg, String group) {
		super();
		this.ssg = ssg;
		toFile=new FileHelper(group);
		initialize();
		this.group = group;
	}

	/**
	 * This method initializes the GUI, setting up the layout.
	 */
	private void initialize() {
		this.setMinimumSize( new Dimension( 750, 550) );
		this.setLayout(new MigLayout("", // Layout Constraints
				"[]5[]5[][]10[][]10[][]", // Column Constraints
				"[center]5[]5[]" // Row Constraints
				) );
		//First row only has the Plot
		this.add(getJPanel(), "span" );
		
		//Second Row
		this.add(getStartButton());
		this.add(getStopButton());		
		this.add(getFrequencyLabel());
		this.add(getFreqTextField());
		this.add(getTimeSampLabel());
		this.add(getTimeSampTextField());
		this.add(getTimeWindowLabel());
		this.add(getTimeWindowTextField(), "wrap");
		
		this.add(getFileNameLabel(), "span");
		
		this.getStopButton().setEnabled(false);
		
		this.setTitle("Plotting Sampling Group: "+ group);
		this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener(this);
		
		samplers = new ArrayList<DataPrinter>();
	}
	
	/**
	 * This method initializes startButton<br>
	 * This JButton when click do a lot of effects, among them:<br>
	 * - Enabling and Disabling the corresponding widgets in the GUI.<br>
	 * - Initialize the FileHelper Object, which is used to store data to file.<br>
	 * - Starts the Sample 	
	 * @return javax.swing.JButton Reference to the Start Button.
	 */
	private JButton getStartButton() {
		if (startButton == null) {
			startButton = new JButton();
			startButton.setText("Start");
			startButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					stopButton.setEnabled(true);
					startButton.setEnabled(false);
					getFreqTextField().setEnabled(false);
					getTimeSampTextField().setEnabled(false);
					try{
						Integer.parseInt(getFreqTextField().getText());
					}catch(NumberFormatException ex){
						getFreqTextField().setText("100");
					}
					toFile.initialize(Integer.parseInt(getFreqTextField().getText()));
					startSample();
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
			stopButton = new JButton("Stop");
			stopButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(ActionEvent arg0) {
					
					// Stop the samples
					stopSample();
					
					// Enable/disable buttons
					timeSampTextField.setEnabled(true);
					freqTextField.setEnabled(true);
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
		}
		return timeSampLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Sampling Time for the Sampling Group.<br>
	 * By default the value is 0, which means infinite time (or until stop button is pressed).<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */ 
	private JTextField getTimeSampTextField() {
		if(timeSampTextField == null){
			timeSampTextField = new JTextField();
			timeSampTextField.setText("0");
			timeSampTextField.setMinimumSize(new Dimension(50,19));
			timeSampTextField.addFocusListener(new FocusListener() {

				public void focusGained(FocusEvent e) {
					// Do nothing special :)
				}

				public void focusLost(FocusEvent e) {
					if( !timeSampTextField.getText().matches("^([0-9])+$") ) {
						JOptionPane.showConfirmDialog(BeanGrouper.this,
								"Sampling time '" + timeSampTextField.getText() + "' is invalid.\nPlease use only numeric characters.",
								"Invalid sampling time",
								JOptionPane.PLAIN_MESSAGE,JOptionPane.WARNING_MESSAGE);
						timeSampTextField.setText("0");
						timeSampTextField.grabFocus();
					}
				}
			});
		}
		return timeSampTextField;
	}
	
	/**
	 * Initializes the Label that says "Time Window:"
	 * @return javax.swing.JLabel Reference to the Label.
	 */
	private JLabel getTimeWindowLabel() {
		if(timeWindowLabel==null){
			timeWindowLabel=new JLabel();
			timeWindowLabel.setText("Time Window (min):");
		}
		return timeWindowLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Time Window for the Sampling Group.<br>
	 * By default the value is 10, which means 10 minutes.<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */ 
	private JTextField getTimeWindowTextField() {
		if(timeWindowTextField == null){
			timeWindowTextField = new JTextField();
			timeWindowTextField.setText("0");
			timeWindowTextField.setMinimumSize(new Dimension(50,19));
			timeWindowTextField.addFocusListener(new FocusListener() {

				public void focusGained(FocusEvent e) {
					// Do nothing special :)
				}

				public void focusLost(FocusEvent e) {
					if( !timeWindowTextField.getText().matches("^([0-9])+$") ) {
						JOptionPane.showConfirmDialog(BeanGrouper.this,
								"Time window '" + timeWindowTextField.getText() + "' is invalid.\nPlease use only numeric characters.",
								"Invalid time window.",
								JOptionPane.PLAIN_MESSAGE,JOptionPane.WARNING_MESSAGE);
						timeWindowTextField.setText("0");
						timeWindowTextField.grabFocus();
					}
				}
			});
		}
		return timeWindowTextField;
	}

	/**
	 * Initializes the Label that says "Sampling To:"
	 * @return javax.swing.JLabel Reference to the Label.
	 */
	private JLabel getFileNameLabel(){
		if(fileNameLabel == null){
			fileNameLabel = new JLabel();
			fileNameLabel.setText("Sampling to:");
			//jLabel.setText(toFile.getFileName());
		}
		return fileNameLabel;
	}
	
	/**
	 * Sets the File Name where the information will be stored.
	 * @param text String containing the filename.
	 */
	private void setFileNameLabel(String text){
		fileNameLabel.setText(text);
	}
	
	/**
	 * Initializes the Frequency Label
	 * @return javax.swing.JLabel Reference to the Label says "Frequency".
	 */
	private JLabel getFrequencyLabel() {
		if(frecuencyLabel==null){
			frecuencyLabel=new JLabel();
			frecuencyLabel.setText("Frequency (Hz): ");
		}
		return frecuencyLabel;
	}
	
	/**
	 * Initializes the TextField that will allow to input the desired Frequency for the Sampling Group.<br>
	 * By default the value is 10 Hz.<br>
	 * Also checks for its correctness when the value changes.
	 * @return javax.swing.JTextField Reference to the Text Field containing the number.
	 */
	private JTextField getFreqTextField() {
		if(freqTextField==null){
			freqTextField=new JTextField();
			freqTextField.setText("10");
			freqTextField.setMinimumSize( new Dimension(50, 19) );

			freqTextField.addFocusListener(new FocusListener() {

				public void focusGained(FocusEvent e) {
					// Do nothing special :)
				}

				public void focusLost(FocusEvent e) {
					if( !freqTextField.getText().matches("^([0-9])+$") ) {
						JOptionPane.showConfirmDialog(BeanGrouper.this,
								"Frequency value '" + freqTextField.getText() + "' is invalid.\nPlease use only numeric characters.",
								"Invalid frequency value",
								JOptionPane.PLAIN_MESSAGE,JOptionPane.WARNING_MESSAGE);
						freqTextField.setText("100");
						freqTextField.grabFocus();
					}
				}
			});
		}
		return freqTextField;
	}

	/**
	 * This method initializes jPanel, which will contains the BeanLister widgets (in this case, Plots)
	 * 	
	 * @return javax.swing.JPanel Reference to the JPanel containing the BeanLister.
	 */
	private JPanel getJPanel() {
		if (jPanel == null) {
			jPanel = new JPanel();
			jPanel.setLayout( new GridBagLayout() );
		}
		return jPanel;
	}

	/**
	 * This method adds a new sample (represented as a {@link DataPrinter}) into this class.<br>
	 * This class should be able to hold N samples inside itself. <br>
	 * - First, the {@link BeanLister} widget contained by the {@link DataPrinter} is added to the jPanel<br>
	 * - Second, the {@link DataPrinter} is added to the list of samplers.
	 * @param w The Printer Object that is used to represent a sampling by its own.
	 */
	private void addSamp(DataPrinter w){
		GridBagConstraints tmp = new GridBagConstraints();
		tmp.anchor = GridBagConstraints.NORTHWEST;
		tmp.fill = GridBagConstraints.HORIZONTAL;
		tmp.gridy = jPanel.getComponentCount();
		tmp.weightx = 1;
		//Only add one BeanLister, as the other ones, will be the same.
		if( samplers.size() == 0 ){
			jPanel.add( (JPanel)w.getWidget(), tmp);
		}		
		updateLabel();
		jPanel.validate();
		samplers.add(w);
	}
	
	/**
	 * This method ads to the Sampling Group, a new sample. This method is used by <br>
	 * {@link SamplingSystemGUI} to add Sampling to the Sampling Group, represented by {@link BeanGrouper}.<br>
	 * - Creates a {@link PlotPrinter}, which extends from {@link DataPrinter}.
	 * - Set the Components and Properties to the PlotPrinter
	 * - Adds this new Sample to the Sampling Group using the other addSamp method. 
	 * @param component Name of the Component containing the property to be sampled.
	 * @param property Name of the Property to be sampled.
	 */
	public void addSamp(String component, String property) {
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
		addSamp(w);
		
	}
	
	public void updateLabel(){
	}
	
	/**
	 * Method to begin the sampling of all the samples contained in the class.
	 *
	 */
	private void startSample(){

		int freq=0;
		try {
			freq = Integer.parseInt(getFreqTextField().getText());
		} catch (NumberFormatException ex) {
			// Shouldn't happen
		}
		
		startTimestamp = new Date();
		
		setFileNameLabel("Sampling to file: " + toFile.getFileName());
		freq = Integer.parseInt(getFreqTextField().getText());
		for(DataPrinter wp : samplers){
			wp.setFrecuency(freq);
			try {
				wp.startSample();
				isStopped = false;
			} catch(alma.ACSErrTypeCommon.CouldntAccessComponentEx e) {
				wp.setComponentAvailable(false,"Cannot access component");
			} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
				wp.setComponentAvailable(false,"Type not supported");
			}
		}

		if( isStopped ) {
			stopButton.setEnabled(false);
			startButton.setEnabled(true);
			freqTextField.setEnabled(true);
			timeSampTextField.setEnabled(true);
			return;
		}
		
		int mins=0;
		try{
			mins = Integer.parseInt(getTimeSampTextField().getText());
		}catch(NumberFormatException ex){
			getTimeSampTextField().setText("0");
		}
		
		if(mins!=0)
			new Watchdog(mins).start();
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
		
		for (Iterator iter = samplers.iterator(); iter.hasNext();) {
			DataPrinter element = (DataPrinter) iter.next();
			if( element.component == component &&
				element.property  == property )
				return true;
		}
		return false;
	}

	private void stopSample(){
		isStopped=true;
		stopButton.setEnabled(false);
		startButton.setEnabled(true);
		
//		String header ="Time";
//		toFile.removeSamplingSets();
		SamplingDataCorrelator sdc = new SamplingDataCorrelator(group, Integer.parseInt(getFreqTextField().getText()), startTimestamp);
		for(DataPrinter i : samplers) {
			i.stopSampling();
			if( i.isComponentAvailable() == true ) {
				sdc.addSamplingSet(((PlotPrinter)i).getFilename());
				//System.out.println("Adding: " + ((FilePrinter)i).getFilename());
			}
//				toFile.addSamplingSet(i.getSamples());
//				header+=";"+i.getComponent()+"."+i.getProperty();
//			}
		}
		sdc.dumpToFile();
//		toFile.setHeaderFile(header);
//		toFile.dumpToFile(Integer.parseInt(getFreqTextField().getText()));
	}

	/**
	 * Class in charge of stopping the Sampling for all the DataPrinter objects that composes the BeanGrouper<br />
	 * A parameters consistings in minutes has to be passed to the constructor, and after that time has passed <br />
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

	@Override
	public void windowActivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosed(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowIconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowOpened(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void windowClosing(WindowEvent e) {
        if(!isStopped)
        	stopSample();
		ssg.deleteBeanGrouper(samplers,getGroupName());
    }
}
